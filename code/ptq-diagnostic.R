## ptq-diagnostic.R
## Run this from the repo root to diagnose why PTQ workers return NULL.
## Tests 3 cells, 1 core (sequential), returns detailed info from inside each worker.

suppressPackageStartupMessages({
  library(apsimx)
  library(dplyr)
  library(doParallel)
  library(foreach)
})

LOCAL_DATA_CACHE <- "C:/temp/soybean-data"
WEATHER_DIR <- NULL
for (p in c(file.path(LOCAL_DATA_CACHE, "weather"),
            "intermediate-data/weather", "data/raw/weather")) {
  if (!is.null(p) && dir.exists(p)) { WEATHER_DIR <- normalizePath(p); break }
}
cat("WEATHER_DIR:", WEATHER_DIR, "\n")
cat(".met files :", length(list.files(WEATHER_DIR, "\\.met$")), "\n\n")

sim <- readRDS("data/outputs/simulated-scenarios-df.rds")
if (is.list(sim) && !is.data.frame(sim))
  sim <- dplyr::bind_rows(Filter(Negate(is.null), sim))
cells     <- unique(sim$cellid)[1:3]
sim_split <- split(as.data.frame(sim), sim$cellid)
cat("Testing cells:", paste(cells, collapse = ", "), "\n\n")

## ── Test 1: sequential (no cluster) ─────────────────────────────────────────
cat("=== TEST 1: sequential %do% ===\n")
cl1 <- makeCluster(1L); registerDoParallel(cl1)
res1 <- foreach(cid = cells, cell_rows = sim_split[as.character(cells)],
                .packages = "apsimx", .export = c("WEATHER_DIR"),
                .errorhandling = "pass") %do% {
  mp <- file.path(WEATHER_DIR, paste0(cid, ".met"))
  list(cid = cid, path = mp, exists = file.exists(mp),
       wd = getwd(), nrows = nrow(cell_rows))
}
stopCluster(cl1)
for (r in res1) cat(sprintf("  cell %d | exists=%s | nrows=%d | path=%s\n",
                             r$cid, r$exists, r$nrows, r$path))

## ── Test 2: parallel, 4 cores ────────────────────────────────────────────────
cat("\n=== TEST 2: parallel %dopar%, 4 cores ===\n")
cl2 <- makeCluster(4L); registerDoParallel(cl2)
res2 <- foreach(cid = cells, cell_rows = sim_split[as.character(cells)],
                .packages = "apsimx", .export = c("WEATHER_DIR"),
                .errorhandling = "pass") %dopar% {
  mp <- file.path(WEATHER_DIR, paste0(cid, ".met"))
  list(cid = cid, path = mp, exists = file.exists(mp),
       wd = getwd(), nrows = nrow(cell_rows))
}
stopCluster(cl2)
for (r in res2) {
  if (inherits(r, "condition"))
    cat(sprintf("  cell ERROR: %s\n", conditionMessage(r)))
  else
    cat(sprintf("  cell %d | exists=%s | nrows=%d | wd=%s\n",
                r$cid, r$exists, r$nrows, r$wd))
}

## ── Test 3: capture read_apsim_met error message from inside a worker ────────
cat("\n=== TEST 3: read_apsim_met in a worker (captures error message) ===\n")
cl3 <- makeCluster(1L); registerDoParallel(cl3)
res3 <- foreach(cid = cells[1], .packages = "apsimx",
                .export = c("WEATHER_DIR"), .errorhandling = "pass") %dopar% {
  mp  <- normalizePath(file.path(WEATHER_DIR, paste0(cid, ".met")), mustWork = FALSE)
  if (!file.exists(mp)) return(list(ok = FALSE, step = "file.exists", msg = mp))
  read_err <- NULL
  met <- tryCatch(
    read_apsim_met(mp, verbose = FALSE),
    error   = function(e) { read_err <<- e$message; NULL },
    warning = function(w) { read_err <<- paste("WARNING:", w$message); NULL }
  )
  if (is.null(met))
    return(list(ok = FALSE, step = "read_apsim_met", msg = read_err))
  df_err <- NULL
  met_df <- tryCatch(
    as.data.frame(met),
    error = function(e) { df_err <<- e$message; NULL }
  )
  if (is.null(met_df))
    return(list(ok = FALSE, step = "as.data.frame", msg = df_err))
  list(ok = TRUE, nrow = nrow(met_df), cols = names(met_df))
}[[1]]
stopCluster(cl3)
if (is.null(res3)) {
  cat("  Worker returned NULL with no error captured (foreach result empty)\n")
} else if (isTRUE(res3$ok)) {
  cat(sprintf("  OK — rows: %d | cols: %s\n", res3$nrow, paste(res3$cols, collapse = ", ")))
} else {
  cat(sprintf("  FAILED at step '%s': %s\n", res3$step, res3$msg))
}

## ── Test 4: read .met sequentially in main process ──────────────────────────
cat("\n=== TEST 4: read_apsim_met in MAIN process (sanity check) ===\n")
mp4 <- normalizePath(file.path(WEATHER_DIR, paste0(cells[1], ".met")), mustWork = FALSE)
met4 <- tryCatch(read_apsim_met(mp4, verbose = FALSE), error = function(e) e)
if (inherits(met4, "error")) {
  cat("  FAILED:", met4$message, "\n")
} else {
  df4 <- as.data.frame(met4)
  cat(sprintf("  OK — rows: %d | cols: %s\n", nrow(df4), paste(names(df4), collapse = ", ")))
}

cat("\nDiagnostic complete.\n")
