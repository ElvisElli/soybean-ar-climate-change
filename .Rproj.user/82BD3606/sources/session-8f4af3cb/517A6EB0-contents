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

## ── Test 3: if file exists in workers, try reading it ───────────────────────
cat("\n=== TEST 3: read one .met file in a worker ===\n")
cl3 <- makeCluster(1L); registerDoParallel(cl3)
res3 <- foreach(cid = cells[1], .packages = "apsimx",
                .export = c("WEATHER_DIR"), .errorhandling = "pass") %dopar% {
  mp  <- file.path(WEATHER_DIR, paste0(cid, ".met"))
  if (!file.exists(mp)) return(list(ok = FALSE, msg = paste("not found:", mp)))
  met <- tryCatch(read_apsim_met(mp, verbose = FALSE),
                  error = function(e) list(err = e$message))
  if (is.list(met) && !is.null(met$err))
    return(list(ok = FALSE, msg = met$err))
  list(ok = TRUE, nrow = nrow(as.data.frame(met)),
       cols = names(as.data.frame(met)))
}[[1]]
stopCluster(cl3)
cat(sprintf("  ok=%s | %s\n", res3$ok,
            if (res3$ok) paste("rows:", res3$nrow, "| cols:", paste(res3$cols, collapse=","))
            else res3$msg))

cat("\nDiagnostic complete.\n")
