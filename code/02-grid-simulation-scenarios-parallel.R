## ============================================================
## Grid simulation — soybean AR climate change
## Auto-detects cloud vs. local Windows machine.
##
## Key design:
##   - Cluster started ONCE, reused across all scenarios
##   - Static data exported to workers once at startup
##   - Per-chunk RDS checkpointing (fully resumable)
##   - Per-cell isolated subdirectory prevents .db race conditions
##   - Progress log CSV + end-of-run summary report
## ============================================================

rm(list = ls())

## ── Working directory ────────────────────────────────────────
## Works whether sourced in RStudio or via Rscript from command line.
if (interactive() &&
    requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()) {
  doc <- rstudioapi::getActiveDocumentContext()$path
  if (nchar(doc) > 0) {
    setwd(dirname(doc))   # code/
    setwd("..")           # repo root
  }
}
if (!file.exists("intermediate-data/sim-grid.rds"))
  stop("Working directory must be the repo root.\n",
       "Current dir: ", getwd(), "\n",
       "Run from RStudio with the script open, or:\n",
       "  Rscript code/02-grid-simulation-scenarios-parallel.R")

suppressPackageStartupMessages({
  library(apsimx)
  library(doParallel)
  library(foreach)
  library(dplyr)
  library(readr)
  library(readxl)
  library(parallel)
})

## ── Settings ─────────────────────────────────────────────────
CHUNK_SIZE <- 50          # cells per parallel task
DATE_START <- "1985-01-01"
DATE_END   <- "2024-12-31"

## ── Test mode ────────────────────────────────────────────────
## Set TEST_RUN <- TRUE for a quick validation before the full run.
TEST_RUN         <- TRUE
TEST_N_CELLS     <- 10
TEST_DATE_START  <- "2015-01-01"
TEST_DATE_END    <- "2020-12-31"
TEST_N_SCENARIOS <- 1

## ── Notifications ────────────────────────────────────────────
## One-time setup in RStudio console (Windows):
##   install.packages(c("blastula", "keyring", "DBI", "RSQLite"))
##   blastula::create_smtp_creds_key(
##     id = "gmail", user = "elvisfelipeelli@gmail.com", provider = "gmail"
##   )
##   (enter your Gmail App Password when prompted)
NOTIFY    <- TRUE
NOTIFY_TO <- "5157156541@vtext.com"        # Verizon SMS gateway
NOTIFY_FROM <- "elvisfelipeelli@gmail.com"

## ── Diagnostic mode ──────────────────────────────────────────
## Runs one cell sequentially with full APSIM output visible.
## Set TRUE, source the script, inspect output, then set back to FALSE.
RUN_DIAGNOSTIC <- FALSE

## ── Environment detection ────────────────────────────────────
detect_env <- function() {
  host <- tolower(Sys.info()[["nodename"]])
  os   <- .Platform$OS.type

  if (os == "windows") {
    local_app  <- Sys.getenv("LOCALAPPDATA",
                              file.path(Sys.getenv("USERPROFILE"), "AppData", "Local"))
    apsim_dirs <- sort(grep("APSIM",
                             list.dirs(file.path(local_app, "Programs"), recursive = FALSE),
                             value = TRUE, ignore.case = TRUE))
    if (length(apsim_dirs) == 0)
      stop("[ERROR] APSIM not found under ", file.path(local_app, "Programs"))
    apsim_exe <- file.path(tail(apsim_dirs, 1), "bin", "Models.exe")

    ## Box auto-detection — machine-agnostic
    user_home  <- Sys.getenv("USERPROFILE", path.expand("~"))
    box_suffix <- file.path("_Projects", "Scale-Sims",
                            "soybean-ar-climate-change", "intermediate-data")
    box_mounts <- c("Box", "Box Sync", "Box Drive")
    candidates <- unlist(lapply(box_mounts, function(m) file.path(user_home, m, box_suffix)))
    users_dir  <- dirname(user_home)
    if (dir.exists(users_dir)) {
      other_homes <- list.dirs(users_dir, recursive = FALSE)
      candidates  <- c(candidates, unlist(lapply(other_homes, function(h)
        unlist(lapply(box_mounts, function(m) file.path(h, m, box_suffix))))))
    }
    box_root <- Filter(dir.exists, unique(candidates))

    n_cores <- max(1L, parallel::detectCores(logical = FALSE) - 2L)
    message("[ENV] Windows : ", host, " | cores: ", n_cores)
    message("[ENV] APSIM   : ", apsim_exe)
    message("[ENV] Box     : ", if (length(box_root) > 0) box_root[[1]] else "NOT FOUND")

    list(apsim_exe  = apsim_exe,
         box_root   = if (length(box_root) > 0) box_root[[1]] else NULL,
         cores_use  = n_cores,
         is_windows = TRUE)
  } else {
    ## Linux / cloud — APSIM installed system-wide
    apsim_bin <- Sys.which("Models")
    if (nchar(apsim_bin) == 0) {
      candidate <- "/usr/local/lib/apsim/2025.3.7681.0/bin/Models"
      apsim_bin <- if (file.exists(candidate)) candidate else ""
    }
    message("[ENV] Linux/cloud : ", host,
            " | APSIM: ", if (nchar(apsim_bin) > 0) apsim_bin else "not found")
    list(apsim_exe  = if (nchar(apsim_bin) > 0) apsim_bin else NULL,
         box_root   = NULL,
         cores_use  = max(1L, parallel::detectCores(logical = FALSE) - 1L),
         is_windows = FALSE)
  }
}

ENV <- detect_env()

## ── APSIM exe ────────────────────────────────────────────────
if (!is.null(ENV$apsim_exe)) {
  if (!file.exists(ENV$apsim_exe))
    stop("[ERROR] APSIM not found: ", ENV$apsim_exe)
  apsimx_options(exe.path = ENV$apsim_exe)
}

## ── Data paths ───────────────────────────────────────────────
intermediate_data <- if (!is.null(ENV$box_root)) {
  ENV$box_root
} else {
  normalizePath("intermediate-data", mustWork = FALSE)
}

weather_path   <- normalizePath(file.path(intermediate_data, "weather"), mustWork = FALSE)
soil_path      <- normalizePath(file.path(intermediate_data, "soil"),    mustWork = FALSE)
checkpoint_dir <- normalizePath("intermediate-data/sim-chunks",          mustWork = FALSE)
log_file       <- normalizePath("intermediate-data/sim-run-log.csv",     mustWork = FALSE)

message("[PATHS] Weather  : ", weather_path)
message("[PATHS] Soil     : ", soil_path)
message("[PATHS] Chunks   : ", checkpoint_dir)

dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)

if (!dir.exists(weather_path)) stop("[ERROR] Weather directory not found: ", weather_path)
if (!dir.exists(soil_path))    stop("[ERROR] Soil directory not found: ",    soil_path)

n_met <- length(list.files(weather_path, "\\.met$"))
n_rds <- length(list.files(soil_path,    "\\.rds$"))
message(sprintf("[CHECK] Weather files: %d | Soil files: %d", n_met, n_rds))

## ── Load grid & scenarios ────────────────────────────────────
sim.grid        <- readRDS("intermediate-data/sim-grid.rds")
sim.grid$cellid <- seq_len(nrow(sim.grid))
sim.grid1       <- dplyr::filter(sim.grid, !is.na(cultivated))

scenarios <- read_excel("intermediate-data/scenarios/soy-scenarios-10-24.xlsx",
                        sheet = "Sheet1") %>% as.data.frame()

## ── Apply test limits ────────────────────────────────────────
if (TEST_RUN) {
  set.seed(42)
  sim.grid1  <- sim.grid1[sample(nrow(sim.grid1), min(TEST_N_CELLS, nrow(sim.grid1))), ]
  scenarios  <- scenarios[seq_len(min(TEST_N_SCENARIOS, nrow(scenarios))), ]
  DATE_START <- TEST_DATE_START
  DATE_END   <- TEST_DATE_END
  CHUNK_SIZE <- max(1L, ceiling(nrow(sim.grid1) / 2L))
  message(sprintf("[TEST] %d cells | %s to %s | %d scenario(s)",
                  nrow(sim.grid1), DATE_START, DATE_END, nrow(scenarios)))
}

message(sprintf("[INFO] Grid cells : %d", nrow(sim.grid1)))
message(sprintf("[INFO] Scenarios  : %d", nrow(scenarios)))
message(sprintf("[INFO] Cores      : %d", ENV$cores_use))
message(sprintf("[INFO] Chunk size : %d | Estimated chunks/sc: %d",
                CHUNK_SIZE, ceiling(nrow(sim.grid1) / CHUNK_SIZE)))

## ── APSIM working directory ───────────────────────────────────
base_apsimx <- normalizePath("processed-data/_soybean-10-24-25.apsimx", mustWork = FALSE)
if (!file.exists(base_apsimx))
  stop("[ERROR] APSIM template not found: ", base_apsimx)

## Cell working dirs live alongside the template in processed-data/.
## On Linux use intermediate-data/apsim-work/ (keeps the repo cleaner).
apsim_dir <- if (ENV$is_windows) {
  normalizePath("processed-data", mustWork = FALSE)
} else {
  normalizePath("intermediate-data/apsim-work", mustWork = FALSE)
}
dir.create(apsim_dir, recursive = TRUE, showWarnings = FALSE)
message("[PATHS] APSIM work : ", apsim_dir)

## ── Root parameters (depth-decay) ────────────────────────────
KL_VEC <- c(0.08, 0.08, 0.08, 0.08, 0.07, 0.07, 0.07, 0.07,
            0.06, 0.06, 0.06, 0.06, 0.05, 0.05, 0.04, 0.04,
            0.03, 0.03, 0.02, 0.02)
XF_VEC <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0)

## ── Helper: prepare soil profile ─────────────────────────────
prepare_soil <- function(soil_rds_path, KL_VEC, XF_VEC) {
  soil.result <- tryCatch(readRDS(soil_rds_path), error = function(e) NULL)
  if (is.null(soil.result) ||
      !is.list(soil.result[[1]]) ||
      !inherits(soil.result[[1]][[1]], "soil_profile"))
    return(NULL)
  soils <- soil.result[[1]][[1]]
  KS_max        <- max(soils$soil$KS, na.rm = TRUE)
  soils$soil$KS <- KS_max * exp(seq(0, log(1e-4), length.out = length(soils$soil$KS)))
  soils <- apsimx:::fix_apsimx_soil_profile(soils, verbose = FALSE)
  soils$initialwater <- initialwater_parms(
    Depth = soils$soil$Depth, Thickness = soils$soil$Thickness,
    InitialValues = soils$soil$DUL)
  soils$crops <- c("Soybean", "Wheat", "Maize")
  n_lay <- nrow(soils$soil)
  list(soils = soils,
       KL    = KL_VEC[seq_len(min(n_lay, length(KL_VEC)))],
       XF    = XF_VEC[seq_len(min(n_lay, length(XF_VEC)))])
}

## ── Helper: assemble result columns ──────────────────────────
extract_sim_columns <- function(sim, sc_row, grid_row) {
  sc_cols <- data.frame(
    cultivar        = sc_row$cultivar,
    sowing          = sc_row$sowing,
    scenario        = sc_row$scenario,
    climate.control = sc_row$climate.control,
    co2             = sc_row$co2,
    rowSpacing      = sc_row$RowSpacing
  )
  keep <- c("Date",
            "EmergenceDAS", "FloweringDAS", "SeedFillingDAS", "MaturityDAS",
            "CumRadiationInterceptionOnGreen",
            "Yield_kgha", "biomass_kgha",
            "SeasonRain", "SeasonRadn",
            "SeasonMaxt", "SeasonMint", "SeasonMeanT", "BloomingSeasonMaxt",
            "Silking_RUE_Temp", "Silking_Supply_Demand_Ratio",
            "swhc_6in", "swhc_12in", "swhc_24in",
            "Crop_ET", "WDrainage", "WRunoff", "sWUE")
  sim_cols <- sim[, intersect(names(sim), keep), drop = FALSE]
  n <- nrow(sim)
  cbind(grid_row[rep(1L, n), , drop = FALSE],
        sc_cols[rep(1L, n), , drop = FALSE],
        sim_cols,
        row.names = NULL)
}

## ── Notification helper ───────────────────────────────────────
send_notification <- function(subject, body) {
  if (!NOTIFY) return(invisible(NULL))
  tryCatch({
    if (!requireNamespace("blastula", quietly = TRUE))
      stop("blastula not installed")
    email <- blastula::compose_email(body = blastula::md(body))
    blastula::smtp_send(email,
                        from        = NOTIFY_FROM,
                        to          = NOTIFY_TO,
                        subject     = subject,
                        credentials = blastula::creds_key("gmail"))
    message("[NOTIFY] Sent: ", subject)
  }, error = function(e) message("[NOTIFY] Failed — ", e$message))
}

## ── Diagnostic mode ──────────────────────────────────────────
if (RUN_DIAGNOSTIC) {
  cat("\n[DIAG] Single-cell sequential test\n")
  sc  <- scenarios[1, ]
  j   <- sim.grid1$cellid[1]

  file.copy(base_apsimx, file.path(apsim_dir, "grid-simulation-file.apsimx"), overwrite = TRUE)
  for (pv in list(list("Start", DATE_START), list("End", DATE_END)))
    edit_apsimx("grid-simulation-file.apsimx", apsim_dir, apsim_dir,
                node = "Clock", parm = pv[[1]], value = pv[[2]],
                overwrite = TRUE, verbose = FALSE)

  for (pe in list(
    list(".Simulations.Simulation.Field.SowSoybean.CultivarName",     sc$cultivar),
    list(".Simulations.Simulation.Field.SowSoybean.SowDate",          sc$sowing),
    list(".Simulations.Simulation.Field.ClimateController.EnableDate", sc$climate.control),
    list(".Simulations.Simulation.Field.SowSoybean.RowSpacing",       sc$RowSpacing),
    list(".Simulations.Simulation.Field.CO2.CO2",                     sc$co2)
  ))
    edit_apsimx("grid-simulation-file.apsimx", apsim_dir, apsim_dir,
                node = "Other", parm.path = pe[[1]], value = pe[[2]],
                overwrite = TRUE, verbose = FALSE)

  sp <- prepare_soil(file.path(soil_path, paste0(j, ".rds")), KL_VEC, XF_VEC)
  if (is.null(sp)) stop("[DIAG] Soil load failed for cell ", j)

  diag_dir <- file.path(apsim_dir, paste0("diag-", j))
  dir.create(diag_dir, showWarnings = FALSE, recursive = TRUE)
  file.copy(file.path(apsim_dir, "grid-simulation-file.apsimx"),
            file.path(diag_dir, "sim.apsimx"), overwrite = TRUE)

  edit_apsimx_replace_soil_profile("sim.apsimx", diag_dir, diag_dir,
    soil.profile = sp$soils, verbose = FALSE, overwrite = TRUE)
  edit_apsimx("sim.apsimx", diag_dir, diag_dir, node = "Soil",
    soil.child = "Physical", parm = "KL", value = sp$KL,
    verbose = FALSE, overwrite = TRUE)
  edit_apsimx("sim.apsimx", diag_dir, diag_dir, node = "Soil",
    soil.child = "Physical", parm = "XF", value = sp$XF,
    verbose = FALSE, overwrite = TRUE)

  met_path <- normalizePath(file.path(weather_path, paste0(j, ".met")), mustWork = FALSE)
  cat("[DIAG] Weather path:", met_path, "| exists:", file.exists(met_path), "\n")
  edit_apsimx("sim.apsimx", diag_dir, diag_dir,
    node = "Weather", value = met_path, overwrite = TRUE, verbose = FALSE)

  cat("[DIAG] Running APSIM for cell", j, "...\n")
  sim_diag <- tryCatch(
    apsimx("sim.apsimx", src.dir = diag_dir, cleanup = FALSE, silent = FALSE),
    error = function(e) { cat("[DIAG] ERROR:", e$message, "\n"); NULL }
  )
  cat("[DIAG] Files after run:", paste(list.files(diag_dir), collapse = ", "), "\n")
  if (!is.null(sim_diag) && nrow(sim_diag) > 0) {
    cat(sprintf("[DIAG] SUCCESS — %d rows | Yield[1] = %.0f kg/ha\n",
                nrow(sim_diag), sim_diag$Yield_kgha[1]))
  } else {
    cat("[DIAG] APSIM returned NULL/empty — .apsimx kept at:", diag_dir, "\n")
    cat("[DIAG] Open it in APSIM GUI to see the error.\n")
  }
  stop("[DIAG] Done — set RUN_DIAGNOSTIC <- FALSE to run normally.")
}

## ── Start cluster ─────────────────────────────────────────────
n_workers <- min(ENV$cores_use, nrow(sim.grid1))
cat(sprintf("\n[CLUSTER] Starting %d workers (PSOCK)\n", n_workers))
cl <- makeCluster(n_workers, type = "PSOCK", outfile = "")

tryCatch({
  registerDoParallel(cl)

  clusterExport(cl,
    c("ENV", "KL_VEC", "XF_VEC",
      "prepare_soil", "extract_sim_columns",
      "apsim_dir", "weather_path", "soil_path", "checkpoint_dir"),
    envir = environment())

  clusterEvalQ(cl, {
    suppressPackageStartupMessages({
      library(apsimx)
      library(dplyr)
      library(DBI)
      library(RSQLite)
    })
    if (!is.null(ENV$apsim_exe) && file.exists(ENV$apsim_exe))
      apsimx_options(exe.path = ENV$apsim_exe)
  })

  ## ── Scenario loop ─────────────────────────────────────────
  final.df    <- vector("list", nrow(scenarios))
  run_started <- Sys.time()

  for (i in seq_len(nrow(scenarios))) {
    sc    <- scenarios[i, ]
    sc_t0 <- proc.time()[["elapsed"]]

    cat(sprintf("\n====================================================\n"))
    cat(sprintf("Scenario %d/%d : %-30s | CO2=%s | %s\n",
                i, nrow(scenarios), sc$scenario, sc$co2,
                format(Sys.time(), "%H:%M:%S")))
    cat(sprintf("====================================================\n"))

    ## Update template with scenario parameters
    file.copy(base_apsimx, file.path(apsim_dir, "grid-simulation-file.apsimx"),
              overwrite = TRUE)
    for (pv in list(list("Start", DATE_START), list("End", DATE_END)))
      edit_apsimx("grid-simulation-file.apsimx", apsim_dir, apsim_dir,
                  node = "Clock", parm = pv[[1]], value = pv[[2]],
                  overwrite = TRUE, verbose = FALSE)
    for (pe in list(
      list(".Simulations.Simulation.Field.SowSoybean.CultivarName",     sc$cultivar),
      list(".Simulations.Simulation.Field.SowSoybean.SowDate",          sc$sowing),
      list(".Simulations.Simulation.Field.ClimateController.EnableDate", sc$climate.control),
      list(".Simulations.Simulation.Field.SowSoybean.RowSpacing",       sc$RowSpacing),
      list(".Simulations.Simulation.Field.CO2.CO2",                     sc$co2)
    ))
      edit_apsimx("grid-simulation-file.apsimx", apsim_dir, apsim_dir,
                  node = "Other", parm.path = pe[[1]], value = pe[[2]],
                  overwrite = TRUE, verbose = FALSE)

    ## Resume: find already-completed cells from saved chunk filenames
    chunk_pattern   <- sprintf("^chunk_sc%02d_ck", i)
    done_files      <- list.files(checkpoint_dir, pattern = chunk_pattern)
    done_cells_full <- if (length(done_files) > 0) {
      unlist(lapply(file.path(checkpoint_dir, done_files), function(f) readRDS(f)$cellid))
    } else {
      integer(0)
    }

    todo_rows <- sim.grid1[!sim.grid1$cellid %in% done_cells_full, ]
    cat(sprintf("[INFO] Done: %d cells (%d chunks) | Remaining: %d cells\n",
                length(done_cells_full), length(done_files), nrow(todo_rows)))

    if (nrow(todo_rows) == 0) {
      cat("[INFO] Scenario complete — loading from checkpoints.\n")
      final.df[[i]] <- dplyr::bind_rows(lapply(
        file.path(checkpoint_dir,
                  list.files(checkpoint_dir, pattern = sprintf("^chunk_sc%02d_", i))),
        readRDS))
      next
    }

    chunks    <- split(seq_len(nrow(todo_rows)),
                       ceiling(seq_len(nrow(todo_rows)) / CHUNK_SIZE))
    ci_offset <- length(done_files)

    sc_row <- sc
    clusterExport(cl, c("todo_rows", "sc_row", "i", "chunks", "ci_offset"),
                  envir = environment())

    ## ── Parallel chunk processing ──────────────────────────
    chunk_summaries <- foreach(
      ci             = seq_along(chunks),
      .errorhandling = "pass"
    ) %dopar% {

      t0       <- proc.time()[["elapsed"]]
      idx      <- chunks[[ci]]
      sub_rows <- todo_rows[idx, ]
      n_cells  <- nrow(sub_rows)
      res_list <- vector("list", n_cells)
      n_ok     <- 0L
      n_fail   <- 0L
      err_msgs <- character(0)

      for (k in seq_len(n_cells)) {
        j        <- sub_rows$cellid[k]
        grid_row <- sub_rows[k, , drop = FALSE]

        if (k == 1L || k %% 50L == 0L)
          cat(sprintf("[Worker] Sc %d | Chunk %d | Cell %d/%d (cell %d)\n",
                      i, ci + ci_offset, k, n_cells, j))

        ## ── Soil ────────────────────────────────────────
        sp <- prepare_soil(file.path(soil_path, paste0(j, ".rds")), KL_VEC, XF_VEC)
        if (is.null(sp)) {
          n_fail   <- n_fail + 1L
          err_msgs <- c(err_msgs, paste0("cell ", j, ": soil load failed"))
          next
        }

        ## ── Weather path ────────────────────────────────
        met_path <- normalizePath(file.path(weather_path, paste0(j, ".met")),
                                  mustWork = FALSE)
        if (!file.exists(met_path)) {
          n_fail   <- n_fail + 1L
          err_msgs <- c(err_msgs, paste0("cell ", j, ": weather file missing — ", met_path))
          next
        }

        ## ── Build per-cell APSIM file ────────────────────
        ## Each cell gets its own subdirectory so cleanup=TRUE never
        ## touches another worker's .db (isolated I/O, no race condition).
        cell_dir <- file.path(apsim_dir, paste0("cell-", j))
        dir.create(cell_dir, showWarnings = FALSE)

        built <- tryCatch({
          file.copy(file.path(apsim_dir, "grid-simulation-file.apsimx"),
                    file.path(cell_dir, "sim.apsimx"), overwrite = TRUE)
          edit_apsimx_replace_soil_profile("sim.apsimx", cell_dir, cell_dir,
            soil.profile = sp$soils, verbose = FALSE, overwrite = TRUE)
          edit_apsimx("sim.apsimx", cell_dir, cell_dir,
            node = "Soil", soil.child = "Physical",
            parm = "KL", value = sp$KL, verbose = FALSE, overwrite = TRUE)
          edit_apsimx("sim.apsimx", cell_dir, cell_dir,
            node = "Soil", soil.child = "Physical",
            parm = "XF", value = sp$XF, verbose = FALSE, overwrite = TRUE)
          edit_apsimx("sim.apsimx", cell_dir, cell_dir,
            node = "Weather", value = met_path,
            overwrite = TRUE, verbose = FALSE)
          TRUE
        }, error = function(e) {
          err_msgs <<- c(err_msgs, paste0("cell ", j, ": build failed — ", e$message))
          FALSE
        })

        if (!built) {
          unlink(cell_dir, recursive = TRUE)
          n_fail <- n_fail + 1L
          next
        }

        ## ── Run APSIM ────────────────────────────────────
        ## On first cell of each run, print pre-run diagnostics so we can
        ## see cell_dir, whether the .apsimx was built, and what APSIM writes.
        first_cell <- (k == 1L && ci == 1L)
        if (first_cell) {
          cat(sprintf("[PRE]  cell %d | cell_dir: %s | exists: %s\n",
                      j, cell_dir, dir.exists(cell_dir)))
          cat(sprintf("[PRE]  files in cell_dir: %s\n",
                      paste(list.files(cell_dir), collapse = ", ")))
          cat(sprintf("[PRE]  tempdir: %s\n", tempdir()))
        }

        apsimx_err <- NULL
        sim <- tryCatch(
          apsimx("sim.apsimx", src.dir = cell_dir, cleanup = FALSE, silent = TRUE),
          error = function(e) { apsimx_err <<- e$message; NULL }
        )

        if (first_cell) {
          cat(sprintf("[POST] files in cell_dir after APSIM: %s\n",
                      paste(list.files(cell_dir), collapse = ", ")))
          cat(sprintf("[POST] sim rows: %s | apsimx_err: %s\n",
                      if (is.null(sim)) "NULL" else nrow(sim),
                      if (is.null(apsimx_err)) "none" else apsimx_err))
        }
        unlink(cell_dir, recursive = TRUE)

        ## ── Handle failure ───────────────────────────────
        if (is.null(sim) || nrow(sim) == 0) {
          msg <- if (!is.null(apsimx_err)) apsimx_err
                 else "APSIM returned no data (check template/soil/weather)"
          cat(sprintf("[FAIL] cell %d: %s\n", j, msg))
          n_fail   <- n_fail + 1L
          err_msgs <- c(err_msgs, paste0("cell ", j, ": ", msg))
          next
        }

        ## ── Extract columns ──────────────────────────────
        row_result <- tryCatch(
          extract_sim_columns(sim, sc_row, grid_row),
          error = function(e) {
            err_msgs <<- c(err_msgs,
              paste0("cell ", j, ": extract failed — ", e$message))
            NULL
          })

        if (!is.null(row_result)) {
          res_list[[k]] <- row_result
          n_ok <- n_ok + 1L
        } else {
          n_fail <- n_fail + 1L
        }
      }

      ## ── Save checkpoint ──────────────────────────────
      chunk_df <- dplyr::bind_rows(Filter(Negate(is.null), res_list))
      if (nrow(chunk_df) > 0)
        saveRDS(chunk_df,
                file.path(checkpoint_dir,
                          sprintf("chunk_sc%02d_ck%04d_%d.rds",
                                  i, ci + ci_offset, sub_rows$cellid[1L])))

      list(scenario    = sc_row$scenario,
           sc_idx      = i,
           co2         = sc_row$co2,
           chunk       = ci + ci_offset,
           cells_total = n_cells,
           cells_ok    = n_ok,
           cells_fail  = n_fail,
           elapsed_sec = round(proc.time()[["elapsed"]] - t0, 1),
           errors      = paste(err_msgs, collapse = "; "))
    }

    ## ── Log chunk summaries ────────────────────────────
    for (cs in chunk_summaries) {
      if (inherits(cs, "error")) {
        message(sprintf("[ERROR] Chunk failed: %s", cs$message))
      } else if (cs$cells_fail > 0 || cs$cells_ok == 0) {
        message(sprintf("[WARN] Chunk %d: %d ok / %d fail | %s",
                        cs$chunk, cs$cells_ok, cs$cells_fail, cs$errors))
      } else {
        message(sprintf("[OK]   Chunk %d: %d/%d cells | %.0f s",
                        cs$chunk, cs$cells_ok, cs$cells_total, cs$elapsed_sec))
      }
    }

    log_rows <- dplyr::bind_rows(lapply(chunk_summaries, function(x)
      if (!inherits(x, "error")) as.data.frame(x) else NULL))
    if (nrow(log_rows) > 0) {
      log_rows$timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      write.table(log_rows, log_file,
                  append     = file.exists(log_file),
                  sep        = ",",
                  row.names  = FALSE,
                  col.names  = !file.exists(log_file))
    }

    ## ── Collect scenario results ───────────────────────
    all_chunk_files <- file.path(checkpoint_dir,
      list.files(checkpoint_dir, pattern = sprintf("^chunk_sc%02d_", i)))
    final.df[[i]] <- dplyr::bind_rows(lapply(all_chunk_files, readRDS))

    sc_elapsed   <- round(proc.time()[["elapsed"]] - sc_t0)
    elapsed_min  <- round(as.numeric(difftime(Sys.time(), run_started, units = "mins")), 1)
    sc_remaining <- nrow(scenarios) - i
    eta_min      <- if (i > 0) round(elapsed_min / i * sc_remaining, 0) else NA

    n_cells_done <- dplyr::n_distinct(final.df[[i]]$cellid)
    cat(sprintf("[INFO] Scenario %d done: %d rows | %d cells | %.0f min elapsed\n",
                i, nrow(final.df[[i]]), n_cells_done, sc_elapsed / 60))
    cat(sprintf("[INFO] Total: %.1f min | Remaining scenarios: %d | ETA: ~%d min\n",
                elapsed_min, sc_remaining, eta_min))

    send_notification(
      subject = sprintf("Soybean sim: scenario %d/%d done (%s)",
                        i, nrow(scenarios), sc$scenario),
      body    = paste0(
        "**Scenario ", i, "/", nrow(scenarios), " complete**\n\n",
        "- Scenario : ", sc$scenario, " | CO2 = ", sc$co2, "\n",
        "- Cells    : ", n_cells_done, "\n",
        "- Rows     : ", nrow(final.df[[i]]), "\n",
        "- Time this scenario: ", round(sc_elapsed / 60, 1), " min\n",
        "- Total elapsed     : ", elapsed_min, " min\n",
        "- Scenarios left    : ", sc_remaining, "\n",
        "- ETA               : ~", eta_min, " min\n",
        "Machine: ", Sys.info()[["nodename"]]
      )
    )
  }

}, finally = {
  stopCluster(cl)
  cat("\n[CLUSTER] Stopped.\n")
})

## ── Final save ───────────────────────────────────────────────
final.df     <- dplyr::bind_rows(Filter(Negate(is.null), final.df))
total_elapsed <- round(as.numeric(difftime(Sys.time(), run_started, units = "mins")), 1)

saveRDS(final.df, "intermediate-data/simulated-scenarios-df.rds")
write_csv(final.df, "intermediate-data/simulated-scenarios-df.csv")

cat(sprintf("\n[DONE] %d rows | %d scenarios | %d cells | %.1f min\n",
            nrow(final.df),
            dplyr::n_distinct(final.df$scenario),
            dplyr::n_distinct(final.df$cellid),
            total_elapsed))

send_notification(
  subject = sprintf("Soybean sim COMPLETE — %.0f min | %s",
                    total_elapsed, Sys.info()[["nodename"]]),
  body    = paste0(
    "**All scenarios complete!**\n\n",
    "- Total rows : ", nrow(final.df), "\n",
    "- Scenarios  : ", dplyr::n_distinct(final.df$scenario), "\n",
    "- Cells      : ", dplyr::n_distinct(final.df$cellid), "\n",
    "- Total time : ", total_elapsed, " min\n",
    "- Output     : intermediate-data/simulated-scenarios-df.rds\n",
    "Machine: ", Sys.info()[["nodename"]]
  )
)

## ── Summary report ───────────────────────────────────────────
tryCatch({
  report <- c(
    "================================================================",
    sprintf("SIMULATION SUMMARY — %s", format(Sys.time(), "%Y-%m-%d %H:%M")),
    "================================================================",
    sprintf("Machine    : %s (%s)", Sys.info()[["nodename"]], .Platform$OS.type),
    sprintf("Workers    : %d | Chunk size: %d", n_workers, CHUNK_SIZE),
    sprintf("Total time : %.1f minutes", total_elapsed),
    "",
    "── Yield by scenario ────────────────────────────────────────",
    capture.output(print(
      final.df %>%
        group_by(scenario, co2, cultivar, sowing) %>%
        summarise(cells            = dplyr::n_distinct(cellid),
                  years            = dplyr::n_distinct(Date),
                  yield_mean_kgha  = round(mean(Yield_kgha, na.rm = TRUE), 0),
                  yield_sd_kgha    = round(sd(Yield_kgha,   na.rm = TRUE), 0),
                  .groups = "drop"),
      n = 50)),
    "",
    "── Coverage check ───────────────────────────────────────────",
    capture.output(print(
      final.df %>%
        group_by(scenario, co2) %>%
        summarise(cells   = dplyr::n_distinct(cellid),
                  missing = nrow(sim.grid1) - dplyr::n_distinct(cellid),
                  pct_ok  = round(dplyr::n_distinct(cellid) / nrow(sim.grid1) * 100, 1),
                  .groups = "drop"),
      n = 50)),
    "================================================================"
  )
  writeLines(report, "intermediate-data/run-summary.txt")
  cat(paste(report, collapse = "\n"), "\n")
}, error = function(e) message("[WARN] Summary report failed: ", e$message))
