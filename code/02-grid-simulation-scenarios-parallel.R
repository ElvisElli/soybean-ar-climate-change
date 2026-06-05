## ============================================================
## Grid simulation script — soybean AR climate change
## Auto-detects cloud vs. local Windows machine.
## Scales to 30 cores on new Windows desktop.
##
## Improvements from apsim-arkansas-grid:
##   - APSIM exe auto-detected from installed Programs dir
##   - Box folder auto-detected (multiple candidate paths)
##   - Cluster started ONCE and reused across all scenarios
##   - Chunk-based parallel processing + RDS checkpointing
##     (replaces 240,000 tiny CSVs with ~200 RDS files)
##   - Within-scenario resume: skips completed chunks on restart
##   - Fixed clusterEvalQ/exe_path scoping bug
## ============================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(apsimx)
  library(doParallel)
  library(foreach)
  library(dplyr)
  library(readr)
  library(readxl)
  library(lubridate)
  library(parallel)
})

## ── Environment detection ────────────────────────────────────
detect_env <- function() {
  host <- tolower(Sys.info()[["nodename"]])
  os   <- .Platform$OS.type          # "windows" | "unix"

  if (os == "windows") {
    ## ── Auto-detect latest APSIM version ──────────────────────
    local_app  <- Sys.getenv("LOCALAPPDATA",
                              file.path(Sys.getenv("USERPROFILE"),
                                        "AppData", "Local"))
    prog_dir   <- file.path(local_app, "Programs")
    apsim_dirs <- sort(list.dirs(prog_dir, recursive = FALSE))
    apsim_dirs <- grep("APSIM", apsim_dirs, value = TRUE, ignore.case = TRUE)
    if (length(apsim_dirs) > 0) {
      apsim_exe <- file.path(tail(apsim_dirs, 1), "bin", "Models.exe")
    } else {
      apsim_exe <- "C:/Users/efelli/AppData/Local/Programs/APSIM2025.3.7681.0/bin/Models.exe"
    }

    ## ── Auto-detect Box folder ────────────────────────────────
    user_home <- Sys.getenv("USERPROFILE", path.expand("~"))
    box_candidates <- c(
      file.path(user_home, "Box", "_Projects", "Scale-Sims",
                "soybean-ar-climate-change", "intermediate-data"),
      file.path(user_home, "Box Sync", "_Projects", "Scale-Sims",
                "soybean-ar-climate-change", "intermediate-data"),
      "C:/Users/efelli/Box/_Projects/Scale-Sims/soybean-ar-climate-change/intermediate-data"
    )
    box_root <- Filter(dir.exists, box_candidates)

    ## ── Cores ─────────────────────────────────────────────────
    n_cores <- max(1L, parallel::detectCores(logical = FALSE) - 2L)

    message("[ENV] Windows machine: ", host)
    message("[ENV] APSIM exe      : ", apsim_exe)
    if (length(box_root) > 0) {
      message("[ENV] Box root       : ", box_root[[1]])
    } else {
      message("[ENV] Box root       : NOT FOUND — using project intermediate-data/")
    }

    return(list(
      apsim_exe  = apsim_exe,
      box_root   = if (length(box_root) > 0) box_root[[1]] else NULL,
      local_tmp  = "C:/temp/apsim-proc",
      cores_use  = n_cores,
      is_local   = TRUE,
      is_windows = TRUE
    ))
  }

  ## ── Linux / cloud ─────────────────────────────────────────
  message("[ENV] Cloud/Linux environment: ", host)
  list(
    apsim_exe  = NULL,
    box_root   = NULL,
    local_tmp  = "/tmp/apsim-proc",
    cores_use  = max(1L, parallel::detectCores(logical = FALSE) - 1L),
    is_local   = FALSE,
    is_windows = FALSE
  )
}

ENV <- detect_env()

## ── APSIM exe ────────────────────────────────────────────────
if (!is.null(ENV$apsim_exe)) {
  if (!file.exists(ENV$apsim_exe))
    stop("[ERROR] APSIM not found at: ", ENV$apsim_exe,
         "\nUpdate APSIM or re-install.")
  apsimx_options(exe.path = ENV$apsim_exe)
}

## ── Data paths ───────────────────────────────────────────────
if (ENV$is_local && !is.null(ENV$box_root)) {
  intermediate_data <- ENV$box_root
} else {
  intermediate_data <- normalizePath("intermediate-data", mustWork = FALSE)
}

weather_path  <- file.path(intermediate_data, "weather")
soil_path     <- file.path(intermediate_data, "soil")
checkpoint_dir <- normalizePath("intermediate-data/sim-chunks",  mustWork = FALSE)
final_out_dir  <- normalizePath("intermediate-data",             mustWork = FALSE)
local_tmp      <- normalizePath(ENV$local_tmp, mustWork = FALSE)

message("[PATHS] Weather     : ", weather_path)
message("[PATHS] Soil        : ", soil_path)
message("[PATHS] Chunks      : ", checkpoint_dir)
message("[PATHS] Tmp dir     : ", local_tmp)

for (d in c(local_tmp, checkpoint_dir))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)

## ── Load grid & scenarios ────────────────────────────────────
sim.grid <- readRDS("intermediate-data/sim-grid.rds")
sim.grid$cellid <- seq_len(nrow(sim.grid))
sim.grid1 <- sim.grid %>% filter(!is.na(cultivated))

scenarios <- read_excel("intermediate-data/scenarios/soy-scenarios-10-24.xlsx",
                        sheet = "Sheet1") %>% as.data.frame()

message("[INFO] Grid cells (cultivated): ", nrow(sim.grid1))
message("[INFO] Scenarios              : ", nrow(scenarios))
message("[INFO] Cores to use           : ", ENV$cores_use)

## ── Copy base template once ──────────────────────────────────
base_apsimx <- normalizePath("processed data/_soybean-10-24-25.apsimx",
                              mustWork = FALSE)
if (!file.exists(base_apsimx))
  base_apsimx <- normalizePath("processed-data/_soybean-10-24-25.apsimx",
                                mustWork = FALSE)
if (!file.exists(base_apsimx))
  stop("[ERROR] Cannot find base APSIM template: ", base_apsimx)

template_file <- file.path(local_tmp, "grid-simulation-file.apsimx")
file.copy(base_apsimx, template_file, overwrite = TRUE)

edit_apsimx(file = "grid-simulation-file.apsimx",
            src.dir = local_tmp, wrt.dir = local_tmp,
            node = "Clock", parm = "Start", value = "1985-01-01",
            overwrite = TRUE, verbose = FALSE)

edit_apsimx(file = "grid-simulation-file.apsimx",
            src.dir = local_tmp, wrt.dir = local_tmp,
            node = "Clock", parm = "End", value = "2024-12-31",
            overwrite = TRUE, verbose = FALSE)

## ── KL / XF root parameters ──────────────────────────────────
KL_VEC <- c(0.08,0.08,0.08,0.08,0.07,0.07,0.07,0.07,
            0.06,0.06,0.06,0.06,0.05,0.05,0.04,0.04,
            0.03,0.03,0.02,0.02)
XF_VEC <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0)

## ── Helper: assemble result row ──────────────────────────────
extract_sim_columns <- function(sim, sc_row, grid_row) {
  ans <- data.frame(
    cultivar         = sc_row$cultivar,
    sowing           = sc_row$sowing,
    scenario         = sc_row$scenario,
    climate.control  = sc_row$climate.control,
    co2              = sc_row$co2,
    rowSpacing       = sc_row$RowSpacing,
    date             = sim$Date,
    EmergenceDAS                    = sim$EmergenceDAS,
    FloweringDAS                    = sim$FloweringDAS,
    SeedFillingDAS                  = sim$SeedFillingDAS,
    MaturityDAS                     = sim$MaturityDAS,
    CumRadiationInterceptionOnGreen = sim$CumRadiationInterceptionOnGreen,
    Yield_kgha                      = sim$Yield_kgha,
    biomass_kgha                    = sim$biomass_kgha,
    SeasonRain                      = sim$SeasonRain,
    SeasonRadn                      = sim$SeasonRadn,
    SeasonMaxt                      = sim$SeasonMaxt,
    SeasonMint                      = sim$SeasonMint,
    SeasonMeanT                     = sim$SeasonMeanT,
    BloomingSeasonMaxt              = sim$BloomingSeasonMaxt,
    Silking_RUE_Temp                = sim$Silking_RUE_Temp,
    Silking_Supply_Demand_Ratio     = sim$Silking_Supply_Demand_Ratio,
    swhc_6in                        = sim$swhc_6in,
    swhc_12in                       = sim$swhc_12in,
    swhc_24in                       = sim$swhc_24in,
    Crop_ET                         = sim$Crop_ET,
    WDrainage                       = sim$WDrainage,
    WRunoff                         = sim$WRunoff,
    sWUE                            = sim$sWUE
  )
  merge(grid_row, ans)
}

## ── Cluster — started ONCE, reused across all scenarios ──────
CHUNK_SIZE <- 50
n_workers  <- min(ENV$cores_use, nrow(sim.grid1))

cat(sprintf("\n[CLUSTER] Starting %d workers\n", n_workers))
cl <- makeCluster(n_workers, type = "PSOCK",
                  outfile = file.path(local_tmp, "log.txt"))

tryCatch({
  registerDoParallel(cl)

  ## Load apsimx on workers + set exe path (export first, then eval)
  clusterExport(cl, c("ENV"), envir = environment())
  clusterEvalQ(cl, {
    suppressPackageStartupMessages(library(apsimx))
    suppressPackageStartupMessages(library(dplyr))
    if (!is.null(ENV$apsim_exe) && file.exists(ENV$apsim_exe))
      apsimx_options(exe.path = ENV$apsim_exe)
  })

  ## ── Scenario loop ───────────────────────────────────────────
  final.df <- vector("list", nrow(scenarios))

  for (i in seq_len(nrow(scenarios))) {
    sc <- scenarios[i, ]

    cat(sprintf("\n====================================================\n"))
    cat(sprintf("Scenario %d / %d : %s | CO2 = %s\n",
                i, nrow(scenarios), sc$scenario, sc$co2))
    cat(sprintf("====================================================\n"))

    ## -- Edit scenario parameters on the shared template -------
    parm_edits <- list(
      list(parm.path = ".Simulations.Simulation.Field.SowSoybean.CultivarName",
           value = sc$cultivar),
      list(parm.path = ".Simulations.Simulation.Field.SowSoybean.SowDate",
           value = sc$sowing),
      list(parm.path = ".Simulations.Simulation.Field.ClimateController.EnableDate",
           value = sc$climate.control),
      list(parm.path = ".Simulations.Simulation.Field.SowSoybean.RowSpacing",
           value = sc$RowSpacing),
      list(parm.path = ".Simulations.Simulation.Field.CO2.CO2",
           value = sc$co2)
    )
    for (pe in parm_edits) {
      edit_apsimx(file = "grid-simulation-file.apsimx",
                  src.dir = local_tmp, wrt.dir = local_tmp,
                  node = "Other", parm.path = pe$parm.path,
                  value = pe$value, verbose = FALSE, overwrite = TRUE)
    }

    ## -- Skip cells already in saved chunks --------------------
    chunk_pattern <- sprintf("^chunk_sc%02d_", i)
    done_chunks   <- list.files(checkpoint_dir, pattern = chunk_pattern)
    done_cells    <- unlist(lapply(done_chunks, function(f) {
      df <- readRDS(file.path(checkpoint_dir, f))
      unique(df$cellid)
    }))

    todo_rows <- sim.grid1[!sim.grid1$cellid %in% done_cells, ]
    cat(sprintf("[INFO] Cells done: %d | Cells remaining: %d\n",
                nrow(sim.grid1) - nrow(todo_rows), nrow(todo_rows)))

    if (nrow(todo_rows) == 0) {
      cat("[INFO] Scenario already complete — loading from chunks.\n")
      chunk_files <- file.path(checkpoint_dir,
                               list.files(checkpoint_dir, pattern = chunk_pattern))
      final.df[[i]] <- dplyr::bind_rows(lapply(chunk_files, readRDS))
      next
    }

    ## -- Split remaining cells into chunks ---------------------
    chunks <- split(seq_len(nrow(todo_rows)),
                    ceiling(seq_len(nrow(todo_rows)) / CHUNK_SIZE))

    ## Export everything workers need for this scenario
    sc_row <- sc
    clusterExport(cl,
      c("todo_rows", "sc_row", "i", "chunks",
        "local_tmp", "weather_path", "soil_path", "checkpoint_dir",
        "KL_VEC", "XF_VEC", "extract_sim_columns"),
      envir = environment())

    ## -- Parallel chunk processing -----------------------------
    foreach(
      ci             = seq_along(chunks),
      .errorhandling = "pass",
      .packages      = c("apsimx","dplyr")
    ) %dopar% {

      idx       <- chunks[[ci]]
      sub_rows  <- todo_rows[idx, ]
      res_list  <- vector("list", nrow(sub_rows))

      for (k in seq_len(nrow(sub_rows))) {
        j         <- sub_rows$cellid[k]
        grid_row  <- sub_rows[k, ]

        if (k %% 100 == 0)
          cat(sprintf("[Worker] Scenario %d | Chunk %d | Cell %d\n", i, ci, j))

        ## Load soil
        soil_file   <- file.path(soil_path, paste0(j, ".rds"))
        soil.result <- tryCatch(readRDS(soil_file), error = function(e) NULL)

        if (is.null(soil.result) ||
            !is.list(soil.result) || length(soil.result) < 1 ||
            !is.list(soil.result[[1]]) || length(soil.result[[1]]) < 1 ||
            !inherits(soil.result[[1]][[1]], "soil_profile")) next

        soils <- soil.result[[1]][[1]]

        ## KSAT exponential decay (prevents excessive deep drainage)
        KS_max       <- max(soils$soil$KS, na.rm = TRUE)
        fracs         <- exp(seq(0, log(1e-4), length.out = length(soils$soil$KS)))
        soils$soil$KS <- KS_max * fracs
        soils          <- apsimx:::fix_apsimx_soil_profile(soils, verbose = FALSE)
        soils$initialwater <- initialwater_parms(
          Depth         = soils$soil$Depth,
          Thickness     = soils$soil$Thickness,
          InitialValues = soils$soil$DUL)
        soils$crops <- c("Soybean", "Wheat", "Maize")

        ## Truncate KL/XF to actual layer count
        n_layers <- nrow(soils$soil)
        KL <- KL_VEC[seq_len(min(n_layers, length(KL_VEC)))]
        XF <- XF_VEC[seq_len(min(n_layers, length(XF_VEC)))]

        ## Per-worker APSIM file (no race conditions)
        par_file <- paste0("par-sim-", j, ".apsimx")
        par_abs  <- file.path(local_tmp, par_file)

        built <- tryCatch({
          file.copy(file.path(local_tmp, "grid-simulation-file.apsimx"),
                    par_abs, overwrite = TRUE)
          edit_apsimx_replace_soil_profile(
            file = par_file, src.dir = local_tmp, wrt.dir = local_tmp,
            soil.profile = soils, verbose = FALSE, overwrite = TRUE)
          edit_apsimx(par_file, local_tmp, local_tmp,
            node = "Soil", soil.child = "Physical",
            parm = "KL", value = KL, verbose = FALSE, overwrite = TRUE)
          edit_apsimx(par_file, local_tmp, local_tmp,
            node = "Soil", soil.child = "Physical",
            parm = "XF", value = XF, verbose = FALSE, overwrite = TRUE)
          edit_apsimx(par_file, local_tmp, local_tmp,
            node = "Weather",
            value = normalizePath(file.path(weather_path, paste0(j, ".met")),
                                  mustWork = FALSE),
            overwrite = TRUE, verbose = FALSE)
          TRUE
        }, error = function(e) FALSE)

        if (!built) { unlink(par_abs); next }

        sim <- tryCatch(
          apsimx(file = par_file, src.dir = local_tmp, cleanup = TRUE),
          error = function(e) NULL)
        unlink(par_abs)

        if (!is.null(sim) && nrow(sim) > 0)
          res_list[[k]] <- extract_sim_columns(sim, sc_row, grid_row)
      }

      ## Save chunk RDS checkpoint
      chunk_df <- dplyr::bind_rows(Filter(Negate(is.null), res_list))
      if (nrow(chunk_df) > 0) {
        out_rds <- file.path(checkpoint_dir,
                             sprintf("chunk_sc%02d_ck%04d_%d.rds",
                                     i, ci, sub_rows$cellid[1]))
        saveRDS(chunk_df, out_rds)
      }
      NULL
    }

    ## -- Collect all chunks for this scenario ------------------
    chunk_files <- file.path(checkpoint_dir,
                             list.files(checkpoint_dir, pattern = chunk_pattern))
    scenario_df <- dplyr::bind_rows(lapply(chunk_files, readRDS))
    final.df[[i]] <- scenario_df

    ## Rolling checkpoint across scenarios
    saveRDS(dplyr::bind_rows(Filter(Negate(is.null), final.df)),
            "intermediate-data/simulated-scenarios-df-checkpoint.rds")

    cat(sprintf("[INFO] Scenario %d complete: %d rows\n", i, nrow(scenario_df)))
  }

}, finally = {
  stopCluster(cl)
  cat("\n[CLUSTER] Stopped.\n")
})

## ── Final save ───────────────────────────────────────────────
final.df <- dplyr::bind_rows(Filter(Negate(is.null), final.df))
saveRDS(final.df, "intermediate-data/simulated-scenarios-df.rds")
write_csv(final.df, "intermediate-data/simulated-scenarios-df.csv")

cat(sprintf("\n[DONE] Total rows: %d | Scenarios: %d | Cells: %d\n",
            nrow(final.df),
            dplyr::n_distinct(final.df$scenario),
            dplyr::n_distinct(final.df$cellid)))
