## ============================================================
## Grid simulation script - soybean AR climate change
## Auto-detects cloud vs. local Windows machine
## Scales to all available cores (30 on new Windows desktop)
## ============================================================

rm(list = ls())

## ── Libraries ────────────────────────────────────────────────
library(apsimx)
library(ggplot2)
library(stars)
library(sf)
library(readr)
library(dplyr)
library(readxl)
library(lubridate)
library(parallel)

## ── Environment detection ────────────────────────────────────
## Determines paths and APSIM exe location automatically.
## Add new machines by extending the hostname list below.

detect_env <- function() {
  host <- tolower(Sys.info()[["nodename"]])
  os   <- .Platform$OS.type          # "windows" | "unix"

  ## ---- Known Windows desktops / laptops ----------------------
  windows_machines <- list(

    ## Elvis desktop (30-core Windows machine)
    list(
      pattern    = "efelli",          # partial hostname match
      apsim_exe  = "C:/Users/efelli/AppData/Local/Programs/APSIM2025.3.7681.0/bin/Models.exe",
      box_root   = "C:/Users/efelli/Box/_Projects/Scale-Sims/soybean-ar-climate-change/intermediate-data",
      local_tmp  = "C:/temp/apsim-proc",
      cores_use  = 28                 # leave 2 free for the OS
    )
    ## Add more Windows machines here as needed:
    # list(pattern = "other-hostname", apsim_exe = ..., ...)
  )

  ## ---- Cloud / Linux environment -----------------------------
  cloud_env <- list(
    apsim_exe  = NULL,                # apsimx finds it automatically
    box_root   = NULL,                # Box not mounted; data in project dir
    local_tmp  = "/tmp/apsim-proc",
    cores_use  = max(1, detectCores(logical = FALSE) - 1)
  )

  if (os == "windows") {
    for (m in windows_machines) {
      if (grepl(m$pattern, host, fixed = TRUE)) {
        message("[ENV] Detected Windows machine: ", host)
        return(c(m, list(is_local = TRUE, is_windows = TRUE)))
      }
    }
    ## Unknown Windows machine — use conservative defaults
    message("[ENV] Unknown Windows machine '", host, "' — using defaults")
    return(list(
      apsim_exe  = NULL,
      box_root   = NULL,
      local_tmp  = "C:/temp/apsim-proc",
      cores_use  = max(1, detectCores(logical = FALSE) - 2),
      is_local   = TRUE,
      is_windows = TRUE
    ))
  }

  message("[ENV] Detected cloud/Linux environment: ", host)
  return(c(cloud_env, list(is_local = FALSE, is_windows = FALSE)))
}

ENV <- detect_env()

## ── APSIM exe path (Windows only) ───────────────────────────
if (!is.null(ENV$apsim_exe) && file.exists(ENV$apsim_exe)) {
  apsimx_options(exe.path = ENV$apsim_exe)
  message("[APSIM] Using exe: ", ENV$apsim_exe)
} else {
  message("[APSIM] Using system APSIM (auto-detected)")
}

## ── Data paths ───────────────────────────────────────────────
## On local Windows: weather & soil live in Box.
## On cloud:        weather & soil live inside the project dir.

if (ENV$is_local && !is.null(ENV$box_root)) {
  weather_path <- file.path(ENV$box_root, "weather")
  soil_path    <- file.path(ENV$box_root, "soil")
} else {
  weather_path <- file.path("intermediate-data", "weather")
  soil_path    <- file.path("intermediate-data", "soil")
}

weather_path <- normalizePath(weather_path, mustWork = FALSE)
soil_path    <- normalizePath(soil_path,    mustWork = FALSE)
crop_path    <- normalizePath("intermediate-data/crop-output", mustWork = FALSE)
local_tmp    <- normalizePath(ENV$local_tmp, mustWork = FALSE)

message("[PATHS] Weather : ", weather_path)
message("[PATHS] Soil    : ", soil_path)
message("[PATHS] Output  : ", crop_path)
message("[PATHS] Tmp dir : ", local_tmp)

## ── Create required directories ──────────────────────────────
for (d in c(local_tmp, crop_path)) {
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE)
    message("[DIR] Created: ", d)
  }
}

## ── Load grid & scenarios ────────────────────────────────────
sim.grid <- readRDS("intermediate-data/sim-grid.rds")
sim.grid$cellid <- seq_len(nrow(sim.grid))
sim.grid1 <- sim.grid %>% filter(!is.na(cultivated))

scenarios <- read_excel("intermediate-data/scenarios/soy-scenarios-10-24.xlsx",
                        sheet = "Sheet1") %>%
  as.data.frame()

message("[INFO] Grid cells (cultivated): ", nrow(sim.grid1))
message("[INFO] Scenarios              : ", nrow(scenarios))
message("[INFO] Cores to use           : ", ENV$cores_use)

## ── Copy base APSIM file ─────────────────────────────────────
## The project has a folder called "processed data" (with a space)
base_apsimx <- normalizePath("processed data/_soybean-10-24-25.apsimx",
                              mustWork = FALSE)

## fallback — try without space (in case folder was renamed)
if (!file.exists(base_apsimx)) {
  base_apsimx <- normalizePath("processed-data/_soybean-10-24-25.apsimx",
                                mustWork = FALSE)
}

if (!file.exists(base_apsimx)) {
  stop("[ERROR] Cannot find base APSIM file. Expected: ", base_apsimx)
}

template_file <- file.path(local_tmp, "grid-simulation-file.apsimx")
file.copy(base_apsimx, template_file, overwrite = TRUE)

## ── Set simulation clock once on the template ────────────────
edit_apsimx(file = "grid-simulation-file.apsimx",
            src.dir = local_tmp, wrt.dir = local_tmp,
            node = "Clock", parm = "Start", value = "1985-01-01",
            overwrite = TRUE)

edit_apsimx(file = "grid-simulation-file.apsimx",
            src.dir = local_tmp, wrt.dir = local_tmp,
            node = "Clock", parm = "End", value = "2024-12-31",
            overwrite = TRUE)

## ── Helper: extract result columns ───────────────────────────
extract_sim_columns <- function(sim, scenarios, i, sim.grid, j) {
  data.frame(
    cultivar        = scenarios$cultivar[i],
    sowing          = scenarios$sowing[i],
    scenario        = scenarios$scenario[i],
    climate.control = scenarios$climate.control[i],
    co2             = scenarios$co2[i],
    rowSpacing      = scenarios$RowSpacing[i],
    date            = sim$Date,
    EmergenceDAS                   = sim$EmergenceDAS,
    FloweringDAS                   = sim$FloweringDAS,
    SeedFillingDAS                 = sim$SeedFillingDAS,
    MaturityDAS                    = sim$MaturityDAS,
    CumRadiationInterceptionOnGreen = sim$CumRadiationInterceptionOnGreen,
    Yield_kgha                     = sim$Yield_kgha,
    biomass_kgha                   = sim$biomass_kgha,
    SeasonRain                     = sim$SeasonRain,
    SeasonRadn                     = sim$SeasonRadn,
    SeasonMaxt                     = sim$SeasonMaxt,
    SeasonMint                     = sim$SeasonMint,
    SeasonMeanT                    = sim$SeasonMeanT,
    BloomingSeasonMaxt             = sim$BloomingSeasonMaxt,
    Silking_RUE_Temp               = sim$Silking_RUE_Temp,
    Silking_Supply_Demand_Ratio    = sim$Silking_Supply_Demand_Ratio,
    swhc_6in                       = sim$swhc_6in,
    swhc_12in                      = sim$swhc_12in,
    swhc_24in                      = sim$swhc_24in,
    Crop_ET                        = sim$Crop_ET,
    WDrainage                      = sim$WDrainage,
    WRunoff                        = sim$WRunoff,
    sWUE                           = sim$sWUE
  )
}

## ── KL / XF crop root parameters ─────────────────────────────
KL_VEC <- c(0.08,0.08,0.08,0.08,0.07,0.07,0.07,0.07,
            0.06,0.06,0.06,0.06,0.05,0.05,0.04,0.04,
            0.03,0.03,0.02,0.02)
XF_VEC <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0)

## ── Main simulation loop ─────────────────────────────────────
final.df <- vector("list", nrow(scenarios))

for (i in seq_len(nrow(scenarios))) {

  cat("\n====================================================\n")
  cat("Scenario", i, "/", nrow(scenarios), ":", scenarios$scenario[i],
      "| CO2 =", scenarios$co2[i], "\n")
  cat("====================================================\n")

  ## -- Edit scenario parameters on the shared template --------
  parm_edits <- list(
    list(parm.path = ".Simulations.Simulation.Field.SowSoybean.CultivarName",
         value = scenarios[i, "cultivar"]),
    list(parm.path = ".Simulations.Simulation.Field.SowSoybean.SowDate",
         value = scenarios[i, "sowing"]),
    list(parm.path = ".Simulations.Simulation.Field.ClimateController.EnableDate",
         value = scenarios[i, "climate.control"]),
    list(parm.path = ".Simulations.Simulation.Field.SowSoybean.RowSpacing",
         value = scenarios[i, "RowSpacing"]),
    list(parm.path = ".Simulations.Simulation.Field.CO2.CO2",
         value = scenarios[i, "co2"])
  )

  for (pe in parm_edits) {
    edit_apsimx(file = "grid-simulation-file.apsimx",
                src.dir = local_tmp, wrt.dir = local_tmp,
                node = "Other",
                parm.path = pe$parm.path,
                value = pe$value,
                verbose = FALSE, overwrite = TRUE)
  }

  ## -- Cluster setup ------------------------------------------
  ncores <- min(ENV$cores_use, detectCores(logical = FALSE))
  cat(sprintf("[INFO] Starting cluster with %d workers\n", ncores))
  cl <- makeCluster(ncores, outfile = file.path(local_tmp, "log.txt"))

  scenario.df <- tryCatch({

    ## Load apsimx on workers; set exe path if needed
    if (!is.null(ENV$apsim_exe) && file.exists(ENV$apsim_exe)) {
      exe_path <- ENV$apsim_exe
      clusterEvalQ(cl, {
        library(apsimx)
        apsimx_options(exe.path = exe_path)
      })
    } else {
      clusterEvalQ(cl, library(apsimx))
    }

    ## Export what workers need
    clusterExport(cl, c(
      "sim.grid", "scenarios", "i",
      "local_tmp", "weather_path", "soil_path", "crop_path",
      "KL_VEC", "XF_VEC",
      "extract_sim_columns"
    ))

    ## -- Parallel cell loop -----------------------------------
    parLapply(cl, seq_len(nrow(sim.grid)), function(j) {

      if (is.na(sim.grid[j, "cultivated"])) return(NULL)

      ## Progress ticker (visible in log.txt)
      if (j %% 100 == 0)
        cat(sprintf("[Worker] Cell %d / %d | Scenario %d (%s)\n",
                    j, nrow(sim.grid), i, scenarios$scenario[i]))

      ## -- Load soil ------------------------------------------
      soil_file <- file.path(soil_path, paste0(j, ".rds"))
      soil.result <- try(readRDS(soil_file), silent = TRUE)

      if (inherits(soil.result, "try-error") ||
          !is.list(soil.result) || length(soil.result) < 1 ||
          !is.list(soil.result[[1]]) || length(soil.result[[1]]) < 1 ||
          !inherits(soil.result[[1]][[1]], "soil_profile")) {
        return(NULL)
      }

      soils <- soil.result[[1]][[1]]

      ## KSAT exponential decay (prevents excessive deep drainage)
      KS_max <- max(soils$soil$KS, na.rm = TRUE)
      fracs   <- exp(seq(0, log(0.0001), length.out = length(soils$soil$KS)))
      soils$soil$KS <- KS_max * fracs

      soils <- apsimx:::fix_apsimx_soil_profile(soils, verbose = FALSE)
      soils$initialwater <- initialwater_parms(
        Depth         = soils$soil$Depth,
        Thickness     = soils$soil$Thickness,
        InitialValues = soils$soil$DUL
      )
      soils$crops <- c("Soybean", "Wheat", "Maize")

      ## -- Per-worker APSIM file (no race conditions) ---------
      par_file <- paste0("par-sim-", j, ".apsimx")

      file.copy(file.path(local_tmp, "grid-simulation-file.apsimx"),
                file.path(local_tmp, par_file),
                overwrite = TRUE)

      edit_apsimx_replace_soil_profile(
        file = par_file, src.dir = local_tmp, wrt.dir = local_tmp,
        soil.profile = soils, verbose = FALSE, overwrite = TRUE
      )

      edit_apsimx(file = par_file, src.dir = local_tmp, wrt.dir = local_tmp,
                  node = "Soil", soil.child = "Physical",
                  parm = "KL", value = KL_VEC,
                  verbose = FALSE, overwrite = TRUE)

      edit_apsimx(file = par_file, src.dir = local_tmp, wrt.dir = local_tmp,
                  node = "Soil", soil.child = "Physical",
                  parm = "XF", value = XF_VEC,
                  verbose = FALSE, overwrite = TRUE)

      ## Weather
      edit_apsimx(file = par_file, src.dir = local_tmp, wrt.dir = local_tmp,
                  node = "Weather",
                  value = file.path(weather_path, paste0(j, ".met")),
                  overwrite = TRUE, verbose = FALSE)

      ## -- Run APSIM ------------------------------------------
      sim <- try(apsimx(file = par_file, src.dir = local_tmp,
                        cleanup = TRUE), silent = TRUE)

      if (inherits(sim, "try-error")) {
        cat(sprintf("[Worker] APSIM error at cell %d scenario %d\n", j, i))
        return(NULL)
      }

      ## -- Assemble result row --------------------------------
      ans <- extract_sim_columns(sim, scenarios, i, sim.grid, j)
      ans <- merge(sim.grid[j, ], ans)

      ## Write per-cell CSV checkpoint
      out_csv <- file.path(crop_path,
                           paste0(j, "_", scenarios$scenario[i], ".csv"))
      readr::write_csv(ans, out_csv)

      file.remove(file.path(local_tmp, par_file))
      return(ans)
    })

  }, error = function(e) {
    message("[ERROR] Cluster job failed for scenario ", i, ": ", e$message)
    list()
  }, finally = {
    stopCluster(cl)
  })

  ## -- Aggregate scenario results -----------------------------
  scenario.df <- Filter(Negate(is.null), scenario.df)
  scenario.df <- do.call(rbind, scenario.df)
  final.df[[i]] <- scenario.df

  ## Checkpoint RDS after each scenario
  saveRDS(do.call(rbind, Filter(Negate(is.null), final.df)),
          "intermediate-data/simulated-scenarios-df-checkpoint.rds")

  cat("[INFO] Scenario", i, "complete.",
      if (!is.null(scenario.df)) nrow(scenario.df) else 0, "rows saved.\n")
}

## ── Final save ───────────────────────────────────────────────
final.df <- do.call(rbind, final.df)
saveRDS(final.df, "intermediate-data/simulated-scenarios-df.rds")
write_csv(final.df, "intermediate-data/simulated-scenarios-df.csv")

cat("\n[DONE] Total rows in final dataset:", nrow(final.df), "\n")
