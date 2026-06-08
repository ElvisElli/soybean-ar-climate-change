## ── Windows APSIM diagnostic ─────────────────────────────────
## Run this entire script in RStudio on Windows to find the .db problem.
## It tests one cell sequentially with full output at every step.
## ─────────────────────────────────────────────────────────────

library(apsimx)

## ── 1. Paths ─────────────────────────────────────────────────
repo_root   <- "C:/Users/efelli/Documents/GitHub/soybean-ar-climate-change"
template    <- file.path(repo_root, "templates/soybean-mg4-baseline.apsimx")
work_dir    <- file.path(repo_root, "data/outputs/apsim-work")
weather_dir <- "C:/Users/efelli/Box/_Projects/Scale-Sims/soybean-ar-climate-change/intermediate-data/weather"
soil_dir    <- "C:/Users/efelli/Box/_Projects/Scale-Sims/soybean-ar-climate-change/intermediate-data/soil"

## Pick the first available cell
met_files <- list.files(weather_dir, "\\.met$", full.names = FALSE)
j         <- as.integer(sub("\\.met$", "", met_files[1]))

cat("=== STEP 1: Paths ===\n")
cat("Repo root    :", repo_root,  "| exists:", dir.exists(repo_root),  "\n")
cat("Template     :", template,   "| exists:", file.exists(template),  "\n")
cat("Work dir     :", work_dir,   "| exists:", dir.exists(work_dir),   "\n")
cat("Weather dir  :", weather_dir,"| exists:", dir.exists(weather_dir),"\n")
cat("Soil dir     :", soil_dir,   "| exists:", dir.exists(soil_dir),   "\n")
cat("Test cell    :", j, "\n")

## ── 2. APSIM exe ─────────────────────────────────────────────
local_app  <- Sys.getenv("LOCALAPPDATA")
search_roots <- c(
  file.path(local_app, "Programs"),
  "C:/Program Files",
  "C:/Program Files (x86)"
)
apsim_dirs <- sort(grep("APSIM",
  unlist(lapply(search_roots, function(d) {
    if (dir.exists(d)) list.dirs(d, recursive = FALSE) else character(0)
  })),
  value = TRUE, ignore.case = TRUE))
apsim_exe  <- utils::shortPathName(file.path(tail(apsim_dirs, 1), "bin", "Models.exe"))

cat("\n=== STEP 2: APSIM exe ===\n")
cat("Exe path :", apsim_exe, "| exists:", file.exists(apsim_exe), "\n")
apsimx_options(exe.path = apsim_exe)

## ── 3. Soil ──────────────────────────────────────────────────
cat("\n=== STEP 3: Soil ===\n")
soil_file   <- file.path(soil_dir, paste0(j, ".rds"))
cat("Soil file:", soil_file, "| exists:", file.exists(soil_file), "\n")
soil.result <- readRDS(soil_file)
soils       <- soil.result[[1]][[1]]
cat("Soil layers:", nrow(soils$soil), "\n")

KL_VEC <- c(0.08,0.08,0.08,0.08,0.07,0.07,0.07,0.07,
            0.06,0.06,0.06,0.06,0.05,0.05,0.04,0.04,0.03,0.03,0.02,0.02)
XF_VEC <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0)

KS_max        <- max(soils$soil$KS, na.rm = TRUE)
soils$soil$KS <- KS_max * exp(seq(0, log(1e-4), length.out = length(soils$soil$KS)))
soils         <- apsimx:::fix_apsimx_soil_profile(soils, verbose = FALSE)
soils$initialwater <- initialwater_parms(
  Depth = soils$soil$Depth, Thickness = soils$soil$Thickness,
  InitialValues = soils$soil$DUL)
soils$crops <- c("Soybean", "Wheat", "Maize")
n_lay <- nrow(soils$soil)
KL <- KL_VEC[seq_len(min(n_lay, length(KL_VEC)))]
XF <- XF_VEC[seq_len(min(n_lay, length(XF_VEC)))]
cat("Soil OK\n")

## ── 4. Build cell dir ─────────────────────────────────────────
cat("\n=== STEP 4: Build cell dir ===\n")

## Set template dates
file.copy(template, file.path(work_dir, "grid-simulation-file.apsimx"), overwrite = TRUE)
cat("Template copied to work_dir\n")
for (pv in list(list("Start", "2015-01-01"), list("End", "2020-12-31")))
  edit_apsimx("grid-simulation-file.apsimx", work_dir, work_dir,
              node = "Clock", parm = pv[[1]], value = pv[[2]],
              overwrite = TRUE, verbose = FALSE)

## Apply baseline scenario
for (pe in list(
  list(".Simulations.Simulation.Field.SowSoybean.CultivarName",     "PurcellMG4"),
  list(".Simulations.Simulation.Field.SowSoybean.SowDate",          "15-May"),
  list(".Simulations.Simulation.Field.ClimateController.EnableDate", "1/1/2025"),
  list(".Simulations.Simulation.Field.SowSoybean.RowSpacing",       750),
  list(".Simulations.Simulation.Field.CO2.CO2",                     350)
))
  edit_apsimx("grid-simulation-file.apsimx", work_dir, work_dir,
              node = "Other", parm.path = pe[[1]], value = pe[[2]],
              verbose = FALSE, overwrite = TRUE)

cell_dir <- file.path(work_dir, paste0("cell-", j))
dir.create(cell_dir, showWarnings = TRUE)   # showWarnings=TRUE to catch failures
cat("cell_dir     :", cell_dir, "\n")
cat("cell_dir exists:", dir.exists(cell_dir), "\n")

file.copy(file.path(work_dir, "grid-simulation-file.apsimx"),
          file.path(cell_dir, "sim.apsimx"), overwrite = TRUE)
cat("Files in cell_dir (before soil inject):", paste(list.files(cell_dir), collapse=", "), "\n")

edit_apsimx_replace_soil_profile("sim.apsimx", cell_dir, cell_dir,
  soil.profile = soils, verbose = FALSE, overwrite = TRUE)
edit_apsimx("sim.apsimx", cell_dir, cell_dir, node = "Soil",
  soil.child = "Physical", parm = "KL", value = KL, verbose = FALSE, overwrite = TRUE)
edit_apsimx("sim.apsimx", cell_dir, cell_dir, node = "Soil",
  soil.child = "Physical", parm = "XF", value = XF, verbose = FALSE, overwrite = TRUE)

met_path <- normalizePath(file.path(weather_dir, paste0(j, ".met")), mustWork = TRUE)
cat("Weather path :", met_path, "\n")
edit_apsimx("sim.apsimx", cell_dir, cell_dir,
  node = "Weather", value = met_path, overwrite = TRUE, verbose = FALSE)

cat("Files in cell_dir (ready to run):", paste(list.files(cell_dir), collapse=", "), "\n")

## ── 5. Run APSIM ─────────────────────────────────────────────
cat("\n=== STEP 5: Run APSIM (cleanup=FALSE, silent=FALSE) ===\n")
cat("Running... this should take ~10-20 seconds\n")
t0 <- proc.time()[["elapsed"]]
sim <- tryCatch(
  apsimx("sim.apsimx", src.dir = cell_dir, cleanup = FALSE, silent = FALSE),
  error = function(e) { cat("ERROR from apsimx():", e$message, "\n"); NULL }
)
cat(sprintf("Elapsed: %.1f s\n", proc.time()[["elapsed"]] - t0))

## ── 6. Inspect results ───────────────────────────────────────
cat("\n=== STEP 6: Results ===\n")
cat("Files in cell_dir after run:", paste(list.files(cell_dir), collapse=", "), "\n")
cat("Also check R working dir for stray .db:\n")
cat("  getwd():", getwd(), "\n")
cat("  .db files there:", paste(list.files(getwd(), "\\.db$"), collapse=", "), "\n")

if (!is.null(sim) && nrow(sim) > 0) {
  cat(sprintf("SUCCESS: %d rows | Yield[1]=%.0f kg/ha\n", nrow(sim), sim$Yield_kgha[1]))
} else {
  cat("FAILED — inspecting .db for APSIM error messages...\n")
  db <- list.files(cell_dir, "\\.db$", full.names = TRUE)
  if (length(db) > 0) {
    library(DBI); library(RSQLite)
    con  <- dbConnect(SQLite(), db[1])
    msgs <- dbReadTable(con, "_Messages")
    dbDisconnect(con)
    errs <- msgs[msgs$MessageType == 0, "Message"]
    cat("APSIM errors:\n")
    cat(paste(substr(errs, 1, 500), collapse="\n"), "\n")
  } else {
    cat("No .db file found in cell_dir — APSIM did not run or wrote .db elsewhere.\n")
    cat("Check manually: does", cell_dir, "exist? What's in it?\n")
  }
}

cat("\n=== DONE ===\n")
cat("cell_dir kept for inspection:", cell_dir, "\n")
