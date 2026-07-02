## =====================================================================
## modules/setup.R — environment, libraries, APSIM executable
## =====================================================================
## Makes the framework portable: finds the project root from the config
## location, loads packages, and locates the APSIM `Models` binary on
## Windows, Linux, or macOS with no manual configuration.
## =====================================================================

suppressMessages({
  library(apsimx)
  library(parallel)
  library(jsonlite)
})

## ---- Project root ----------------------------------------------------
## PROJECT_ROOT is set by run.R (the directory two levels above this
## file: optimization/framework/ -> repo root). All CONFIG paths are
## resolved relative to it, so the repo can live anywhere.
abspath <- function(p) normalizePath(file.path(PROJECT_ROOT, p), mustWork = FALSE)

dir.create(abspath(CONFIG$out_dir), recursive = TRUE, showWarnings = FALSE)

## ---- APSIM executable auto-detection --------------------------------
## Tries common install locations across platforms; falls back to
## apsimx's own detector. The resolved path is captured so it can be
## handed explicitly to each parallel worker (workers are fresh R
## processes whose apsimx options start empty).
detect_apsim <- function() {
  candidates <- c(
    "/usr/local/bin/Models",                         # Linux/cloud
    Sys.glob("/Applications/APSIM*/bin/Models"),     # macOS
    Sys.glob("C:/PROGRA~1/APSIM*/bin/Models.exe"),   # Windows (Program Files)
    Sys.glob(file.path(Sys.getenv("LOCALAPPDATA"), "Programs/APSIM*/bin/Models.exe"))
  )
  hit <- candidates[file.exists(candidates)]
  if (length(hit) > 0) {
    apsimx_options(exe.path = hit[1])
  } else {
    ## Last resort: let apsimx try (may prompt / scan).
    try(apsimx:::auto_detect_apsimx(), silent = TRUE)
  }
  get("exe.path", envir = apsimx:::apsimx.options)
}

RESOLVED_EXE <- detect_apsim()
message("[setup] APSIM executable: ", RESOLVED_EXE)
message("[setup] Physical cores in use: ", CONFIG$cores)
