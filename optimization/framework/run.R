## =====================================================================
## run.R — entrypoint for the soybean calibration framework
## =====================================================================
## Usage (from anywhere):
##     Rscript optimization/framework/run.R
##
## It self-locates the project root, loads config + engine modules, then
## calibrates every GROUP through the staged plan, saves results, writes
## ready-to-use calibrated cultivars, and renders the figures. The whole
## thing is resumable at the group level: completed groups' outputs are
## reused if present (delete a group's *-coefficients.csv to redo it).
## =====================================================================

## ---- Locate the project root (repo root = two levels up from here) ---
args <- commandArgs(trailingOnly = FALSE)
this <- sub("^--file=", "", args[grep("^--file=", args)])
FRAMEWORK_DIR <- if (length(this)) dirname(normalizePath(this)) else getwd()
PROJECT_ROOT  <- normalizePath(file.path(FRAMEWORK_DIR, "..", ".."))

source(file.path(FRAMEWORK_DIR, "config.R"))
for (m in c("setup", "data", "template", "simulate", "optimize", "outputs", "figures"))
  source(file.path(FRAMEWORK_DIR, "modules", paste0(m, ".R")))

## ---- Calibrate every group ------------------------------------------
all_best <- list()
for (group in names(GROUPS)) {
  coef_file <- file.path(abspath(CONFIG$out_dir), paste0(group, "-coefficients.csv"))
  if (file.exists(coef_file)) {                     # resume: reuse finished group
    message("[run] ", group, " already done — reusing (delete its CSV to redo)")
    cf <- read.csv(coef_file)
    all_best[[group]] <- setNames(
      lapply(names(PARAMETERS), function(pn) cf$fitted[cf$parameter %in% param_columns(pn)]),
      names(PARAMETERS))
    next
  }
  gdat <- OBS[OBS$mg2 == GROUPS[[group]]$mg2, ]
  ncore <- min(CONFIG$cores, length(unique(gdat$id)))
  cl <- makeCluster(max(1, ncore))
  res <- tryCatch({
    clusterExport(cl, "RESOLVED_EXE", envir = environment())
    clusterEvalQ(cl, { library(apsimx); apsimx_options(exe.path = RESOLVED_EXE) })
    calibrate_group(group, gdat, cl)
  }, finally = stopCluster(cl))

  save_group_outputs(group, res)
  all_best[[group]] <- setNames(
    lapply(names(PARAMETERS), function(pn) res$coef$fitted[res$coef$parameter %in% param_columns(pn)]),
    names(PARAMETERS))
}

## ---- Combine, write cultivars, plot ---------------------------------
coefs <- do.call(rbind, lapply(names(GROUPS), function(g)
  read.csv(file.path(abspath(CONFIG$out_dir), paste0(g, "-coefficients.csv")))))
write.csv(coefs, file.path(abspath(CONFIG$out_dir), "all-coefficients.csv"), row.names = FALSE)

write_calibrated_cultivars(all_best)
if (length(PROGRESS$rows)) save_progress()
make_figures()

message("\n[run] DONE — results in ", abspath(CONFIG$out_dir))
