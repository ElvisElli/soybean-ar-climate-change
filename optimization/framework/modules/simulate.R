## =====================================================================
## modules/simulate.R — run APSIM for each site, in parallel
## =====================================================================
## `run_one_sim()` runs a single site-year-planting through APSIM and
## returns its full daily output (phenology stage names + daily yield).
## `run_group()` fans every site of a group across a PSOCK cluster.
##
## Efficiency notes:
##   * one cluster is built per GROUP and REUSED across all calibration
##     stages and the final comparison runs (cluster startup is not
##     repeated per objective evaluation).
##   * the cluster is sized to min(cores, #sites) — never more workers
##     than there is work.
##   * per-worker soil/weather reads are memoized (constant across the
##     thousands of evaluations a group needs).
##   * the dominant cost is the APSIM process launch per site; the single
##     biggest further speedup (batching all sites into one multi-
##     simulation .apsimx file APSIM runs internally in parallel) is
##     described in the README as future work.
## =====================================================================

run_one_sim <- function(id, idat, group, worker_dir, sim_dir) {
  ## Memoize soil profile + met path per site on this worker.
  if (!exists(".SITE_CACHE", envir = globalenv())) assign(".SITE_CACHE", new.env(), globalenv())
  cache <- get(".SITE_CACHE", envir = globalenv())
  site.name <- idat$site[1]
  if (is.null(cache[[site.name]])) {
    soil <- readRDS(file.path(sim_dir, "soil", paste0(site.name, ".rds")))
    if (inherits(soil, "list")) soil <- soil[[1]]
    soil$initialwater <- initialwater_parms(Thickness = soil$soil$Thickness,
                                             InitialValues = soil$soil$DUL)
    ## normalizePath is required: APSIM resolves relative paths against its
    ## own launch context, not the R worker's getwd().
    met <- normalizePath(file.path(sim_dir, "weather", paste0(site.name, ".met")))
    cache[[site.name]] <- list(soil = soil, met = met)
  }
  soil <- cache[[site.name]]$soil
  met  <- cache[[site.name]]$met

  if (!dir.exists(worker_dir)) dir.create(worker_dir, recursive = TRUE)
  wf <- paste0("sim-", gsub("[^A-Za-z0-9]", "_", id), ".apsimx")
  file.copy(file.path(sim_dir, CONFIG$base_file), file.path(worker_dir, wf), overwrite = TRUE)

  ## Window: start 90 d before sowing (soil equilibration), end 30 d after
  ## the latest observed stage.
  start.date <- as.character(idat$pd[1] - 90)
  end.date   <- as.character(max(idat$date) + 30)
  sim.pd     <- strftime(unique(idat$pd), "%b-%d")
  E <- function(...) edit_apsimx(file = wf, src.dir = worker_dir, wrt.dir = worker_dir,
                                 verbose = FALSE, overwrite = TRUE, ...)
  E(node = "Clock", parm = "Start", value = start.date)
  E(node = "Clock", parm = "End",   value = end.date)
  E(node = "Other", parm.path = CONFIG$sowing_path, parm = "CultivarName", value = group)
  E(node = "Other", parm.path = CONFIG$sowing_path, parm = "SowDate",      value = sim.pd)
  E(node = "Weather", value = met)
  edit_apsimx_replace_soil_profile(file = wf, src.dir = worker_dir, wrt.dir = worker_dir,
                                   soil.profile = soil, verbose = FALSE, overwrite = TRUE)
  ## Neutralize the cold-snap harvest manager (see README): its rolling
  ## min-temp trigger can force instant maturity when the 90-d pre-sowing
  ## buffer still carries winter temperatures at an early observed sowing.
  E(node = "Other", parm.path = CONFIG$cold_harvest_path, parm = "Threshold", value = -50)

  sim <- try(apsimx(file = wf, src.dir = worker_dir, cleanup = TRUE), silent = TRUE)
  file.remove(file.path(worker_dir, wf))
  if (inherits(sim, "try-error")) return(NULL)
  sim$id <- id
  row.names(sim) <- NULL
  sim
}

## Fan every site of `gdat` (one group's observations) across the cluster.
## sim_dir is resolved to an absolute path HERE (on the master) and passed
## in, because workers don't have PROJECT_ROOT / abspath() in scope.
run_group <- function(gdat, group, cl) {
  ids <- unique(gdat$id)
  sim_dir_abs <- abspath(CONFIG$sim_dir)
  clusterExport(cl, c("run_one_sim", "CONFIG", "gdat", "group", "sim_dir_abs"), envir = environment())
  res <- parLapply(cl, ids, function(target_id) {
    idat <- gdat[gdat$id == target_id, ]
    try(run_one_sim(target_id, idat, group,
                    worker_dir = file.path(tempdir(), paste0("w_", Sys.getpid())),
                    sim_dir = sim_dir_abs), silent = TRUE)
  })
  res <- Filter(function(x) !is.null(x) && !inherits(x, "try-error"), res)
  if (!length(res)) return(NULL)
  do.call(rbind, res)
}
