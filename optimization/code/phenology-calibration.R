## ============================================================
## APSIM soybean phenology calibration — multi-site, parallel
##
## Calibrates the phenology parameters of the cultivars that are
## ACTUALLY used in the grid simulation (PurcellMG4/5/6, defined
## in the production template templates/soybean-mg4-baseline.apsimx)
## against multi-site observed phenology data. Using the same
## template/APSIM version as the grid simulation keeps this
## calibration consistent with what the manuscript results are
## actually built on.
##
## Generalized so the same script can be reused for other
## crops/projects: everything project-specific lives in CONFIG.
## ============================================================

rm(list = ls())

## ── Libraries ────────────────────────────────────────────────
## apsimx drives the APSIM Next Gen executable from R; dplyr/parallel
## are only used for light data wrangling and the worker cluster.
library(apsimx)
library(dplyr)
library(parallel)

## ── CONFIG ──────────────────────────────────────────────────
## Everything project-specific (paths, node names, which cultivars
## to calibrate) lives here. To reuse this script for a different
## crop/project, only this block should need to change.

CONFIG <- list(
  sim_dir         = "optimization/simulations",   # holds base_file + soil/ + weather/
  base_file       = "base-simulation.apsimx",      # copy of the grid-sim production template
  raw_dat         = "optimization/raw-data/phenology-all-sites.csv",
  site_info       = "optimization/raw-data/site-info.csv",
  cultivar_root   = ".Simulations.Replacements.Soybean.Elli",  # folder holding the cultivars
  sowing_path     = ".Simulations.Simulation.Field.SowSoybean", # sowing manager (same as grid sim)
  cultivars       = c("PurcellMG4", "PurcellMG5", "PurcellMG6"), # cultivars actually used in the grid sim
  cultivar_mg     = c(PurcellMG4 = 4, PurcellMG5 = 5, PurcellMG6 = 6), # maps cultivar -> observed mg
  cores           = max(1, detectCores(logical = FALSE) - 1),
  ## Each objective-function evaluation reruns every observed
  ## site/year/planting-date combo (measured ~6.4 min for 168 combos
  ## on 4 cores here). Nelder-Mead in 6 dimensions needs a 7-point
  ## initial simplex plus further evals per iteration, so maxit=100
  ## (the original default) is impractical on modest hardware —
  ## lower it for a bounded run; raise it again on a faster/more-core
  ## machine for a fuller search.
  maxit           = 15,
  out_dir         = "optimization/results"
)

## Create the output folder up front so later write.csv()/saveRDS()
## calls never fail on a missing directory.
if (!dir.exists(CONFIG$out_dir)) dir.create(CONFIG$out_dir, recursive = TRUE)

## ── APSIM executable auto-detect ─────────────────────────────
## Tries the Linux CLI binary installed in this sandbox first,
## then falls back to the Windows paths used on the team's
## desktops, then to apsimx's own auto-detection as a last resort.
candidate_exes <- c(
  "/usr/local/bin/Models",
  "C:/APSIM2025.10.7895.0/bin/Models.exe",
  "C:/PROGRA~1/APSIM2025.10.7895.0/bin/Models.exe"
)
exe <- candidate_exes[file.exists(candidate_exes)][1]
if (!is.na(exe)) {
  apsimx_options(exe.path = exe)
} else {
  apsimx:::auto_detect_apsimx()
}

## ── Make sure the calibration template matches the grid sim ──
## Always (re)copy the production template into the optimization
## sandbox before running, so the calibration can never silently
## drift out of sync with what the grid simulation actually uses.
grid_template <- normalizePath("templates/soybean-mg4-baseline.apsimx", mustWork = FALSE)
if (file.exists(grid_template)) {
  file.copy(grid_template, file.path(CONFIG$sim_dir, CONFIG$base_file), overwrite = TRUE)
}

## ── Patch the copied template for calibration-only needs ─────
## The grid-sim template's daily report node (Field.Report) was
## built for the annual-summary use case and (a) doesn't record
## phenology stage names, and (b) shares the literal Name "Report"
## with Simulations.Replacements.Report, which APSIM resolves to
## the SAME results-database table — so daily rows silently
## collide with/are shadowed by the other report's annual rows.
## Neither problem affects the grid simulation itself, so this
## patch is applied only to our local optimization copy, never to
## the shared template.
library(jsonlite)
patch_report_for_calibration <- function(file) {
  tpl <- fromJSON(file, simplifyVector = FALSE)

  ## Recursively find the node at Simulations.Simulation.Field.Report
  find_node <- function(node, target_path, path = "") {
    nm <- node$Name
    newpath <- if (nzchar(path)) paste(path, nm, sep = ".") else nm
    if (identical(newpath, target_path)) return(node)
    kids <- node$Children
    if (is.null(kids)) return(NULL)
    for (i in seq_along(kids)) {
      hit <- find_node(kids[[i]], target_path, newpath)
      if (!is.null(hit)) return(list(hit = hit, parent = node, idx = i))
    }
    NULL
  }

  ## Walk again, this time mutating in place via recursive rebuild.
  patch_node <- function(node, path = "") {
    nm <- node$Name
    newpath <- if (nzchar(path)) paste(path, nm, sep = ".") else nm
    if (identical(newpath, "Simulations.Simulation.Field.Report")) {
      node$Name <- "PhenologyReport"   # avoid table-name collision with Replacements.Report
      extra <- c("[Soybean].Phenology.Stage",
                 "[Soybean].Phenology.CurrentPhaseName",
                 "[Soybean].Phenology.CurrentStageName")
      existing <- vapply(node$VariableNames, identity, character(1))
      new_vars <- setdiff(extra, existing)
      if (length(new_vars) > 0) {
        node$VariableNames <- c(node$VariableNames[1:2], as.list(new_vars), node$VariableNames[-(1:2)])
      }
      return(node)
    }
    if (!is.null(node$Children)) {
      node$Children <- lapply(node$Children, patch_node, path = newpath)
    }
    node
  }

  tpl <- patch_node(tpl)
  write(toJSON(tpl, auto_unbox = TRUE, null = "null", pretty = TRUE), file)
}
patch_report_for_calibration(file.path(CONFIG$sim_dir, CONFIG$base_file))

## ── Load and clean observed phenology data ───────────────────
## `dat` = one row per observed stage/site/planting-date; merged
## with site lat/lon so we can drop a known data-quality issue
## (Southern Hemisphere observations mis-coded against a
## December cutoff).
## fileEncoding strips the UTF-8 BOM these CSVs were saved with;
## without it, read.csv mangles the first column name (site ->
## X...site) and every merge/subset on `site` below silently
## fails or no-ops.
dat <- read.csv(CONFIG$raw_dat, fileEncoding = "UTF-8-BOM")
site.info <- read.csv(CONFIG$site_info, fileEncoding = "UTF-8-BOM")

dat <- merge(dat, site.info)
dat <- dat[order(dat$mg), ]
dat$date <- as.Date(dat$date, "%m/%d/%Y")
dat <- subset(dat, !(as.numeric(strftime(date, "%m")) == 12 & lat > 0))

## APSIM internally names phenology stages differently from the
## standard Fehr & Caviness (1977) soybean staging used in the
## field observations — this table translates between the two so
## predicted and observed stages can be matched up below.
phen.dict <- na.omit(data.frame(
  apsim.phen = c("Sowing", "Germination", "Emergence", "StartFlowering",
                 "StartPodDevelopment", "StartGrainFilling", "EndCanopyDevelopment",
                 "EndPodDevelopment", "EndGrainFill", "Maturity", "HarvestRipe"),
  fc.phen    = c(NA, NA, "VE", "R1", "R3", "R5", NA, NA, "R6", "R7", "R8")
))

## Initial parameter guesses by (rounded) maturity group. These
## are the starting points `optim()` searches from; only mg 4/5/6
## are used here since those are the only cultivars in production,
## but the table is kept full-range so it's easy to calibrate more
## maturity groups later just by editing CONFIG$cultivars.
apsim.mg.values <- data.frame(
  mg = 0:8,
  vegetative            = c(320, 340, 348, 380, 388, 396, 404, 416, 430),
  early.flowering       = c(100, 120, 120, 120, 140, 160, 180, 200, 200),
  early.grain           = c(0.467, 0.411, 0.386, 0.361, 0.324, 0.072, 0.056, 0.055, 0.054),
  late.grain            = c(600, 632, 648, 664, 664, 696, 712, 728, 744),
  photoperiod.modifier1 = c(14.35, 13.84, 13.59, 13.4, 13.1, 12.83, 12.58, 12.33, 12.07),
  photoperiod.modifer2  = c(21.11, 18.77, 17.61, 16.91, 16.49, 16.13, 15.8, 15.46, 15.1)
)

## ── Run one site/year/planting-date combo through APSIM ─────
## Each parallel worker gets its OWN copy of the base file
## (`worker_file` inside `worker_dir`) so concurrent edits/runs
## from different workers never clobber each other or the shared
## template that `make_objfun()` edits below.
run_one_sim <- function(id, idat, cultivar.name, worker_dir, sim_dir) {

  ## Soil profile for this site, with initial water set to DUL
  ## (drained upper limit) — i.e. simulations start at field capacity.
  site.name <- idat$site[1]
  soil <- readRDS(file.path(sim_dir, "soil", paste0(site.name, ".rds")))
  if (inherits(soil, "list")) soil <- soil[[1]]
  soil$initialwater <- initialwater_parms(
    Thickness = soil$soil$Thickness,
    InitialValues = soil$soil$DUL
  )
  ## normalizePath() is required here: APSIM resolves relative paths
  ## against its own working/launch context, not the R process's
  ## getwd(), so a bare relative path written into the .apsimx file
  ## fails to resolve once each parallel worker is running in its
  ## own temp directory.
  met <- normalizePath(file.path(sim_dir, "weather", paste0(site.name, ".met")))

  ## Make a private working copy of the (already cultivar-edited)
  ## base file for this specific site/year/planting-date run.
  if (!dir.exists(worker_dir)) dir.create(worker_dir, recursive = TRUE)
  worker_file <- paste0("sim-", gsub("[^A-Za-z0-9]", "_", id), ".apsimx")
  file.copy(file.path(sim_dir, CONFIG$base_file),
            file.path(worker_dir, worker_file), overwrite = TRUE)

  ## Simulation window: start 90 days before sowing (to allow soil
  ## equilibration) and end 30 days after the latest observed stage.
  start.date <- as.character(idat$pd[1] - 90)
  end.date   <- as.character(max(idat$date) + 30)
  sim.pd     <- strftime(unique(idat$pd), "%b-%d")

  ## Point the clock, sowing manager, and weather node at this run's
  ## specific site/cultivar/planting-date before executing APSIM.
  edit_apsimx(file = worker_file, src.dir = worker_dir, wrt.dir = worker_dir,
              node = "Clock", parm = "Start", value = start.date,
              verbose = FALSE, overwrite = TRUE)
  edit_apsimx(file = worker_file, src.dir = worker_dir, wrt.dir = worker_dir,
              node = "Clock", parm = "End", value = end.date,
              verbose = FALSE, overwrite = TRUE)
  edit_apsimx(file = worker_file, src.dir = worker_dir, wrt.dir = worker_dir,
              node = "Other", parm.path = CONFIG$sowing_path, parm = "CultivarName",
              value = cultivar.name, verbose = FALSE, overwrite = TRUE)
  edit_apsimx(file = worker_file, src.dir = worker_dir, wrt.dir = worker_dir,
              node = "Other", parm.path = CONFIG$sowing_path, parm = "SowDate",
              value = sim.pd, verbose = FALSE, overwrite = TRUE)
  edit_apsimx(file = worker_file, src.dir = worker_dir, wrt.dir = worker_dir,
              node = "Weather", value = met, verbose = FALSE, overwrite = TRUE)
  edit_apsimx_replace_soil_profile(file = worker_file, src.dir = worker_dir,
                                    wrt.dir = worker_dir, soil.profile = soil,
                                    verbose = FALSE, overwrite = TRUE)

  ## The template's "Harvest by min temp" manager force-ends the
  ## crop once the rolling 5-day mean MinT drops below -2 C — this
  ## is meant to catch an autumn cold snap *after* a normal-season
  ## crop has matured. Our 90-day pre-sowing buffer (above) means
  ## that rolling average is often still carrying winter
  ## temperatures right when an early/atypical observed planting
  ## date is sown, which forces instant maturity on day 1. The grid
  ## simulation never hits this (it only ever sows in mid-May), so
  ## disabling it here is calibration-only and does not affect the
  ## production template.
  edit_apsimx(file = worker_file, src.dir = worker_dir, wrt.dir = worker_dir,
              node = "Other", parm.path = ".Simulations.Simulation.Field.Harvest by min temp",
              parm = "Threshold", value = -50, verbose = FALSE, overwrite = TRUE)

  ## Run APSIM itself; `cleanup = TRUE` removes the .db apsimx
  ## creates per run so worker dirs don't fill up with DB files.
  sim0 <- try(apsimx(file = worker_file, src.dir = worker_dir, cleanup = TRUE),
              silent = TRUE)
  file.remove(file.path(worker_dir, worker_file))

  if (inherits(sim0, "try-error")) return(NULL)
  sim0$id <- id
  row.names(sim0) <- NULL
  sim0
}

## ── Run all site/year/planting-date combos for one cultivar ──
## This is the parallel fan-out: every `id` (site x year x
## planting-date) is an independent APSIM run, so they're
## distributed across the cluster `cl` with parLapply instead of
## the original script's serial for-loop.
run_cultivar_parallel <- function(mdat, cultivar.name, cl) {
  ids <- unique(mdat$id)
  ## PSOCK workers are separate R processes that start with an empty
  ## global environment — a closure defined here would normally
  ## carry its captured variables (mdat, cultivar.name) along when
  ## serialized, but because this function's enclosing environment
  ## chain bottoms out at .GlobalEnv, parallel's serialization
  ## resolves that reference to the WORKER's (empty) global env
  ## instead of copying these values across. Every variable the
  ## parLapply closure below touches must therefore be listed here
  ## explicitly, not just relied on via lexical scoping.
  clusterExport(cl, c("run_one_sim", "CONFIG", "mdat", "cultivar.name"), envir = environment())
  res <- parLapply(cl, ids, function(target_id) {
    idat <- mdat[mdat$id == target_id, ]
    try(run_one_sim(target_id, idat, cultivar.name,
                     worker_dir = file.path(tempdir(), paste0("w_", Sys.getpid())),
                     sim_dir = CONFIG$sim_dir),
        silent = TRUE)
  })
  res <- Filter(function(x) !is.null(x) && !inherits(x, "try-error"), res)
  if (length(res) == 0) return(NULL)
  do.call(rbind, res)
}

## ── Objective function ───────────────────────────────────────
## `metric()` is intentionally factored out: it currently scores
## fit on day-of-year (DOY) error, matching the original
## calibration. To address the reviewer's request for a
## development-rate-based objective instead of a days-based one,
## only this function needs to change (e.g. compare 1/DAS rates
## per stage instead of raw DOY).
metric <- function(predicted_doy, observed_doy) {
  log(sum((predicted_doy - observed_doy)^2, na.rm = TRUE))
}

## `make_objfun()` returns a closure `optim()` can call directly:
## it edits the cultivar's phenology parameters in the shared base
## file, reruns every site/year/planting-date combo for that
## cultivar, lines predicted up against observed DOY, and scores
## the fit with `metric()`.
make_objfun <- function(mdat, cultivar.name, cl) {
  function(parms, starting.values) {
    ## `parms` are multipliers on the starting values — this is
    ## how the original script kept all 6 parameters on a similar
    ## (~1) scale for Nelder-Mead, despite spanning days, fractions,
    ## and photoperiod hours.
    phen.parms <- parms * starting.values
    ns <- paste(CONFIG$cultivar_root, cultivar.name, sep = ".")

    ## Reject parameter sets where late-grain-fill target would be
    ## shorter than early-grain-fill target — physically invalid.
    if (phen.parms[6] <= phen.parms[5]) return(NA)

    ## Push the 6 candidate parameters into the cultivar's Command
    ## list in the shared base file (every worker's copy is made
    ## from this file, so editing it here applies to the whole run).
    ## NOTE: "Vegetative.Target" (not just "Vegetative") is required
    ## here — edit_apsimx_replacement() matches `parm` against the
    ## cultivar's Command lines with grepl(), and the bare substring
    ## "Vegetative" also matches "VegetativePhotoperiodModifier",
    ## which makes the match ambiguous and crashes the edit.
    edits <- list(
      list(parm = "Vegetative.Target", value = phen.parms[1]),
      list(parm = "EarlyFlowering", value = phen.parms[2]),
      list(parm = "EarlyGrainFilling", value = phen.parms[3]),
      list(parm = "LateGrainFilling", value = phen.parms[4]),
      list(parm = "ReproductivePhotoperiodModifier",
           value = paste(phen.parms[5], phen.parms[6], sep = ", "))
    )
    for (e in edits) {
      edit_apsimx_replacement(file = CONFIG$base_file, src.dir = CONFIG$sim_dir,
                               wrt.dir = CONFIG$sim_dir,
                               root = list("Models.Core.Folder", 1),
                               node.string = ns, parm = e$parm, value = e$value,
                               verbose = FALSE, overwrite = TRUE)
    }

    ## Rerun every observed site/year/planting-date for this
    ## cultivar with the candidate parameters in place.
    mg.res <- run_cultivar_parallel(mdat, cultivar.name, cl)
    if (is.null(mg.res)) return(NA)

    ## Translate APSIM's predicted stage names to Fehr & Caviness
    ## stages, then join predicted dates to observed dates on
    ## (site/year/planting-date, stage) so each comparison is like
    ## for like.
    mg.res <- merge(phen.dict, mg.res,
                     by.x = "apsim.phen", by.y = "Soybean.Phenology.CurrentStageName")
    comb <- merge(mdat, mg.res, by.x = c("id", "phenology"), by.y = c("id", "fc.phen"))
    comb$doy.x <- as.numeric(strftime(comb$date, "%j"))
    comb$doy.y <- as.numeric(strftime(comb$Date, "%j"))

    rss <- metric(comb$doy.y, comb$doy.x)
    cat(strftime(Sys.time()), "-", cultivar.name, "-",
        paste(round(phen.parms, 2), collapse = ", "), "- RSS =", round(rss, 2), "\n")
    rss
  }
}

## ── Main loop: calibrate each production cultivar in turn ────
## One cultivar at a time (not in parallel across cultivars)
## because each cultivar's own `optim()` call already parallelizes
## across all of that cultivar's site/year/planting-date runs —
## doing both at once would oversubscribe the available cores.
results <- list()

for (cultivar.name in CONFIG$cultivars) {

  ## Subset observed data to this cultivar's maturity group, and
  ## build a unique run id per site/year/planting-date combo.
  mg.tgt <- CONFIG$cultivar_mg[[cultivar.name]]
  mdat <- subset(dat, round(mg) == mg.tgt)
  mdat$year <- as.numeric(strftime(mdat$date, "%Y"))
  mdat$pd <- as.Date(mdat$pd, tryFormats = c("%m/%d/%Y", "%m-%d-%Y"))
  mdat$id <- paste(mdat$site, mdat$year, as.numeric(mdat$pd), sep = "-")
  mdat <- subset(mdat, !is.na(pd))

  ivi <- which(apsim.mg.values$mg == mg.tgt)
  initial.values <- unlist(apsim.mg.values[ivi, -1])

  cat("\n==== Calibrating", cultivar.name, "(mg =", mg.tgt, ") ====\n")
  cat("Observations:", length(unique(mdat$id)), "site/year/planting-date combos\n")

  ## One cluster per cultivar, sized to the number of distinct runs
  ## (no point starting more workers than there is work for).
  ncores <- min(CONFIG$cores, length(unique(mdat$id)))
  cl <- makeCluster(max(1, ncores))

  ## tryCatch + finally guarantees stopCluster() runs even if
  ## optim()/objfun() errors out mid-calibration, so a single bad
  ## cultivar can't leak worker processes into the next iteration.
  op1 <- tryCatch({
    clusterEvalQ(cl, { library(apsimx); apsimx_options(exe.path = apsimx::apsimx_options()$exe.path) })
    objfun <- make_objfun(mdat, cultivar.name, cl)
    optim(par = rep(1, 6), fn = objfun, method = "Nelder-Mead",
          starting.values = initial.values, control = list(maxit = CONFIG$maxit))
  }, error = function(e) {
    message("[ERROR] Calibration failed for ", cultivar.name, ": ", e$message)
    NULL
  }, finally = stopCluster(cl))

  ## Save this cultivar's fitted coefficients immediately (not just
  ## at the very end) so a crash partway through the loop doesn't
  ## lose already-completed calibrations.
  if (!is.null(op1)) {
    coef.df <- data.frame(cultivar = cultivar.name,
                           parameter = names(initial.values),
                           initial   = as.numeric(initial.values),
                           fitted    = as.numeric(initial.values) * op1$par,
                           rss       = op1$value)
    results[[cultivar.name]] <- coef.df
    write.csv(coef.df, file.path(CONFIG$out_dir, paste0(cultivar.name, "-coefficients.csv")),
              row.names = FALSE)
  }
}

## ── Combine and save final results ───────────────────────────
final.coefs <- do.call(rbind, results)
saveRDS(final.coefs, file.path(CONFIG$out_dir, "all-coefficients.rds"))
write.csv(final.coefs, file.path(CONFIG$out_dir, "all-coefficients.csv"), row.names = FALSE)

cat("\n[DONE] Calibrated", length(results), "cultivars. See", CONFIG$out_dir, "\n")
