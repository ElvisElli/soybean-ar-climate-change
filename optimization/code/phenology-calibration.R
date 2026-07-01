## ============================================================
## APSIM soybean phenology calibration — multi-site, parallel
##
## Calibrates 4 maturity-group subclasses — early4, late4, early5,
## late5 — matching the classification used in Table S1 of the
## supplementary material (supp-mat-06-23-2026.docx), against the
## production grid-sim template (templates/soybean-mg4-baseline.apsimx).
## Using the same template/APSIM version as the grid simulation keeps
## this calibration consistent with what the manuscript results are
## actually built on.
##
## In addition to the 6 phenology parameters calibrated previously,
## this version also optimizes 2 growth parameters simultaneously
## (Area of Largest Leaf, Radiation Use Efficiency), per the
## supplementary methods: "all parameters were optimized
## simultaneously to potentially capture parameter interactions."
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
  ## The 4 maturity-group subclasses from Table S1. These cultivar
  ## nodes don't exist in the production template yet — they're
  ## cloned from the nearest existing MG4/MG5 cultivar by
  ## clone_calibration_cultivars() below, purely inside our local
  ## calibration copy (never touches the shared template).
  cultivars       = c("EarlyMG4", "LateMG4", "EarlyMG5", "LateMG5"),
  ## Maps each calibration cultivar -> (a) the mg2 label used to
  ## subset observed data, and (b) the existing production cultivar
  ## its Command block (and therefore its Cultivar node scaffold) is
  ## cloned from.
  cultivar_mg2    = c(EarlyMG4 = "early4", LateMG4 = "late4",
                       EarlyMG5 = "early5", LateMG5 = "late5"),
  cultivar_clone_source = c(EarlyMG4 = "PurcellMG4", LateMG4 = "PurcellMG4",
                             EarlyMG5 = "PurcellMG5", LateMG5 = "PurcellMG5"),
  cores           = max(1, detectCores(logical = FALSE) - 1),
  ## Each objective-function evaluation reruns every observed
  ## site/year/planting-date combo (measured ~6.4 min for 168 combos
  ## on 4 cores here). Nelder-Mead in 8 dimensions needs a 9-point
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
## Resolved path, captured now so it can be handed explicitly to each
## PSOCK cluster worker below — workers start as fresh R processes
## with their own unset apsimx_options(), so reading
## apsimx::apsimx_options()$exe.path *inside* a worker (instead of
## passing this master-resolved value in) silently picks up nothing
## and breaks every APSIM call on that worker.
resolved_exe_path <- get("exe.path", envir = apsimx:::apsimx.options)

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

## ── Clone the 4 calibration cultivars into the local copy ────
## The production template only defines PurcellMG4/5/6 — it has no
## early4/late4/early5/late5 subclasses. We clone each target
## cultivar from its nearest existing production cultivar (same
## $type/Command scaffold, just a new Name) and append it under
## Replacements.Soybean.Elli, purely in this local calibration
## copy. No GUID/ID field exists on Cultivar nodes in this APSIM
## version, so a name change is a safe, complete clone.
clone_calibration_cultivars <- function(file) {
  tpl <- fromJSON(file, simplifyVector = FALSE)

  find_node <- function(node, target_name, path = "") {
    nm <- node$Name
    newpath <- if (nzchar(path)) paste(path, nm, sep = ".") else nm
    if (identical(nm, target_name)) return(node)
    kids <- node$Children
    if (is.null(kids)) return(NULL)
    for (k in kids) {
      hit <- find_node(k, target_name, newpath)
      if (!is.null(hit)) return(hit)
    }
    NULL
  }

  add_clones <- function(node, path = "") {
    nm <- node$Name
    newpath <- if (nzchar(path)) paste(path, nm, sep = ".") else nm
    if (identical(newpath, "Simulations.Replacements.Soybean.Elli")) {
      existing_names <- vapply(node$Children, function(c) c$Name, character(1))
      for (new.name in CONFIG$cultivars) {
        if (new.name %in% existing_names) next  # already present (re-run safety)
        src.name <- CONFIG$cultivar_clone_source[[new.name]]
        src.node <- find_node(tpl, src.name)
        if (is.null(src.node)) stop("Clone source cultivar not found: ", src.name)
        clone <- src.node       # R copy-on-modify: this is an independent deep copy
        clone$Name <- new.name
        node$Children <- c(node$Children, list(clone))
      }
      return(node)
    }
    if (!is.null(node$Children)) {
      node$Children <- lapply(node$Children, add_clones, path = newpath)
    }
    node
  }

  tpl <- add_clones(tpl)
  write(toJSON(tpl, auto_unbox = TRUE, null = "null", pretty = TRUE), file)
}
clone_calibration_cultivars(file.path(CONFIG$sim_dir, CONFIG$base_file))

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

## Initial parameter guesses, per calibration cultivar. 8 columns:
## the 6 phenology parameters from before, plus the 2 growth
## parameters (area.largest.leaf, rue) now optimized alongside them.
##
## EarlyMG4/EarlyMG5 starting values are taken directly from
## Table S1 of supp-mat-06-23-2026.docx (already-calibrated values
## from the team's own prior optimization run) — reusing them as
## the starting point should let `optim()` converge quickly if
## they're already close to optimal, while still re-verifying the
## fit end-to-end through this pipeline.
##
## LateMG4/LateMG5 have no equivalent published starting point yet,
## so they start from the nearest previous mg-indexed default
## (apsim.mg.values row mg=4 / mg=5 from the earlier 3-cultivar
## calibration) for phenology, and the production template's
## default AreaLargestLeaf/RUE (0.007 / 1.25) for the 2 growth
## parameters.
apsim.cultivar.values <- data.frame(
  cultivar               = c("EarlyMG4", "LateMG4", "EarlyMG5", "LateMG5"),
  vegetative              = c(484,   388,   634,   396),
  early.flowering         = c(185,   140,   189,   160),
  early.grain             = c(0.338, 0.324, 0.042, 0.072),
  late.grain              = c(384,   664,   418,   696),
  photoperiod.modifier1   = c(11.48, 13.1,  12.70, 12.83),
  photoperiod.modifer2    = c(17.24, 16.49, 14.31, 16.13),
  area.largest.leaf       = c(0.004, 0.007, 0.004, 0.007),
  rue                     = c(1.48,  1.25,  1.05,  1.25)
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
    ## how the original script kept all parameters on a similar
    ## (~1) scale for Nelder-Mead, despite spanning days, fractions,
    ## photoperiod hours, leaf area (m2), and RUE (g/MJ).
    phen.parms <- parms * starting.values
    ns <- paste(CONFIG$cultivar_root, cultivar.name, sep = ".")

    ## Reject parameter sets where late-grain-fill target would be
    ## shorter than early-grain-fill target, or where either growth
    ## parameter has gone non-positive — all physically invalid.
    if (phen.parms[6] <= phen.parms[5]) return(NA)
    if (phen.parms[7] <= 0 || phen.parms[8] <= 0) return(NA)

    ## Push the 8 candidate parameters into the cultivar's Command
    ## list in the shared base file (every worker's copy is made
    ## from this file, so editing it here applies to the whole run).
    ## NOTE: "Vegetative.Target" (not just "Vegetative") is required
    ## here — edit_apsimx_replacement() matches `parm` against the
    ## cultivar's Command lines with grepl(), and the bare substring
    ## "Vegetative" also matches "VegetativePhotoperiodModifier",
    ## which makes the match ambiguous and crashes the edit.
    ## "AreaLargestLeaf" and "RUE" are each unique substrings within
    ## the cultivar's Command list, so no similar ambiguity there.
    edits <- list(
      list(parm = "Vegetative.Target", value = phen.parms[1]),
      list(parm = "EarlyFlowering", value = phen.parms[2]),
      list(parm = "EarlyGrainFilling", value = phen.parms[3]),
      list(parm = "LateGrainFilling", value = phen.parms[4]),
      list(parm = "ReproductivePhotoperiodModifier",
           value = paste(phen.parms[5], phen.parms[6], sep = ", ")),
      list(parm = "AreaLargestLeaf", value = phen.parms[7]),
      list(parm = "RUE", value = phen.parms[8])
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

  ## Subset observed data to this cultivar's mg2 subclass (early4,
  ## late4, early5, or late5), and build a unique run id per
  ## site/year/planting-date combo.
  mg2.tgt <- CONFIG$cultivar_mg2[[cultivar.name]]
  mdat <- subset(dat, mg2 == mg2.tgt)
  mdat$year <- as.numeric(strftime(mdat$date, "%Y"))
  mdat$pd <- as.Date(mdat$pd, tryFormats = c("%m/%d/%Y", "%m-%d-%Y"))
  mdat$id <- paste(mdat$site, mdat$year, as.numeric(mdat$pd), sep = "-")
  mdat <- subset(mdat, !is.na(pd))

  ivi <- which(apsim.cultivar.values$cultivar == cultivar.name)
  initial.values <- unlist(apsim.cultivar.values[ivi, -1])

  cat("\n==== Calibrating", cultivar.name, "(mg2 =", mg2.tgt, ") ====\n")
  cat("Observations:", length(unique(mdat$id)), "site/year/planting-date combos\n")

  ## One cluster per cultivar, sized to the number of distinct runs
  ## (no point starting more workers than there is work for).
  ncores <- min(CONFIG$cores, length(unique(mdat$id)))
  cl <- makeCluster(max(1, ncores))

  ## tryCatch + finally guarantees stopCluster() runs even if
  ## optim()/objfun() errors out mid-calibration, so a single bad
  ## cultivar can't leak worker processes into the next iteration.
  op1 <- tryCatch({
    clusterExport(cl, "resolved_exe_path", envir = environment())
    clusterEvalQ(cl, { library(apsimx); apsimx_options(exe.path = resolved_exe_path) })
    objfun <- make_objfun(mdat, cultivar.name, cl)
    optim(par = rep(1, 8), fn = objfun, method = "Nelder-Mead",
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
