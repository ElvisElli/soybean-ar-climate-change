## ============================================================
## APSIM soybean phenology calibration — multi-site, parallel
## Generalized from optimization-example-caio.R so the same
## script can be reused for other crops/projects: everything
## project-specific lives in CONFIG below.
## ============================================================

rm(list = ls())

library(apsimx)
library(dplyr)
library(parallel)

## ── CONFIG ──────────────────────────────────────────────────
## Edit this block to point the script at a different project.

CONFIG <- list(
  sim_dir       = "optimization/simulations",     # base-simulation + soil/ + weather/
  base_file     = "base-simulation.apsimx",
  raw_dat       = "optimization/raw-data/phenology-all-sites.csv",
  site_info     = "optimization/raw-data/site-info.csv",
  cultivar_root = ".Simulations.Replacements.Soybean.Elli",
  sowing_path   = ".Simulations.Simulation.Field.Sowing",
  cultivar_prefix = "Elli_",
  mg_target_range = 7:17,         # index into unique(dat$mg2)
  cores         = max(1, detectCores(logical = FALSE) - 1),
  maxit         = 100,
  out_dir       = "optimization/results"
)

if (!dir.exists(CONFIG$out_dir)) dir.create(CONFIG$out_dir, recursive = TRUE)

## ── APSIM exe auto-detect ───────────────────────────────────
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

## ── Load and clean observed data ────────────────────────────
dat <- read.csv(CONFIG$raw_dat)
site.info <- read.csv(CONFIG$site_info)

dat <- merge(dat, site.info)
dat <- dat[order(dat$mg), ]
dat$date <- as.Date(dat$date, "%m/%d/%Y")
dat <- subset(dat, !(as.numeric(strftime(date, "%m")) == 12 & lat > 0))

## APSIM phenology stage -> Fehr & Caviness stage
phen.dict <- na.omit(data.frame(
  apsim.phen = c("Sowing", "Germination", "Emergence", "StartFlowering",
                 "StartPodDevelopment", "StartGrainFilling", "EndCanopyDevelopment",
                 "EndPodDevelopment", "EndGrainFill", "Maturity", "HarvestRipe"),
  fc.phen    = c(NA, NA, "VE", "R1", "R3", "R5", NA, NA, "R6", "R7", "R8")
))

## Initial parameter guesses by maturity group (project-specific
## priors; swap this table out for a different crop/cultivar set)
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
## `worker_dir` isolates each parallel worker's copy of the base
## file so concurrent runs don't clobber each other.
run_one_sim <- function(id, idat, cultivar.name, worker_dir, sim_dir) {

  site.name <- idat$site[1]
  soil <- readRDS(file.path(sim_dir, "soil", paste0(site.name, ".rds")))
  if (inherits(soil, "list")) soil <- soil[[1]]
  soil$initialwater <- initialwater_parms(
    Thickness = soil$soil$Thickness,
    InitialValues = soil$soil$DUL
  )
  met <- file.path(sim_dir, "weather", paste0(site.name, ".met"))

  if (!dir.exists(worker_dir)) dir.create(worker_dir, recursive = TRUE)
  worker_file <- paste0("sim-", gsub("[^A-Za-z0-9]", "_", id), ".apsimx")
  file.copy(file.path(sim_dir, CONFIG$base_file),
            file.path(worker_dir, worker_file), overwrite = TRUE)

  start.date <- as.character(idat$pd[1] - 90)
  end.date   <- as.character(max(idat$date) + 30)
  sim.pd     <- strftime(unique(idat$pd), "%b-%d")

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

  sim0 <- try(apsimx(file = worker_file, src.dir = worker_dir, cleanup = TRUE),
              silent = TRUE)
  file.remove(file.path(worker_dir, worker_file))

  if (inherits(sim0, "try-error")) return(NULL)
  sim0$id <- id
  row.names(sim0) <- NULL
  sim0
}

## ── Run all id's for one maturity group, in parallel ────────
run_mg_parallel <- function(mdat, cultivar.name, cl) {
  ids <- unique(mdat$id)
  clusterExport(cl, c("run_one_sim", "CONFIG"), envir = environment())
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

## ── Objective function: predicted vs observed DOY (RSS) ─────
## Kept on the original days/DOY basis for now; swap `metric()`
## below for a development-rate objective (e.g. 1/DAS per stage)
## without touching the rest of the pipeline.
metric <- function(predicted_doy, observed_doy) {
  log(sum((predicted_doy - observed_doy)^2, na.rm = TRUE))
}

make_objfun <- function(mdat, cultivar.name, cl) {
  function(parms, starting.values) {
    phen.parms <- parms * starting.values
    ns <- paste(CONFIG$cultivar_root, cultivar.name, sep = ".")
    if (phen.parms[6] <= phen.parms[5]) return(NA)

    edits <- list(
      list(parm = "Vegetative", value = phen.parms[1]),
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

    mg.res <- run_mg_parallel(mdat, cultivar.name, cl)
    if (is.null(mg.res)) return(NA)

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

## ── Main loop over maturity groups ───────────────────────────
results <- list()

for (mg.tgt in unique(dat$mg2)[CONFIG$mg_target_range]) {

  mdat <- subset(dat, mg2 == mg.tgt)
  mdat$year <- as.numeric(strftime(mdat$date, "%Y"))
  mdat$pd <- as.Date(mdat$pd, tryFormats = c("%m/%d/%Y", "%m-%d-%Y"))
  mdat$id <- paste(mdat$site, mdat$year, as.numeric(mdat$pd), sep = "-")
  mdat <- subset(mdat, !is.na(pd))

  cultivar.name <- paste0(CONFIG$cultivar_prefix, mg.tgt)
  ivi <- which(apsim.mg.values$mg == round(mdat$mg[1], 0))
  initial.values <- unlist(apsim.mg.values[ivi, -1])

  cat("\n==== Calibrating", cultivar.name, "====\n")

  ncores <- min(CONFIG$cores, length(unique(mdat$id)))
  cl <- makeCluster(max(1, ncores))

  op1 <- tryCatch({
    clusterEvalQ(cl, { library(apsimx); apsimx_options(exe.path = apsimx::apsimx_options()$exe.path) })
    objfun <- make_objfun(mdat, cultivar.name, cl)
    optim(par = rep(1, 6), fn = objfun, method = "Nelder-Mead",
          starting.values = initial.values, control = list(maxit = CONFIG$maxit))
  }, error = function(e) {
    message("[ERROR] Calibration failed for ", cultivar.name, ": ", e$message)
    NULL
  }, finally = stopCluster(cl))

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

final.coefs <- do.call(rbind, results)
saveRDS(final.coefs, file.path(CONFIG$out_dir, "all-coefficients.rds"))
write.csv(final.coefs, file.path(CONFIG$out_dir, "all-coefficients.csv"), row.names = FALSE)

cat("\n[DONE] Calibrated", length(results), "cultivars. See", CONFIG$out_dir, "\n")
