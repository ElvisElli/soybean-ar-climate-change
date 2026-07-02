## =====================================================================
## modules/optimize.R — parameter mapping, objective, staged optimizer
## =====================================================================
## Implements the STAGES plan from config.R: for each group, each stage
## optimizes only its own parameters (as multipliers on their starting
## values, keeping every scalar near 1 for Nelder-Mead) while holding all
## others fixed at their current best, then carries the result forward.
## =====================================================================

## ---- Parameter layout helpers ---------------------------------------
## Column name(s) a parameter occupies in START_VALUES: "veg" for n=1,
## c("photoperiod1","photoperiod2") for n=2.
param_columns <- function(pn) {
  n <- PARAMETERS[[pn]]$n
  if (n == 1) pn else paste0(pn, seq_len(n))
}

## Validate the START_VALUES layout against PARAMETERS once, up front.
local({
  expected <- c("group", unlist(lapply(names(PARAMETERS), param_columns)))
  missing  <- setdiff(expected, names(START_VALUES))
  if (length(missing)) stop("START_VALUES is missing columns: ", paste(missing, collapse = ", "))
})

## Group's starting scalars as a named list of numeric vectors.
group_start <- function(group) {
  row <- START_VALUES[START_VALUES$group == group, ]
  setNames(lapply(names(PARAMETERS), function(pn) as.numeric(row[param_columns(pn)])), names(PARAMETERS))
}

## Reconstruct per-parameter values for a stage from a flat multiplier
## vector (× the starting scalars for that stage's params).
map_multipliers <- function(stage, mult, start) {
  out <- list(); i <- 1
  for (pn in stage$params) {
    n <- PARAMETERS[[pn]]$n
    out[[pn]] <- mult[i:(i + n - 1)] * start[[pn]]
    i <- i + n
  }
  out
}

## ---- Write parameter values into the cultivar -----------------------
apply_params <- function(group, values) {
  ns <- paste(CONFIG$cultivar_root, group, sep = ".")
  for (pn in names(values)) {
    spec <- PARAMETERS[[pn]]
    edit_apsimx_replacement(file = CONFIG$base_file,
                            src.dir = abspath(CONFIG$sim_dir), wrt.dir = abspath(CONFIG$sim_dir),
                            root = list("Models.Core.Folder", 1), node.string = ns,
                            parm = spec$apsim, value = spec$render(values[[pn]]),
                            verbose = FALSE, overwrite = TRUE)
  }
}

## ---- Match predicted to observed ------------------------------------
## Phenology: first day the model enters each stage (the transition),
## joined to observed stage dates; restricted to the stage's fit set.
match_phenology <- function(gdat, sim, fit_stages) {
  m <- merge(PHEN_DICT, sim, by.x = "apsim.phen", by.y = "Soybean.Phenology.CurrentStageName")
  m <- m[order(m$id, m$Date), ]
  m <- m[!duplicated(m[, c("id", "apsim.phen")]), ]     # transition day only
  comb <- merge(gdat, m, by.x = c("id", "phenology"), by.y = c("id", "fc.phen"))
  comb <- comb[comb$phenology %in% fit_stages, ]
  comb$doy.obs  <- as.numeric(format(comb$date, "%j"))
  comb$doy.pred <- as.numeric(format(comb$Date, "%j"))
  comb
}

## Yield: simulated final yield = peak daily Yield_kgha per id (reached at
## maturity, before any harvest reset), joined to observed yield.
match_yield <- function(gdat, sim) {
  ymax <- aggregate(Yield_kgha ~ id, data = sim, FUN = function(x) max(x, na.rm = TRUE))
  obs  <- gdat[!duplicated(gdat$id), c("id", "yield")]
  comb <- merge(obs, ymax, by = "id")
  comb$yield.obs  <- comb$yield
  comb$yield.pred <- comb$Yield_kgha
  comb
}

## ---- Progress log (drives the progress plot) ------------------------
PROGRESS <- new.env()
PROGRESS$rows <- list()
log_eval <- function(group, stage, values, error) {
  flat <- unlist(lapply(names(values), function(pn) {
    v <- values[[pn]]; setNames(v, if (length(v) == 1) pn else paste0(pn, seq_along(v)))
  }))
  PROGRESS$rows[[length(PROGRESS$rows) + 1]] <-
    data.frame(group = group, stage = stage, error = error,
               as.list(round(flat, 4)), stringsAsFactors = FALSE)
  cat(sprintf("%s  %-9s [%-12s] err=%7.3f  %s\n", format(Sys.time(), "%H:%M:%S"),
              group, stage, error,
              paste(sprintf("%s=%.3g", names(flat), flat), collapse = " ")))
}

## ---- Objective for one stage ----------------------------------------
make_objfun <- function(group, stage, gdat, cl, best) {
  start <- group_start(group)
  function(mult) {
    cand <- map_multipliers(stage, mult, start)
    if (!CONSTRAINTS(cand)) return(NA)
    apply_params(group, cand)                       # edit only this stage's params
    sim <- run_group(gdat, group, cl)
    if (is.null(sim)) return(NA)
    if (stage$target == "phenology") {
      comb <- match_phenology(gdat, sim, stage$fit_stages)
      if (!nrow(comb)) return(NA)
      err <- OBJECTIVE(comb$doy.pred, comb$doy.obs)
    } else {
      comb <- match_yield(gdat, sim)
      if (!nrow(comb)) return(NA)
      err <- OBJECTIVE(comb$yield.pred, comb$yield.obs)
    }
    log_eval(group, stage$name, cand, err)
    err
  }
}

## ---- Calibrate one group through all stages -------------------------
calibrate_group <- function(group, gdat, cl) {
  message(sprintf("\n==== %s : %d site-year-plantings, %d obs rows ====",
                  group, length(unique(gdat$id)), nrow(gdat)))
  start <- group_start(group)
  best  <- start                                    # running best (fixed carry-forward)

  ## Put ALL starting values into the cultivar first, so params not yet
  ## optimized sit at the config start (not the cloned parent's defaults).
  apply_params(group, best)

  for (stage in STAGES) {
    n_scalar <- sum(vapply(stage$params, function(pn) PARAMETERS[[pn]]$n, numeric(1)))
    message(sprintf("  -- stage '%s': optimizing {%s} (%d scalars) against %s",
                    stage$name, paste(stage$params, collapse = ", "), n_scalar,
                    if (stage$target == "phenology") paste(stage$fit_stages, collapse = "/") else "yield"))
    objfun <- make_objfun(group, stage, gdat, cl, best)
    op <- optim(par = rep(1, n_scalar), fn = objfun, method = CONFIG$method,
                control = list(maxit = CONFIG$maxit))
    fitted <- map_multipliers(stage, op$par, start)
    best[stage$params] <- fitted
    apply_params(group, fitted)                     # lock this stage's best into the file
    message(sprintf("     -> best error %.3f", op$value))
  }

  ## Final comparison runs: uncalibrated (all params at start) and
  ## calibrated (all params at best), each captured for the figures.
  capture <- function(values) {
    apply_params(group, values)
    sim <- run_group(gdat, group, cl)
    phen  <- match_phenology(gdat, sim, unique(PHEN_DICT$fc.phen))
    yield <- match_yield(gdat, sim)
    list(phen = phen, yield = yield)
  }
  uncal <- capture(start)
  cal   <- capture(best)
  apply_params(group, best)                         # leave the file at the calibrated state

  ## Flatten best into a tidy coefficient table.
  coef <- do.call(rbind, lapply(names(PARAMETERS), function(pn) {
    s <- start[[pn]]; b <- best[[pn]]
    data.frame(group = group, parameter = param_columns(pn),
               start = s, fitted = b, stringsAsFactors = FALSE)
  }))
  list(coef = coef, uncal = uncal, cal = cal)
}
