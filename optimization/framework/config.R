## =====================================================================
## config.R — the ONLY file you normally edit
## =====================================================================
## Everything project-specific lives here: which parameters to optimize,
## which groups to calibrate, how to clean the observed data, and how
## hard to search. The engine in modules/ reads this and needs no edits.
##
## To ADD a parameter:    add one entry to PARAMETERS and one column to
##                        START_VALUES. Nothing else changes.
## To REMOVE a parameter: delete its PARAMETERS entry and its column.
## To calibrate new groups: edit GROUPS + START_VALUES.
##
## Paths are all RELATIVE to PROJECT_ROOT, which is auto-detected, so the
## whole repo can be moved/downloaded and run without editing anything.
## =====================================================================

CONFIG <- list(

  ## ---- Paths (relative to auto-detected project root) --------------
  ## sim_dir must contain: the base template + soil/ + weather/ folders.
  sim_dir   = "optimization/simulations",
  base_file = "base-simulation.apsimx",         # working copy (regenerated each run)
  template  = "templates/soybean-mg4-baseline.apsimx",  # the production grid-sim template
  raw_dat   = "optimization/raw-data/phenology-all-sites.csv",
  site_info = "optimization/raw-data/site-info.csv",
  out_dir   = "optimization/framework/results",

  ## ---- APSIM node addresses (change only if your template differs) --
  cultivar_root = ".Simulations.Replacements.Soybean.Elli",
  sowing_path   = ".Simulations.Simulation.Field.SowSoybean",
  ## Managers that must be neutralized for calibration (see modules/template.R).
  cold_harvest_path = ".Simulations.Simulation.Field.Harvest by min temp",

  ## ---- Parallelism --------------------------------------------------
  ## Leave 1 physical core free for the OS. Each objective evaluation
  ## fans every site out across this many workers.
  cores = max(1, parallel::detectCores(logical = FALSE) - 1),

  ## ---- Optimizer ----------------------------------------------------
  ## Nelder-Mead needs (n_scalar_params + 1) evaluations just to build
  ## its starting simplex, then more per iteration. Each evaluation
  ## reruns every site. maxit is deliberately modest; raise it on a
  ## faster/more-core machine for a fuller search.
  maxit  = 15,
  method = "Nelder-Mead"
)

## =====================================================================
## PARAMETERS — declarative list of what to optimize
## =====================================================================
## Each entry describes ONE calibratable parameter:
##   apsim  : the substring matched against the cultivar's APSIM Command
##            lines (must be UNIQUE within the cultivar, or the edit is
##            ambiguous and fails — e.g. use "Vegetative.Target", not the
##            bare "Vegetative" which also matches "VegetativePhotoperiod").
##   n      : how many scalar values this parameter occupies in the
##            optimizer's search vector (1 for a single value; 2 for the
##            photoperiod XY pair, etc.).
##   render : function turning the n scalar value(s) into the exact string
##            APSIM expects for that Command.
##   lower  : optional lower bound as a MULTIPLIER of the starting value
##            (the objective rejects sets that violate it). NA = no bound.
##
## The optimizer works on a flat vector = c(all params' scalars in this
## order). Adding/removing a parameter here automatically resizes that
## vector, the simplex, the edits, and the saved coefficients.
## =====================================================================
PARAMETERS <- list(
  veg = list(
    apsim  = "Vegetative.Target",
    n      = 1,
    render = function(v) v[1]),
  early_flowering = list(
    apsim  = "EarlyFlowering",
    n      = 1,
    render = function(v) v[1]),
  veg_photoperiod = list(
    ## Vegetative photoperiod sensitivity (XY pair). Included in the
    ## vegetative stage because, as in DSSAT, this can shift the
    ## emergence->flowering duration and is not safely assumed constant.
    apsim  = "VegetativePhotoperiodModifier",
    n      = 2,
    render = function(v) paste(v[1], v[2], sep = ", ")),
  early_grain = list(
    apsim  = "EarlyGrainFilling",
    n      = 1,
    render = function(v) v[1]),
  late_grain = list(
    apsim  = "LateGrainFilling",
    n      = 1,
    render = function(v) v[1]),
  photoperiod = list(
    ## Reproductive photoperiod sensitivity (XY pair). Rendered "x1, x2".
    apsim  = "ReproductivePhotoperiodModifier",
    n      = 2,
    render = function(v) paste(v[1], v[2], sep = ", ")),
  area_largest_leaf = list(
    apsim  = "AreaLargestLeaf",
    n      = 1,
    render = function(v) v[1]),
  rue = list(
    apsim  = "RUE",
    n      = 1,
    render = function(v) v[1])
)

## =====================================================================
## STAGES — sequential calibration plan
## =====================================================================
## Rather than optimize all parameters at once, calibration proceeds in
## ordered stages. Each stage optimizes only its own `params` (holding
## every other parameter fixed at its current best value), scored against
## a stage-appropriate target. The best-fit values from one stage carry
## forward as fixed inputs to the next. This mirrors standard crop-model
## practice: pin down vegetative timing first, then reproductive timing,
## then yield — so later, higher-order fits can't distort earlier ones.
##
## Each stage entry:
##   name       : label for logs / progress plots.
##   params     : names of PARAMETERS entries this stage optimizes.
##   target     : "phenology" (fit day-of-year of `fit_stages`) or
##                "yield" (fit final simulated vs observed yield).
##   fit_stages : (phenology only) which observed Fehr-Caviness stages to
##                score against.
STAGES <- list(
  list(name       = "vegetative",
       params     = c("veg", "early_flowering", "veg_photoperiod"),
       target     = "phenology",
       fit_stages = c("VE", "R1")),
  list(name       = "reproductive",
       params     = c("early_grain", "late_grain", "photoperiod"),
       target     = "phenology",
       fit_stages = c("R3", "R5", "R7", "R8")),
  list(name       = "yield",
       params     = c("rue", "area_largest_leaf"),
       target     = "yield")
)

## =====================================================================
## CONSTRAINTS — reject physically-impossible parameter sets
## =====================================================================
## Receives the named list of rendered scalar values (one numeric vector
## per PARAMETERS entry) and returns TRUE if the set is admissible. The
## objective returns NA (an automatic reject) when this is FALSE, so the
## optimizer steers away from it.
CONSTRAINTS <- function(p) {
  ## Photoperiod XY pairs must be increasing (x1 < x2). Only checked for
  ## parameters present in the current stage (others aren't in `p`).
  if (!is.null(p$photoperiod)     && p$photoperiod[2]     <= p$photoperiod[1])     return(FALSE)
  if (!is.null(p$veg_photoperiod) && p$veg_photoperiod[2] <= p$veg_photoperiod[1]) return(FALSE)
  ## Every scalar must be positive.
  if (any(unlist(p) <= 0)) return(FALSE)
  TRUE
}

## =====================================================================
## GROUPS — which maturity subclasses to calibrate
## =====================================================================
## `mg2` is the column in the observed data that labels each subclass.
## `clone_from` is the existing template cultivar whose APSIM scaffold is
## cloned to create this group's calibration cultivar (see template.R).
GROUPS <- list(
  EarlyMG4 = list(mg2 = "early4", clone_from = "PurcellMG4"),
  LateMG4  = list(mg2 = "late4",  clone_from = "PurcellMG4"),
  EarlyMG5 = list(mg2 = "early5", clone_from = "PurcellMG5"),
  LateMG5  = list(mg2 = "late5",  clone_from = "PurcellMG5")
)

## =====================================================================
## START_VALUES — starting point for each group
## =====================================================================
## One row per group; columns are the scalar values in PARAMETERS order
## (photoperiod contributes TWO columns: photoperiod1, photoperiod2).
## EarlyMG4/EarlyMG5 use the team's published Table S1 values; Late groups
## start from the nearest MG default plus template growth-parameter
## defaults. These are ALSO the "uncalibrated" baseline in the figures.
## NOTE: column order must match PARAMETERS order, and each n=2 parameter
## contributes TWO columns named <param>1 and <param>2. veg_photoperiod
## defaults come from the template cultivars (MG4: 13.1/16.49, MG5:
## 12.83/16.13); the engine validates this layout on load.
START_VALUES <- data.frame(
  group             = c("EarlyMG4", "LateMG4", "EarlyMG5", "LateMG5"),
  veg               = c(484,   388,   634,   396),
  early_flowering   = c(185,   140,   189,   160),
  veg_photoperiod1  = c(13.1,  13.1,  12.83, 12.83),
  veg_photoperiod2  = c(16.49, 16.49, 16.13, 16.13),
  early_grain       = c(0.338, 0.324, 0.042, 0.072),
  late_grain        = c(384,   664,   418,   696),
  photoperiod1      = c(11.48, 13.1,  12.70, 12.83),
  photoperiod2      = c(17.24, 16.49, 14.31, 16.13),
  area_largest_leaf = c(0.004, 0.007, 0.004, 0.007),
  rue               = c(1.48,  1.25,  1.05,  1.25),
  stringsAsFactors  = FALSE
)

## =====================================================================
## QC — observed-data quality control (see modules/data.R)
## =====================================================================
## Diagnosed issues in this dataset:
##   * 25-70% of records carry agronomically-impossible planting dates
##     (Feb/Mar/Dec) for Northern-hemisphere US sites, which wreck the
##     phenology match. The paper's own Fig S1 used only Apr-Jul plantings.
##   * Replicate plots collapse to the same site-year-planting id, so the
##     raw data plots each point many times over (cosmetic, but misleading).
##   * APSIM's R6 (end grain fill) stage is structurally mismatched to the
##     observed R6 (both this work and the paper show ~35-42 d RMSE there).
QC <- list(
  planting_doy_min = 91,    # Apr 1  — drop earlier plantings
  planting_doy_max = 182,   # Jul 1  — drop later plantings
  drop_southern    = TRUE,  # drop lat<0 sites (DOY seasons are inverted)
  dedup_replicates = TRUE,  # one row per (id, stage): mean observed date/yield
  fit_drop_stages  = c("R6")  # excluded from FIT + statistics (still plotted, greyed)
)

## =====================================================================
## OBJECTIVE — how model-vs-observed error is scored
## =====================================================================
## Factored out so the fit target is swappable. Default: log of the sum of
## squared day-of-year errors (matches the original calibration). To fit on
## development RATE or days-after-planting instead, change only this.
OBJECTIVE <- function(predicted_doy, observed_doy) {
  log(sum((predicted_doy - observed_doy)^2, na.rm = TRUE))
}
