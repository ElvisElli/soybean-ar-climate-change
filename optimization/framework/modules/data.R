## =====================================================================
## modules/data.R — load observed data and apply quality control
## =====================================================================
## Produces `OBS`, the cleaned observation table, and prints a QC report
## so every record dropped is accounted for. QC rules come from CONFIG's
## QC list; nothing here is hard-coded.
## =====================================================================

## APSIM stage names <-> Fehr & Caviness (1977) field staging.
PHEN_DICT <- na.omit(data.frame(
  apsim.phen = c("Sowing", "Germination", "Emergence", "StartFlowering",
                 "StartPodDevelopment", "StartGrainFilling", "EndCanopyDevelopment",
                 "EndPodDevelopment", "EndGrainFill", "Maturity", "HarvestRipe"),
  fc.phen    = c(NA, NA, "VE", "R1", "R3", "R5", NA, NA, "R6", "R7", "R8"),
  stringsAsFactors = FALSE
))

load_observations <- function() {
  ## fileEncoding strips the UTF-8 BOM these CSVs carry; without it the
  ## first column name is mangled (site -> X...site) and joins silently fail.
  dat <- read.csv(abspath(CONFIG$raw_dat),   fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE)
  si  <- read.csv(abspath(CONFIG$site_info), fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE)
  dat <- merge(dat, si)                        # add lat/lon by site
  dat$date <- as.Date(dat$date, "%m/%d/%Y")
  dat$pd   <- as.Date(dat$pd, tryFormats = c("%m/%d/%Y", "%m-%d-%Y"))
  dat$pd.doy <- as.numeric(format(dat$pd, "%j"))
  dat$year   <- as.numeric(format(dat$date, "%Y"))
  ## id MUST include mg2: the same site-year-planting hosts several
  ## maturity groups, so an id without mg2 is not unique to a group and
  ## the replicate-dedup would merge different groups' observations.
  dat$id     <- paste(dat$mg2, dat$site, dat$year, as.numeric(dat$pd), sep = "-")

  n0 <- nrow(dat)
  report <- function(msg, n) message(sprintf("[QC] %-46s %6d rows", msg, n))
  message("[QC] ------- observed-data quality control -------")
  report("loaded", n0)

  ## (1) Southern-hemisphere sites: DOY seasons are inverted, so DOY-based
  ## phenology comparison is meaningless. Drop if configured.
  if (isTRUE(QC$drop_southern)) {
    dat <- dat[dat$lat > 0, ]
    report("after dropping southern hemisphere (lat<0)", nrow(dat))
  }

  ## (2) Implausible planting dates (the main contaminant — see README).
  keep_pd <- !is.na(dat$pd.doy) &
             dat$pd.doy >= QC$planting_doy_min &
             dat$pd.doy <= QC$planting_doy_max
  dat <- dat[keep_pd, ]
  report(sprintf("after planting window [%d,%d] DOY",
                 QC$planting_doy_min, QC$planting_doy_max), nrow(dat))

  ## (3) Deduplicate replicate plots to one row per (id, stage): average
  ## the observed stage date and yield across replicates. Keeps the point
  ## cloud honest (one point per site-year-planting-stage).
  if (isTRUE(QC$dedup_replicates)) {
    keys <- c("id", "phenology")
    agg <- aggregate(cbind(date_num = as.numeric(dat$date),
                           yield     = dat$yield) ~ id + phenology,
                     data = dat, FUN = function(x) mean(x, na.rm = TRUE))
    ## carry the descriptor columns (constant within id) back in
    meta <- dat[!duplicated(dat$id), setdiff(names(dat), c("phenology", "date", "yield", "date_num"))]
    dat  <- merge(agg, meta, by = "id")
    dat$date <- as.Date(dat$date_num, origin = "1970-01-01")
    dat$date_num <- NULL
    report("after de-duplicating replicate plots", nrow(dat))
  }

  message(sprintf("[QC] dropped %d of %d rows (%.0f%%)", n0 - nrow(dat), n0, 100 * (n0 - nrow(dat)) / n0))
  dat
}

OBS <- load_observations()
