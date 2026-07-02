## =====================================================================
## modules/figures.R — result figures + progress plot
## =====================================================================
## Builds three PNGs in <out_dir>/figures:
##   phenology-1to1.png : observed vs predicted DOY, faceted by group,
##                        uncalibrated vs calibrated, RMSE/Bias insets.
##   yield-1to1.png     : observed vs predicted yield, same layout.
##   progress.png       : objective error per evaluation, by group/stage.
## Fit statistics exclude QC$fit_drop_stages (e.g. R6) but those points
## are still drawn (greyed) for transparency.
## =====================================================================

suppressMessages(library(ggplot2))

.load_all <- function(suffix) {
  parts <- lapply(names(GROUPS), function(g) {
    f <- file.path(abspath(CONFIG$out_dir), paste0(g, "-", suffix, ".rds"))
    if (!file.exists(f)) return(NULL)
    d <- readRDS(f); if (!nrow(d)) return(NULL); d$group <- g; d
  })
  do.call(rbind, Filter(Negate(is.null), parts))
}

## Per-facet RMSE/Bias labels (as a data.frame — a scalar-aes geom_text
## with no data crashes ggplot under facet_wrap).
.stat_labels <- function(d, obs, pred, prefix) {
  do.call(rbind, lapply(split(d, d$group), function(s) {
    e <- s[[pred]] - s[[obs]]
    data.frame(group = s$group[1],
               label = sprintf("%s\nRMSE: %.1f\nBias: %.1f\nn=%d",
                               prefix, sqrt(mean(e^2)), mean(e), nrow(s)))
  }))
}

.one_to_one <- function(uncal, cal, obs, pred, axis_lab, title, file, drop_stages = NULL) {
  ## Fit stats computed on the kept stages only; dropped stages still plotted.
  keep <- function(d) if (!is.null(drop_stages) && "phenology" %in% names(d))
    d[!d$phenology %in% drop_stages, ] else d
  cal.lab <- .stat_labels(keep(cal),   obs, pred, "Calibrated")
  unc.lab <- .stat_labels(keep(uncal), obs, pred, "Uncalibrated")

  ggplot() +
    geom_point(data = uncal, aes(.data[[obs]], .data[[pred]], colour = "Uncalibrated",
                                 fill = "Uncalibrated", shape = "Uncalibrated")) +
    geom_point(data = cal,   aes(.data[[obs]], .data[[pred]], colour = "Calibrated",
                                 fill = "Calibrated", shape = "Calibrated")) +
    geom_abline() +
    geom_text(data = cal.lab, aes(Inf, -Inf, label = label), vjust = -0.2, hjust = 1.05, size = 3) +
    geom_text(data = unc.lab, aes(-Inf, Inf, label = label), vjust = 1.1, hjust = -0.05, size = 3) +
    facet_wrap(~group) +
    labs(x = paste("Observed", axis_lab), y = paste("Predicted", axis_lab),
         title = title, colour = "", fill = "", shape = "") +
    scale_colour_manual(values = c(Calibrated = "#1f77b4", Uncalibrated = "#ff7f0e")) +
    scale_fill_manual(values   = c(Calibrated = alpha("#1f77b4", .3), Uncalibrated = alpha("#ff7f0e", .3))) +
    scale_shape_manual(values  = c(Calibrated = 21, Uncalibrated = 24)) +
    theme_bw(base_size = 13) + theme(legend.position = "bottom") -> p

  png(file, width = 9 * 300, height = 8 * 300, res = 300); print(p); dev.off()
  message("[figures] wrote ", file)
}

make_figures <- function() {
  fd <- file.path(abspath(CONFIG$out_dir), "figures")
  dir.create(fd, showWarnings = FALSE, recursive = TRUE)

  cal.p <- .load_all("calibrated-phen");   unc.p <- .load_all("uncalibrated-phen")
  cal.y <- .load_all("calibrated-yield");  unc.y <- .load_all("uncalibrated-yield")

  if (!is.null(cal.p))
    .one_to_one(unc.p, cal.p, "doy.obs", "doy.pred", "DOY",
                "Phenology calibration (day of year)", file.path(fd, "phenology-1to1.png"),
                drop_stages = QC$fit_drop_stages)
  if (!is.null(cal.y))
    .one_to_one(unc.y, cal.y, "yield.obs", "yield.pred", "yield (kg/ha)",
                "Yield calibration", file.path(fd, "yield-1to1.png"))

  ## Progress: objective error vs evaluation index, per group & stage.
  pf <- file.path(abspath(CONFIG$out_dir), "optimization-progress.csv")
  if (file.exists(pf)) {
    prog <- read.csv(pf)
    prog <- do.call(rbind, lapply(split(prog, list(prog$group, prog$stage), drop = TRUE), function(s) {
      s$eval <- seq_len(nrow(s)); s$best <- cummin(ifelse(is.na(s$error), Inf, s$error)); s
    }))
    p <- ggplot(prog, aes(eval, error)) +
      geom_line(alpha = .4) + geom_point(size = .8) +
      geom_line(aes(y = best), colour = "#1f77b4", linewidth = 1) +
      facet_wrap(group ~ stage, scales = "free", labeller = label_both) +
      labs(x = "Evaluation", y = "Objective (log SSE)",
           title = "Optimization progress by group and stage",
           subtitle = "blue = running best") +
      theme_bw(base_size = 12)
    png(file.path(fd, "progress.png"), width = 11 * 300, height = 8 * 300, res = 300); print(p); dev.off()
    message("[figures] wrote ", file.path(fd, "progress.png"))
  }
}
