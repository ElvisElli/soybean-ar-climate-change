## ============================================================
## Post-calibration 1:1 figure — mirrors the Figure S1 (panel B)
## style used in paper/to-submit, but built directly from this
## calibration run's own outputs (optimization/results/*-phen.rds)
## rather than the finalized processed-data used in figure-s1/.
##
## Sourced automatically at the end of phenology-calibration.R so a
## fresh 1:1 comparison figure is produced every time the
## calibration is rerun, without needing a second APSIM pass.
## ============================================================

library(ggplot2)

## `CONFIG` and `results` (the list of per-cultivar coefficient
## data.frames) are expected to already exist in the calling
## environment when this is `source()`-d from phenology-calibration.R.
## Guard so the script can also be run standalone for a re-plot
## after the fact, without rerunning the whole calibration.
if (!exists("CONFIG")) {
  CONFIG <- list(out_dir = "optimization/results")
}
if (!exists("results")) {
  results <- NULL
}
cultivars.done <- if (!is.null(results)) names(results) else
  c("EarlyMG4", "LateMG4", "EarlyMG5", "LateMG5")

## ── Load and combine per-cultivar comparison tables ───────────
## Only cultivars that actually finished (files exist) are included,
## so a partial/still-running calibration can still be re-plotted.
load_combined <- function(suffix) {
  parts <- lapply(cultivars.done, function(cv) {
    f <- file.path(CONFIG$out_dir, paste0(cv, "-", suffix, ".rds"))
    if (!file.exists(f)) return(NULL)
    d <- readRDS(f)
    d$cultivar <- cv
    d
  })
  do.call(rbind, Filter(Negate(is.null), parts))
}

cal.phen   <- load_combined("calibrated-phen")
uncal.phen <- load_combined("uncalibrated-phen")

if (is.null(cal.phen) || is.null(uncal.phen) || nrow(cal.phen) == 0) {
  message("[calibration-figure] No completed cultivar comparison data yet — skipping figure.")
} else {

  ## Overall (pooled across all 4 groups) RMSE/Bias, same formulas
  ## used in figure-s1/code/figures.R panel B, for the inset labels.
  bias.cal <- sum(cal.phen$doy.x - cal.phen$doy.y) / nrow(cal.phen)
  rmse.cal <- sqrt(sum((cal.phen$doy.x - cal.phen$doy.y)^2) / nrow(cal.phen))
  rmse.pre <- sqrt(sum((uncal.phen$doy.x - uncal.phen$doy.y)^2) / nrow(uncal.phen))
  bias.pre <- sum(uncal.phen$doy.x - uncal.phen$doy.y) / nrow(uncal.phen)
  cal.label <- paste0("Optimized\nRMSE: ", round(rmse.cal, 1), " d\nBias: ", round(bias.cal, 1), " d")
  pre.label <- paste0("Uncalibrated\nRMSE: ", round(rmse.pre, 1), " d\nBias: ", round(bias.pre, 1), " d")

  p <- ggplot() +
    geom_point(data = uncal.phen, aes(x = doy.y, y = doy.x, fill = "Uncalibrated",
                                       pch = "Uncalibrated", colour = "Uncalibrated")) +
    geom_point(data = cal.phen, aes(x = doy.y, y = doy.x, fill = "Optimized",
                                     pch = "Optimized", colour = "Optimized")) +
    geom_text(aes(x = Inf, y = -Inf, label = cal.label), vjust = -0.15, hjust = 1.05) +
    geom_text(aes(x = -Inf, y = Inf, label = pre.label), vjust = 1.15, hjust = -0.05) +
    geom_abline() +
    facet_wrap(~cultivar) +
    labs(x = "Predicted DOY", y = "Observed DOY", fill = "", col = "", shape = "",
         title = "Calibration 1:1 — early4 / late4 / early5 / late5",
         subtitle = "8-parameter fit (6 phenology + Area of Largest Leaf + RUE)") +
    scale_color_manual(values = c("Optimized" = alpha("#1f77b4", 1),
                                   "Uncalibrated" = alpha("#ff7f0e", 1))) +
    scale_fill_manual(values = c("Optimized" = alpha("#1f77b4", 0.3),
                                  "Uncalibrated" = alpha("#ff7f0e", 0.3))) +
    scale_shape_manual(values = c("Optimized" = 21, "Uncalibrated" = 24)) +
    theme_bw(base_size = 14) +
    theme(legend.position = "bottom", legend.title.position = "top",
          axis.text = element_text(colour = "black"))

  fig.dir <- file.path(CONFIG$out_dir, "figures")
  if (!dir.exists(fig.dir)) dir.create(fig.dir, recursive = TRUE)
  png(file.path(fig.dir, "calibration-1to1.png"), width = 9 * 300, height = 8 * 300, res = 300)
  print(p)
  dev.off()

  cat("[calibration-figure] Wrote", file.path(fig.dir, "calibration-1to1.png"), "\n")
}
