## ============================================================
## 05-phenology-phases-supp.R  —  Supplementary phenology-phase analysis
## Run from repo root:  source("code/05-phenology-phases-supp.R")
##
## Addresses reviewer request (revision round 2): show the duration of the
## vegetative and reproductive phases (not only seed filling), and give a
## statistically sound account of which phase drives the yield response.
##
## Companion to MAIN Fig 2 (02-analysis.R): same data, cleaning, grid,
## theme, scenarios and diverging colour scale. Four phenological phases,
## which sum EXACTLY to the whole cycle:
##   veg  = FloweringDAS   - EmergenceDAS    (VE-R1, vegetative)
##   erep = SeedFillingDAS - FloweringDAS    (R1-R5, early reproductive)
##   sf   = MaturityDAS    - SeedFillingDAS  (R5-R7, seed filling)     [Fig 2B]
##   cyc  = MaturityDAS    - EmergenceDAS    (VE-R7, whole crop cycle) [Fig 2A]
##
## Outputs (all CO2 = 350 ppm):
##   (1) FigS-phase-durations               absolute duration (days), incl. baseline
##   (2) FigS-phase-duration-change         change vs baseline (days), shared scale
##   (3) FigS-phase-relative-change         relative change vs baseline (%), shared scale
##   (4) FigS-phase-variance-contribution   LMG variance decomposition of yield change
##   + printed statistics tables.
##
## The change/relative figures use a SINGLE shared colour scale across all
## phases (one legend) for direct comparability; the shared change scale is
## the Fig 2 seed-filling scheme, so the R5-R7 panel still matches Fig 2.
## ============================================================

## ── 0. Setup (mirrors 02-analysis.R) ─────────────────────────────────────────
suppressPackageStartupMessages({
  library(ggplot2); library(sf); library(viridis); library(stars)
  library(dplyr); library(tidyr); library(cowplot); library(lubridate)
})
dir.create("figures", showWarnings = FALSE, recursive = TRUE)
source("code/utils/plot-theme.R")

## ── 1. Load & quality-filter simulation results (identical to 02-analysis.R) ──
simulated0 <- readRDS("data/outputs/simulated-scenarios-df.rds")
if (is.list(simulated0) && !is.data.frame(simulated0))
  simulated0 <- dplyr::bind_rows(Filter(Negate(is.null), simulated0))
simulated0 <- as_tibble(simulated0) %>% rename(any_of(c(date = "Date"))) %>%
  filter(!is.na(MaturityDAS),   MaturityDAS   > 0,
         !is.na(EmergenceDAS),  EmergenceDAS  > 0,
         !is.na(FloweringDAS),  FloweringDAS  > 0,
         !is.na(SeedFillingDAS), SeedFillingDAS > 0,
         MaturityDAS - EmergenceDAS >= 60,
         FloweringDAS  < MaturityDAS,
         SeedFillingDAS < MaturityDAS,
         !is.na(Yield_kgha), Yield_kgha > 0) %>%
  mutate(veg  = FloweringDAS   - EmergenceDAS,   # the four phase durations
         erep = SeedFillingDAS - FloweringDAS,   # (sum to the whole cycle)
         sf   = MaturityDAS    - SeedFillingDAS,
         cyc  = MaturityDAS    - EmergenceDAS)

## ── 2. Spatial layers, grid, scenarios, helpers (as 02-analysis.R) ───────────
ark <- st_read("data/raw/cropland/cb_2018_us_state_20m/cb_2018_us_state_20m.shp",
               quiet = TRUE) %>% subset(STUSPS == "AR") %>% st_transform(5070)
usa_counties <- st_read(
  "data/raw/cropland/Elvis-Crop-Data/Arkansas_Counties_4269.shp", quiet = TRUE)
df2 <- st_as_stars(st_bbox(ark), dx = 2500, dy = 2500)

scenario_levels <- c("climate_change", "longer_mat", "early_sowing",
                     "early_sowing_longer_mat")
scenario_labels <- c("2°C-increase", "Late-Maturing", "Early Sowing", "LM & ES")
dur_levels <- c("baseline", scenario_levels)
dur_labels <- c("Baseline", scenario_labels)

## Short phase tags used as panel titles.
phase_tag <- c(veg = "(A)  VE-R1", erep = "(B)  R1-R5",
               sf  = "(C)  R5-R7", cyc  = "(D)  VE-R7")

to_stars_grid <- function(df, dims) {                 # identical to 02-analysis.R
  s <- st_as_stars(df, dims = dims, xy = c("x", "y"), proxy = TRUE)
  st_crs(s) <- "epsg:4326"; s <- st_transform(s, 5070)
  st_warp(s, df2, no_data_value = NA)
}
map_theme <- theme(
  axis.title = element_blank(), axis.text = element_blank(),
  legend.position = "top", legend.direction = "horizontal",
  legend.title = element_text(size = 11, hjust = 0), strip.text = element_text(size = 8),
  plot.title = element_text(face = "plain", size = 12))   # non-bold panel titles

div_cols5 <- c("#b10026", "#e5f5f9", "#99d8c9", "#41ae76", "#005824")  # Fig 2 palette
seq_cols5 <- viridis(5, option = "D", direction = 1)                    # for durations

## One phase-map panel (faceted across scenarios). `legend_name` NULL hides the
## legend title; `title` is the short panel tag placed top-left.
make_phase_panel <- function(data, var, breaks, labels, colors, title,
                             sc_levels, sc_labels, legend_name = NULL) {
  df <- data %>%
    mutate(scenario = factor(scenario, levels = sc_levels, labels = sc_labels)) %>%
    group_by(x, y, scenario) %>%
    summarise(value = round(mean(.data[[var]], na.rm = TRUE), 1), .groups = "drop") %>%
    mutate(value_bin = cut(value, breaks = breaks, labels = labels, include.lowest = TRUE))
  s <- to_stars_grid(df, dims = c("x", "y", "scenario"))
  ggplot() +
    geom_sf(data = ark, fill = "grey50", color = "black") +
    geom_stars(data = s, aes(fill = value_bin)) +
    geom_sf(data = usa_counties, fill = NA, colour = "black", linewidth = 0.5) +
    coord_sf(xlim = c(360000, 570000)) +
    facet_wrap(~ scenario, nrow = 1) + temp + ggtitle(title) +
    scale_fill_manual(values = colors, na.value = "grey50", na.translate = FALSE,
                      name = legend_name) +
    map_theme
}

## Assemble phase panels that SHARE one colour scale into one figure with a
## single common legend at the bottom. `ncols` = number of panel-columns
## (2 gives the requested 2x2 grid for four phases).
save_shared <- function(panels, file, ncol_scn, ncols = 2) {
  leg  <- get_legend(panels[[1]] +
            theme(legend.position = "bottom", legend.direction = "horizontal"))
  bare <- lapply(panels, function(p) p + theme(legend.position = "none"))
  nrows <- ceiling(length(panels) / ncols)
  body <- plot_grid(plotlist = bare, ncol = ncols, align = "hv", axis = "tblr")
  fig  <- plot_grid(body, leg, ncol = 1, rel_heights = c(nrows, 0.4))
  ggsave(paste0(file, ".tiff"), fig, width = 3.6 * ncol_scn * ncols, height = 6 * nrows + 1.4,
         units = "cm", dpi = 600, compression = "lzw", bg = "white", limitsize = FALSE)
  ggsave(paste0(file, ".png"),  fig, width = 3.6 * ncol_scn * ncols, height = 6 * nrows + 1.4,
         units = "cm", dpi = 300, bg = "white", limitsize = FALSE)
  message("[fig] wrote ", file, ".{tiff,png}")
}
## Assemble panels that each keep their OWN legend (for absolute durations,
## whose ranges differ per phase and cannot share one scale). Also 2x2.
save_perpanel <- function(panels, file, ncol_scn, ncols = 2) {
  nrows <- ceiling(length(panels) / ncols)
  fig <- plot_grid(plotlist = panels, ncol = ncols, align = "hv", axis = "tblr")
  ggsave(paste0(file, ".tiff"), fig, width = 3.6 * ncol_scn * ncols, height = 6.8 * nrows,
         units = "cm", dpi = 600, compression = "lzw", bg = "white", limitsize = FALSE)
  ggsave(paste0(file, ".png"),  fig, width = 3.6 * ncol_scn * ncols, height = 6.8 * nrows,
         units = "cm", dpi = 300, bg = "white", limitsize = FALSE)
  message("[fig] wrote ", file, ".{tiff,png}")
}

## ── 3. Long-format data + change vs baseline (per field-year) ────────────────
dur_data <- simulated0 %>% filter(co2 == 350 | scenario == "baseline") %>%
  select(x, y, date, scenario, veg, erep, sf, cyc, Yield_kgha)

bl <- simulated0 %>% filter(scenario == "baseline") %>%
  select(x, y, date, bveg = veg, berep = erep, bsf = sf, bcyc = cyc, byld = Yield_kgha)
chg_data <- simulated0 %>%
  filter(scenario != "baseline", co2 == 350) %>%
  select(x, y, date, scenario, veg, erep, sf, cyc, Yield_kgha) %>%
  left_join(bl, by = c("x", "y", "date")) %>% filter(!is.na(bcyc)) %>%
  mutate(dveg = veg - bveg, derep = erep - berep, dsf = sf - bsf, dcyc = cyc - bcyc,
         dyld = Yield_kgha - byld,                                   # yield change
         rveg = 100*(dveg)/bveg, rerep = 100*(derep)/berep,
         rsf  = 100*(dsf)/bsf,   rcyc  = 100*(dcyc)/bcyc)

## ── 4a. FIGURE 1 — absolute phase durations (days), incl. baseline ───────────
## Sequential (viridis) bins; ranges differ per phase, so per-phase legends.
dur_bins <- list(
  veg  = list(c(20,35,40,45,50,70),   c("< 35","35-40","40-45","45-50","> 50")),
  erep = list(c(15,25,28,31,34,50),   c("< 25","25-28","28-31","31-34","> 34")),
  sf   = list(c(30,43,48,53,58,80),   c("< 43","43-48","48-53","53-58","> 58")),
  cyc  = list(c(80,105,113,121,129,170), c("< 105","105-113","113-121","121-129","> 129")))
dur_panels <- lapply(names(dur_bins), function(v) {
  b <- dur_bins[[v]]
  make_phase_panel(dur_data, v, b[[1]], b[[2]], setNames(seq_cols5, b[[2]]),
                   paste0(phase_tag[[v]], "  duration (days)"),
                   dur_levels, dur_labels, legend_name = NULL)
})
save_perpanel(dur_panels, "figures/FigS-phase-durations", ncol_scn = 5)

## ── 4b. FIGURE 2 — change vs baseline (days), SHARED scale (= Fig 2B scheme) ──
chg_breaks <- c(-Inf, 0, 5, 10, 15, Inf)
chg_labels <- c("< 0", "0 to 5", "5 to 10", "10 to 15", "> 15")
chg_panels <- lapply(c("dveg","derep","dsf","dcyc"), function(v) {
  tag <- phase_tag[[sub("^d", "", v)]]
  make_phase_panel(chg_data, v, chg_breaks, chg_labels, setNames(div_cols5, chg_labels),
                   tag, scenario_levels, scenario_labels,
                   legend_name = "Change vs baseline (days)")
})
save_shared(chg_panels, "figures/FigS-phase-duration-change", ncol_scn = 4)

## ── 4b-bis. ALTERNATIVE Figure 2 — the two Fig-2 phases on the shared scale ───
## Same two phases and EXACT same layout/output as the main Fig 2 in
## 02-analysis.R (two stacked panels, per-panel top legend, 18x18 cm, 600 dpi
## lzw TIFF), but on the single updated shared change scale. The "(A)/(B)"
## descriptive panel text is kept as the panel title; the legend has no title.
plot9a_alt <- make_phase_panel(chg_data, "dcyc", chg_breaks, chg_labels,
                setNames(div_cols5, chg_labels), "(A)  Total crop cycle change (days)",
                scenario_levels, scenario_labels, legend_name = NULL)
plot9b_alt <- make_phase_panel(chg_data, "dsf", chg_breaks, chg_labels,
                setNames(div_cols5, chg_labels), "(B)  Seed-filling period change (days)",
                scenario_levels, scenario_labels, legend_name = NULL)
plot9_alt <- plot_grid(plot9a_alt, plot9b_alt, ncol = 1, align = "v", axis = "lr")
ggsave("figures/Fig2-alternative-shared-scale.tiff", plot = plot9_alt,
       width = 18, height = 18, units = "cm", dpi = 600, compression = "lzw", bg = "white")
ggsave("figures/Fig2-alternative-shared-scale.png", plot = plot9_alt,
       width = 18, height = 18, units = "cm", dpi = 300, bg = "white")
message("[fig] wrote figures/Fig2-alternative-shared-scale.{tiff,png}")

## ── 4c. FIGURE 3 — relative change vs baseline (%), SHARED scale ──────────────
rel_breaks <- c(-Inf, 0, 8, 16, 24, Inf)
rel_labels <- c("< 0", "0 to 8", "8 to 16", "16 to 24", "> 24")
rel_panels <- lapply(c("rveg","rerep","rsf","rcyc"), function(v) {
  tag <- phase_tag[[sub("^r", "", v)]]
  make_phase_panel(chg_data, v, rel_breaks, rel_labels, setNames(div_cols5, rel_labels),
                   tag, scenario_levels, scenario_labels,
                   legend_name = "Relative change vs baseline (%)")
})
save_shared(rel_panels, "figures/FigS-phase-relative-change", ncol_scn = 4)

## ── 5. Variance decomposition: which phase drives the YIELD change? ──────────
## Model: dyld ~ dVeg + dRep_early + dSeedFill  (whole-cycle change is their
## exact sum, so excluded to avoid perfect collinearity). Because the phase
## changes are themselves correlated, ordinary sequential R2 is order-dependent;
## we therefore use the LMG (Lindeman-Merenda-Gold) decomposition, which averages
## each predictor's incremental R2 over ALL predictor orderings — the standard,
## order-invariant measure of relative importance (Grömping 2006). Implemented
## directly (3 predictors -> 8 subset models) for full transparency.
lmg_shares <- function(dat, y = "dyld", preds = c("dveg","derep","dsf")) {
  p <- length(preds)
  r2 <- function(vars) if (!length(vars)) 0 else
    summary(lm(reformulate(vars, y), data = dat))$r.squared
  contrib <- setNames(numeric(p), preds)
  for (i in preds) {
    others <- setdiff(preds, i)
    for (k in 0:length(others)) {
      subs <- if (k == 0) list(character(0)) else combn(others, k, simplify = FALSE)
      w <- factorial(k) * factorial(p - k - 1) / factorial(p)     # LMG weight
      for (S in subs) contrib[i] <- contrib[i] + w * (r2(c(S, i)) - r2(S))
    }
  }
  R2 <- r2(preds)
  list(R2 = R2, share = 100 * contrib / R2,                       # % of explained var
       beta = coef(lm(reformulate(preds, y),
                      data = as.data.frame(scale(dat[c(y, preds)]))))[preds])
}

sc_map <- setNames(scenario_labels, scenario_levels)
va <- c(list(Pooled = lmg_shares(chg_data)),
        lapply(scenario_levels, function(s) lmg_shares(chg_data[chg_data$scenario == s, ])))
names(va)[-1] <- scenario_labels

cat(paste(rep("=", 78), collapse = ""), "\n")
cat("VARIANCE DECOMPOSITION of yield change (dyld ~ dVeg + dRep_early + dSeedFill)\n")
cat("LMG relative importance (% of explained variance); std. coef sign in ( ).\n")
cat(paste(rep("-", 78), collapse = ""), "\n")
cat(sprintf("%-14s %6s | %16s %16s %16s\n", "scenario", "R2", "VE-R1", "R1-R5", "R5-R7"))
for (nm in names(va)) {
  z <- va[[nm]]; f <- function(k) sprintf("%5.1f%% (%+.2f)", z$share[k], z$beta[k])
  cat(sprintf("%-14s %5.1f%% | %16s %16s %16s\n", nm, 100*z$R2,
              f("dveg"), f("derep"), f("dsf")))
}
cat(paste(rep("=", 78), collapse = ""), "\n")

## Barplot of LMG shares (stacked to 100%) with R2 annotated per scenario.
va_df <- do.call(rbind, lapply(names(va), function(nm) data.frame(
  scenario = nm, R2 = va[[nm]]$R2, phase = c("VE-R1","R1-R5","R5-R7"),
  share = as.numeric(va[[nm]]$share))))
va_df$scenario <- factor(va_df$scenario, levels = c("Pooled", scenario_labels))
va_df$phase    <- factor(va_df$phase, levels = c("VE-R1","R1-R5","R5-R7"))
lab_r2 <- va_df[!duplicated(va_df$scenario), ]

pbar <- ggplot(va_df, aes(scenario, share, fill = phase)) +
  geom_col(width = 0.7, colour = "black", linewidth = 0.3) +
  geom_text(aes(label = sprintf("%.0f%%", share)),
            position = position_stack(vjust = 0.5), size = 3) +
  geom_text(data = lab_r2, aes(scenario, 104, label = sprintf("R^2==%.0f*'%%'", 100*R2)),
            inherit.aes = FALSE, size = 3, parse = TRUE) +
  scale_fill_manual(values = c("VE-R1" = "#8c96c6", "R1-R5" = "#88419d",
                               "R5-R7" = "#005824"), name = "Phase") +
  labs(x = NULL, y = "Share of explained yield-change variance (LMG, %)",
       title = "Which phase's duration change explains the yield change?",
       subtitle = "Stacked to 100% of explained variance; total R2 above each bar") +
  coord_cartesian(ylim = c(0, 110)) +
  theme_bw(base_size = 12) + theme(legend.position = "top")
ggsave("figures/FigS-phase-variance-contribution.tiff", pbar, width = 16, height = 11,
       units = "cm", dpi = 600, compression = "lzw", bg = "white")
ggsave("figures/FigS-phase-variance-contribution.png", pbar, width = 16, height = 11,
       units = "cm", dpi = 300, bg = "white")
message("[fig] wrote figures/FigS-phase-variance-contribution.{tiff,png}")

## ── 6. Average magnitude of the NEGATIVE (shortening) changes, per phase ─────
## For each phase x scenario, restricted to field-years where the change is
## negative: the mean shortening magnitude (|days|) and the % of field-years
## that shorten. Reported at the field-year level (CO2 = 350).
cat(paste(rep("=", 78), collapse = ""), "\n")
cat("AVERAGE MAGNITUDE OF NEGATIVE (SHORTENING) CHANGES, days, CO2=350\n")
cat("  mean_abs_neg = mean |change| over shortening field-years; pct_neg = % shortening\n")
cat(paste(rep("-", 78), collapse = ""), "\n")
neg_mag <- function(v) {
  chg_data %>%
    mutate(scenario = factor(scenario, levels = scenario_levels, labels = scenario_labels)) %>%
    group_by(scenario) %>%
    summarise(mean_abs_neg = round(mean(abs(.data[[v]][.data[[v]] < 0]), na.rm = TRUE), 2),
              pct_neg      = round(100 * mean(.data[[v]] < 0, na.rm = TRUE), 1), .groups = "drop")
}
for (v in c("dveg","derep","dsf","dcyc")) {
  cat("\nPhase ", phase_tag[[sub("^d","",v)]], ":\n", sep = "")
  print(as.data.frame(neg_mag(v)))
}
cat(paste(rep("=", 78), collapse = ""), "\n")

## ── 7. Correlation plots: yield vs each phase duration (all scenarios) ───────
## Absolute yield against absolute phase duration, ALL scenarios (incl. baseline)
## in a single panel per phase, coloured by scenario, with one overall linear fit
## and the pooled Pearson r annotated. Points sub-sampled for legibility; r and
## the fit are computed on the FULL data.
sc_all_lev <- dur_levels; sc_all_lab <- dur_labels
sc_cols <- setNames(c("grey40", "#b2182b", "#2166ac", "#1b7837", "#762a83"), sc_all_lab)
phase_axis <- c(veg = "Vegetative VE-R1 (days)", erep = "Early reproductive R1-R5 (days)",
                sf = "Seed filling R5-R7 (days)", cyc = "Whole crop cycle VE-R7 (days)")

set.seed(1)
corr_long <- dur_data %>%
  mutate(scenario = factor(scenario, levels = sc_all_lev, labels = sc_all_lab)) %>%
  tidyr::pivot_longer(c(veg, erep, sf, cyc), names_to = "phase", values_to = "dur")
corr_long$phase <- factor(corr_long$phase, levels = c("veg","erep","sf","cyc"),
                          labels = phase_axis)
## Pearson r (full data) per phase, for the annotation.
rlab <- corr_long %>% group_by(phase) %>%
  summarise(r = cor(dur, Yield_kgha, use = "complete.obs"),
            x = min(dur, na.rm = TRUE), .groups = "drop") %>%
  mutate(label = sprintf("r = %.2f", r))
plot_pts <- corr_long %>% group_by(phase) %>% slice_sample(n = 6000) %>% ungroup()

pcorr <- ggplot(plot_pts, aes(dur, Yield_kgha)) +
  geom_point(aes(colour = scenario), alpha = 0.25, size = 0.5) +
  geom_smooth(method = "lm", se = FALSE, colour = "black", linewidth = 0.8) +
  geom_text(data = rlab, aes(x = x, y = Inf, label = label),
            hjust = -0.1, vjust = 1.4, size = 3.5, inherit.aes = FALSE) +
  facet_wrap(~ phase, scales = "free_x", nrow = 2) +
  scale_colour_manual(values = sc_cols, name = NULL) +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 2))) +
  labs(x = "Phase duration (days)", y = "Simulated seed yield (kg/ha)",
       title = "Seed yield vs phenological-phase duration (all scenarios)") +
  theme_bw(base_size = 12) + theme(legend.position = "top")
ggsave("figures/FigS-yield-phase-correlation.tiff", pcorr, width = 20, height = 16,
       units = "cm", dpi = 600, compression = "lzw", bg = "white")
ggsave("figures/FigS-yield-phase-correlation.png", pcorr, width = 20, height = 16,
       units = "cm", dpi = 300, bg = "white")
message("[fig] wrote figures/FigS-yield-phase-correlation.{tiff,png}")

cat("\n[done] wrote 6 figures to figures/{FigS-phase-*, Fig2-alternative-*, FigS-yield-phase-*}.{tiff,png}\n")
