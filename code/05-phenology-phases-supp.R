## ============================================================
## 05-phenology-phases-supp.R  —  Supplementary phenology-phase analysis
## Run from repo root:  source("code/05-phenology-phases-supp.R")
##
## Reviewer request (revision round 2): show the duration of the vegetative and
## reproductive phases (not only seed filling) and explain which phase drives
## the yield response. Companion to MAIN Fig 2 in 02-analysis.R (same data,
## cleaning, grid, theme, scenarios, diverging colour scale).
##
## Four phenological phases (they sum EXACTLY to the whole cycle):
##   veg  = FloweringDAS   - EmergenceDAS    VE-R1  vegetative
##   erep = SeedFillingDAS - FloweringDAS    R1-R5  early reproductive
##   sf   = MaturityDAS    - SeedFillingDAS  R5-R7  seed filling      [main Fig 2B]
##   cyc  = MaturityDAS    - EmergenceDAS    VE-R7  whole crop cycle  [main Fig 2A]
##
## Outputs (figures/, CO2 = 350 ppm):
##   FigS-phase-durations              absolute duration (days), 2x2, incl. baseline
##   FigS-phase-duration-change        change vs baseline (days), 2x2, shared scale
##   FigS-phase-relative-change        relative change vs baseline (%), 2x2, shared scale
##   Fig2-alternative-shared-scale     the two main-Fig-2 phases, shared scale (Fig-2 size)
##   FigS-phase-variance-contribution  LMG variance decomposition of the yield change
##   FigS-yield-phase-correlation      yield vs each phase duration, all scenarios
##   + printed statistics tables.
##
## SPEED: every map shows the 40-year MEAN per field. That mean is computed ONCE
## per metric (field_dur / field_chg) and reused, so each map only bins + warps a
## ~20k-row field table instead of re-summarising the 1.6M-row table each time.
## ============================================================

## ── 0. Setup ─────────────────────────────────────────────────────────────────
suppressPackageStartupMessages({
  library(ggplot2); library(sf); library(stars); library(viridis)
  library(dplyr); library(tidyr); library(cowplot)
})
dir.create("figures", showWarnings = FALSE, recursive = TRUE)
source("code/utils/plot-theme.R")

div_cols5 <- c("#b10026", "#e5f5f9", "#99d8c9", "#41ae76", "#005824")  # Fig 2 palette
seq_cols5 <- viridis(5)                                                 # for durations

## ── 1. Load & quality-filter simulation results (identical to 02-analysis.R) ──
sim <- readRDS("data/outputs/simulated-scenarios-df.rds")
if (is.list(sim) && !is.data.frame(sim)) sim <- dplyr::bind_rows(Filter(Negate(is.null), sim))
sim <- as_tibble(sim) %>% rename(any_of(c(date = "Date"))) %>%
  filter(MaturityDAS > 0, EmergenceDAS > 0, FloweringDAS > 0, SeedFillingDAS > 0,
         MaturityDAS - EmergenceDAS >= 60, FloweringDAS < MaturityDAS,
         SeedFillingDAS < MaturityDAS, Yield_kgha > 0) %>%
  mutate(veg  = FloweringDAS   - EmergenceDAS,
         erep = SeedFillingDAS - FloweringDAS,
         sf   = MaturityDAS    - SeedFillingDAS,
         cyc  = MaturityDAS    - EmergenceDAS)

scenario_levels <- c("climate_change", "longer_mat", "early_sowing", "early_sowing_longer_mat")
scenario_labels <- c("2°C-increase", "Late-Maturing", "Early Sowing", "LM & ES")
dur_levels <- c("baseline", scenario_levels)
dur_labels <- c("Baseline", scenario_labels)
phase_tag  <- c(veg = "(A)  VE-R1", erep = "(B)  R1-R5", sf = "(C)  R5-R7", cyc = "(D)  VE-R7")

## ── 2. Per-field-year data: durations, and change vs baseline ────────────────
dur_fy <- sim %>% filter(co2 == 350 | scenario == "baseline") %>%
  select(x, y, date, scenario, veg, erep, sf, cyc, Yield_kgha)

baseline_fy <- sim %>% filter(scenario == "baseline") %>%
  select(x, y, date, b_veg = veg, b_erep = erep, b_sf = sf, b_cyc = cyc, b_yld = Yield_kgha)
chg_fy <- sim %>% filter(scenario != "baseline", co2 == 350) %>%
  select(x, y, date, scenario, veg, erep, sf, cyc, Yield_kgha) %>%
  inner_join(baseline_fy, by = c("x", "y", "date")) %>%
  transmute(x, y, scenario,
            dveg = veg - b_veg, derep = erep - b_erep, dsf = sf - b_sf, dcyc = cyc - b_cyc,
            dyld = Yield_kgha - b_yld,
            rveg = 100*(veg - b_veg)/b_veg, rerep = 100*(erep - b_erep)/b_erep,
            rsf  = 100*(sf  - b_sf )/b_sf,  rcyc  = 100*(cyc - b_cyc)/b_cyc)

## Aggregate to 40-yr MEAN per field ONCE (this is the only heavy step per table).
agg_fields <- function(df, cols, lev, lab) df %>%
  mutate(scenario = factor(scenario, lev, lab)) %>%
  group_by(x, y, scenario) %>%
  summarise(across(all_of(cols), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

field_dur <- agg_fields(dur_fy, c("veg","erep","sf","cyc"), dur_levels, dur_labels)
field_chg <- agg_fields(chg_fy, c("dveg","derep","dsf","dcyc","rveg","rerep","rsf","rcyc"),
                        scenario_levels, scenario_labels)

## ── 3. Map helpers (mirror 02-analysis.R; input is already field-level) ──────
ark <- st_read("data/raw/cropland/cb_2018_us_state_20m/cb_2018_us_state_20m.shp", quiet = TRUE) %>%
  subset(STUSPS == "AR") %>% st_transform(5070)
usa_counties <- st_read("data/raw/cropland/Elvis-Crop-Data/Arkansas_Counties_4269.shp", quiet = TRUE)
grid_tmpl <- st_as_stars(st_bbox(ark), dx = 2500, dy = 2500)

to_grid <- function(df) {
  s <- st_as_stars(df, dims = c("x", "y", "scenario"), xy = c("x", "y"), proxy = TRUE)
  st_crs(s) <- "epsg:4326"
  st_warp(st_transform(s, 5070), grid_tmpl, no_data_value = NA)
}
## Tight margins + small facet gap so stacked panels sit close together.
map_theme <- theme(
  axis.title = element_blank(), axis.text = element_blank(),
  legend.position = "top", legend.direction = "horizontal",
  legend.title = element_text(size = 11, hjust = 0), strip.text = element_text(size = 10),
  plot.title  = element_text(face = "plain", size = 12, margin = margin(b = 1)),
  plot.margin = margin(5, 2, 5, 2), panel.spacing = unit(1, "pt"))

## One phase map (a phase's field means, faceted over scenarios), pre-binned.
phase_map <- function(fielddf, var, breaks, labels, title, cols, legend_name = NULL) {
  d <- fielddf %>% transmute(x, y, scenario,
                             bin = cut(.data[[var]], breaks, labels, include.lowest = TRUE))
  ggplot() +
    geom_sf(data = ark, fill = "grey50", colour = "black") +
    geom_stars(data = to_grid(d), aes(fill = bin)) +
    geom_sf(data = usa_counties, fill = NA, colour = "black", linewidth = 0.4) +
    coord_sf(xlim = c(360000, 570000)) +
    facet_wrap(~ scenario, nrow = 1) + temp + ggtitle(title) +
    scale_fill_manual(values = setNames(cols, labels), drop = FALSE,
                      na.value = "grey50", na.translate = FALSE, name = legend_name) +
    map_theme
}

save_fig <- function(plot, file, w, h) {
  ggsave(paste0(file, ".tiff"), plot, width = w, height = h, units = "cm",
         dpi = 600, compression = "lzw", bg = "white", limitsize = FALSE)
  ggsave(paste0(file, ".png"),  plot, width = w, height = h, units = "cm",
         dpi = 300, bg = "white", limitsize = FALSE)
  message("[fig] ", file, ".{tiff,png}")
}

## Stack four phase panels in a tight 2x2 grid with ONE shared bottom legend.
grid_2x2 <- function(panels, file, ncol_scn) {
  leg  <- get_legend(panels[[1]] + theme(legend.position = "bottom"))
  bare <- lapply(panels, function(p) p + theme(legend.position = "none"))
  body <- plot_grid(plotlist = bare, ncol = 2, align = "hv", axis = "tblr")
  save_fig(plot_grid(body, leg, ncol = 1, rel_heights = c(2, 0.18)),
           file, w = 3.25 * ncol_scn * 2, h = 13.5)
}

## Stack four phase panels in a single tall column with ONE shared TOP legend.
## The legend is built from a complete label/colour source (not from a panel) so
## every class shows even when a given panel has no data in it (e.g. "> 15").
stack_1col <- function(panels, file, labels, cols, w = 17, h = 28.5) {
  legdf <- data.frame(x = seq_along(labels), y = 1, bin = factor(labels, levels = labels))
  leg <- get_legend(ggplot(legdf, aes(x, y, fill = bin)) + geom_tile() +
    scale_fill_manual(values = setNames(cols, labels), name = NULL, guide = guide_legend(nrow = 1)) +
    theme(legend.position = "top", legend.direction = "horizontal", legend.title = element_blank()))
  body <- plot_grid(plotlist = lapply(panels, function(p) p + theme(legend.position = "none")),
                    ncol = 1, align = "v", axis = "lr")
  save_fig(plot_grid(leg, body, ncol = 1, rel_heights = c(0.05, 1)), file, w, h)
}

## ── 4. Figures 1-3: the four phases as durations / change / relative ─────────
dur_bins <- list(
  veg  = list(c(20,35,40,45,50,70),      c("< 35","35-40","40-45","45-50","> 50")),
  erep = list(c(15,25,28,31,34,50),      c("< 25","25-28","28-31","31-34","> 34")),
  sf   = list(c(30,43,48,53,58,80),      c("< 43","43-48","48-53","53-58","> 58")),
  cyc  = list(c(80,105,113,121,129,170), c("< 105","105-113","113-121","121-129","> 129")))
grid_2x2(lapply(names(dur_bins), function(v)
  phase_map(field_dur, v, dur_bins[[v]][[1]], dur_bins[[v]][[2]],
            paste0(phase_tag[[v]], "  duration (days)"), seq_cols5)),
  "figures/FigS-phase-durations", ncol_scn = 5)

chg_breaks <- c(-Inf, 0, 5, 10, 15, Inf)
chg_labels <- c("< 0", "0 to 5", "5 to 10", "10 to 15", "> 15")
chg_titles <- c(veg = "(A)  Change in VE-R1 period (days)", erep = "(B)  Change in R1-R5 period (days)",
                sf  = "(C)  Change in R5-R7 period (days)", cyc  = "(D)  Change in VE-R7 period (days)")
chg_panel  <- function(v) phase_map(field_chg, paste0("d", v), chg_breaks, chg_labels,
                                    chg_titles[[v]], div_cols5, legend_name = NULL)
stack_1col(lapply(c("veg","erep","sf","cyc"), chg_panel),
           "figures/FigS-phase-duration-change", labels = chg_labels, cols = div_cols5)

rel_breaks <- c(-Inf, 0, 8, 16, 24, Inf)
rel_labels <- c("< 0", "0 to 8", "8 to 16", "16 to 24", "> 24")
rel_panel  <- function(v) phase_map(field_chg, paste0("r", v), rel_breaks, rel_labels,
                                    phase_tag[[v]], div_cols5, "Relative change vs baseline (%)")
grid_2x2(lapply(c("veg","erep","sf","cyc"), rel_panel),
         "figures/FigS-phase-relative-change", ncol_scn = 4)

## ── 4d. Alternative Figure 2: main-Fig-2 phases, one shared TOP legend ───────
## Same two phases/layout/size as the main Fig 2 (18x18 cm, 600 dpi lzw TIFF).
alt_a <- phase_map(field_chg, "dcyc", chg_breaks, chg_labels,
                   "(A)  Total crop cycle change (days)", div_cols5)
alt_b <- phase_map(field_chg, "dsf",  chg_breaks, chg_labels,
                   "(B)  Seed-filling period change (days)", div_cols5)
alt_leg  <- get_legend(alt_a + theme(legend.position = "top"))
alt_body <- plot_grid(alt_a + theme(legend.position = "none"),
                      alt_b + theme(legend.position = "none"), ncol = 1, align = "v", axis = "lr")
save_fig(plot_grid(alt_leg, alt_body, ncol = 1, rel_heights = c(0.08, 1)),
         "figures/Fig2-alternative-shared-scale", w = 18, h = 18)

## ── 5. Variance decomposition: which phase drives the YIELD change? ──────────
## dyld ~ dVeg + dRep_early + dSeedFill  (dcyc is their exact sum, so excluded).
## The phase changes are correlated, so we use the order-invariant LMG
## (Lindeman-Merenda-Gold) decomposition: each predictor's incremental R2
## averaged over all predictor orderings. Computed directly (8 subset models).
lmg <- function(dat, y = "dyld", preds = c("dveg","derep","dsf")) {
  r2 <- function(v) if (!length(v)) 0 else summary(lm(reformulate(v, y), dat))$r.squared
  share <- setNames(numeric(length(preds)), preds)
  for (i in preds) for (k in 0:(length(preds) - 1)) {
    subs <- combn(setdiff(preds, i), k, simplify = FALSE)
    w <- factorial(k) * factorial(length(preds) - k - 1) / factorial(length(preds))
    for (S in subs) share[i] <- share[i] + w * (r2(c(S, i)) - r2(S))
  }
  R2 <- r2(preds)
  data.frame(phase = c("VE-R1","R1-R5","R5-R7"), share = 100 * share / R2,
             beta = coef(lm(reformulate(preds, y), as.data.frame(scale(dat[c(y, preds)]))))[preds],
             R2 = R2, row.names = NULL)
}
va <- bind_rows(cbind(scenario = "Pooled", lmg(chg_fy)),
                lapply(seq_along(scenario_levels), function(i)
                  cbind(scenario = scenario_labels[i], lmg(chg_fy[chg_fy$scenario == scenario_levels[i], ]))))
va$scenario <- factor(va$scenario, levels = c("Pooled", scenario_labels))
va$phase    <- factor(va$phase, levels = c("VE-R1","R1-R5","R5-R7"))

cat("\n== LMG variance decomposition of yield change (share of explained variance) ==\n")
print(va %>% mutate(share = round(share, 1), beta = round(beta, 2), R2 = round(100*R2, 1)))

pbar <- ggplot(va, aes(scenario, share, fill = phase)) +
  geom_col(width = 0.7, colour = "black", linewidth = 0.3) +
  geom_text(aes(label = sprintf("%.0f%%", share)), position = position_stack(vjust = 0.5), size = 3) +
  geom_text(data = distinct(va, scenario, R2), aes(scenario, 104, label = sprintf("R^2==%.0f*'%%'", 100*R2)),
            inherit.aes = FALSE, size = 3, parse = TRUE) +
  scale_fill_manual(values = c("VE-R1" = "#8c96c6", "R1-R5" = "#88419d", "R5-R7" = "#005824"), name = "Phase") +
  labs(x = NULL, y = "Share of explained yield-change variance (LMG, %)",
       title = "Which phase's duration change explains the yield change?") +
  coord_cartesian(ylim = c(0, 110)) + theme_bw(base_size = 12) + theme(legend.position = "top")
save_fig(pbar, "figures/FigS-phase-variance-contribution", w = 16, h = 11)

## ── 6. Average magnitude of the NEGATIVE (shortening) changes, per phase ─────
cat("\n== Average shortening magnitude (days) among field-years that shorten, CO2=350 ==\n")
neg_tab <- chg_fy %>% mutate(scenario = factor(scenario, scenario_levels, scenario_labels)) %>%
  group_by(scenario) %>%
  summarise(across(c(dveg, derep, dsf, dcyc),
                   list(mean_abs = ~ round(mean(abs(.x[.x < 0])), 2),
                        pct_neg  = ~ round(100 * mean(.x < 0), 1)), .names = "{.col}_{.fn}"),
            .groups = "drop")
print(as.data.frame(neg_tab))

## ── 7. Yield vs phase-duration correlation (all scenarios, one panel/phase) ──
phase_lab <- c(veg = "Vegetative VE-R1", erep = "Early reproductive R1-R5",
               sf = "Seed filling R5-R7", cyc = "Whole crop cycle VE-R7")
rvals <- sapply(names(phase_lab), function(v) cor(dur_fy[[v]], dur_fy$Yield_kgha))
set.seed(1)
samp <- dur_fy %>% slice_sample(n = 24000) %>%
  mutate(scenario = factor(scenario, dur_levels, dur_labels)) %>%
  pivot_longer(c(veg, erep, sf, cyc), names_to = "phase", values_to = "dur")
samp$phase <- factor(samp$phase, names(phase_lab), phase_lab)
rlab <- data.frame(phase = factor(phase_lab, levels = phase_lab),
                   label = sprintf("r = %.2f", rvals))

pcorr <- ggplot(samp, aes(dur, Yield_kgha)) +
  geom_point(aes(colour = scenario), alpha = 0.25, size = 0.5) +
  geom_smooth(method = "lm", se = FALSE, colour = "black", linewidth = 0.8) +
  geom_text(data = rlab, aes(-Inf, Inf, label = label), hjust = -0.15, vjust = 1.5,
            size = 3.5, inherit.aes = FALSE) +
  facet_wrap(~ phase, scales = "free_x", nrow = 2) +
  scale_colour_manual(values = setNames(c("grey40","#b2182b","#2166ac","#1b7837","#762a83"), dur_labels),
                      name = NULL) +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 2))) +
  labs(x = "Phase duration (days)", y = "Simulated seed yield (kg/ha)",
       title = "Seed yield vs phenological-phase duration (all scenarios)") +
  theme_bw(base_size = 12) + theme(legend.position = "top")
save_fig(pcorr, "figures/FigS-yield-phase-correlation", w = 20, h = 16)

cat("\n[done] 6 figures written to figures/\n")
