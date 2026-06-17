## ============================================================
## 02-analysis.R  —  Manuscript figures & supplementary plots
## Run from repo root:  source("code/02-analysis.R")
##
## Figure order follows the paper (paper-06-17-2026.docx):
##
##  MAIN MANUSCRIPT
##   Fig 1  → fig05  adaptation strategies merged (violin panel A + map panel B)
##   Fig 2  → fig09  phenology change maps (crop cycle + seed-filling, panel A/B)
##
##  SUPPLEMENTARY MATERIAL
##   Fig S1 → EXTERNAL — MET locations map + phenology calibration + yield calibration
##   Fig S2 → EXTERNAL — biomass validation (simulated vs observed, Fayetteville 2008)
##   Fig S3 → FigS3-environmental-characterization.tiff
##   Fig S4 → FigS4-sowing-progress-all-years.tiff
##   Fig S5 → FigS5-yield-density-no-adaptation.tiff
##   Fig S6 → FigS6-temperature-yield-scatter.tiff
##
##  ADDITIONAL / DIAGNOSTIC (not in current paper)
##   ExtraFig1 → yield change by scenario (violin)
##   ExtraFig2 → yield change spatial map
##   ExtraFig3 → seed-filling duration distribution
##   ExtraFig4 → phenology timeline by stage
##   ExtraFig5 → water-use efficiency spatial maps
##   ExtraFig6 → sowing progress recent 5 years
## ============================================================

## ── 0. Setup ─────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(ggplot2)
  library(sf)
  library(viridis)
  library(stars)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(lubridate)
  library(lme4)
  library(emmeans)
  library(ggridges)
  library(cowplot)
  library(gghalves)
})

dir.create("figures", showWarnings = FALSE, recursive = TRUE)
source("code/utils/plot-theme.R")

## Simulation results
simulated0 <- readRDS("data/outputs/simulated-scenarios-df.rds")
## RDS may be a list of per-scenario data frames (if saved before bind_rows)
if (is.list(simulated0) && !is.data.frame(simulated0))
  simulated0 <- dplyr::bind_rows(Filter(Negate(is.null), simulated0))
simulated0 <- as_tibble(simulated0) %>%
  rename(any_of(c(date = "Date"))) %>%
  ## Compute PTQ if critical-period columns are present (requires re-run with updated template)
  ## PTQ = mean daily Radn / (mean daily T - Tbase); Tbase = 10°C (Zanon et al. 2016)
  { if (all(c("CriticalPeriodRadn","CriticalPeriodMeanT") %in% names(.)))
      mutate(., PTQ = CriticalPeriodRadn / (CriticalPeriodMeanT - 10))
    else . }

## ── 0b. Data inspection & quality filter ─────────────────────────────────────
## Removes rows with biologically impossible phenology before any figure is made.
## All figures downstream use the cleaned `simulated0`.

n_raw <- nrow(simulated0)

## Flag each row with the reason it fails (NA = passes all checks)
simulated0_raw <- simulated0 %>%
  mutate(
    .fail = case_when(
      is.na(MaturityDAS)   | MaturityDAS   <= 0  ~ "MaturityDAS missing/zero",
      is.na(EmergenceDAS)  | EmergenceDAS  <= 0  ~ "EmergenceDAS missing/zero",
      is.na(FloweringDAS)  | FloweringDAS  <= 0  ~ "FloweringDAS missing/zero",
      is.na(SeedFillingDAS)| SeedFillingDAS<= 0  ~ "SeedFillingDAS missing/zero",
      MaturityDAS - EmergenceDAS < 60            ~ "Emergence-to-maturity < 60 days",
      FloweringDAS  >= MaturityDAS               ~ "Flowering at or after maturity",
      SeedFillingDAS >= MaturityDAS              ~ "Seed fill starts at or after maturity",
      is.na(Yield_kgha) | Yield_kgha <= 0       ~ "Zero or missing yield",
      TRUE                                       ~ NA_character_
    )
  )

## Summary of filtered rows
filter_summary <- simulated0_raw %>%
  filter(!is.na(.fail)) %>%
  count(scenario, .fail, name = "n_removed") %>%
  arrange(scenario, desc(n_removed))

n_removed <- sum(!is.na(simulated0_raw$.fail))

cat(paste(rep("─", 60), collapse = ""), "\n")
cat(sprintf("DATA QUALITY FILTER\n"))
cat(sprintf("  Raw rows     : %d\n", n_raw))
cat(sprintf("  Removed rows : %d (%.2f%%)\n", n_removed, n_removed / n_raw * 100))
cat(sprintf("  Clean rows   : %d\n\n", n_raw - n_removed))
cat("Removed rows by scenario and reason:\n")
print(filter_summary, n = Inf)
cat(paste(rep("─", 60), collapse = ""), "\n\n")

## Apply filter — simulated0 is now clean for all figures
simulated0 <- simulated0_raw %>%
  filter(is.na(.fail)) %>%
  select(-.fail)

## Quick scenario coverage check after filtering
treatment_cols <- intersect(
  c("cultivar", "sowing", "scenario", "climate.control", "co2", "rowSpacing"),
  names(simulated0)
)
simulated0 %>%
  group_by(across(all_of(treatment_cols))) %>%
  summarise(n_rows  = n(),
            n_cells = n_distinct(cellid),
            n_years = n_distinct(lubridate::year(.data[["date"]])),
            .groups = "drop") %>%
  arrange(desc(n_rows)) %>%
  print()

## Spatial layers
ark <- st_read("data/raw/cropland/cb_2018_us_state_20m/cb_2018_us_state_20m.shp",
               quiet = TRUE) %>%
  subset(STUSPS == "AR") %>%
  st_transform(5070)

usa_counties <- st_read(
  "data/raw/cropland/Elvis-Crop-Data/Arkansas_Counties_4269.shp", quiet = TRUE)

## Shared stars grid template (used by all spatial map figures)
df2 <- st_as_stars(st_bbox(ark), dx = 2500, dy = 2500)

## Shared scenario factor order and labels
scenario_levels <- c("climate_change", "longer_mat", "early_sowing",
                     "early_sowing_longer_mat")
scenario_labels <- c("2°C-increase", "Late-Maturing", "Early Sowing", "LM & ES")
scenario_labels_long <- c("2°C-increase", "Late-Maturing", "Early Sowing",
                          "Late-Maturing & Early Sowing")

## Helper: warp a data frame to the shared stars grid
to_stars_grid <- function(df, dims) {
  s <- st_as_stars(df, dims = dims, xy = c("x", "y"), proxy = TRUE)
  st_crs(s) <- "epsg:4326"
  s <- st_transform(s, 5070)
  st_warp(s, df2, no_data_value = NA)
}

## Helper: common map theme
map_theme <- theme(
  axis.title       = element_blank(),
  axis.text        = element_blank(),
  legend.position  = "top",
  legend.direction = "horizontal",
  legend.title     = element_text(size = 11, hjust = 0),
  strip.text       = element_text(size = 10)
)


## ══════════════════════════════════════════════════════════════════════════════
## MAIN MANUSCRIPT FIGURES
## ══════════════════════════════════════════════════════════════════════════════

## ── MAIN FIG 1: Adaptation strategies — merged (violin + map) ──────────── ----
## Paper: Figure 1 — "Overall relative effects of rising temperatures and adaptive
##        genotypic and agronomic management strategies on soybean seed yield
##        across 4,651 soybean fields and 40 weather years in the US Mid-South (A),
##        and their geographical distribution (B). Values per pixel in panel B are
##        averages across 40 weather years. LM & ES represent the combined effect
##        of a late-maturing variety and early sowing."
## Combined manuscript figure: half-violin + boxplot (panel A) + spatial map (panel B)

p5_baseline <- simulated0 %>%
  filter(scenario == "baseline") %>%
  select(x, y, date, baseline = Yield_kgha)

p5_long <- simulated0 %>%
  filter(scenario != "baseline") %>%
  select(x, y, scenario, co2, date, Yield_kgha) %>%
  pivot_wider(names_from = scenario, values_from = Yield_kgha) %>%
  left_join(p5_baseline, by = c("x", "y", "date")) %>%
  mutate(across(all_of(scenario_levels), ~ (. - baseline) / baseline * 100)) %>%
  select(-baseline) %>%
  pivot_longer(cols = all_of(scenario_levels),
               names_to = "scenario", values_to = "Yield_kgha") %>%
  mutate(scenario = factor(scenario, levels = scenario_levels, labels = scenario_labels_long))

plot5_violin <- ggplot(p5_long,
    aes(x = factor(scenario), y = Yield_kgha,
        fill = as.factor(co2), colour = as.factor(co2))) +
  geom_abline(intercept = 0, slope = 0, linetype = "dashed") +
  geom_half_violin(
    aes(group = interaction(factor(scenario), as.factor(co2))),
    side = "l", trim = FALSE, alpha = 0.4, colour = NA,
    position = position_dodge(width = 0.6)) +
  geom_half_boxplot(
    aes(group = interaction(factor(scenario), as.factor(co2))),
    side = "r", position = position_dodge(width = 0.6),
    outlier.shape = NA, width = 0.5, color = "black", size = 0.5) +
  temp +
  scale_colour_manual(values = c("#4dac26", "#d01c8b"),
                      label  = c("Without elevated CO2", "With elevated CO2")) +
  scale_fill_manual(values   = c("#4dac26", "#d01c8b"),
                    label    = c("Without elevated CO2", "With elevated CO2")) +
  labs(x = element_blank(), y = "Yield change (%)") +
  theme(legend.position = "top", axis.text.x = element_text(angle = 30, hjust = 1)) +
  scale_y_continuous(breaks = seq(-20, 40, 10), limits = c(-20, 40))

p5_map_data <- p5_long %>%
  mutate(scenario = factor(scenario, levels = scenario_labels_long, labels = scenario_labels)) %>%
  group_by(x, y, scenario, co2) %>%
  summarise(Yield_kgha = round(mean(Yield_kgha, na.rm = TRUE), 1), .groups = "drop") %>%
  mutate(
    yield_bin = cut(Yield_kgha,
      breaks = c(-15, 0, 15, 30, 45),
      labels = c("-15 to 0", "0 to 15", "15 to 30", "30 to 45"),
      include.lowest = TRUE),
    co2 = factor(as.character(co2),
      levels = c("350", "540"),
      labels = c("Without elevated CO2", "With elevated CO2"))
  )

df_p5_map <- to_stars_grid(p5_map_data, dims = c("x", "y", "scenario", "co2"))
plot5_map <- ggplot() +
  geom_sf(data = ark, fill = "grey50", color = "black") +
  geom_stars(data = df_p5_map, aes(fill = yield_bin)) +
  geom_sf(data = usa_counties, fill = NA, colour = "black", linewidth = 0.5) +
  coord_sf(xlim = c(360000, 570000)) +
  facet_grid(co2 ~ scenario) +
  temp +
  scale_fill_manual(
    values = c("-15 to 0" = "#b10026", "0 to 15" = "#e5f5f9",
               "15 to 30" = "#99d8c9", "30 to 45" = "#2ca25f"),
    na.value = "grey50", na.translate = FALSE,
    name = "Yield change (%)") +
  map_theme

plot5_merged <- plot_grid(
  plot5_violin + theme(plot.margin = margin(5, 5, 5, 5)),
  plot5_map    + theme(plot.margin = margin(5, 5, 5, 5)),
  ncol = 2, align = "v", axis = "tb",
  labels = "AUTO", rel_widths = c(0.8, 1)
)

ggsave("figures/Fig1-yield-adaptation-strategies.tiff", plot = plot5_merged,
       width = 30, height = 15, units = "cm", dpi = 600, compression = "lzw", bg = "white")

## ── STATS 1: Main Fig 1 — all cited numbers for Results §3.1 and §3.2 ────────
## Paper cites (§3.1): "decreased yields 5.9% on average, values ranging from
##   -14.8% to +2.6%"; "median baseline yield 4,525 kg/ha"; "117 kg/ha per °C";
##   "167 kg/ha per °C under warm/dry"; "12.4% greater under combined elevated T+CO2"
## Paper cites (§3.2): "increased yield by [X]% under 2°C without CO2";
##   "standalone LM: median yields comparable to baseline"; "61% of fields positive"
##   "combined elevated CO2 + stacked management increased yields 30%, 16-44%"
{
  cat(paste(rep("═", 70), collapse = ""), "\n")
  cat("STATS 1 — MAIN FIG 1 (all CO2 levels, all scenarios vs baseline)\n")
  cat(paste(rep("─", 70), collapse = ""), "\n")

  ## ── §3.1: Temperature and CO2 impacts (no adaptation) ──────────────────────
  cat("\n§3.1  Climate change without adaptation (CO2 = 350 ppm)\n\n")

  bl     <- simulated0 %>% filter(scenario == "baseline")
  cc     <- simulated0 %>% filter(scenario == "climate_change", co2 == 350)
  cc_co2 <- simulated0 %>% filter(scenario == "climate_change", co2 == 540)

  bl_sy  <- bl %>% select(x, y, date, yield_bl = Yield_kgha)
  cc_sy  <- cc %>% left_join(bl_sy, by = c("x","y","date")) %>%
                   mutate(pct = (Yield_kgha - yield_bl) / yield_bl * 100)
  cc2_sy <- cc_co2 %>% left_join(bl_sy, by = c("x","y","date")) %>%
                       mutate(pct = (Yield_kgha - yield_bl) / yield_bl * 100)

  cat(sprintf("  2°C mean yield change:  %+.1f%%  (range: %.1f%% to %+.1f%%)\n",
              mean(cc_sy$pct,  na.rm=TRUE),
              min(cc_sy$pct,   na.rm=TRUE),
              max(cc_sy$pct,   na.rm=TRUE)))
  cat(sprintf("  Baseline median yield:  %.0f kg/ha\n",
              median(bl$Yield_kgha, na.rm=TRUE)))
  cat(sprintf("  Baseline yield range:   %.0f – %.0f kg/ha\n",
              min(bl$Yield_kgha, na.rm=TRUE), max(bl$Yield_kgha, na.rm=TRUE)))

  ## Temperature slope (kg/ha per °C) under 2°C scenario
  lm_all  <- lm(Yield_kgha ~ SeasonMeanT, data = cc)
  sl_all  <- coef(lm_all)["SeasonMeanT"]
  sl_pct  <- sl_all / mean(cc$Yield_kgha, na.rm=TRUE) * 100
  rain_mu <- mean(cc$SeasonRain, na.rm=TRUE)
  t_mu    <- mean(cc$SeasonMeanT, na.rm=TRUE)
  cc_wd   <- cc %>% filter(SeasonMeanT > t_mu, SeasonRain < rain_mu)
  sl_wd   <- coef(lm(Yield_kgha ~ SeasonMeanT, data = cc_wd))["SeasonMeanT"]
  cat(sprintf("  Temp slope (overall):   %.0f kg/ha/°C  (%.1f%%/°C)\n", sl_all, sl_pct))
  cat(sprintf("  Temp slope (Warm/Dry):  %.0f kg/ha/°C\n", sl_wd))

  cat(sprintf("  Elev CO2 yield change:  %+.1f%%  (range: %+.1f%% to %+.1f%%)\n",
              mean(cc2_sy$pct, na.rm=TRUE),
              min(cc2_sy$pct,  na.rm=TRUE),
              max(cc2_sy$pct,  na.rm=TRUE)))

  ## ── §3.2: Adaptive strategies (CO2 = 350, all vs baseline) ─────────────────
  cat("\n§3.2  Adaptive strategies (CO2 = 350 ppm, vs baseline)\n\n")

  s350 <- simulated0 %>% filter(co2 == 350)
  MY   <- function(sc) mean(s350$Yield_kgha[s350$scenario == sc], na.rm=TRUE)

  ybl  <- MY("baseline"); ycc <- MY("climate_change")
  ylm  <- MY("longer_mat"); yes <- MY("early_sowing"); ycmb <- MY("early_sowing_longer_mat")
  pct  <- function(y) round(100*(y - ybl)/ybl, 1)

  d_cc <- pct(ycc); d_lm <- pct(ylm); d_es <- pct(yes); d_cmb <- pct(ycmb)
  d_add <- d_lm + d_es; d_syn <- d_cmb - d_add

  cat(sprintf("  %-42s  %+5.1f%%  (%+.0f kg/ha)\n", "Baseline:", 0, 0))
  cat(sprintf("  %-42s  %+5.1f%%  (%+.0f kg/ha)\n",
              "2°C-increase (no adaptation):", d_cc, ycc - ybl))
  cat(sprintf("  %-42s  %+5.1f%%  (%+.0f kg/ha)\n",
              "Late-Maturing alone (MG5, May-22):", d_lm, ylm - ybl))
  cat(sprintf("  %-42s  %+5.1f%%  (%+.0f kg/ha)\n",
              "Early Sowing alone (MG4, Apr-24):", d_es, yes - ybl))
  cat(sprintf("  %-42s  %+5.1f%%  (%+.0f kg/ha)\n",
              "LM + ES combined (MG5, Apr-24):", d_cmb, ycmb - ybl))
  cat(paste(rep("─", 70), collapse=""), "\n")
  cat(sprintf("  Expected additive effect (LM + ES):       %+5.1f%%\n", d_add))
  cat(sprintf("  Observed combined:                        %+5.1f%%\n", d_cmb))
  cat(sprintf("  ► Synergy bonus:                          %+5.1f%%  (%.1fx additive)\n",
              d_syn, d_cmb / d_add))
  cat(sprintf("  ► Combined recovers %.0f%% of warming loss\n",
              100*(ycmb - ycc) / (ybl - ycc)))

  lm_fields <- s350 %>%
    filter(scenario %in% c("baseline","longer_mat")) %>%
    select(x,y,date,scenario,Yield_kgha) %>%
    pivot_wider(names_from=scenario, values_from=Yield_kgha) %>%
    mutate(pct_chg = (longer_mat - baseline)/baseline*100) %>%
    group_by(x,y) %>% summarise(mean_chg = mean(pct_chg, na.rm=TRUE), .groups="drop")
  cat(sprintf("\n  LM alone: %.0f%% of fields show positive yield change\n",
              100*mean(lm_fields$mean_chg > 0, na.rm=TRUE)))

  cmb_co2 <- simulated0 %>%
    filter(scenario == "early_sowing_longer_mat", co2 == 540) %>%
    left_join(bl_sy, by = c("x","y","date")) %>%
    mutate(pct = (Yield_kgha - yield_bl)/yield_bl*100)
  cat(sprintf("  LM+ES + elevated CO2: mean %+.0f%%  (range: %+.0f%% to %+.0f%%)\n",
              mean(cmb_co2$pct, na.rm=TRUE),
              min(cmb_co2$pct,  na.rm=TRUE),
              max(cmb_co2$pct,  na.rm=TRUE)))

  cat(paste(rep("═", 70), collapse = ""), "\n\n")
}

## ── STATS 1b: §3.2 detailed field-level breakdown (CO2 = 350) ────────────────
## Supplements STATS 1 with field-level medians, IQR, seed-fill and crop-cycle
## durations — used for discussion paragraph precision.
## Compares each adaptation scenario against:
##   (a) baseline         → overall yield recovery relative to no-warming
##   (b) climate_change   → incremental gain from each adaptation strategy

s32_base <- simulated0 %>%
  filter(scenario == "baseline") %>%
  select(x, y, date,
         yield_bl  = Yield_kgha,
         sf_bl     = SeedFillingDAS,
         mat_bl    = MaturityDAS,
         emg_bl    = EmergenceDAS)

s32_cc <- simulated0 %>%
  filter(scenario == "climate_change", co2 == 350) %>%
  select(x, y, date,
         yield_cc  = Yield_kgha,
         sf_cc     = SeedFillingDAS,
         mat_cc    = MaturityDAS,
         emg_cc    = EmergenceDAS)

s32_data <- simulated0 %>%
  filter(scenario %in% c("longer_mat", "early_sowing", "early_sowing_longer_mat"),
         co2 == 350) %>%
  select(x, y, date, scenario,
         Yield_kgha, SeedFillingDAS, MaturityDAS, EmergenceDAS) %>%
  left_join(s32_base, by = c("x", "y", "date")) %>%
  left_join(s32_cc,   by = c("x", "y", "date")) %>%
  mutate(
    yld_chg_vs_baseline = (Yield_kgha - yield_bl) / yield_bl * 100,
    yld_chg_vs_cc       = (Yield_kgha - yield_cc) / yield_cc * 100,
    sf_dur     = MaturityDAS - SeedFillingDAS,
    sf_dur_bl  = mat_bl      - sf_bl,
    sf_dur_cc  = mat_cc      - sf_cc,
    sf_chg_vs_baseline = sf_dur - sf_dur_bl,
    sf_chg_vs_cc       = sf_dur - sf_dur_cc,
    tc_dur     = MaturityDAS - EmergenceDAS,
    tc_dur_bl  = mat_bl      - emg_bl,
    tc_dur_cc  = mat_cc      - emg_cc,
    tc_chg_vs_baseline = tc_dur - tc_dur_bl,
    tc_chg_vs_cc       = tc_dur - tc_dur_cc
  )

s32_fields <- s32_data %>%
  group_by(x, y, scenario) %>%
  summarise(across(c(yld_chg_vs_baseline, yld_chg_vs_cc,
                     sf_chg_vs_baseline, sf_chg_vs_cc,
                     tc_chg_vs_baseline, tc_chg_vs_cc),
                   ~ mean(.x, na.rm = TRUE)),
            .groups = "drop")

cat(paste(rep("─", 65), collapse = ""), "\n")
cat("SECTION 3.2 — ADAPTIVE STRATEGIES (CO2 = 350 ppm)\n\n")

cat("YIELD CHANGE vs BASELINE (field means, %):\n")
s32_fields %>%
  group_by(scenario) %>%
  summarise(
    mean  = round(mean(yld_chg_vs_baseline, na.rm = TRUE), 1),
    median = round(median(yld_chg_vs_baseline, na.rm = TRUE), 1),
    q1    = round(quantile(yld_chg_vs_baseline, 0.25, na.rm = TRUE), 1),
    q3    = round(quantile(yld_chg_vs_baseline, 0.75, na.rm = TRUE), 1),
    pct_above_baseline = round(mean(yld_chg_vs_baseline > 0, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  ) %>% print()

cat("\nYIELD CHANGE vs CLIMATE_CHANGE (+2C, no adaptation, %):\n")
s32_fields %>%
  group_by(scenario) %>%
  summarise(
    mean   = round(mean(yld_chg_vs_cc, na.rm = TRUE), 1),
    median = round(median(yld_chg_vs_cc, na.rm = TRUE), 1),
    q1     = round(quantile(yld_chg_vs_cc, 0.25, na.rm = TRUE), 1),
    q3     = round(quantile(yld_chg_vs_cc, 0.75, na.rm = TRUE), 1),
    pct_above_cc = round(mean(yld_chg_vs_cc > 0, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  ) %>% print()

cat("\nSYNERGY CHECK (combined vs sum of individual, field means):\n")
synergy <- s32_fields %>%
  select(x, y, scenario, yld_chg_vs_cc) %>%
  pivot_wider(names_from = scenario, values_from = yld_chg_vs_cc) %>%
  mutate(
    sum_individual  = longer_mat + early_sowing,
    synergy_effect  = early_sowing_longer_mat - sum_individual
  )
cat(sprintf("  Mean sum of individual strategies : %.1f%%\n",
            mean(synergy$sum_individual, na.rm = TRUE)))
cat(sprintf("  Mean combined strategy            : %.1f%%\n",
            mean(synergy$early_sowing_longer_mat, na.rm = TRUE)))
cat(sprintf("  Mean synergy effect (extra gain)  : %.1f%%\n",
            mean(synergy$synergy_effect, na.rm = TRUE)))

cat("\nSEED-FILLING DURATION CHANGE vs CLIMATE_CHANGE (days, field means):\n")
s32_fields %>%
  group_by(scenario) %>%
  summarise(
    mean   = round(mean(sf_chg_vs_cc, na.rm = TRUE), 1),
    median = round(median(sf_chg_vs_cc, na.rm = TRUE), 1),
    max    = round(max(sf_chg_vs_cc, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>% print()

cat("\nSEED-FILLING DURATION CHANGE vs CLIMATE_CHANGE (days, all site-years):\n")
s32_data %>%
  group_by(scenario) %>%
  summarise(
    mean        = round(mean(sf_chg_vs_cc, na.rm = TRUE), 1),
    max_gain    = round(max(sf_chg_vs_cc,  na.rm = TRUE), 1),
    max_loss    = round(min(sf_chg_vs_cc,  na.rm = TRUE), 1),
    pct_longer  = round(mean(sf_chg_vs_cc > 0, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  ) %>% print()

cat("\nTOTAL CROP CYCLE CHANGE vs CLIMATE_CHANGE (days, field means):\n")
s32_fields %>%
  group_by(scenario) %>%
  summarise(
    mean   = round(mean(tc_chg_vs_cc, na.rm = TRUE), 1),
    median = round(median(tc_chg_vs_cc, na.rm = TRUE), 1),
    max    = round(max(tc_chg_vs_cc, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>% print()

cat(paste(rep("─", 65), collapse = ""), "\n\n")


## ── MAIN FIG 2: Phenology change maps ────────────────────────────────────── ----
## Paper: Figure 2 — "Geographical distribution of the relative effects of rising
##        temperatures and adaptive genotypic and agronomic management strategies
##        on changes in soybean total crop cycle and seed-filling period across
##        4,651 soybean fields in the US Mid-South. Values per pixel are averages
##        across 40 weather years. LM & ES represent the combined effect of a
##        late-maturing variety and early sowing."
## CO2 = 350 ppm only; panels A (crop cycle) and B (seed-filling period)

p9_baseline <- simulated0 %>%
  filter(scenario == "baseline") %>%
  mutate(bl_sf = MaturityDAS - SeedFillingDAS,
         bl_tc = MaturityDAS - EmergenceDAS) %>%
  select(x, y, date, bl_sf, bl_tc)

p9_data <- simulated0 %>%
  filter(scenario != "baseline", co2 == 350) %>%
  mutate(sf_dur = MaturityDAS - SeedFillingDAS,
         tc_dur = MaturityDAS - EmergenceDAS) %>%
  select(x, y, date, scenario, sf_dur, tc_dur) %>%
  left_join(p9_baseline, by = c("x", "y", "date")) %>%
  filter(!is.na(bl_tc)) %>%
  mutate(sf_chg = sf_dur - bl_sf,
         tc_chg = tc_dur - bl_tc)

make_pheno_map <- function(data, var, breaks, labels, colors, legend_title) {
  df <- data %>%
    mutate(scenario = factor(scenario, levels = scenario_levels,
                             labels = scenario_labels)) %>%
    group_by(x, y, scenario) %>%
    summarise(value = round(mean(.data[[var]], na.rm = TRUE), 1), .groups = "drop") %>%
    mutate(value_bin = cut(value, breaks = breaks, labels = labels,
                           include.lowest = TRUE))
  s <- to_stars_grid(df, dims = c("x", "y", "scenario"))
  ggplot() +
    geom_sf(data = ark, fill = "grey50", color = "black") +
    geom_stars(data = s, aes(fill = value_bin)) +
    geom_sf(data = usa_counties, fill = NA, colour = "black", linewidth = 0.5) +
    coord_sf(xlim = c(360000, 570000)) +
    facet_wrap(~ scenario, nrow = 1) +
    temp +
    scale_fill_manual(values = colors, na.value = "grey50", na.translate = FALSE,
                      name = legend_title) +
    map_theme
}

## Panel A — total cycle change
plot9a <- make_pheno_map(p9_data, "tc_chg",
  breaks = c(-8, 0, 8, 15, 22, 35),
  labels = c("-8 to 0", "0 to 8", "8 to 15", "15 to 22", "> 22"),
  colors = c("-8 to 0"  = "#b10026", "0 to 8"  = "#e5f5f9",
             "8 to 15"  = "#99d8c9", "15 to 22" = "#41ae76", "> 22" = "#005824"),
  legend_title = "(A)  Total crop cycle change (days)")

## Panel B — seed-filling period change
plot9b <- make_pheno_map(p9_data, "sf_chg",
  breaks = c(-5, 0, 5, 10, 15, 30),
  labels = c("-5 to 0", "0 to 5", "5 to 10", "10 to 15", "> 15"),
  colors = c("-5 to 0"  = "#b10026", "0 to 5"  = "#e5f5f9",
             "5 to 10"  = "#99d8c9", "10 to 15" = "#41ae76", "> 15" = "#005824"),
  legend_title = "(B)  Seed-filling period change (days)")

plot9 <- plot_grid(plot9a, plot9b, ncol = 1, align = "v", axis = "lr")

ggsave("figures/Fig2-phenology-change-maps.tiff", plot = plot9,
       width = 18, height = 18, units = "cm", dpi = 600, compression = "lzw", bg = "white")

## ── STATS 2: Main Fig 2 — crop cycle and seed-filling duration changes ────────
## Paper (§3.2): seed-filling and total crop cycle changes per scenario (CO2=350)
## and climate_change-only stats for the "shortening" paragraph in Results
{
  cat(paste(rep("═", 70), collapse=""), "\n")
  cat("STATS 2 — MAIN FIG 2: Phenology changes (CO2=350, vs baseline)\n")
  cat(paste(rep("─", 70), collapse=""), "\n")

  ## Climate change scenario only — field-level means for the crop-cycle shortening paragraph
  p9_cc_site_years <- p9_data %>% filter(scenario == "climate_change")

  p9_cc <- p9_cc_site_years %>%
    group_by(x, y) %>%
    summarise(
      tc_chg_mean = mean(tc_chg, na.rm = TRUE),
      sf_chg_mean = mean(sf_chg, na.rm = TRUE),
      .groups = "drop"
    )

  lat_breaks <- quantile(p9_cc$y, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
  p9_cc <- p9_cc %>%
    mutate(lat_zone = cut(y, breaks = lat_breaks, include.lowest = TRUE,
                          labels = c("South", "Central", "North")))

  cat("\nTOTAL CROP CYCLE — climate_change vs baseline:\n")
  cat(sprintf("  Fields with shorter cycle: %d of %d (%.0f%%)\n",
              sum(p9_cc$tc_chg_mean < 0, na.rm = TRUE),
              nrow(p9_cc),
              mean(p9_cc$tc_chg_mean < 0, na.rm = TRUE) * 100))
  cat(sprintf("  Max shortening : %.1f days (%.1f weeks)\n",
              min(p9_cc_site_years$tc_chg, na.rm = TRUE),
              min(p9_cc_site_years$tc_chg, na.rm = TRUE) / 7))
  cat(sprintf("  Max lengthening: %.1f days\n",
              max(p9_cc_site_years$tc_chg, na.rm = TRUE)))
  cat(sprintf("  Mean           : %.1f days\n",
              mean(p9_cc_site_years$tc_chg, na.rm = TRUE)))

  cat("\nSEED-FILLING — climate_change vs baseline (all site-years):\n")
  cat(sprintf("  Max shortening : %.1f days\n",
              min(p9_cc_site_years$sf_chg, na.rm = TRUE)))
  cat(sprintf("  Max lengthening: %.1f days\n",
              max(p9_cc_site_years$sf_chg, na.rm = TRUE)))
  cat(sprintf("  Mean           : %.1f days\n",
              mean(p9_cc_site_years$sf_chg, na.rm = TRUE)))
  cat(sprintf("  Site-years with shorter seed-fill: %.0f%%\n",
              mean(p9_cc_site_years$sf_chg < 0, na.rm = TRUE) * 100))

  cat("\nSEED-FILLING CHANGE by latitude zone (field means):\n")
  p9_cc %>%
    group_by(lat_zone) %>%
    summarise(
      n_fields     = n(),
      mean_sf_chg  = round(mean(sf_chg_mean, na.rm = TRUE), 1),
      pct_negative = round(mean(sf_chg_mean < 0, na.rm = TRUE) * 100, 1),
      .groups = "drop"
    ) %>% print()

  ## All adaptation scenarios — phenology changes vs baseline
  s350 <- simulated0 %>% filter(co2 == 350)
  bl_ph <- s350 %>% filter(scenario == "baseline") %>%
    select(x, y, date,
           sf_bl  = SeedFillingDAS, mat_bl = MaturityDAS, emg_bl = EmergenceDAS)

  ph_data <- s350 %>%
    filter(scenario %in% c("climate_change","longer_mat","early_sowing",
                           "early_sowing_longer_mat")) %>%
    select(x, y, date, scenario, SeedFillingDAS, MaturityDAS, EmergenceDAS) %>%
    left_join(bl_ph, by = c("x","y","date")) %>%
    mutate(
      sf_dur    = MaturityDAS - SeedFillingDAS,
      sf_dur_bl = mat_bl     - sf_bl,
      tc_dur    = MaturityDAS - EmergenceDAS,
      tc_dur_bl = mat_bl     - emg_bl,
      sf_chg    = sf_dur - sf_dur_bl,
      tc_chg    = tc_dur - tc_dur_bl
    )

  cat("\nTotal crop-cycle change vs baseline (days, mean ± sd):\n")
  ph_data %>% group_by(scenario) %>%
    summarise(mean = round(mean(tc_chg, na.rm=TRUE), 1),
              sd   = round(sd(tc_chg,   na.rm=TRUE), 1),
              min  = round(min(tc_chg,   na.rm=TRUE), 1),
              max  = round(max(tc_chg,   na.rm=TRUE), 1), .groups="drop") %>%
    print()

  cat("\nSeed-filling duration change vs baseline (days, mean ± sd):\n")
  ph_data %>% group_by(scenario) %>%
    summarise(mean = round(mean(sf_chg, na.rm=TRUE), 1),
              sd   = round(sd(sf_chg,   na.rm=TRUE), 1),
              min  = round(min(sf_chg,   na.rm=TRUE), 1),
              max  = round(max(sf_chg,   na.rm=TRUE), 1), .groups="drop") %>%
    print()

  cat("\nAbsolute durations (days, mean across all site-years):\n")
  cat(sprintf("  Baseline seed-filling: %.1f days | crop cycle: %.1f days\n",
      mean(ph_data$sf_dur_bl, na.rm=TRUE), mean(ph_data$tc_dur_bl, na.rm=TRUE)))
  ph_data %>% group_by(scenario) %>%
    summarise(sf = round(mean(sf_dur, na.rm=TRUE), 1),
              tc = round(mean(tc_dur, na.rm=TRUE), 1), .groups="drop") %>%
    print()

  cat(paste(rep("═", 70), collapse=""), "\n\n")
}


## ══════════════════════════════════════════════════════════════════════════════
## SUPPLEMENTARY FIGURES
## ══════════════════════════════════════════════════════════════════════════════

## ── SUPP FIG S1: MET locations + phenology calibration + yield calibration ── ----
## Paper: Figure S1 — "Locations of multi-environment trials used to collect
##        phenology and yield data for soybean maturity groups 4 to 5 (A).
##        Relationship between APSIM-simulated and observed DOY for key
##        phenological stages: emergence, beginning of flowering, beginning of
##        pod development, beginning of seed filling, and physiological maturity (B).
##        Relationship between APSIM-simulated and observed soybean yield at
##        13% grain moisture (C)."
## → EXTERNAL FIGURE — generated outside this script (model calibration dataset)

## ── SUPP FIG S2: Biomass validation — Fayetteville 2008 ─────────────────── ----
## Paper: Figure S2 — "APSIM-simulated (lines) and measured (points) leaf biomass
##        (green), seed biomass (black), and total biomass (yellow) in Fayetteville,
##        NW Arkansas, for 2 maturity groups (early MG4 and early MG5). Measured
##        data from irrigated experiment in 2008 (RCBD, 4 reps; Mastrodomenico &
##        Purcell, 2012). RMSEs: seed yield 659 kg/ha, leaf biomass 572 kg/ha,
##        total biomass 1335 kg/ha."
## → EXTERNAL FIGURE — generated outside this script (biomass validation dataset)

## ── SUPP FIG S3: Environmental characterization ──────────────────────────── ----
## Paper: Figure S3 — "Environmental characterization of baseline conditions
##        across all studied site-years. The APSIM phenology response curve (0-1)
##        is presented, where 1 represents maximum development rate. The solid
##        vertical and horizontal lines indicate the median growing-season average
##        temperature and total growing-season precipitation, respectively."

p6_weather <- simulated0 %>%
  filter(scenario == "baseline", co2 == 350) %>%
  mutate(year = year(date)) %>%
  mutate(
    Rain.avg    = mean(SeasonRain),
    Meat.avg    = mean(SeasonMeanT),
    Rain.sd     = sd(SeasonRain),
    Meat.sd     = sd(SeasonMeanT),
    Normal      = ifelse(
      SeasonRain  < Rain.avg + Rain.sd * 0.5 & SeasonRain  > Rain.avg - Rain.sd * 0.5 &
      SeasonMeanT < Meat.avg + Meat.sd * 0.5 & SeasonMeanT > Meat.avg - Meat.sd * 0.5,
      "Yes", "No"),
    Rain.class    = ifelse(SeasonRain > Rain.avg, "Wet", "Dry"),
    Meat_class    = ifelse(SeasonMeanT > Meat.avg, "Warm", "Cool"),
    weather.class = factor(
      ifelse(Normal == "Yes", "Normal", paste0(Meat_class, "/", Rain.class)),
      levels = c("Cool/Wet", "Warm/Wet", "Normal", "Cool/Dry", "Warm/Dry"))
  ) %>%
  select(x, y, year, weather.class, Meat.avg, Rain.avg, Meat.sd, Rain.sd,
         SeasonMeanT, SeasonRain, EmergenceDAS, FloweringDAS, MaturityDAS)

temp_response <- data.frame(temp = seq(10, 40, 0.1)) %>%
  mutate(response = case_when(
    temp <= 10 ~ 0,
    temp <= 30 ~ (temp - 10) / 20,
    temp <= 40 ~ 1 - (temp - 30) / 10,
    TRUE       ~ 0),
    response_scaled = response * (1400 - 300) + 300)

plot6 <- p6_weather %>%
  filter(SeasonMeanT > 20) %>%
  ggplot(aes(x = SeasonMeanT, y = SeasonRain, fill = weather.class)) +
  geom_point(size = 3, alpha = 0.2, shape = 21) +
  geom_vline(aes(xintercept = Meat.avg), linewidth = 0.5) +
  geom_hline(aes(yintercept = Rain.avg), linewidth = 0.5) +
  geom_rect(aes(xmin = Meat.avg - Meat.sd * 0.5, xmax = Meat.avg + Meat.sd * 0.5,
                ymin = Rain.avg - Rain.sd * 0.5, ymax = Rain.avg + Rain.sd * 0.5),
            color = "black", fill = NA, linetype = "dashed") +
  geom_line(data = temp_response, aes(x = temp, y = response_scaled),
            inherit.aes = FALSE, color = "black", linewidth = 1.5) +
  annotate("segment", x = 35, xend = 32.5, y = 1330, yend = 1150,
           arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
           linewidth = 0.2, color = "black") +
  annotate("text", x = 35.2, y = 1350,
           label = "APSIM phenology\nresponse curve\n(0-1)", hjust = 0, vjust = 1, size = 3.5) +
  temp +
  theme(legend.position = "top") +
  labs(x = "Season temperature (°C)", y = "Season Rainfall (mm)") +
  scale_y_continuous(limits = c(300, 1400),
    sec.axis = sec_axis(~ (. - 300) / 1100, name = "Phenology Temperature Response (0-1)",
                        breaks = seq(0, 1, 0.2))) +
  scale_x_continuous(limits = c(10, 40)) +
  scale_fill_manual(values = c("Warm/Dry" = "#bd0026", "Warm/Wet" = "#fc8d59",
                               "Cool/Dry" = "#91cf60", "Cool/Wet" = "#1a9850",
                               "Normal" = "#d95f0e")) +
  guides(fill = guide_legend(override.aes = list(alpha = 1, size = 4, shape = 21)))

ggsave("figures/FigS3-environmental-characterization.tiff", plot = plot6,
       width = 20, height = 15, units = "cm", dpi = 600, compression = "lzw", bg = "white")


## ── SUPP FIG S4: Soybean sowing progress in Arkansas ────────────────────── ----
## Paper: Figure S4 — "USDA-NASS soybean sowing progress in Arkansas from 1990
##        to 2025. Grey lines represent the historical years, while the green,
##        thicker line indicates the average sowing progress. 50% and 10% average
##        sowing progress are indicated in the figure."
## Figure: FigS4-sowing-progress-all-years.tiff
## Also generates: ExtraFig6-sowing-progress-recent-5yr.tiff (not in paper)

suppressPackageStartupMessages({
  library(purrr)
  library(broom)
  library(readxl)
  library(httr)
  library(jsonlite)
})

## ── S4 Settings ──────────────────────────────────────────────────────────────
NASS_API_KEY  <- "D228A372-93ED-3BF7-9699-D2D0DDD3C88D"
FALLBACK_FILE <- "data/raw/progress.xlsx"
CACHE_FILE    <- "data/raw/nass-progress-cache.rds"
YEAR_MIN      <- 1990
YEAR_MAX      <- as.integer(format(Sys.Date(), "%Y")) - 1L
N_RECENT      <- 5
RECENT_COL    <- "#31a354"
AVG_COL       <- "#00BF7D"

## ── S4 Step 1: Download or load data ─────────────────────────────────────────
download_nass_progress <- function(api_key) {
  message("[Progress] Downloading from USDA-NASS Quick Stats API...")
  resp <- tryCatch(
    GET("https://quickstats.nass.usda.gov/api/api_GET/",
        query   = list(key               = api_key,
                       commodity_desc    = "SOYBEANS",
                       statisticcat_desc = "PROGRESS",
                       unit_desc         = "PCT PLANTED",
                       state_alpha       = "AR",
                       freq_desc         = "WEEKLY",
                       format            = "JSON"),
        timeout(30)),
    error = function(e) { message("[Progress] API error: ", e$message); NULL }
  )
  if (is.null(resp) || http_error(resp)) {
    message("[Progress] API status: ", if (!is.null(resp)) status_code(resp) else "N/A")
    return(NULL)
  }
  j <- tryCatch(fromJSON(content(resp, "text", encoding = "UTF-8")), error = function(e) NULL)
  if (is.null(j) || is.null(j$data) || nrow(j$data) == 0) {
    message("[Progress] API returned empty data."); return(NULL)
  }
  df <- as.data.frame(j$data) %>%
    transmute(
      year  = as.integer(year),
      week  = as.Date(week_ending),
      DOY   = as.integer(strftime(week, "%j")),
      value = suppressWarnings(as.numeric(Value)) / 100
    ) %>%
    filter(!is.na(value), !is.na(DOY), year >= YEAR_MIN, year <= YEAR_MAX)
  message(sprintf("[Progress] Downloaded %d records (%d–%d).",
                  nrow(df), min(df$year), max(df$year)))
  df
}

nass_progress <- download_nass_progress(NASS_API_KEY)

if (!is.null(nass_progress)) {
  saveRDS(nass_progress, CACHE_FILE)
} else if (file.exists(CACHE_FILE)) {
  nass_progress <- readRDS(CACHE_FILE)
  message(sprintf("[Progress] Loaded from cache (%d–%d).",
                  min(nass_progress$year), max(nass_progress$year)))
} else {
  message("[Progress] Loading from ", FALLBACK_FILE)
  raw <- read_excel(FALLBACK_FILE)
  names(raw) <- gsub(" ", "_", toupper(trimws(names(raw))))
  nass_progress <- raw %>%
    filter(grepl("PLANTED",  DATA_ITEM, ignore.case = TRUE),
           grepl("SOYBEANS", DATA_ITEM, ignore.case = TRUE),
           YEAR >= YEAR_MIN, YEAR <= YEAR_MAX) %>%
    transmute(year  = as.integer(YEAR),
              week  = as.Date(WEEK_ENDING),
              DOY   = as.integer(strftime(week, "%j")),
              value = VALUE / 100) %>%
    filter(!is.na(value), !is.na(DOY))
}

cat(sprintf("[Progress] %d records | %d years (%d–%d)\n",
            nrow(nass_progress), n_distinct(nass_progress$year),
            min(nass_progress$year), max(nass_progress$year)))

## ── S4 Step 2: Fit logistic model per year ───────────────────────────────────
fit_logistic_progress <- function(df) {
  tryCatch(
    nls(value ~ 1 / (1 + exp(-b * (DOY - c))),
        data = df, start = list(b = 0.10, c = 135)),
    error = function(e) NULL
  )
}

doy_at_pct <- function(b, c, pct) c - log(100 / pct - 1) / b

progress_models <- nass_progress %>%
  group_by(year) %>%
  nest() %>%
  mutate(model  = map(data, fit_logistic_progress),
         failed = map_lgl(model, is.null))

n_prog_failed <- sum(progress_models$failed)
if (n_prog_failed > 0)
  message(sprintf("[Progress] %d year(s) did not converge and are excluded.", n_prog_failed))

params_df <- progress_models %>%
  filter(!failed) %>%
  mutate(b      = map_dbl(model, ~ coef(.x)["b"]),
         c      = map_dbl(model, ~ coef(.x)["c"]),
         doy_10 = map2_dbl(b, c, ~ doy_at_pct(.x, .y, 10)),
         doy_50 = map2_dbl(b, c, ~ doy_at_pct(.x, .y, 50))) %>%
  select(year, b, c, doy_10, doy_50)

DOY_seq <- 95:175

pred_curves <- params_df %>%
  mutate(pred = map2(b, c, ~ data.frame(
    DOY      = DOY_seq,
    progress = 100 / (1 + exp(-.x * (DOY_seq - .y)))
  ))) %>%
  select(year, doy_10, doy_50, pred) %>%
  unnest(pred)

avg_curve  <- pred_curves %>%
  group_by(DOY) %>%
  summarise(progress = mean(progress, na.rm = TRUE), .groups = "drop")

avg_doy_10 <- mean(params_df$doy_10, na.rm = TRUE)
avg_doy_50 <- mean(params_df$doy_50, na.rm = TRUE)

cat(sprintf("[Progress] Avg DOY at 10%% planted: %.1f (~%s)\n", avg_doy_10,
            format(as.Date(paste0("2023-", round(avg_doy_10)), "%Y-%j"), "%b %d")))
cat(sprintf("[Progress] Avg DOY at 50%% planted: %.1f (~%s)\n", avg_doy_50,
            format(as.Date(paste0("2023-", round(avg_doy_50)), "%Y-%j"), "%b %d")))

## ── S4 Shared axis settings ───────────────────────────────────────────────────
doy_breaks <- c(100, 110, 120, 130, 140, 150, 160, 170)
doy_labels <- c("100\n(Apr 10)", "110\n(Apr 20)", "120\n(Apr 30)",
                "130\n(May 10)", "140\n(May 20)", "150\n(May 30)",
                "160\n(Jun 9)",  "170\n(Jun 19)")

## ── S4 Figure (all years): all years + average, 10% and 50% annotated ────────
all_10_text_x <- 100;  all_10_text_y <- 50
all_50_text_x <- 100;  all_50_text_y <- 72

plot_progress_all <- ggplot() +
  geom_line(data = pred_curves,
            aes(x = DOY, y = progress, group = year),
            colour = "grey70", linewidth = 0.55, alpha = 0.7) +
  geom_line(data = avg_curve, aes(x = DOY, y = progress),
            colour = AVG_COL, linewidth = 1.5) +
  annotate("segment",
           x = all_10_text_x + 7, y = all_10_text_y - 3,
           xend = avg_doy_10, yend = 14,
           arrow = arrow(length = unit(0.13, "cm"), type = "closed"),
           colour = "black", linewidth = 0.45) +
  annotate("segment",
           x = all_50_text_x + 17, y = all_50_text_y - 3,
           xend = avg_doy_50, yend = 51,
           arrow = arrow(length = unit(0.13, "cm"), type = "closed"),
           colour = "black", linewidth = 0.45) +
  annotate("text", x = all_10_text_x, y = all_10_text_y,
           label = sprintf("10%% planted\nDOY %.0f (%s)", avg_doy_10,
                           format(as.Date(paste0("2023-", round(avg_doy_10)), "%Y-%j"), "%b %d")),
           hjust = 0, size = 2.9, colour = "black", lineheight = 0.95) +
  annotate("text", x = all_50_text_x, y = all_50_text_y,
           label = sprintf("50%% planted\nDOY %.0f (%s)", avg_doy_50,
                           format(as.Date(paste0("2023-", round(avg_doy_50)), "%Y-%j"), "%b %d")),
           hjust = 0, size = 2.9, colour = "black", lineheight = 0.95) +
  temp +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "grey90"),
        axis.text.x = element_text(angle = 35, hjust = 1)) +
  scale_x_continuous(name = "Day of year", breaks = doy_breaks, labels = doy_labels,
                     expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(name = "Soybean planting progress (%)",
                     limits = c(0, 100), breaks = seq(0, 100, 20))

ggsave("figures/FigS4-sowing-progress-all-years.tiff", plot = plot_progress_all,
       width = 14, height = 14, units = "cm",
       dpi = 600, compression = "lzw", bg = "white")
cat("[Progress] Saved: figures/FigS4-sowing-progress-all-years.tiff\n")

## ── S4 Figure (recent 5 years): last 5 years + 5-yr average ─────────────────
recent_years <- sort(unique(params_df$year), decreasing = TRUE)[seq_len(N_RECENT)]

pred_bg     <- filter(pred_curves, !year %in% recent_years)
pred_recent <- filter(pred_curves,  year %in% recent_years)

avg5_b      <- mean(filter(params_df, year %in% recent_years)$b)
avg5_c      <- mean(filter(params_df, year %in% recent_years)$c)
avg5_doy_10 <- doy_at_pct(avg5_b, avg5_c, 10)
avg5_doy_50 <- doy_at_pct(avg5_b, avg5_c, 50)
avg5_curve  <- data.frame(DOY      = DOY_seq,
                          progress = 100 / (1 + exp(-avg5_b * (DOY_seq - avg5_c))))

arr_10_text_x <- avg5_doy_10 + 10;  arr_10_text_y <- 25
arr_50_text_x <- avg5_doy_50 + 5;   arr_50_text_y <- 63

plot_progress_recent <- ggplot() +
  geom_line(data = pred_bg,
            aes(x = DOY, y = progress, group = year),
            colour = "grey75", linewidth = 0.3, alpha = 0.6) +
  geom_line(data = pred_recent,
            aes(x = DOY, y = progress, group = year),
            colour = RECENT_COL, linewidth = 0.45, alpha = 0.7) +
  geom_line(data = avg5_curve, aes(x = DOY, y = progress),
            colour = RECENT_COL, linewidth = 2.0) +
  geom_hline(yintercept = c(10, 50), linetype = "dashed",
             colour = "grey40", linewidth = 0.4) +
  annotate("segment",
           x = arr_10_text_x, y = arr_10_text_y - 3,
           xend = avg5_doy_10 + 1, yend = 11,
           arrow = arrow(length = unit(0.13, "cm"), type = "closed"),
           colour = "black", linewidth = 0.45) +
  annotate("segment",
           x = arr_50_text_x, y = arr_50_text_y - 3,
           xend = avg5_doy_50 + 1, yend = 51,
           arrow = arrow(length = unit(0.13, "cm"), type = "closed"),
           colour = "black", linewidth = 0.45) +
  annotate("text",
           x = arr_10_text_x, y = arr_10_text_y,
           label = sprintf("10%% planted\nDOY %.0f (%s)", avg5_doy_10,
                           format(as.Date(paste0("2023-", round(avg5_doy_10)), "%Y-%j"), "%b %d")),
           hjust = 0, size = 2.9, colour = "black", lineheight = 0.95) +
  annotate("text",
           x = arr_50_text_x, y = arr_50_text_y,
           label = sprintf("50%% planted\nDOY %.0f (%s)", avg5_doy_50,
                           format(as.Date(paste0("2023-", round(avg5_doy_50)), "%Y-%j"), "%b %d")),
           hjust = 0, size = 2.9, colour = "black", lineheight = 0.95) +
  temp +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "grey90"),
        axis.text.x = element_text(angle = 35, hjust = 1)) +
  scale_x_continuous(name = "Day of year", breaks = doy_breaks, labels = doy_labels,
                     expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(name = "Soybean planting progress (%)",
                     limits = c(0, 100), breaks = seq(0, 100, 20))

ggsave("figures/ExtraFig6-sowing-progress-recent-5yr.tiff", plot = plot_progress_recent,
       width = 16, height = 11, units = "cm",
       dpi = 600, compression = "lzw", bg = "white")
cat("[Progress] Saved: figures/ExtraFig6-sowing-progress-recent-5yr.tiff\n")

cat(sprintf("\n── Per-year 50%% and 10%% planting DOY (%d–%d) ──\n",
            min(params_df$year), max(params_df$year)))
params_df %>%
  transmute(year,
            doy_10 = round(doy_10, 1),
            date_10 = format(as.Date(paste0(year, "-", round(doy_10)), "%Y-%j"), "%b %d"),
            doy_50 = round(doy_50, 1),
            date_50 = format(as.Date(paste0(year, "-", round(doy_50)), "%Y-%j"), "%b %d")) %>%
  arrange(desc(year)) %>%
  print(n = Inf)


## ── SUPP FIG S5: Yield density + spatial map (climate change without adapt.) ── ----
## Paper: Figure S5 — "Density distribution of simulated soybean yields without
##        adaptation under the baseline and +2°C scenarios with and without
##        elevated [CO2] (A), and its geographical distribution (B)."

p1_data <- simulated0 %>%
  filter(scenario %in% c("baseline", "climate_change")) %>%
  group_by(x, y, scenario, co2) %>%
  summarise(Yield_kgha = mean(Yield_kgha, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    scenario_co2 = factor(
      paste0(scenario, co2),
      levels = c("baseline350", "climate_change350", "climate_change540"),
      labels = c("Baseline", "2°C-increase", "2°C-increase with CO2")
    ),
    yield_bin = cut(Yield_kgha,
      breaks = c(-Inf, 2500, 3500, 4500, 5500, Inf),
      labels = c("<2500", "2500-3500", "3500-4500", "4500-5500", ">5500"),
      include.lowest = TRUE)
  )

summary(p1_data %>% filter(scenario_co2 == "Baseline"))

plot1a <- p1_data %>%
  mutate(scenario_co2 = factor(scenario_co2,
    labels = c("Baseline", "2°C-increase", "2°C-increase\nwith elevated CO2"))) %>%
  ggplot(aes(x = Yield_kgha, y = scenario_co2, colour = scenario_co2)) +
  geom_density_ridges(scale = 0.9, fill = NA, linewidth = 1) +
  temp +
  labs(x = "Yield (kg/ha)", y = element_blank()) +
  scale_colour_manual(values = c("black", "#bd0026", "#1a9850")) +
  theme(legend.position = "none") +
  scale_y_discrete(expand = expansion(mult = c(0, 0.4)))

df_p1 <- to_stars_grid(p1_data, dims = c("x", "y", "scenario_co2"))
plot1b <- ggplot() +
  geom_sf(data = ark, fill = "grey30", color = "black") +
  geom_stars(data = df_p1, aes(fill = yield_bin)) +
  geom_sf(data = usa_counties, fill = NA, colour = "black", linewidth = 0.5) +
  coord_sf(xlim = c(360000, 570000)) +
  facet_wrap(~ scenario_co2, nrow = 1) +
  temp +
  scale_fill_manual(
    values = c("<2500" = "#bd0026", "2500-3500" = "#e31a1c", "3500-4500" = "#fc8d59",
               "4500-5500" = "#91cf60", ">5500" = "#1a9850"),
    na.value = "grey50", na.translate = FALSE,
    name = "Yield (kg/ha)") +
  theme(axis.title = element_blank(), axis.text = element_blank(),
        legend.position = "left", legend.direction = "vertical",
        legend.title = element_text(size = 12))

plot1 <- plot_grid(plot1a, plot1b, ncol = 1, align = "v", axis = "r",
                   labels = "AUTO", rel_heights = c(0.4, 1))

ggsave("figures/FigS5-yield-density-no-adaptation.tiff", plot = plot1,
       width = 20, height = 15, units = "cm", dpi = 600, compression = "lzw", bg = "white")

## ── STATS S5: Supp Fig S5 — baseline yield distribution ──────────────────────
## Paper (§3.1): "median baseline yield 4,525 kg/ha with a south-to-north
##   gradient ranging from 2,235 to 4,981 kg/ha"
{
  cat(paste(rep("═", 70), collapse=""), "\n")
  cat("STATS S5 — SUPP FIG S5: Baseline yield distribution\n")
  cat(paste(rep("─", 70), collapse=""), "\n")
  bl <- simulated0 %>% filter(scenario == "baseline") %>%
        group_by(x, y) %>% summarise(yield = mean(Yield_kgha, na.rm=TRUE), .groups="drop")
  cat(sprintf("  Median field-mean baseline yield: %.0f kg/ha\n", median(bl$yield, na.rm=TRUE)))
  cat(sprintf("  Field-mean yield range:           %.0f – %.0f kg/ha\n",
              min(bl$yield, na.rm=TRUE), max(bl$yield, na.rm=TRUE)))
  cat(sprintf("  Mean baseline yield (all site-years): %.0f kg/ha\n",
              mean(simulated0$Yield_kgha[simulated0$scenario=="baseline"], na.rm=TRUE)))
  cat(paste(rep("═", 70), collapse=""), "\n\n")
}


## ── SUPP FIG S6: Temperature–yield scatter ────────────────────────────────── ----
## Paper: Figure S6 — "Relationship between season temperature (May-October) and
##        soybean seed yield across 40 years and 4,651 fields in the US Mid-South
##        under a 2°C temperature increase scenario without adaptation. The text
##        in the panel represents the equation slopes for the overall data and for
##        filtered Warm/Dry site-years."
## Yield vs season temperature under +2°C, colored by latitude

p2_data <- simulated0 %>%
  filter(scenario == "climate_change", co2 == 350) %>%
  mutate(
    Rain.avg    = mean(SeasonRain),
    Meat.avg    = mean(SeasonMeanT),
    Rain.sd     = sd(SeasonRain),
    Meat.sd     = sd(SeasonMeanT),
    Normal      = ifelse(
      SeasonRain  < Rain.avg + Rain.sd  & SeasonRain  > Rain.avg - Rain.sd &
      SeasonMeanT < Meat.avg + Meat.sd  & SeasonMeanT > Meat.avg - Meat.sd,
      "Yes", "No"),
    Rain.class    = ifelse(SeasonRain > Rain.avg, "Wet", "Dry"),
    Meat_class    = ifelse(SeasonMeanT > Meat.avg, "Warm", "Cool"),
    weather.class = factor(
      ifelse(Normal == "Yes", "Normal", paste0(Meat_class, "/", Rain.class)),
      levels = c("Cool/Wet", "Warm/Wet", "Normal", "Cool/Dry", "Warm/Dry"))
  )

slope_overall  <- coef(lm(Yield_kgha ~ SeasonMeanT, data = p2_data))["SeasonMeanT"]
slope_warm_dry <- coef(lm(Yield_kgha ~ SeasonMeanT,
  data = filter(p2_data, weather.class == "Warm/Dry")))["SeasonMeanT"]

cat(sprintf("Overall slope: %.0f kg/ha/°C (%.1f%%/°C)\n",
            slope_overall, slope_overall / mean(p2_data$Yield_kgha) * 100))
cat(sprintf("Warm/Dry slope: %.0f kg/ha/°C\n", slope_warm_dry))

min_y  <- min(p2_data$y);  mean_y <- mean(p2_data$y);  max_y <- max(p2_data$y)

plot2 <- p2_data %>%
  filter(weather.class %in% c("Cool/Wet", "Normal", "Warm/Dry")) %>%
  ggplot(aes(x = SeasonMeanT, y = Yield_kgha)) +
  geom_point(aes(colour = y), size = 2, alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, colour = "black", linewidth = 0.8) +
  geom_smooth(data = ~ filter(., weather.class == "Warm/Dry"),
              aes(group = weather.class), method = "lm", se = FALSE,
              colour = "#008837", linewidth = 1.2) +
  temp +
  labs(x = "Season temperature (°C)", y = "Seed yield (kg/ha)") +
  scale_y_continuous(limits = c(1000, 6300), breaks = seq(1000, 6000, 1000)) +
  scale_x_continuous(limits = c(21, 31), breaks = seq(21, 31, 2)) +
  scale_color_gradientn(
    colors = c("#a50f15", "#fb6a4a", "white"),
    values = scales::rescale(c(min_y, mean_y, max_y)),
    breaks = c(min_y, mean_y, max_y), labels = c("33.0", "35.0", "36.5"),
    limits = c(min_y, max_y),
    guide  = guide_colorbar(barwidth = 1.5, barheight = 5,
                            frame.colour = "black", frame.linewidth = 1),
    name   = "Latitude") +
  theme(legend.title = element_text(size = 12), legend.text = element_text(size = 12)) +
  annotate("segment", x = 22.5, xend = 22.8, y = 5400, yend = 4650,
           arrow = arrow(length = unit(0.25, "cm")), color = "black", linewidth = 0.5) +
  annotate("text", x = 24.2, y = 5500,
           label = paste0("Overall = ", round(slope_overall, 0), " kg/ha/°C"), size = 4) +
  annotate("segment", x = 29, xend = 29, y = 1600, yend = 3600,
           arrow = arrow(length = unit(0.25, "cm")), color = "black", linewidth = 0.5) +
  annotate("text", x = 27.4, y = 1500,
           label = paste0("Warm & Dry = ", round(slope_warm_dry, 0), " kg/ha/°C"),
           size = 4, colour = "#008837") +
  guides(linetype = "none")

ggsave("figures/FigS6-temperature-yield-scatter.tiff", plot = plot2,
       width = 15, height = 12, units = "cm", dpi = 600, compression = "lzw", bg = "white")

## ── STATS S6: Supp Fig S6 — temperature–yield regression ─────────────────────
## Paper (§3.1): "decreased by 117 kg/ha for every degree increase in season
##   temperature"; "167 kg/ha per °C under warm and dry conditions"
{
  cat(paste(rep("═", 70), collapse=""), "\n")
  cat("STATS S6 — SUPP FIG S6: Temperature–yield regression (CO2=350, +2°C)\n")
  cat(paste(rep("─", 70), collapse=""), "\n")
  cc350 <- simulated0 %>% filter(scenario == "climate_change", co2 == 350)
  sl_all <- coef(lm(Yield_kgha ~ SeasonMeanT, data = cc350))["SeasonMeanT"]
  sl_pct <- sl_all / mean(cc350$Yield_kgha, na.rm=TRUE) * 100
  t_mu   <- mean(cc350$SeasonMeanT, na.rm=TRUE)
  r_mu   <- mean(cc350$SeasonRain,  na.rm=TRUE)
  cc_wd  <- cc350 %>% filter(SeasonMeanT > t_mu, SeasonRain < r_mu)
  sl_wd  <- coef(lm(Yield_kgha ~ SeasonMeanT, data = cc_wd))["SeasonMeanT"]
  cat(sprintf("  Overall slope:    %.0f kg/ha/°C  (%.1f%%/°C)\n", sl_all, sl_pct))
  cat(sprintf("  Warm/Dry slope:   %.0f kg/ha/°C\n", sl_wd))
  cat(paste(rep("═", 70), collapse=""), "\n\n")
}


## ══════════════════════════════════════════════════════════════════════════════
## ADDITIONAL / DIAGNOSTIC FIGURES — not cited in current paper version
## (may support reviewer responses or future manuscript sections)
## ══════════════════════════════════════════════════════════════════════════════

## ── ExtraFig 1: Yield change by scenario — violin (CO2 comparison) ──────── ----
## Half-violin + half-boxplot of yield change (%) vs baseline
## All 4 adaptation scenarios × CO2 level

p3_baseline <- simulated0 %>%
  filter(scenario == "baseline") %>%
  select(x, y, date, baseline = Yield_kgha)

p3_data <- simulated0 %>%
  filter(scenario != "baseline") %>%
  select(x, y, scenario, co2, date, Yield_kgha) %>%
  pivot_wider(names_from = scenario, values_from = Yield_kgha) %>%
  left_join(p3_baseline, by = c("x", "y", "date")) %>%
  mutate(across(all_of(scenario_levels),
                ~ (. - baseline) / baseline * 100)) %>%
  select(-baseline) %>%
  pivot_longer(cols = all_of(scenario_levels),
               names_to = "scenario", values_to = "Yield_kgha") %>%
  mutate(scenario = factor(scenario, levels = scenario_levels,
                           labels = scenario_labels_long))

p3_data %>%
  group_by(co2, scenario) %>%
  summarise(
    median = round(median(Yield_kgha, na.rm = TRUE), 1),
    q1     = round(quantile(Yield_kgha, 0.25, na.rm = TRUE), 1),
    q3     = round(quantile(Yield_kgha, 0.75, na.rm = TRUE), 1),
    .groups = "drop") %>%
  print()

plot3 <- ggplot(p3_data,
    aes(x = factor(scenario), y = Yield_kgha,
        fill = as.factor(co2), colour = as.factor(co2))) +
  geom_abline(intercept = 0, slope = 0, linetype = "dashed") +
  geom_half_violin(
    aes(group = interaction(factor(scenario), as.factor(co2))),
    side = "l", trim = FALSE, alpha = 0.4, colour = NA,
    position = position_dodge(width = 0.6)) +
  geom_half_boxplot(
    aes(group = interaction(factor(scenario), as.factor(co2))),
    side = "r", position = position_dodge(width = 0.6),
    outlier.shape = NA, width = 0.5, color = "black", size = 0.5) +
  temp +
  scale_colour_manual(values = c("#4dac26", "#d01c8b"),
                      label  = c("Without elevated CO2", "With elevated CO2")) +
  scale_fill_manual(values   = c("#4dac26", "#d01c8b"),
                    label    = c("Without elevated CO2", "With elevated CO2")) +
  labs(x = element_blank(), y = "Yield change (%)") +
  theme(legend.position = "top", axis.text.x = element_text(angle = 30, hjust = 1)) +
  scale_y_continuous(breaks = seq(-20, 40, 10), limits = c(-20, 40))

ggsave("figures/ExtraFig1-yield-change-by-scenario.tiff", plot = plot3,
       width = 15, height = 15, units = "cm", dpi = 600, compression = "lzw", bg = "white")


## ── ExtraFig 2: Yield change — spatial map (scenario × CO2) ──────── ----
## Mean yield change (%) vs baseline, averaged across 40 years per cell
## Faceted by scenario × CO2

p4_baseline <- simulated0 %>%
  filter(scenario == "baseline") %>%
  select(x, y, date, baseline = Yield_kgha)

p4_data <- simulated0 %>%
  filter(scenario != "baseline") %>%
  select(x, y, scenario, co2, date, Yield_kgha) %>%
  left_join(p4_baseline, by = c("x", "y", "date")) %>%
  mutate(pct_change = (Yield_kgha - baseline) / baseline * 100,
         scenario   = factor(scenario, levels = scenario_levels, labels = scenario_labels)) %>%
  group_by(x, y, scenario, co2) %>%
  summarise(Yield_kgha = round(mean(pct_change, na.rm = TRUE), 1), .groups = "drop") %>%
  mutate(
    yield_bin = cut(Yield_kgha,
      breaks = c(-15, 0, 15, 30, 45),
      labels = c("-15 to 0", "0 to 15", "15 to 30", "30 to 45"),
      include.lowest = TRUE),
    co2 = factor(as.character(co2),
      levels = c("350", "540"),
      labels = c("Without elevated CO2", "With elevated CO2"))
  )

p4_data %>%
  group_by(co2, scenario) %>%
  summarise(pct_positive = round(mean(Yield_kgha > 0, na.rm = TRUE) * 100, 1),
            .groups = "drop") %>%
  print()

df_p4 <- to_stars_grid(p4_data, dims = c("x", "y", "scenario", "co2"))

plot4 <- ggplot() +
  geom_sf(data = ark, fill = "grey50", color = "black") +
  geom_stars(data = df_p4, aes(fill = yield_bin)) +
  geom_sf(data = usa_counties, fill = NA, colour = "black", linewidth = 0.5) +
  coord_sf(xlim = c(360000, 570000)) +
  facet_grid(co2 ~ scenario) +
  temp +
  scale_fill_manual(
    values = c("-15 to 0" = "#b10026", "0 to 15" = "#e5f5f9",
               "15 to 30" = "#99d8c9", "30 to 45" = "#2ca25f"),
    na.value = "grey50", na.translate = FALSE,
    name = "Yield change (%)") +
  map_theme

ggsave("figures/ExtraFig2-yield-change-spatial-map.tiff", plot = plot4,
       width = 15, height = 15, units = "cm", dpi = 600, compression = "lzw", bg = "white")


## ── ExtraFig 3: Seed-filling duration distribution ─────────────────────── ----
## Ridgeline density by scenario and CO2 level

p7_data <- simulated0 %>%
  mutate(seed_fill_dur = MaturityDAS - SeedFillingDAS,
         co2 = as.factor(co2))

plot7 <- p7_data %>%
  ggplot(aes(x = seed_fill_dur, y = scenario, colour = co2, fill = co2)) +
  geom_density_ridges(scale = 0.9, alpha = 0.3, linewidth = 0.8) +
  temp +
  labs(x = "Seed-filling duration (days)", y = element_blank(),
       colour = "CO2 (ppm)", fill = "CO2 (ppm)") +
  scale_colour_manual(values = c("350" = "#4dac26", "540" = "#d01c8b")) +
  scale_fill_manual(values   = c("350" = "#4dac26", "540" = "#d01c8b")) +
  scale_y_discrete(expand = expansion(mult = c(0, 0.4))) +
  coord_cartesian(clip = "off") +
  theme(legend.position = "top")

ggsave("figures/ExtraFig3-seed-filling-duration.tiff", plot = plot7,
       width = 18, height = 14, units = "cm", dpi = 600, compression = "lzw", bg = "white")


## ── ExtraFig 4: Phenology timeline by stage and scenario ─────────────────────────── ----
## Boxplot of emergence, flowering, seed-fill start, maturity (DAS)

p8_data <- simulated0 %>%
  select(scenario, co2, EmergenceDAS, FloweringDAS, SeedFillingDAS, MaturityDAS) %>%
  pivot_longer(cols = c(EmergenceDAS, FloweringDAS, SeedFillingDAS, MaturityDAS),
               names_to = "DAS_variable", values_to = "DAS") %>%
  mutate(DAS_variable = factor(DAS_variable,
    levels = c("EmergenceDAS", "FloweringDAS", "SeedFillingDAS", "MaturityDAS")))

plot8 <- p8_data %>%
  ggplot(aes(x = DAS, y = scenario, fill = scenario)) +
  geom_boxplot(alpha = 0.6, outlier.size = 0.3, width = 0.5) +
  facet_wrap(~ DAS_variable, scales = "free_x") +
  temp +
  labs(x = "Days after sowing", y = element_blank()) +
  theme(legend.position = "none", axis.text.y = element_text(size = 10))

ggsave("figures/ExtraFig4-phenology-timeline-by-stage.tiff", plot = plot8,
       width = 22, height = 14, units = "cm", dpi = 600, compression = "lzw", bg = "white")


## ── ExtraFig 5: Water-use efficiency spatial maps ──────────────────────── ----
## One TIFF per variable; CO2 = 350 only; faceted by scenario

save_wue_map <- function(var, label, df_stars, counties) {
  p <- ggplot() +
    geom_sf(data = ark, fill = "grey30", color = "black") +
    geom_stars(data = df_stars, aes(fill = .data[[var]])) +
    geom_sf(data = counties, fill = NA, colour = "black", linewidth = 0.5) +
    coord_sf(xlim = c(360000, 570000)) +
    facet_wrap(~ scenario, nrow = 1) +
    temp +
    scale_fill_viridis_c(option = "viridis", na.value = "transparent", name = label) +
    theme(axis.title = element_blank(), axis.text = element_blank(),
          legend.position = "left", legend.direction = "vertical",
          legend.title = element_text(size = 12))
  ggsave(paste0("figures/ExtraFig5-water-use-efficiency-", var, ".tiff"), plot = p,
         width = 30, height = 10, units = "cm", dpi = 600,
         compression = "lzw", bg = "white")
  invisible(p)
}

p10_data <- simulated0 %>%
  filter(co2 == 350) %>%
  group_by(x, y, scenario) %>%
  summarise(
    Yield_kgha  = mean(Yield_kgha,  na.rm = TRUE),
    SeasonMeanT = mean(SeasonMeanT, na.rm = TRUE),
    Crop_ET     = mean(Crop_ET,     na.rm = TRUE),
    WDrainage   = mean(WDrainage,   na.rm = TRUE),
    WRunoff     = mean(WRunoff,     na.rm = TRUE),
    sWUE        = mean(sWUE,        na.rm = TRUE),
    SeasonRain  = mean(SeasonRain,  na.rm = TRUE),
    swhc_60cm   = mean(swhc_24in,   na.rm = TRUE) * 2.54,
    .groups     = "drop")

df_wue <- to_stars_grid(p10_data, dims = c("x", "y", "scenario"))

wue_vars <- list(
  list(var = "sWUE",        label = "sWUE"),
  list(var = "Yield_kgha",  label = "Yield (kg/ha)"),
  list(var = "SeasonMeanT", label = "Season Mean T (°C)"),
  list(var = "Crop_ET",     label = "Crop ET (mm)"),
  list(var = "WDrainage",   label = "Drainage (mm)"),
  list(var = "WRunoff",     label = "Runoff (mm)"),
  list(var = "swhc_60cm",   label = "SWHC 60cm (cm)"),
  list(var = "SeasonRain",  label = "Season Rain (mm)")
)

for (wv in wue_vars) save_wue_map(wv$var, wv$label, df_wue, usa_counties)
