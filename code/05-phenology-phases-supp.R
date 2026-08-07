## ============================================================
## 05-phenology-phases-supp.R  —  Supplementary phenology-phase figure
## Run from repo root:  source("code/05-phenology-phases-supp.R")
##
## Addresses reviewer request (revision round 2): "Presenting the
## duration of the vegetative and reproductive phases (or their relative
## changes) would provide a clearer mechanistic explanation of the
## observed yield responses."
##
## Companion to MAIN Fig 2 (crop cycle + seed-filling maps in
## 02-analysis.R). Same data, cleaning, grid, theme, scenarios and
## map style — but panels for the VEGETATIVE and REPRODUCTIVE phases:
##
##   Panel A → vegetative phase change   (emergence -> R1 flowering)
##   Panel B → reproductive phase change (R1 flowering -> R7 maturity)
##
## Phase durations are read from APSIM's days-after-sowing stage outputs:
##   vegetative   = FloweringDAS   - EmergenceDAS
##   reproductive = MaturityDAS    - FloweringDAS
## (seed-filling = MaturityDAS - SeedFillingDAS and total cycle =
##  MaturityDAS - EmergenceDAS are shown in the main Fig 2.)
## CO2 = 350 ppm only; changes are relative to the baseline, matching Fig 2.
## ============================================================

## ── 0. Setup (mirrors 02-analysis.R) ─────────────────────────────────────────
suppressPackageStartupMessages({
  library(ggplot2)
  library(sf)
  library(viridis)
  library(stars)
  library(dplyr)
  library(cowplot)
  library(lubridate)
})

dir.create("figures", showWarnings = FALSE, recursive = TRUE)
source("code/utils/plot-theme.R")

## ── 1. Load and quality-filter simulation results (same as 02-analysis.R) ────
simulated0 <- readRDS("data/outputs/simulated-scenarios-df.rds")
if (is.list(simulated0) && !is.data.frame(simulated0))
  simulated0 <- dplyr::bind_rows(Filter(Negate(is.null), simulated0))
simulated0 <- as_tibble(simulated0) %>%
  rename(any_of(c(date = "Date")))

## Identical biological-plausibility filter to the one used for all figures.
simulated0 <- simulated0 %>%
  filter(!is.na(MaturityDAS),   MaturityDAS   > 0,
         !is.na(EmergenceDAS),  EmergenceDAS  > 0,
         !is.na(FloweringDAS),  FloweringDAS  > 0,
         !is.na(SeedFillingDAS), SeedFillingDAS > 0,
         MaturityDAS - EmergenceDAS >= 60,
         FloweringDAS  < MaturityDAS,
         SeedFillingDAS < MaturityDAS,
         !is.na(Yield_kgha), Yield_kgha > 0)

## ── 2. Spatial layers & shared grid (same as 02-analysis.R) ──────────────────
ark <- st_read("data/raw/cropland/cb_2018_us_state_20m/cb_2018_us_state_20m.shp",
               quiet = TRUE) %>%
  subset(STUSPS == "AR") %>%
  st_transform(5070)
usa_counties <- st_read(
  "data/raw/cropland/Elvis-Crop-Data/Arkansas_Counties_4269.shp", quiet = TRUE)
df2 <- st_as_stars(st_bbox(ark), dx = 2500, dy = 2500)

## Shared scenario order/labels (same as 02-analysis.R)
scenario_levels <- c("climate_change", "longer_mat", "early_sowing",
                     "early_sowing_longer_mat")
scenario_labels <- c("2°C-increase", "Late-Maturing", "Early Sowing", "LM & ES")

## Shared helpers (identical to 02-analysis.R)
to_stars_grid <- function(df, dims) {
  s <- st_as_stars(df, dims = dims, xy = c("x", "y"), proxy = TRUE)
  st_crs(s) <- "epsg:4326"
  s <- st_transform(s, 5070)
  st_warp(s, df2, no_data_value = NA)
}
map_theme <- theme(
  axis.title       = element_blank(),
  axis.text        = element_blank(),
  legend.position  = "top",
  legend.direction = "horizontal",
  legend.title     = element_text(size = 11, hjust = 0),
  strip.text       = element_text(size = 10)
)

## ── 3. Vegetative & reproductive phase-change vs baseline (CO2 = 350) ─────────
## Baseline phase durations per field-year.
ph_baseline <- simulated0 %>%
  filter(scenario == "baseline") %>%
  mutate(bl_veg = FloweringDAS - EmergenceDAS,
         bl_rep = MaturityDAS  - FloweringDAS) %>%
  select(x, y, date, bl_veg, bl_rep)

## Adaptive/warming scenarios, joined to baseline, change = scenario - baseline.
ph_data <- simulated0 %>%
  filter(scenario != "baseline", co2 == 350) %>%
  mutate(veg_dur = FloweringDAS - EmergenceDAS,
         rep_dur = MaturityDAS  - FloweringDAS) %>%
  select(x, y, date, scenario, veg_dur, rep_dur) %>%
  left_join(ph_baseline, by = c("x", "y", "date")) %>%
  filter(!is.na(bl_veg)) %>%
  mutate(veg_chg = veg_dur - bl_veg,
         rep_chg = rep_dur - bl_rep)

## ── 4. Map builder (identical structure to make_pheno_map in 02-analysis.R) ───
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

## Diverging red->green palette (red = shortening, green = lengthening),
## matching the main Fig 2 convention.
div_cols5 <- c("#b10026", "#e5f5f9", "#99d8c9", "#41ae76", "#005824")

## Panel A — vegetative phase change (emergence -> R1)
plotA <- make_pheno_map(ph_data, "veg_chg",
  breaks = c(-10, 0, 4, 8, 12, 25),
  labels = c("-10 to 0", "0 to 4", "4 to 8", "8 to 12", "> 12"),
  colors = setNames(div_cols5, c("-10 to 0", "0 to 4", "4 to 8", "8 to 12", "> 12")),
  legend_title = "(A)  Vegetative phase change (days)")

## Panel B — reproductive phase change (R1 -> R7)
plotB <- make_pheno_map(ph_data, "rep_chg",
  breaks = c(-10, 0, 5, 10, 15, 30),
  labels = c("-10 to 0", "0 to 5", "5 to 10", "10 to 15", "> 15"),
  colors = setNames(div_cols5, c("-10 to 0", "0 to 5", "5 to 10", "10 to 15", "> 15")),
  legend_title = "(B)  Reproductive phase change (days)")

supp_fig <- plot_grid(plotA, plotB, ncol = 1, align = "v", axis = "lr")

ggsave("figures/FigS-vegetative-reproductive-change-maps.tiff", plot = supp_fig,
       width = 18, height = 18, units = "cm", dpi = 600, compression = "lzw", bg = "white")
## PNG copy for quick viewing / sharing.
ggsave("figures/FigS-vegetative-reproductive-change-maps.png", plot = supp_fig,
       width = 18, height = 18, units = "cm", dpi = 300, bg = "white")

## ── 5. Supporting statistics (mirrors STATS 2 in 02-analysis.R) ──────────────
{
  cat(paste(rep("=", 70), collapse = ""), "\n")
  cat("Vegetative & reproductive phase change (days) vs baseline, CO2=350\n")
  cat(paste(rep("-", 70), collapse = ""), "\n")

  ph_sy <- ph_data %>%
    mutate(scenario = factor(scenario, levels = scenario_levels, labels = scenario_labels))

  ## Across sites: 40-yr mean per field (= what the maps show).
  ph_sites <- ph_sy %>%
    group_by(x, y, scenario) %>%
    summarise(veg_chg = mean(veg_chg, na.rm = TRUE),
              rep_chg = mean(rep_chg, na.rm = TRUE), .groups = "drop")

  ph_smry <- function(df, var) {
    df %>% group_by(scenario) %>%
      summarise(mean   = round(mean(.data[[var]],   na.rm = TRUE), 1),
                median = round(median(.data[[var]], na.rm = TRUE), 1),
                q1     = round(quantile(.data[[var]], .25, na.rm = TRUE), 1),
                q3     = round(quantile(.data[[var]], .75, na.rm = TRUE), 1),
                pct_pos = round(mean(.data[[var]] > 0, na.rm = TRUE) * 100, 1),
                .groups = "drop")
  }

  cat("\n-- ACROSS SITES (40-yr mean per field — what the maps show) --\n")
  cat("Vegetative phase change vs baseline (days):\n");   print(ph_smry(ph_sites, "veg_chg"))
  cat("\nReproductive phase change vs baseline (days):\n"); print(ph_smry(ph_sites, "rep_chg"))

  cat("\n-- ACROSS SITE-YEARS (all fields x all years — full variability) --\n")
  cat("Vegetative phase change vs baseline (days):\n");   print(ph_smry(ph_sy, "veg_chg"))
  cat("\nReproductive phase change vs baseline (days):\n"); print(ph_smry(ph_sy, "rep_chg"))
  cat(paste(rep("=", 70), collapse = ""), "\n")
}

cat("\n[done] wrote figures/FigS-vegetative-reproductive-change-maps.{tiff,png}\n")
