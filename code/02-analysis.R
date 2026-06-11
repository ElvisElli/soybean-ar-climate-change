## ============================================================
## 02-analysis.R  —  Manuscript figures & supplementary plots
## Run from repo root:  source("code/02-analysis.R")
## ============================================================

## ── 0. Setup ────────────────────────────────────────────────────────────────

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
simulated0 <- readRDS("data/outputs/simulated-scenarios-df.rds") %>%
  as_tibble() %>%
  rename(any_of(c(date = "Date")))

## ── 0b. Data inspection & quality filter ────────────────────────────────────
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


## ── Figure 1: Climate change without adaptation ──────────────────────── ----
## Ridgeline distribution (A) + spatial yield map (B)
## Scenarios: Baseline / +2°C / +2°C with elevated CO2

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

ggsave("figures/fig01 - climate change without adaptation.tiff", plot = plot1,
       width = 20, height = 15, units = "cm", dpi = 600, compression = "lzw", bg = "white")


## ── Figure 2: Temperature-yield relationship ─────────────────────────── ----
## Yield vs season temperature under +2°C, colored by latitude
## Separate regression lines for all data and Warm/Dry site-years

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

ggsave("figures/fig02 - temperature impacts.tiff", plot = plot2,
       width = 15, height = 12, units = "cm", dpi = 600, compression = "lzw", bg = "white")


## ── Figure 3: Adaptation strategies — violin/boxplot summary ─────────── ----
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

ggsave("figures/fig03 - adaptation strategies summary.tiff", plot = plot3,
       width = 15, height = 15, units = "cm", dpi = 600, compression = "lzw", bg = "white")


## ── Figure 4: Adaptation strategies — spatial yield change map ───────── ----
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

ggsave("figures/fig04 - adaptation strategies map.tiff", plot = plot4,
       width = 15, height = 15, units = "cm", dpi = 600, compression = "lzw", bg = "white")


## ── Figure 5: Adaptation strategies — merged (violin + map) ──────────── ----
## Combined manuscript figure (replicates Figures 3 and 4 inline)

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

ggsave("figures/fig05 - adaptation strategies merged.tiff", plot = plot5_merged,
       width = 30, height = 15, units = "cm", dpi = 600, compression = "lzw", bg = "white")


## ── Figure 6: Environmental characterization ─────────────────────────── ----
## A: Weather space (temperature vs rainfall) with APSIM phenology response curve
## B: Days to flowering by weather class, baseline vs +2°C

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
    temp <= 10              ~ 0,
    temp <= 30              ~ (temp - 10) / 20,
    temp <= 40              ~ 1 - (temp - 30) / 10,
    TRUE                    ~ 0),
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

ggsave("figures/fig06 - environmental characterization.tiff", plot = plot6,
       width = 20, height = 15, units = "cm", dpi = 600, compression = "lzw", bg = "white")


## ── Figure 7: Seed-filling duration distribution ─────────────────────── ----
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

ggsave("figures/fig07 - seed filling duration.tiff", plot = plot7,
       width = 18, height = 14, units = "cm", dpi = 600, compression = "lzw", bg = "white")


## ── Figure 8: Phenology timeline by scenario ─────────────────────────── ----
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

ggsave("figures/fig08 - phenology timeline by scenario.tiff", plot = plot8,
       width = 22, height = 14, units = "cm", dpi = 600, compression = "lzw", bg = "white")


## ── Figure 9: Phenology change maps (without elevated CO2) ───────────── ----
## A (upper): total crop cycle change (days vs baseline)
## B (lower): seed-filling period change (days vs baseline)
## Filtered to CO2 = 350 ppm only

p9_baseline <- simulated0 %>%
  filter(scenario == "baseline") %>%
  mutate(bl_sf = MaturityDAS - SeedFillingDAS,
         bl_tc = MaturityDAS - EmergenceDAS) %>%   # emergence → maturity
  select(x, y, date, bl_sf, bl_tc)

p9_data <- simulated0 %>%
  filter(scenario != "baseline", co2 == 350) %>%
  mutate(sf_dur = MaturityDAS - SeedFillingDAS,
         tc_dur = MaturityDAS - EmergenceDAS) %>%   # emergence → maturity
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

## ── Stats for manuscript paragraph ──────────────────────────────────────────
## "The total crop cycle decreased on average by xx days, with values ranging
##  from xx to xx across locations (Figure 2). The shortening of the crop cycle
##  was observed in xx of the 4,651 fields. Seed-filling period decreased in
##  northern environments..."

p9_cc <- p9_data %>%
  filter(scenario == "climate_change") %>%
  group_by(x, y) %>%
  summarise(
    tc_chg_mean = mean(tc_chg, na.rm = TRUE),
    sf_chg_mean = mean(sf_chg, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n── Figure 9 manuscript stats ──────────────────────────────\n")
cat(sprintf("Total crop cycle change  : mean = %.1f days\n",
            mean(p9_cc$tc_chg_mean, na.rm = TRUE)))
cat(sprintf("  Range across locations : %.1f to %.1f days\n",
            min(p9_cc$tc_chg_mean, na.rm = TRUE),
            max(p9_cc$tc_chg_mean, na.rm = TRUE)))
cat(sprintf("  Fields with shorter cycle (<0 days): %d of 4,651 (%.1f%%)\n",
            sum(p9_cc$tc_chg_mean < 0, na.rm = TRUE),
            mean(p9_cc$tc_chg_mean < 0, na.rm = TRUE) * 100))

cat(sprintf("Seed-filling period change: mean = %.1f days\n",
            mean(p9_cc$sf_chg_mean, na.rm = TRUE)))
cat(sprintf("  Range across locations : %.1f to %.1f days\n",
            min(p9_cc$sf_chg_mean, na.rm = TRUE),
            max(p9_cc$sf_chg_mean, na.rm = TRUE)))
cat(sprintf("  Fields with shorter seed-fill (<0 days): %d of 4,651 (%.1f%%)\n",
            sum(p9_cc$sf_chg_mean < 0, na.rm = TRUE),
            mean(p9_cc$sf_chg_mean < 0, na.rm = TRUE) * 100))

## Seed-fill change by latitude tercile (north / central / south)
lat_breaks <- quantile(p9_cc$y, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
p9_cc <- p9_cc %>%
  mutate(lat_zone = cut(y, breaks = lat_breaks, include.lowest = TRUE,
                        labels = c("South", "Central", "North")))
cat("\nSeed-filling change by latitude zone:\n")
p9_cc %>%
  group_by(lat_zone) %>%
  summarise(
    n          = n(),
    mean_sf_chg = round(mean(sf_chg_mean, na.rm = TRUE), 1),
    pct_negative = round(mean(sf_chg_mean < 0, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  ) %>%
  print()
cat("───────────────────────────────────────────────────────────\n\n")

## Panel A — total cycle change: one negative bin "-8 to 0" mirrors the first
## positive bin "0 to 8" for visual consistency
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

ggsave("figures/fig09 - phenology change maps.tiff", plot = plot9,
       width = 28, height = 18, units = "cm", dpi = 600, compression = "lzw", bg = "white")


## ── Figure 10: Water-use efficiency spatial maps ──────────────────────── ----
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
  ggsave(paste0("figures/fig10 - water use efficiency - ", var, ".tiff"), plot = p,
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
