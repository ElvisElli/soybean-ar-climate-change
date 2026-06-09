rm(list = ls())

library(ggplot2)
library(sf)
library(viridis)
library(stars)
library(dplyr)
library(tidyr)
library(readr)
library(data.table)
library(lubridate)
library(lme4)
library(emmeans)
library(ggridges)
library(cowplot)
library(gghalves)
library(scales)

dir.create("figures", showWarnings = FALSE, recursive = TRUE)
dir.create("reports", showWarnings = FALSE, recursive = TRUE)

source("code/utils/plot-theme.R")

## load data ====

simulated0 <- readRDS("data/outputs/simulated-scenarios-df.rds") %>%
  as_tibble() %>%
  rename(any_of(c(date = "Date")))

## spatial layers ====

ark <- st_read("data/raw/cropland/cb_2018_us_state_20m/cb_2018_us_state_20m.shp")
ark <- subset(ark, STUSPS == "AR")
ark <- st_transform(ark, 5070)

usa_counties <- st_read("data/raw/cropland/Elvis-Crop-Data/Arkansas_Counties_4269.shp")

xlim_ar <- c(360000, 570000)

## dataset summary stats ====

n_cells    <- n_distinct(simulated0$cellid)
n_years    <- n_distinct(simulated0$date)
n_scenarios <- n_distinct(simulated0$scenario)
n_rows     <- nrow(simulated0)
date_range <- paste(min(year(simulated0$date)), max(year(simulated0$date)), sep = "-")

summary_text <- paste0(
  "Inspection Report — Soybean AR Climate Change\n",
  "Generated: ", Sys.Date(), "\n\n",
  "Dataset summary\n",
  "  Rows:      ", format(n_rows, big.mark = ","), "\n",
  "  Cells:     ", format(n_cells, big.mark = ","), "\n",
  "  Years:     ", n_years, "  (", date_range, ")\n",
  "  Scenarios: ", n_scenarios
)

title_page <- ggdraw() +
  draw_label(summary_text, fontfamily = "mono", size = 14, hjust = 0, x = 0.05, y = 0.6)

## ── PAPER FIGURES ─────────────────────────────────────────────────────────────

##p1 - climate change without adaptation ====

p1 <- simulated0 %>%
  filter(scenario %in% c("baseline", "climate_change")) %>%
  group_by(x, y, scenario, co2) %>%
  summarise(Yield_kgha = mean(Yield_kgha, na.rm = TRUE)) %>%
  mutate(
    scenario_co2 = paste0(scenario, co2),
    scenario_co2 = factor(scenario_co2,
      levels = c("baseline350", "climate_change350", "climate_change540"),
      labels = c("Baseline", "2°C-increase", "2°C-increase with CO2")
    ),
    yield_bin = cut(
      Yield_kgha,
      breaks = c(-Inf, 2500, 3500, 4500, 5500, Inf),
      labels = c("<2500", "2500-3500", "3500-4500", "4500-5500", ">5500"),
      include.lowest = TRUE
    )
  ) %>%
  ungroup() %>%
  select(-scenario, -co2)

plot1a <- p1 %>%
  mutate(scenario_co2 = factor(scenario_co2,
    labels = c("Baseline", "2°C-increase", "2°C-increase\nwith elevated CO2")
  )) %>%
  ggplot(aes(x = Yield_kgha, y = scenario_co2, colour = scenario_co2)) +
  geom_density_ridges(scale = .9, fill = NA, linewidth = 1) +
  temp +
  labs(x = "Yield (kg/ha)", y = element_blank()) +
  scale_colour_manual(values = c("black", "#bd0026", "#1a9850")) +
  theme(legend.position = "none") +
  scale_y_discrete(expand = expansion(mult = c(0, 0.4)))

df_p1 <- st_as_stars(p1, dims = c("x", "y", "scenario_co2"), xy = c("x", "y"), proxy = TRUE)
st_crs(df_p1) <- "epsg:4326"
df_p1 <- st_transform(df_p1, 5070)
df2   <- st_as_stars(st_bbox(ark), dx = 2500, dy = 2500)
df_p1 <- st_warp(df_p1, df2, no_data_value = NA)

plot1b <- ggplot() +
  geom_sf(data = ark, fill = "grey30", color = "black") +
  geom_stars(data = df_p1, aes(fill = yield_bin)) +
  geom_sf(data = usa_counties, aes(geometry = geometry), fill = NA, colour = "black", linewidth = 0.5) +
  coord_sf(xlim = xlim_ar) +
  facet_wrap(~scenario_co2, nrow = 1) +
  temp +
  scale_fill_manual(
    values = c("<2500" = "#bd0026", "2500-3500" = "#e31a1c", "3500-4500" = "#fc8d59",
               "4500-5500" = "#91cf60", ">5500" = "#1a9850", "NA" = "grey50"),
    na.value = "grey50", na.translate = FALSE, name = "Yield (kg/ha)"
  ) +
  theme(axis.title = element_blank(), axis.text = element_blank(),
        legend.position = "left", legend.direction = "vertical",
        legend.title = element_text(size = 12)) +
  labs(fill = "Yield (kg/ha)")

plot1 <- plot_grid(plot1a, plot1b, ncol = 1, align = "v", axis = "r",
                   labels = "AUTO", rel_heights = c(0.4, 1))

##temp.plot - temperature impacts ====

weather.char <- simulated0 %>%
  filter(scenario %in% c("climate_change"), co2 %in% c("350")) %>%
  mutate(year = year(date)) %>%
  ungroup() %>%
  mutate(
    Rain.avg     = mean(SeasonRain),
    Meat.avg     = mean(SeasonMeanT),
    Rain.sd      = sd(SeasonRain),
    Meat.sd      = sd(SeasonMeanT),
    Normal       = ifelse(SeasonRain < Rain.avg + Rain.sd,
                     ifelse(SeasonRain > Rain.avg - Rain.sd,
                       ifelse(SeasonMeanT < Meat.avg + Meat.sd,
                         ifelse(SeasonMeanT > Meat.avg - Meat.sd, "Yes", "No"),
                       "No"), "No"), "No"),
    Rain.class    = ifelse(SeasonRain > Rain.avg, "Wet", "Dry"),
    Meat_class    = ifelse(SeasonMeanT > Meat.avg, "Warm", "Cool"),
    weather.class = ifelse(Normal == "Yes", "Normal", paste0(Meat_class, "/", Rain.class)),
    weather.class = factor(weather.class,
      levels = c("Cool/Wet", "Warm/Wet", "Normal", "Cool/Dry", "Warm/Dry"))
  ) %>%
  select(x, y, year, weather.class, Meat.avg, Rain.avg, Meat.sd, Rain.sd,
         Yield_kgha, SeasonMeanT, SeasonRain)

lm.normal      <- lm(Yield_kgha ~ SeasonMeanT, data = weather.char)
slope.overall  <- coef(lm.normal)["SeasonMeanT"]
lm.warm.dry    <- lm(Yield_kgha ~ SeasonMeanT,
                     data = weather.char %>% filter(weather.class %in% c("Warm/Dry")))
slope.warm.dry <- coef(lm.warm.dry)["SeasonMeanT"]

min_y  <- min(weather.char$y)
mean_y <- mean(weather.char$y)
max_y  <- max(weather.char$y)

temp.plot <- weather.char %>%
  filter(weather.class %in% c("Cool/Wet", "Normal", "Warm/Dry")) %>%
  ggplot(aes(x = SeasonMeanT, y = Yield_kgha)) +
  geom_point(aes(colour = y), size = 2, alpha = 0.3) +
  temp +
  geom_smooth(method = "lm", se = FALSE, colour = "black", size = 0.8) +
  geom_smooth(data = weather.char %>% filter(weather.class %in% c("Warm/Dry")),
              aes(group = weather.class), size = 1.2,
              method = "lm", se = FALSE, colour = "#008837") +
  labs(x = "Season temperature (°C)", y = "Seed yield (kg/ha)") +
  scale_y_continuous(limits = c(1000, 6300), breaks = c(1000, 2000, 3000, 4000, 5000, 6000)) +
  scale_x_continuous(limits = c(21, 31), breaks = c(21, 23, 25, 27, 29, 31)) +
  scale_color_gradientn(
    colors = c("#a50f15", "#fb6a4a", "white"),
    values = scales::rescale(c(min_y, mean_y, max_y)),
    breaks = c(min_y, mean_y, max_y),
    labels = c("33.0", "35.0", "36.5"),
    limits = c(min_y, max_y),
    guide  = guide_colorbar(barwidth = 1.5, barheight = 5,
                            frame.colour = "black", frame.linewidth = 1),
    name   = "Latitude"
  ) +
  theme(legend.title = element_text(size = 12), legend.text = element_text(size = 12)) +
  annotate("segment", x = 22.5, xend = 22.8, y = 5400, yend = 4650,
           arrow = arrow(length = unit(0.25, "cm")), color = "black", size = 0.5) +
  annotate("text", x = 24.2, y = 5500,
           label = paste0("Overall = ", round(slope.overall, 0), " kg/ha/°C"), size = 4) +
  annotate("segment", x = 29, xend = 29, y = 1600, yend = 3600,
           arrow = arrow(length = unit(0.25, "cm")), color = "black", size = 0.5) +
  annotate("text", x = 27.4, y = 1500,
           label = paste0("Warm & Dry = ", round(slope.warm.dry, 0), " kg/ha/°C"),
           size = 4, colour = "#008837") +
  guides(linetype = "none")

##plot3.1 and plot4 - adaptation ====

baseline_yld <- simulated0 %>%
  filter(scenario %in% c("baseline")) %>%
  rename(baseline = Yield_kgha) %>%
  select(x, y, date, baseline)

p3 <- simulated0 %>%
  filter(!scenario %in% c("baseline")) %>%
  select(x, y, scenario, co2, date, Yield_kgha) %>%
  pivot_wider(values_from = Yield_kgha, names_from = scenario) %>%
  left_join(baseline_yld, by = c("x", "y", "date")) %>%
  rename(time = date) %>%
  mutate(
    climate_change          = (climate_change - baseline) / baseline * 100,
    early_sowing            = (early_sowing - baseline) / baseline * 100,
    longer_mat              = (longer_mat - baseline) / baseline * 100,
    early_sowing_longer_mat = (early_sowing_longer_mat - baseline) / baseline * 100
  ) %>%
  select(-baseline) %>%
  pivot_longer(values_to = "Yield_kgha", names_to = "scenario", !c(x, y, time, co2)) %>%
  mutate(
    scenario = factor(scenario,
      levels = c("climate_change", "longer_mat", "early_sowing", "early_sowing_longer_mat"),
      labels = c("2°C-increase", "Late-Maturing", "Early Sowing", "Late-Maturing & Early Sowing")
    )
  )

plot3.1 <-
  ggplot(p3, aes(x = factor(scenario), y = Yield_kgha,
                 fill = as.factor(co2), colour = as.factor(co2))) +
  geom_abline(intercept = 0, slope = 0, linetype = "dashed") +
  geom_half_violin(side = "l", trim = FALSE, nudge = 0.03, alpha = 0.4,
                   width = 1, colour = NA, size = 0.5,
                   position = position_dodge(width = 0.6)) +
  geom_half_boxplot(side = "r", position = position_dodge(width = 0.6),
                    outlier.shape = NA, width = 0.6, color = "black", size = 0.5) +
  temp +
  scale_colour_manual(values = c("#4dac26", "#d01c8b"),
                      label  = c("Without elevated CO2", "With elevated CO2")) +
  scale_fill_manual(values   = c("#4dac26", "#d01c8b"),
                    label    = c("Without elevated CO2", "With elevated CO2")) +
  labs(x = element_blank(), y = "Yield change (%)") +
  theme(legend.position = "top", axis.text.x = element_text(angle = 30, hjust = 1)) +
  scale_y_continuous(breaks = c(-20, -10, 0, 10, 20, 30, 40), limits = c(-20, 40))

p3.2 <- p3 %>%
  mutate(scenario = factor(scenario,
    labels = c("2°C-increase", "Late-Maturing", "Early Sowing", "LM & ES")
  )) %>%
  group_by(x, y, scenario, co2) %>%
  summarise(Yield_kgha = round(mean(Yield_kgha, na.rm = TRUE), 1)) %>%
  mutate(
    yield_bin = cut(Yield_kgha,
      breaks = c(-15, 0, 15, 30, 45),
      labels = c("-15-0", "0-15", "15-30", "30-45"),
      include.lowest = TRUE),
    co2 = factor(as.factor(co2), levels = c("350", "540"),
                 labels = c("Without elevated CO2", "With elevated CO2"))
  )

df_p3 <- st_as_stars(p3.2, dims = c("x", "y", "scenario", "co2"), xy = c("x", "y"), proxy = TRUE)
st_crs(df_p3) <- "epsg:4326"
df_p3  <- st_transform(df_p3, 5070)
df_p3  <- st_warp(df_p3, df2, no_data_value = NA)

plot4 <-
  ggplot() +
  geom_sf(data = ark, fill = "grey50", color = "black") +
  geom_stars(data = df_p3, aes(fill = yield_bin)) +
  geom_sf(data = usa_counties, aes(geometry = geometry), fill = NA, colour = "black", linewidth = 0.5) +
  coord_sf(xlim = xlim_ar) +
  facet_grid(co2 ~ scenario) +
  temp +
  scale_fill_manual(
    values = c("-15-0" = "#b10026", "0-15" = "#e5f5f9",
               "15-30" = "#99d8c9", "30-45" = "#2ca25f", "NA" = "grey50"),
    na.value = "grey50", na.translate = FALSE, name = "Yield Change (%)"
  ) +
  theme(axis.title = element_blank(), axis.text = element_blank(),
        legend.position = "top", legend.direction = "horizontal",
        legend.title = element_text(size = 12))

##plot5a/5b - weather characterization ====

p5a0 <- simulated0 %>%
  filter(scenario %in% c("baseline"), co2 %in% c("350")) %>%
  mutate(year = year(date)) %>%
  group_by(x, y, scenario, co2, year) %>%
  ungroup() %>%
  mutate(
    Rain.avg     = mean(SeasonRain),
    Meat.avg     = mean(SeasonMeanT),
    Rain.sd      = sd(SeasonRain),
    Meat.sd      = sd(SeasonMeanT),
    Normal       = ifelse(SeasonRain < Rain.avg + Rain.sd * 0.5,
                     ifelse(SeasonRain > Rain.avg - Rain.sd * 0.5,
                       ifelse(SeasonMeanT < Meat.avg + Meat.sd * 0.5,
                         ifelse(SeasonMeanT > Meat.avg - Meat.sd * 0.5, "Yes", "No"),
                       "No"), "No"), "No"),
    Rain.class    = ifelse(SeasonRain > Rain.avg, "Wet", "Dry"),
    Meat_class    = ifelse(SeasonMeanT > Meat.avg, "Warm", "Cool"),
    weather.class = ifelse(Normal == "Yes", "Normal", paste0(Meat_class, "/", Rain.class)),
    weather.class = factor(weather.class,
      levels = c("Cool/Wet", "Warm/Wet", "Normal", "Cool/Dry", "Warm/Dry"))
  ) %>%
  select(x, y, year, weather.class, Meat.avg, Rain.avg, Meat.sd, Rain.sd)

p5a <- simulated0 %>%
  filter(scenario %in% c("baseline"), co2 %in% c("350")) %>%
  mutate(year = year(date)) %>%
  select(x, y, year, SeasonMeanT, SeasonRain) %>%
  left_join(p5a0) %>%
  filter(SeasonMeanT > 20)

temp_response <- data.frame(temp = seq(10, 40, 0.1)) %>%
  mutate(
    response = case_when(
      temp <= 10              ~ 0,
      temp > 10 & temp <= 30 ~ (temp - 10) / (30 - 10),
      temp > 30 & temp <= 40 ~ 1 - (temp - 30) / (40 - 30),
      temp > 40              ~ 0
    ),
    response_scaled = response * (1400 - 300) + 300
  )

plot5a <-
  p5a %>%
  ggplot(aes(x = SeasonMeanT, y = SeasonRain, fill = weather.class)) +
  geom_point(size = 3, alpha = 0.2, shape = 21) +
  temp +
  theme(legend.position = "top") +
  labs(x = "Season temperature (°C)", y = "Season Rainfall (mm)") +
  geom_vline(aes(xintercept = Meat.avg), size = 0.5) +
  geom_hline(aes(yintercept = Rain.avg), size = 0.5) +
  geom_rect(aes(
    xmin = Meat.avg - Meat.sd * 0.5, xmax = Meat.avg + Meat.sd * 0.5,
    ymin = Rain.avg - Rain.sd * 0.5, ymax = Rain.avg + Rain.sd * 0.5
  ), color = "black", fill = NA, linetype = "dashed") +
  geom_line(data = temp_response, aes(x = temp, y = response_scaled),
            inherit.aes = FALSE, color = "black", linewidth = 1.5) +
  annotate("segment", x = 35, xend = 32.5, y = 1330, yend = 1150,
           arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
           linewidth = 0.2, color = "black") +
  annotate("text", x = 35.2, y = 1350,
           label = "APSIM phenology\nresponse curve\n(0-1)", hjust = 0, vjust = 1, size = 3.5) +
  scale_y_continuous(limits = c(300, 1400),
    sec.axis = sec_axis(~ (. - 300) / 1100,
      name = "Phenology Temperature Response (0-1)", breaks = seq(0, 1, 0.2))) +
  scale_x_continuous(limits = c(10, 40)) +
  scale_fill_manual(values = c("Warm/Dry" = "#bd0026", "Warm/Wet" = "#fc8d59",
                                "Cool/Dry" = "#91cf60", "Cool/Wet" = "#1a9850",
                                "Normal"   = "#d95f0e")) +
  guides(fill = guide_legend(override.aes = list(alpha = 1, size = 4, shape = 21)))

p5b_data <- simulated0 %>%
  filter(scenario %in% c("baseline", "climate_change"), co2 %in% c("350")) %>%
  mutate(year = year(date)) %>%
  select(x, y, scenario, co2, year, EmergenceDAS, FloweringDAS, MaturityDAS) %>%
  mutate(veg.per = FloweringDAS - EmergenceDAS,
         rep.per = MaturityDAS - FloweringDAS,
         whole.cycle = MaturityDAS - EmergenceDAS) %>%
  ungroup() %>%
  left_join(p5a0) %>%
  filter(!is.na(weather.class)) %>%
  mutate(scenario = factor(scenario, labels = c("Baseline", "2°C-increase")))

plot5b <-
  p5b_data %>%
  ggplot(aes(x = veg.per, y = scenario)) +
  geom_boxplot(aes(fill = scenario), alpha = 0.5, width = 0.4) +
  temp +
  labs(x = "Calendar days to flowering", y = element_blank()) +
  facet_wrap(~weather.class, nrow = 1) +
  scale_fill_manual(values = c("#1a9850", "#bd0026")) +
  theme(legend.position = "top", legend.direction = "horizontal",
        axis.text.y = element_blank(), panel.spacing = unit(0.5, "cm"))

plot5_panel <- plot_grid(plot5a, plot5b, ncol = 1, align = "v", axis = "r",
                         labels = "AUTO", rel_heights = c(1, 0.6))

## ── INSPECTION / REVIEWER FIGURES ─────────────────────────────────────────────

##insp_a - Seed-filling duration ====

insp_a_data <- simulated0 %>%
  mutate(seed_fill_dur = MaturityDAS - SeedFillingDAS,
         co2 = as.factor(co2))

insp_a <- insp_a_data %>%
  ggplot(aes(x = seed_fill_dur, y = scenario, colour = co2, fill = co2)) +
  geom_density_ridges(scale = 0.9, alpha = 0.3, linewidth = 0.8) +
  temp +
  labs(title = "Seed-filling duration by scenario",
       x = "Seed-filling duration (days after sowing)", y = element_blank(),
       colour = "CO2 (ppm)", fill = "CO2 (ppm)") +
  scale_colour_manual(values = c("350" = "#4dac26", "540" = "#d01c8b")) +
  scale_fill_manual(values   = c("350" = "#4dac26", "540" = "#d01c8b")) +
  scale_y_discrete(expand = expansion(mult = c(0, 0.4))) +
  theme(legend.position = "top")

ggsave("figures/insp_a - seed filling duration.tiff", plot = insp_a,
       width = 18, height = 14, units = "cm", dpi = 300, compression = "lzw", bg = "white")

##insp_b - Phenology timeline ====

insp_b_data <- simulated0 %>%
  select(scenario, co2, EmergenceDAS, FloweringDAS, SeedFillingDAS, MaturityDAS) %>%
  pivot_longer(cols = c(EmergenceDAS, FloweringDAS, SeedFillingDAS, MaturityDAS),
               names_to = "DAS_variable", values_to = "DAS") %>%
  mutate(DAS_variable = factor(DAS_variable,
    levels = c("EmergenceDAS", "FloweringDAS", "SeedFillingDAS", "MaturityDAS")),
    co2 = as.factor(co2)) %>%
  group_by(scenario, co2, DAS_variable) %>%
  summarise(
    mean_DAS = mean(DAS, na.rm = TRUE),
    sd_DAS   = sd(DAS,   na.rm = TRUE),
    .groups  = "drop"
  )

insp_b <- insp_b_data %>%
  ggplot(aes(x = mean_DAS, y = scenario, colour = co2)) +
  geom_pointrange(aes(xmin = mean_DAS - sd_DAS, xmax = mean_DAS + sd_DAS),
                  position = position_dodge(width = 0.5), size = 0.6) +
  facet_wrap(~DAS_variable, scales = "free_x") +
  temp +
  labs(title = "Phenology timeline by scenario",
       x = "Days after sowing (mean +/- SD)", y = element_blank(),
       colour = "CO2 (ppm)") +
  scale_colour_manual(values = c("350" = "#4dac26", "540" = "#d01c8b")) +
  theme(legend.position = "top")

ggsave("figures/insp_b - phenology timeline.tiff", plot = insp_b,
       width = 22, height = 14, units = "cm", dpi = 300, compression = "lzw", bg = "white")

##insp_c - Year-to-year yield variability (CV) ====

insp_c_data <- simulated0 %>%
  group_by(cellid, scenario, co2) %>%
  summarise(
    cv_yield = sd(Yield_kgha, na.rm = TRUE) / mean(Yield_kgha, na.rm = TRUE) * 100,
    .groups  = "drop"
  ) %>%
  mutate(co2 = as.factor(co2))

insp_c <- insp_c_data %>%
  ggplot(aes(x = scenario, y = cv_yield, fill = co2)) +
  geom_boxplot(alpha = 0.6, outlier.size = 0.3, position = position_dodge(width = 0.7)) +
  temp +
  labs(title = "Year-to-year yield variability (CV) by scenario",
       x = element_blank(), y = "Coefficient of variation (%)", fill = "CO2 (ppm)") +
  scale_fill_manual(values = c("350" = "#4dac26", "540" = "#d01c8b")) +
  theme(legend.position = "top", axis.text.x = element_text(angle = 30, hjust = 1))

ggsave("figures/insp_c - yield CV by scenario.tiff", plot = insp_c,
       width = 16, height = 14, units = "cm", dpi = 300, compression = "lzw", bg = "white")

##insp_d - Spatial CV map (baseline vs climate_change) ====

insp_d_data <- simulated0 %>%
  filter(scenario %in% c("baseline", "climate_change"), co2 == "350") %>%
  group_by(x, y, scenario) %>%
  summarise(
    cv_yield = sd(Yield_kgha, na.rm = TRUE) / mean(Yield_kgha, na.rm = TRUE) * 100,
    .groups  = "drop"
  )

df_cv <- st_as_stars(insp_d_data, dims = c("x", "y", "scenario"), xy = c("x", "y"), proxy = TRUE)
st_crs(df_cv) <- "epsg:4326"
df_cv  <- st_transform(df_cv, 5070)
df_cv  <- st_warp(df_cv, df2, no_data_value = NA)

insp_d <- ggplot() +
  geom_sf(data = ark, fill = "grey30", color = "black") +
  geom_stars(data = df_cv, aes(fill = cv_yield)) +
  geom_sf(data = usa_counties, aes(geometry = geometry), fill = NA, colour = "black", linewidth = 0.5) +
  coord_sf(xlim = xlim_ar) +
  facet_wrap(~scenario) +
  temp +
  scale_fill_viridis_c(option = "magma", na.value = "transparent", name = "CV (%)") +
  theme(axis.title = element_blank(), axis.text = element_blank(),
        legend.position = "left", legend.title = element_text(size = 12)) +
  labs(title = "Spatial yield CV: baseline vs climate change")

ggsave("figures/insp_d - spatial CV map.tiff", plot = insp_d,
       width = 16, height = 10, units = "cm", dpi = 300, compression = "lzw", bg = "white")

##insp_e - Water balance summary ====

insp_e_data <- simulated0 %>%
  filter(co2 == "350") %>%
  group_by(scenario) %>%
  summarise(
    Crop_ET   = mean(Crop_ET,   na.rm = TRUE),
    WDrainage = mean(WDrainage, na.rm = TRUE),
    WRunoff   = mean(WRunoff,   na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  pivot_longer(cols = c(Crop_ET, WDrainage, WRunoff),
               names_to = "component", values_to = "mm")

insp_e <- insp_e_data %>%
  ggplot(aes(x = scenario, y = mm, fill = component)) +
  geom_col(position = "stack", alpha = 0.85) +
  temp +
  labs(title = "Water balance by scenario (CO2 = 350 ppm)",
       x = element_blank(), y = "Water (mm)", fill = "Component") +
  scale_fill_manual(values = c("Crop_ET" = "#2166ac", "WDrainage" = "#92c5de",
                                "WRunoff" = "#d1e5f0")) +
  theme(legend.position = "top", axis.text.x = element_text(angle = 30, hjust = 1))

ggsave("figures/insp_e - water balance summary.tiff", plot = insp_e,
       width = 16, height = 12, units = "cm", dpi = 300, compression = "lzw", bg = "white")

##insp_f - Yield vs radiation ====

insp_f_data <- simulated0 %>%
  filter(co2 == "350") %>%
  group_by(cellid, scenario) %>%
  summarise(
    mean_radn  = mean(SeasonRadn,  na.rm = TRUE),
    mean_yield = mean(Yield_kgha,  na.rm = TRUE),
    .groups    = "drop"
  )

insp_f <- insp_f_data %>%
  ggplot(aes(x = mean_radn, y = mean_yield, colour = scenario)) +
  geom_point(alpha = 0.2, size = 1) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
  temp +
  labs(title = "Yield vs seasonal radiation by scenario (CO2 = 350 ppm)",
       x = "Season radiation (MJ/m2)", y = "Yield (kg/ha)", colour = "Scenario") +
  theme(legend.position = "top")

ggsave("figures/insp_f - yield vs radiation.tiff", plot = insp_f,
       width = 16, height = 12, units = "cm", dpi = 300, compression = "lzw", bg = "white")

##insp_g - Blooming temperature ====

insp_g <- simulated0 %>%
  ggplot(aes(x = scenario, y = BloomingSeasonMaxt, fill = scenario)) +
  geom_boxplot(alpha = 0.6, outlier.size = 0.3) +
  temp +
  labs(title = "Blooming-period maximum temperature by scenario",
       x = element_blank(), y = "Blooming season max temperature (°C)") +
  theme(legend.position = "none", axis.text.x = element_text(angle = 30, hjust = 1))

ggsave("figures/insp_g - blooming temperature.tiff", plot = insp_g,
       width = 16, height = 12, units = "cm", dpi = 300, compression = "lzw", bg = "white")

##insp_h - Radiation use efficiency proxy ====

insp_h_data <- simulated0 %>%
  filter(co2 == "350")

insp_h <- insp_h_data %>%
  ggplot(aes(x = CumRadiationInterceptionOnGreen, y = Yield_kgha, colour = scenario)) +
  geom_point(alpha = 0.1, size = 0.8) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
  temp +
  labs(title = "RUE proxy: cumulative radiation interception vs yield (CO2 = 350 ppm)",
       x = "Cumulative radiation interception on green (MJ/m2)",
       y = "Yield (kg/ha)", colour = "Scenario") +
  theme(legend.position = "top")

ggsave("figures/insp_h - RUE proxy.tiff", plot = insp_h,
       width = 18, height = 12, units = "cm", dpi = 300, compression = "lzw", bg = "white")

## ── ASSEMBLE PDF ──────────────────────────────────────────────────────────────

pdf("reports/inspection-report.pdf", width = 14, height = 10)

print(title_page)

print(plot1)

print(temp.plot)

print(plot3.1)

print(plot4)

print(plot5_panel)

print(insp_a)

print(insp_b)

print(insp_c)

print(insp_d)

print(insp_e)

print(insp_f)

print(insp_g)

print(insp_h)

dev.off()

message("Inspection report written to reports/inspection-report.pdf")
