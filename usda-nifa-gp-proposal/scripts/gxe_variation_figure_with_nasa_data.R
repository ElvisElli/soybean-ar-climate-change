## ============================================================
## GxE Figure: Response curves + Real NASA POWER environmental data
## Uses 16-year averaged (2010-2025) daily temperatures
## Shows: APSIM response functions with observed real-world variation
## ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(patchwork)
})

## ── APSIM Response Functions ─────────────────────────────────

# Thermal time response (f(T))
thermal_response <- function(temp) {
  Tb <- 10      # base temperature (°C)
  Topt <- 30    # optimum temperature (°C)
  Tmax <- 40    # maximum temperature (°C)

  if_else(
    temp < Tb | temp > Tmax, 0,
    if_else(
      temp <= Topt,
      (temp - Tb) / (Topt - Tb),
      (Tmax - temp) / (Tmax - Topt)
    )
  )
}

# Photoperiod response (f(PP))
photoperiod_response <- function(pp_hours) {
  PPcrit1 <- 13.09  # critical photoperiod 1 (hours)
  PPcrit2 <- 16.45  # critical photoperiod 2 (hours)

  if_else(
    pp_hours <= PPcrit1, 1.0,
    if_else(
      pp_hours >= PPcrit2, 0.0,
      1.0 - (pp_hours - PPcrit1) / (PPcrit2 - PPcrit1)
    )
  )
}

## ── Load NASA POWER averaged data ────────────────────────────
data_file <- "nasa_power_2010_2025_daily_averages.csv"

if (!file.exists(data_file)) {
  stop(paste("Data file not found:", data_file, "\n",
             "Run: Rscript fetch_nasa_power_2010_2025.R"))
}

nasa_data <- read.csv(data_file, stringsAsFactors = FALSE) %>%
  as_tibble() %>%
  mutate(
    temp_mean = as.numeric(temp_mean),
    photoperiod_hours = as.numeric(photoperiod_hours),
    sowing = if_else(grepl("Early", scenario), "Early", "Late")
  )

cat("✓ Loaded", nrow(nasa_data), "daily observations from NASA POWER (2010-2025 average)\n")
cat("✓ Scenarios:", n_distinct(nasa_data$scenario), "\n")
cat("✓ Temperature range:", round(min(nasa_data$temp_mean, na.rm=T), 1),
    "–", round(max(nasa_data$temp_mean, na.rm=T), 1), "°C\n")
cat("✓ Photoperiod range:", round(min(nasa_data$photoperiod_hours, na.rm=T), 1),
    "–", round(max(nasa_data$photoperiod_hours, na.rm=T), 1), "hours\n\n")

## ── Prepare scatter data for panels ──────────────────────────
scatter_data <- nasa_data %>%
  mutate(
    y_temp = -0.05,
    y_pp = -0.05
  )

## ── Panel 1: Temperature response curve with real data scatter ──────
response_curves <- tibble(
  temp = seq(0, 50, 0.5),
  f_thermal = sapply(temp, thermal_response)
) %>%
  ggplot(aes(x = temp, y = f_thermal)) +
  geom_line(colour = "#e74c3c", linewidth = 1.2) +
  geom_jitter(data = scatter_data, aes(x = temp_mean, y = y_temp, colour = sowing),
              width = 0.5, height = 0.08, alpha = 0.6, size = 1.5) +
  scale_colour_manual(name = NULL, values = c("Early" = "#2ecc71", "Late" = "#e74c3c"),
                      labels = c("Early" = "Early sowing", "Late" = "Late sowing")) +
  scale_x_continuous(name = "Temperature (°C)", limits = c(0, 50),
                     labels = function(x) paste0(x, "°")) +
  scale_y_continuous(name = "f(T)", limits = c(-0.15, 1.2)) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "grey90"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 9),
        axis.title = element_text(size = 10))

## ── Panel 2: Photoperiod response curve with real data scatter ──────
pp_curves <- tibble(
  pp = seq(10, 18, 0.1),
  f_pp = sapply(pp, photoperiod_response)
) %>%
  ggplot(aes(x = pp, y = f_pp)) +
  geom_line(colour = "#3498db", linewidth = 1.2) +
  geom_jitter(data = scatter_data, aes(x = photoperiod_hours, y = y_pp, colour = sowing),
              width = 0.15, height = 0.08, alpha = 0.6, size = 1.5) +
  scale_colour_manual(name = NULL, values = c("Early" = "#2ecc71", "Late" = "#e74c3c"),
                      labels = c("Early" = "Early sowing", "Late" = "Late sowing")) +
  scale_x_continuous(name = "Photoperiod (h)", limits = c(10, 18)) +
  scale_y_continuous(name = "f(PP)", limits = c(-0.15, 1.2)) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "grey90"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 9),
        axis.title = element_text(size = 10))

## ── Combine panels ──────────────────────────────────────────────
figure <- (response_curves | pp_curves) +
  plot_layout(ncol = 2, widths = c(1, 1), guides = "collect") &
  theme(legend.position = "bottom",
        legend.text = element_text(size = 9),
        legend.margin = margin(t = 5, b = 0))

## ── Save figure ──────────────────────────────────────────────
dir.create("figures", showWarnings = FALSE)
ggsave("figures/gxe_environmental_variation_nasa_2010_2025.tiff",
       plot = figure,
       width = 16, height = 6, units = "cm",
       dpi = 300, compression = "lzw", bg = "white")

cat("✓ Figure saved: figures/gxe_environmental_variation_nasa_2010_2025.tiff\n\n")

## ── Summary statistics ──────────────────────────────────────
cat("── 16-Year (2010-2025) Environmental Statistics ──\n\n")

summary_stats <- nasa_data %>%
  group_by(scenario) %>%
  summarise(
    temp_mean = round(mean(temp_mean), 1),
    temp_range = paste0(round(min(temp_mean), 1), "–", round(max(temp_mean), 1)),
    pp_range = paste0(round(min(photoperiod_hours), 1), "–", round(max(photoperiod_hours), 1)),
    .groups = "drop"
  )

print(summary_stats)

cat("\n── Data source ──\n")
cat("NASA POWER Daily Data\n")
cat("Period: 2010-2025 (16 years), daily averages\n")
cat("Temporal: 130-day growing seasons starting Apr 15 (Early) and Jun 15 (Late)\n")
cat("Spatial: Fayetteville, AR (36.1°N) and Columbia, MO (38.3°N)\n")
cat("Variable: T2M (Mean temperature at 2m height)\n")
cat("Resolution: 4 km × 4 km grid cells\n")
