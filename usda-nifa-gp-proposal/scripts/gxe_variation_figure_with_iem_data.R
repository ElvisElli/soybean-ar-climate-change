## ============================================================
## GxE Figure: Response curves + NASA POWER real weather data
## Uses NASA POWER temperature data via apsimx package
## ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(patchwork)
})

## ── APSIM Response Functions ─────────────────────────────────

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

## ── Load NASA POWER weather data ────────────────────────────
data_file <- "weather_data_nasapower_2010_2024.csv"

if (!file.exists(data_file)) {
  stop(paste("Data file not found:", data_file, "\n",
             "Run: Rscript scripts/fetch_iem_weather_data.R"))
}

weather_data <- read.csv(data_file, stringsAsFactors = FALSE) %>%
  as_tibble() %>%
  mutate(
    temp_mean = as.numeric(temp_mean),
    photoperiod_hours = as.numeric(photoperiod_hours),
    sowing = if_else(grepl("Early", scenario), "Early", "Late")
  )

cat("✓ Loaded", nrow(weather_data), "daily observations\n")
cat("✓ Scenarios:", n_distinct(weather_data$scenario), "\n")
cat("✓ Data sources:", paste(unique(weather_data$source), collapse = ", "), "\n")
cat("✓ Temperature range:", round(min(weather_data$temp_mean, na.rm=T), 1),
    "–", round(max(weather_data$temp_mean, na.rm=T), 1), "°C\n")
cat("✓ Photoperiod range:", round(min(weather_data$photoperiod_hours, na.rm=T), 1),
    "–", round(max(weather_data$photoperiod_hours, na.rm=T), 1), "hours\n\n")

## ── Prepare scatter data for panels ──────────────────────────
scatter_data <- weather_data %>%
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
ggsave("figures/gxe_environmental_variation_nasapower.tiff",
       plot = figure,
       width = 16, height = 6, units = "cm",
       dpi = 300, compression = "lzw", bg = "white")

cat("✓ Figure saved: figures/gxe_environmental_variation_nasapower.tiff\n\n")

## ── Summary statistics ──────────────────────────────────────
cat("── Environmental Statistics (NASA POWER Data 2010-2024) ──\n\n")

summary_stats <- weather_data %>%
  group_by(scenario, source) %>%
  summarise(
    temp_mean = round(mean(temp_mean, na.rm = TRUE), 1),
    temp_range = paste0(round(min(temp_mean, na.rm = TRUE), 1), "–",
                        round(max(temp_mean, na.rm = TRUE), 1)),
    pp_range = paste0(round(min(photoperiod_hours, na.rm = TRUE), 1), "–",
                      round(max(photoperiod_hours, na.rm = TRUE), 1)),
    .groups = "drop"
  ) %>%
  arrange(scenario)

print(summary_stats)

cat("\n── Data Source Note ──\n")
cat("Temperature data: NASA POWER (Prediction Of Worldwide Energy Resources)\n")
cat("                  via apsimx::get_power_apsim_met()\n")
cat("Photoperiod: Calculated from latitude and day-of-year (Spencer 1971)\n")
cat("Time period: 2010-2024 (14-15 years per location/scenario)\n")
cat("Fallback: If NASA POWER unavailable, uses synthetic climate-realistic data\n")
