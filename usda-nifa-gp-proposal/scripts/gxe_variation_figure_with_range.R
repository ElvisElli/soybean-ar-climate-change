## ============================================================
## GxE Figure: Response curves + Environmental range with scatter
## Shows: mean (line) + range (band) + scatter (below axis)
## ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(patchwork)
})

## ── APSIM Response Functions ─────────────────────────────────
thermal_response <- function(temp) {
  Tb <- 10; Topt <- 30; Tmax <- 40
  if_else(temp < Tb | temp > Tmax, 0,
    if_else(temp <= Topt, (temp - Tb) / (Topt - Tb),
            (Tmax - temp) / (Tmax - Topt)))
}

photoperiod_response <- function(pp_hours) {
  PPcrit1 <- 13.09; PPcrit2 <- 16.45
  if_else(pp_hours <= PPcrit1, 1.0,
    if_else(pp_hours >= PPcrit2, 0.0,
            1.0 - (pp_hours - PPcrit1) / (PPcrit2 - PPcrit1)))
}

## ── Load weather data ────────────────────────────────────────
data_file <- "temperature_and_photoperiod_data.csv"
if (!file.exists(data_file)) {
  stop("Data file not found: ", data_file)
}

weather_data <- read.csv(data_file) %>%
  as_tibble() %>%
  mutate(
    temp_mean = as.numeric(temp_mean),
    photoperiod_hours = as.numeric(photoperiod_hours)
  )

cat("✓ Loaded", nrow(weather_data), "daily observations\n")

## ── Panel 1: APSIM response curves ──────────────────────────
# Prepare scatter data for top panels (positioned at bottom)
scatter_top <- weather_data %>%
  mutate(
    y_temp = -0.05,
    y_pp = -0.05,
    sowing = if_else(grepl("Early", scenario), "Early", "Late")
  )

response_curves <- tibble(
  temp = seq(0, 50, 0.5),
  f_thermal = sapply(temp, thermal_response)
) %>%
  ggplot(aes(x = temp, y = f_thermal)) +
  geom_line(colour = "#e74c3c", linewidth = 1.2) +
  geom_jitter(data = scatter_top, aes(x = temp_mean, y = y_temp, colour = sowing),
              width = 0.5, height = 0.08, alpha = 0.6, size = 1.5) +
  scale_colour_manual(name = NULL, values = c("Early" = "#2ecc71", "Late" = "#e74c3c"), labels = c("Early" = "Early sowing", "Late" = "Late sowing")) +
  scale_x_continuous(name = "Temperature (°C)", limits = c(0, 50),
                     labels = function(x) paste0(x, "°")) +
  scale_y_continuous(name = "f(T)", limits = c(-0.15, 1.2)) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "grey90"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 9),
        axis.title = element_text(size = 10))

pp_curves <- tibble(
  pp = seq(10, 18, 0.1),
  f_pp = sapply(pp, photoperiod_response)
) %>%
  ggplot(aes(x = pp, y = f_pp)) +
  geom_line(colour = "#3498db", linewidth = 1.2) +
  geom_jitter(data = scatter_top, aes(x = photoperiod_hours, y = y_pp, colour = sowing),
              width = 0.15, height = 0.08, alpha = 0.6, size = 1.5) +
  scale_colour_manual(name = NULL, values = c("Early" = "#2ecc71", "Late" = "#e74c3c"), labels = c("Early" = "Early sowing", "Late" = "Late sowing")) +
  scale_x_continuous(name = "Photoperiod (h)", limits = c(10, 18)) +
  scale_y_continuous(name = "f(PP)", limits = c(-0.15, 1.2)) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "grey90"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 9),
        axis.title = element_text(size = 10))

## ── Combine panels ───────────────────────────────────────────
figure <- (response_curves | pp_curves) +
  plot_layout(ncol = 2, widths = c(1, 1), guides = "collect") &
  theme(legend.position = "bottom",
        legend.text = element_text(size = 9),
        legend.margin = margin(t = 5, b = 0))

## ── Save figure ────────────────────────────────────────────
dir.create("figures", showWarnings = FALSE)
ggsave("figures/gxe_environmental_variation_real_data.tiff",
       plot = figure,
       width = 16, height = 6, units = "cm",
       dpi = 300, compression = "lzw", bg = "white")

cat("✓ Figure saved: figures/gxe_environmental_variation_real_data.tiff\n")

## ── Summary ──────────────────────────────────────────────────
cat("\nEnvironmental Statistics:\n")
cat("Temperature: ", round(min(weather_data$temp_mean), 1), "–",
    round(max(weather_data$temp_mean), 1), "°C\n", sep = "")
cat("Photoperiod: ", round(min(weather_data$photoperiod_hours), 1), "–",
    round(max(weather_data$photoperiod_hours), 1), "hours\n", sep = "")
cat("Total daily observations: ", nrow(weather_data), "\n", sep = "")
