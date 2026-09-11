#!/usr/bin/env Rscript
## ============================================================
## Generate climate-realistic daily temperature and photoperiod data
## for soybean trial locations (replacement for Python script)
## ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
})

## ── Photoperiod calculation (Spencer 1971) ──────────────────
calc_photoperiod <- function(doy, latitude_deg) {
  latitude_rad <- latitude_deg * pi / 180
  b <- (doy - 1) * 2 * pi / 365
  decl <- 0.006918 - 0.399912 * cos(b) + 0.070257 * sin(b) -
          0.006758 * cos(2*b) + 0.000907 * sin(2*b) -
          0.00002 * cos(3*b) + 0.00029 * sin(3*b)
  cos_h <- -tan(latitude_rad) * tan(decl)
  cos_h <- pmax(-1, pmin(1, cos_h))
  h <- acos(cos_h)
  24 * h / pi
}

## ── Regional climate parameters ──────────────────────────────
locations <- tribble(
  ~location, ~lat, ~lon,
  "Fayetteville, AR", 36.0726, -94.1729,
  "Columbia, MO", 38.2526, -92.2734
)

location_params <- list(
  "Fayetteville, AR" = list(
    seasonal_temp_mean = 24.8,   # May-Aug average
    seasonal_temp_amplitude = 8.5,
    noise_std = 2.5
  ),
  "Columbia, MO" = list(
    seasonal_temp_mean = 23.2,   # Cooler than AR
    seasonal_temp_amplitude = 9.0,
    noise_std = 2.8
  )
)

sowing_dates <- list(
  "Early (Apr 15)" = 105,
  "Late (Jun 15)" = 166
)

## ── Parameters ──────────────────────────────────────────────
set.seed(42)
growing_season_days <- 130

cat("Generating realistic daily temperature and photoperiod data...\n")
cat("========================================================================\n\n")

all_data <- NULL

for (loc_idx in seq_len(nrow(locations))) {
  loc <- locations[loc_idx, ]
  params <- location_params[[loc$location]]

  for (sow_idx in seq_along(sowing_dates)) {
    sow_name <- names(sowing_dates)[sow_idx]
    sow_doy <- sowing_dates[[sow_idx]]

    cat(sprintf("%s - %s\n", loc$location, sow_name))

    # Create sequence of dates
    start_date <- as.Date(paste0("2023-", sow_doy), "%Y-%j")
    date_seq <- seq(start_date, by = "1 day", length.out = growing_season_days)

    scenario_data <- tibble(
      day_in_season = seq_len(growing_season_days),
      calendar_date = date_seq,
      calendar_doy = lubridate::yday(date_seq),
      location = loc$location,
      latitude = loc$lat,
      sowing_date = sow_name,
      sowing_doy = sow_doy,
      scenario = paste0(loc$location, " - ", sow_name)
    ) %>%
      mutate(
        # Temperature model: seasonal curve + daily noise
        season_phase = pmin(pmax((calendar_doy - 105) / 180 * pi, 0), pi),
        seasonal_mean = params$seasonal_temp_mean +
                       params$seasonal_temp_amplitude * sin(season_phase),
        daily_noise = rnorm(n(), mean = 0, sd = params$noise_std),
        temp_mean = seasonal_mean + daily_noise,
        # Diurnal temperature range ~9°C
        temp_min = temp_mean - 4.5,
        temp_max = temp_mean + 4.5,
        # Calculate photoperiod
        photoperiod_hours = calc_photoperiod(calendar_doy, latitude),
        # Round for output
        temp_mean = round(temp_mean, 1),
        temp_min = round(temp_min, 1),
        temp_max = round(temp_max, 1),
        photoperiod_hours = round(photoperiod_hours, 2)
      ) %>%
      select(location, latitude, sowing_date, sowing_doy,
             calendar_date, calendar_doy, day_in_season,
             temp_mean, temp_min, temp_max, photoperiod_hours, scenario)

    all_data <- bind_rows(all_data, scenario_data)
    cat(sprintf("  ✓ Generated %d days of data\n\n", nrow(scenario_data)))
  }
}

## ── Save to CSV ──────────────────────────────────────────────
output_file <- "temperature_and_photoperiod_data.csv"
write.csv(all_data, output_file, row.names = FALSE)

cat("========================================================================\n")
cat(sprintf("✓ Data saved to: %s\n", output_file))
cat(sprintf("✓ Total records: %d\n", nrow(all_data)))

## ── Summary statistics ──────────────────────────────────────
cat("\nTemperature range across all scenarios:\n")
temps <- all_data$temp_mean
cat(sprintf("  Min: %.1f°C\n", min(temps, na.rm = TRUE)))
cat(sprintf("  Max: %.1f°C\n", max(temps, na.rm = TRUE)))
cat(sprintf("  Mean: %.1f°C\n\n", mean(temps, na.rm = TRUE)))

cat("Photoperiod range across all scenarios:\n")
pps <- all_data$photoperiod_hours
cat(sprintf("  Min: %.1f hours\n", min(pps, na.rm = TRUE)))
cat(sprintf("  Max: %.1f hours\n\n", max(pps, na.rm = TRUE)))

# By scenario
cat("Temperature summary by scenario:\n")
summary_by_scenario <- all_data %>%
  group_by(scenario) %>%
  summarise(
    temp_mean = round(mean(temp_mean, na.rm = TRUE), 1),
    temp_range = paste0(round(min(temp_mean, na.rm = TRUE), 1), "–",
                        round(max(temp_mean, na.rm = TRUE), 1)),
    .groups = "drop"
  )
print(summary_by_scenario)

cat("\nPhotoperiod summary by scenario:\n")
summary_by_pp <- all_data %>%
  group_by(scenario) %>%
  summarise(
    pp_range = paste0(round(min(photoperiod_hours, na.rm = TRUE), 1), "–",
                      round(max(photoperiod_hours, na.rm = TRUE), 1)),
    .groups = "drop"
  )
print(summary_by_pp)

cat("\n========================================================================\n")
cat("NOTE: This is climate-realistic simulated data for visualization.\n")
cat("      For actual NASA POWER data, use: fetch_nasa_power_2010_2025.R\n")
