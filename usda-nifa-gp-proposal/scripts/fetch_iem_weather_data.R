## ============================================================
## Fetch real weather data from NASA POWER via apsimx package
## Fallback to synthetic data if NASA POWER unavailable
## Robust approach based on apsim-arkansas-grid/scripts/01_download_met_soil.R
## ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(apsimx)
  library(lubridate)
})

## ── Trial locations ──────────────────────────────────────────
locations <- tribble(
  ~location, ~lat, ~lon, ~state,
  "Fayetteville, AR", 36.0726, -94.1729, "Arkansas",
  "Columbia, MO", 38.2526, -92.2734, "Missouri"
)

sowing_dates <- list(
  "Early (Apr 15)" = 105,
  "Late (Jun 15)" = 166
)

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

## ── Fetch NASA POWER data via apsimx ────────────────────────
fetch_nasapower_data <- function(lonlat, dates, location_name) {
  cat(sprintf("  Attempting NASA POWER (via apsimx) for %s...\n", location_name))

  tryCatch({
    # Get NASA POWER data using apsimx's robust wrapper
    power_data <- apsimx::get_power_apsim_met(
      lonlat = lonlat,
      dates = dates
    )

    if (is.null(power_data) || nrow(power_data) == 0) {
      cat("    ✗ NASA POWER returned empty data\n")
      return(NULL)
    }

    cat(sprintf("    ✓ NASA POWER download successful (%d days)\n", nrow(power_data)))
    return(power_data)
  }, error = function(e) {
    cat(sprintf("    ✗ NASA POWER error: %s\n", conditionMessage(e)))
    return(NULL)
  })
}

## ── Fallback: Generate synthetic data ────────────────────────
generate_synthetic_data <- function(lonlat, location_name, lat_deg) {
  cat(sprintf("  Generating synthetic climate data for %s...\n", location_name))

  set.seed(42)

  # Regional climate parameters
  if (grepl("Fayetteville", location_name)) {
    seasonal_mean <- 24.8
    amplitude <- 8.5
    noise_sd <- 2.5
  } else {
    seasonal_mean <- 23.2
    amplitude <- 9.0
    noise_sd <- 2.8
  }

  all_scenario_data <- NULL

  for (sow_idx in seq_along(sowing_dates)) {
    sow_name <- names(sowing_dates)[sow_idx]
    sow_doy <- sowing_dates[[sow_idx]]

    # Create 130-day growing season
    start_date <- as.Date(paste0("2023-", sow_doy), "%Y-%j")
    date_seq <- seq(start_date, by = "1 day", length.out = 130)

    scenario_data <- tibble(
      date = date_seq,
      year = year(date_seq),
      day = yday(date_seq),
      radn = 20,  # Average radiation (MJ/m²/day)
      maxt = NA_real_,
      mint = NA_real_,
      rain = 0
    )

    # Generate temperatures
    for (i in seq_len(nrow(scenario_data))) {
      doy <- scenario_data$day[i]
      season_phase <- pmin(pmax((doy - 105) / 180 * pi, 0), pi)
      seasonal_t <- seasonal_mean + amplitude * sin(season_phase)
      daily_noise <- rnorm(1, 0, noise_sd)
      mean_t <- seasonal_t + daily_noise

      scenario_data$maxt[i] <- mean_t + 4.5
      scenario_data$mint[i] <- mean_t - 4.5
    }

    scenario_data <- scenario_data %>%
      mutate(
        location = location_name,
        scenario = paste0(location_name, " - ", sow_name),
        sowing_date = sow_name,
        sowing_doy = sow_doy,
        source = "synthetic"
      )

    all_scenario_data <- bind_rows(all_scenario_data, scenario_data)
  }

  cat(sprintf("    ✓ Generated %d days of synthetic data\n", nrow(all_scenario_data)))
  return(all_scenario_data)
}

## ── Main workflow ────────────────────────────────────────────
cat("========================================================================\n")
cat("Fetching weather data: NASA POWER (primary) → Synthetic fallback\n")
cat("========================================================================\n\n")

all_data <- NULL

for (loc_idx in seq_len(nrow(locations))) {
  loc <- locations[loc_idx, ]
  cat(sprintf("%s (%.2f°N, %.2f°W)\n", loc$location, loc$lat, loc$lon))

  lonlat <- c(loc$lon, loc$lat)
  dates <- c("2010-01-01", "2024-12-31")

  # Try NASA POWER first (via apsimx)
  power_data <- fetch_nasapower_data(lonlat, dates, loc$location)

  if (!is.null(power_data) && nrow(power_data) > 0) {
    # NASA POWER successful - process for each scenario
    for (sow_idx in seq_along(sowing_dates)) {
      sow_name <- names(sowing_dates)[sow_idx]
      sow_doy <- sowing_dates[[sow_idx]]

      # Extract 130-day growing season
      scenario_data <- power_data %>%
        as_tibble() %>%
        mutate(
          day = yday(as.Date(YYYYMMDD, "%Y%m%d")),
          year = year(as.Date(YYYYMMDD, "%Y%m%d")),
          date = as.Date(YYYYMMDD, "%Y%m%d")
        ) %>%
        filter((day >= sow_doy & day <= sow_doy + 129) |
               (sow_doy + 129 > 365 & (day >= sow_doy | day <= sow_doy + 129 - 365))) %>%
        mutate(
          location = loc$location,
          scenario = paste0(loc$location, " - ", sow_name),
          sowing_date = sow_name,
          sowing_doy = sow_doy,
          source = "NASA POWER",
          maxt = T2MX,
          mint = T2MN,
          rain = PREC,
          radn = ALLSKY_SFC_SW_DWN / 100  # Convert to MJ/m²/day
        ) %>%
        select(date, year, day, radn, maxt, mint, rain, location, scenario,
               sowing_date, sowing_doy, source)

      if (nrow(scenario_data) > 0) {
        all_data <- bind_rows(all_data, scenario_data)
      }
    }
    cat("  ✓ NASA POWER data processed\n\n")
  } else {
    # NASA POWER failed - use synthetic fallback
    synthetic <- generate_synthetic_data(lonlat, loc$location, loc$lat)
    all_data <- bind_rows(all_data, synthetic)
    cat("\n")
  }
}

## ── Add photoperiod and compute statistics ────────────────────
cat("Computing photoperiod...\n")

result_data <- all_data %>%
  left_join(
    locations %>% select(location, lat),
    by = "location"
  ) %>%
  mutate(
    photoperiod_hours = calc_photoperiod(day, lat),
    temp_mean = (maxt + mint) / 2
  ) %>%
  select(location, scenario, sowing_date, sowing_doy, date, year, day,
         temp_mean, maxt, mint, rain, radn, photoperiod_hours, source)

## ── Save results ──────────────────────────────────────────────
output_file <- "weather_data_nasapower_2010_2024.csv"
write.csv(result_data, output_file, row.names = FALSE)

cat("\n========================================================================\n")
cat(sprintf("✓ Data saved to: %s\n", output_file))
cat(sprintf("✓ Total records: %d\n", nrow(result_data)))
cat(sprintf("✓ Date range: %s to %s\n",
            format(min(result_data$date, na.rm = TRUE), "%Y-%m-%d"),
            format(max(result_data$date, na.rm = TRUE), "%Y-%m-%d")))

# Summary statistics
cat("\n── Temperature Summary (°C) ──\n")
temp_summary <- result_data %>%
  group_by(scenario, source) %>%
  summarise(
    temp_mean = round(mean(temp_mean, na.rm = TRUE), 1),
    temp_range = paste0(round(min(temp_mean, na.rm = TRUE), 1), "–",
                        round(max(temp_mean, na.rm = TRUE), 1)),
    .groups = "drop"
  )
print(temp_summary)

cat("\n– Photoperiod Summary (hours) –\n")
pp_summary <- result_data %>%
  group_by(scenario, source) %>%
  summarise(
    pp_range = paste0(round(min(photoperiod_hours, na.rm = TRUE), 1), "–",
                      round(max(photoperiod_hours, na.rm = TRUE), 1)),
    .groups = "drop"
  )
print(pp_summary)

cat("\n– Data Sources –\n")
source_count <- table(result_data$source)
print(source_count)

cat("\n========================================================================\n")
cat("Next: Run gxe_variation_figure_with_real_weather.R\n")
