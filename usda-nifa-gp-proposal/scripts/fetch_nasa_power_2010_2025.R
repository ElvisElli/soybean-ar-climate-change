## ============================================================
## Fetch NASA POWER Temperature Data 2010-2025
## Average daily temperatures across 16 years per scenario
## For each scenario: 4 sowing dates × 130-day growing seasons
## ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
})

# Install nasapower if needed
if (!require(nasapower, quietly = TRUE)) {
  cat("Installing nasapower package...\n")
  install.packages("nasapower", repos = "https://cloud.r-project.org")
  library(nasapower)
}

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

## ── Trial locations ────────────────────────────────────────
locations <- tribble(
  ~location, ~lat, ~lon, ~state,
  "Fayetteville, AR", 36.0726, -94.1729, "Arkansas",
  "Columbia, MO", 38.2526, -92.2734, "Missouri"
)

## ── Sowing dates ──────────────────────────────────────────
sowing_dates <- list(
  "Early (Apr 15)" = 105,
  "Late (Jun 15)" = 166
)

## ── Years to download ──────────────────────────────────────
years <- 2010:2025
n_years <- length(years)
growing_season_days <- 130

cat("========================================================================\n")
cat("Downloading NASA POWER data: 2010-2025 (16 years)\n")
cat("For 4 scenarios × 130-day growing seasons\n")
cat("This will take 5-15 minutes\n")
cat("========================================================================\n\n")

all_scenario_data <- NULL

for (loc_idx in seq_len(nrow(locations))) {
  loc <- locations[loc_idx, ]

  for (sow_idx in seq_along(sowing_dates)) {
    sow_name <- names(sowing_dates)[sow_idx]
    sow_doy <- sowing_dates[[sow_idx]]

    scenario_label <- paste0(loc$location, " - ", sow_name)
    cat(sprintf("[%s] Downloading... ", scenario_label))

    start_doy <- sow_doy
    end_doy <- sow_doy + 129  # 130-day season

    # Collect raw data across years
    yearly_raw_data <- NULL
    success_count <- 0

    for (year in years) {
      # Convert DOY to calendar dates
      start_date <- as.Date(paste0(year, "-", start_doy), "%Y-%j")
      end_date <- as.Date(paste0(year, "-", end_doy), "%Y-%j")

      tryCatch({
        # Download T2M (mean temperature at 2m height)
        power_data <- nasapower::get_power(
          community = "sb",
          pars = c("T2M"),
          dates = c(format(start_date, "%Y-%m-%d"),
                    format(end_date, "%Y-%m-%d")),
          granularity = "daily",
          lonlat = c(loc$lon, loc$lat),
          resolution = 4
        ) %>%
          as_tibble() %>%
          mutate(
            year = year,
            calendar_date = as.Date(YYYYMMDD, "%Y%m%d"),
            calendar_doy = as.integer(format(calendar_date, "%j")),
            day_in_season = as.integer(calendar_date -
                                       as.Date(paste0(year, "-", start_doy), "%Y-%j")) + 1
          ) %>%
          select(year, calendar_date, calendar_doy, day_in_season, T2M)

        yearly_raw_data <- bind_rows(yearly_raw_data, power_data)
        success_count <- success_count + 1
      }, error = function(e) {
        # Silently continue on API failures
        NULL
      })

      # Polite API delay
      Sys.sleep(0.3)
    }

    if (!is.null(yearly_raw_data)) {
      # Average by calendar day across all years
      scenario_summary <- yearly_raw_data %>%
        group_by(day_in_season, calendar_doy) %>%
        summarise(
          temp_mean = mean(T2M, na.rm = TRUE),
          temp_min = min(T2M, na.rm = TRUE),
          temp_max = max(T2M, na.rm = TRUE),
          temp_sd = sd(T2M, na.rm = TRUE),
          n_years_available = n_distinct(year),
          .groups = "drop"
        ) %>%
        mutate(
          location = loc$location,
          state = loc$state,
          latitude = loc$lat,
          sowing_date = sow_name,
          sowing_doy = sow_doy,
          photoperiod_hours = calc_photoperiod(calendar_doy, loc$lat),
          scenario = scenario_label
        ) %>%
        select(location, state, latitude, sowing_date, sowing_doy,
               day_in_season, calendar_doy,
               temp_mean, temp_min, temp_max, temp_sd, n_years_available,
               photoperiod_hours, scenario)

      all_scenario_data <- bind_rows(all_scenario_data, scenario_summary)
      cat(sprintf("✓ %d years retrieved\n", success_count))
    } else {
      cat("✗ Failed to download\n")
    }
  }
}

## ── Save results ──────────────────────────────────────────
if (!is.null(all_scenario_data)) {
  output_file <- "nasa_power_2010_2025_daily_averages.csv"
  write.csv(all_scenario_data, output_file, row.names = FALSE)

  cat("\n========================================================================\n")
  cat(sprintf("✓ Data saved: %s\n", output_file))
  cat(sprintf("✓ Total records: %d (4 scenarios × 130 days)\n", nrow(all_scenario_data)))

  # Summary statistics
  cat("\n── Temperature Summary (2010-2025 Averages) ──\n\n")

  summary_by_scenario <- all_scenario_data %>%
    group_by(scenario) %>%
    summarise(
      mean_temp = round(mean(temp_mean, na.rm = TRUE), 1),
      temp_range = paste0(round(min(temp_mean, na.rm = TRUE), 1), "–",
                         round(max(temp_mean, na.rm = TRUE), 1)),
      temp_sd = round(mean(temp_sd, na.rm = TRUE), 1),
      pp_range = paste0(round(min(photoperiod_hours, na.rm = TRUE), 1), "–",
                       round(max(photoperiod_hours, na.rm = TRUE), 1)),
      .groups = "drop"
    )

  print(summary_by_scenario)

  cat("\n── Photoperiod Summary ──\n")
  cat(sprintf("Overall range: %.1f–%.1f hours\n",
              min(all_scenario_data$photoperiod_hours, na.rm = TRUE),
              max(all_scenario_data$photoperiod_hours, na.rm = TRUE)))

  cat("\n── Data Quality ──\n")
  cat(sprintf("Years available: %d (2010-2025)\n", n_years))
  cat(sprintf("Avg years per day: %.1f\n",
              mean(all_scenario_data$n_years_available, na.rm = TRUE)))

  cat("\nNEXT STEP:\n")
  cat("Run: gxe_variation_figure_with_nasa_data.R\n")
  cat("========================================================================\n")

} else {
  cat("\n✗ No data retrieved. Check:\n")
  cat("  • Internet connection\n")
  cat("  • NASA POWER API availability\n")
  cat("  • nasapower package version\n")
}
