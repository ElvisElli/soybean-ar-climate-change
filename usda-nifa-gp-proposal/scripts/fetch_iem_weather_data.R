## ============================================================
## Fetch real weather data from NASA POWER via apsimx package
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


## ── Main workflow ────────────────────────────────────────────
cat("========================================================================\n")
cat("Fetching weather data from NASA POWER (via apsimx)\n")
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
      # Note: apsimx already converts NASA POWER columns:
      # T2MX→maxt, T2MN→mint, PREC→rain, ALLSKY_SFC_SW_DWN→radn (in MJ/m²/day)
      scenario_data <- as.data.frame(power_data) %>%
        filter((day >= sow_doy & day <= sow_doy + 129) |
               (sow_doy + 129 > 365 & (day >= sow_doy | day <= sow_doy + 129 - 365))) %>%
        mutate(
          date = as.Date(paste0(year, "-", day), "%Y-%j"),
          location = loc$location,
          scenario = paste0(loc$location, " - ", sow_name),
          sowing_date = sow_name,
          sowing_doy = sow_doy,
          source = "NASA POWER"
        ) %>%
        select(date, year, day, radn, maxt, mint, rain, location, scenario,
               sowing_date, sowing_doy, source)

      if (nrow(scenario_data) > 0) {
        all_data <- bind_rows(all_data, scenario_data)
      }
    }
    cat("  ✓ NASA POWER data processed\n\n")
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
cat("Next: Run gxe_variation_figure_with_iem_data.R\n")
