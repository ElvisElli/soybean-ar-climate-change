## ============================================================
## Fetch REAL NASA POWER daily data for proposed trial locations
##
## Note: Run this script locally where you have apsimx or nasapower R packages
## This requires internet access and may take a few minutes
## ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

# Install nasapower if needed:
# install.packages("nasapower")
# OR
# remotes::install_github("adamhsparks/nasapower")

if (!require(nasapower, quietly = TRUE)) {
  cat("Installing nasapower package...\n")
  install.packages("nasapower")
}

## ── Setup ────────────────────────────────────────────────────
locations <- tribble(
  ~location, ~lat, ~lon,
  "Fayetteville, AR", 36.0726, -94.1729,
  "Columbia, MO", 38.2526, -92.2734
)

sowing_dates <- tribble(
  ~sowing_name, ~start_date,
  "Early (Apr 15)", "2023-04-15",
  "Late (Jun 15)", "2023-06-15"
)

def_photoperiod <- function(doy, latitude_deg) {
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

all_data <- NULL

## ── Fetch data from NASA POWER ──────────────────────────────
cat("Fetching NASA POWER data...\n")
cat("(This may take a few minutes)\n\n")

for (i in seq_len(nrow(locations))) {
  for (j in seq_len(nrow(sowing_dates))) {

    loc <- locations[i, ]
    sow <- sowing_dates[j, ]

    start_date <- as.Date(sow$start_date)
    end_date <- start_date + 115

    cat(sprintf("Fetching: %s, %s (DOY %d–%d)...\n",
                loc$location, sow$sowing_name,
                as.integer(format(start_date, "%j")),
                as.integer(format(end_date, "%j"))))

    tryCatch({
      # Use nasapower to fetch POWER data
      power_data <- nasapower::get_power(
        community = "sb",
        pars = c("T2M", "T2MN", "T2MX"),
        dates = c(format(start_date, "%Y-%m-%d"),
                  format(end_date, "%Y-%m-%d")),
        granularity = "daily",
        lonlat = c(loc$lon, loc$lat),
        resolution = 4
      ) %>%
        as_tibble() %>%
        mutate(
          location = loc$location,
          latitude = loc$lat,
          sowing_date = sow$sowing_name,
          sowing_doy = as.integer(format(start_date, "%j")),
          calendar_date = YYYYMMDD,
          calendar_doy = as.integer(format(as.Date(YYYYMMDD, "%Y%m%d"), "%j")),
          day_in_season = row_number(),
          temp_mean = T2M,
          temp_min = T2MN,
          temp_max = T2MX,
          photoperiod_hours = def_photoperiod(calendar_doy, latitude),
          scenario = paste0(location, " - ", sowing_date)
        ) %>%
        select(location, latitude, sowing_date, sowing_doy,
               calendar_date, calendar_doy, day_in_season,
               temp_mean, temp_min, temp_max, photoperiod_hours, scenario)

      all_data <- bind_rows(all_data, power_data)
      cat("  ✓ Success\n")

    }, error = function(e) {
      cat("  ✗ Error:", e$message, "\n")
    })

    Sys.sleep(1)  # Be respectful to the server
  }
}

## ── Save results ────────────────────────────────────────────
if (!is.null(all_data)) {

  output_file <- "nasa_power_temperature_and_photoperiod_data.csv"
  write.csv(all_data, output_file, row.names = FALSE)

  cat("\n✓ Data saved to:", output_file, "\n")
  cat("✓ Total records:", nrow(all_data), "\n\n")

  # Summary
  cat("Temperature range across all scenarios:\n")
  temps <- all_data$temp_mean
  cat(sprintf("  Min: %.1f°C\n", min(temps, na.rm = TRUE)))
  cat(sprintf("  Max: %.1f°C\n", max(temps, na.rm = TRUE)))
  cat(sprintf("  Mean: %.1f°C\n\n", mean(temps, na.rm = TRUE)))

  cat("Photoperiod range across all scenarios:\n")
  pps <- all_data$photoperiod_hours
  cat(sprintf("  Min: %.1f hours\n", min(pps, na.rm = TRUE)))
  cat(sprintf("  Max: %.1f hours\n\n", max(pps, na.rm = TRUE)))

  cat("Summary by scenario:\n")
  summary_stats <- all_data %>%
    group_by(scenario) %>%
    summarise(
      temp_mean = round(mean(temp_mean, na.rm = TRUE), 1),
      temp_range = paste0(round(min(temp_mean, na.rm = TRUE), 1), "–",
                         round(max(temp_mean, na.rm = TRUE), 1)),
      pp_range = paste0(round(min(photoperiod_hours, na.rm = TRUE), 1), "–",
                        round(max(photoperiod_hours, na.rm = TRUE), 1)),
      .groups = "drop"
    )
  print(summary_stats)

  cat("\nNext step: Update gxe_variation_figure_with_real_data.R to use this file:\n")
  cat("  data_file <- 'nasa_power_temperature_and_photoperiod_data.csv'\n")

} else {
  cat("\n✗ No data retrieved. Check internet connection and credentials.\n")
}
