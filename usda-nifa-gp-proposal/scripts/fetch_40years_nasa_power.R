## ============================================================
## Fetch 40 years of NASA POWER data and average per day
## Then create environmental variation figure
## ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(patchwork)
})

# Install nasapower if needed
if (!require(nasapower, quietly = TRUE)) {
  cat("Installing nasapower package...\n")
  install.packages("nasapower", repos = "https://cloud.r-project.org")
}

## ── Locations and date ranges ────────────────────────────────
locations <- tribble(
  ~location, ~lat, ~lon, ~state,
  "Fayetteville, AR", 36.0726, -94.1729, "Arkansas",
  "Columbia, MO", 38.2526, -92.2734, "Missouri"
)

# 40 years: 1985-2024
years <- 1985:2024
n_years <- length(years)

# Sowing dates
sowing_doys <- c(105, 166)  # Apr 15, Jun 15
names(sowing_doys) <- c("Early (Apr 15)", "Late (Jun 15)")

## ── Photoperiod calculation ──────────────────────────────────
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

## ── Fetch NASA POWER data for 40 years ───────────────────────
cat("Fetching 40 years of NASA POWER data...\n")
cat("This may take 2-5 minutes\n\n")

all_data <- NULL

for (loc_idx in seq_len(nrow(locations))) {
  loc <- locations[loc_idx, ]

  for (sow_idx in seq_along(sowing_doys)) {
    sow_doy <- sowing_doys[sow_idx]
    sow_name <- names(sowing_doys)[sow_idx]

    cat(sprintf("%s - %s: ", loc$location, sow_name))

    # Growing season: 130 days
    start_doy <- sow_doy
    end_doy <- sow_doy + 129

    # Collect data across years
    yearly_data <- NULL

    for (year in years) {
      start_date <- as.Date(paste0(year, "-", start_doy), "%Y-%j")
      end_date <- as.Date(paste0(year, "-", end_doy), "%Y-%j")

      tryCatch({
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
            year = year,
            location = loc$location,
            state = loc$state,
            sowing_date = sow_name,
            sowing_doy = sow_doy
          ) %>%
          select(year, location, state, sowing_date, sowing_doy,
                 YYYYMMDD, T2M, T2MN, T2MX)

        yearly_data <- bind_rows(yearly_data, power_data)
      }, error = function(e) {
        cat(".")
      })

      Sys.sleep(0.5)  # Be respectful to API
    }

    if (!is.null(yearly_data)) {
      all_data <- bind_rows(all_data, yearly_data)
      cat("✓ Retrieved ", nrow(yearly_data) / 116, " years\n", sep = "")
    } else {
      cat("✗ Failed\n")
    }
  }
}

## ── Process: average per day of year ──────────────────────────
if (!is.null(all_data)) {
  cat("\nProcessing data - averaging per day across", n_years, "years...\n")

  summary_data <- all_data %>%
    mutate(
      calendar_date = as.Date(YYYYMMDD, "%Y%m%d"),
      calendar_doy = as.integer(format(calendar_date, "%j")),
      day_in_season = as.integer(calendar_date -
                                  as.Date(paste0(year, "-", sowing_doy), "%Y-%j")) + 1
    ) %>%
    group_by(location, state, sowing_date, sowing_doy, day_in_season) %>%
    summarise(
      calendar_doy = first(calendar_doy),
      temp_mean = mean(T2M, na.rm = TRUE),
      temp_min = min(T2M, na.rm = TRUE),
      temp_max = max(T2M, na.rm = TRUE),
      temp_sd = sd(T2M, na.rm = TRUE),
      n_years = n(),
      .groups = "drop"
    ) %>%
    mutate(
      photoperiod_hours = calc_photoperiod(calendar_doy,
                                          ifelse(state == "Arkansas", 36.0726, 38.2526))
    ) %>%
    filter(n_years >= n_years / 2)  # Keep days with at least 20 years of data

  cat("✓ Processed", nrow(summary_data), "day-aggregates\n\n")

  # Save to CSV
  write.csv(summary_data, "nasa_power_40year_daily_averages.csv", row.names = FALSE)

  cat("Summary statistics:\n")
  cat("Temperature range:", round(min(summary_data$temp_mean, na.rm=T), 1),
      "–", round(max(summary_data$temp_mean, na.rm=T), 1), "°C\n")
  cat("Photoperiod range:", round(min(summary_data$photoperiod_hours, na.rm=T), 1),
      "–", round(max(summary_data$photoperiod_hours, na.rm=T), 1), "hours\n")
  cat("Mean years per day:", round(mean(summary_data$n_years), 1), "\n\n")

  # Show by scenario
  cat("Years of data per scenario:\n")
  summary_data %>%
    group_by(location, sowing_date) %>%
    summarise(mean_years = round(mean(n_years), 1),
              min_years = min(n_years),
              max_years = max(n_years),
              .groups = "drop") %>%
    print()

  cat("\n✓ Saved: nasa_power_40year_daily_averages.csv\n")
  cat("Next: Run gxe_variation_figure_40years.R\n")

} else {
  cat("\n✗ No data retrieved. Check internet and API access.\n")
}
