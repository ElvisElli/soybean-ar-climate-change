# NASA POWER Weather Data Workflow for G×E Figure

## Overview
This workflow downloads 16 years of real NASA POWER temperature data (2010-2025), averages it daily, and creates a publication-ready figure showing APSIM response functions with observed environmental variation.

---

## Prerequisites

### Required R Packages
Install these packages before running the scripts:

```r
# Install from CRAN
install.packages(c("nasapower", "dplyr", "tidyr", "ggplot2", "patchwork", "apsimx", "lubridate"))

# Or install individually as needed
install.packages("nasapower")
```

### System Requirements
- R version 4.0+
- Internet connection (for NASA POWER API access)
- ~100 MB free disk space
- Processing time: ~5-15 minutes for download

---

## Workflow: Step-by-Step

### Step 1: Download and Average NASA POWER Data

Run this script to download 16 years (2010-2025) of daily temperature data:

```r
source("fetch_nasa_power_2010_2025.R")
```

**What it does:**
- Downloads daily mean temperature (T2M) from NASA POWER API
- For each scenario (4 total: 2 locations × 2 sowing dates)
- Averages temperature across all 16 years for each calendar day
- Calculates photoperiod (deterministic, based on latitude and day-of-year)
- Outputs: `nasa_power_2010_2025_daily_averages.csv`

**Output file structure:**
```
location, state, latitude, sowing_date, sowing_doy,
day_in_season, calendar_doy,
temp_mean, temp_min, temp_max, temp_sd, n_years_available,
photoperiod_hours, scenario
```

### Step 2: Create Figure with Real Data

Run this script to generate the publication-ready figure:

```r
source("gxe_variation_figure_with_nasa_data.R")
```

**What it does:**
- Reads the averaged NASA POWER data
- Calculates APSIM response functions: f(T) and f(PP)
- Creates 2-panel figure showing:
  - Left: Thermal time response with scatter points
  - Right: Photoperiod response with scatter points
- Scatter points colored by sowing date:
  - Early (Apr 15): Green
  - Late (Jun 15): Red
- Outputs: `figures/gxe_environmental_variation_nasa_2010_2025.tiff` (300 dpi)

---

## Trial Scenarios

### Locations
| Location | Latitude | Longitude | State |
|----------|----------|-----------|-------|
| Fayetteville | 36.07°N | -94.17°W | Arkansas |
| Columbia | 38.25°N | -92.27°W | Missouri |

### Sowing Dates
| Sowing Date | Calendar Date | DOY |
|-------------|---------------|-----|
| Early | April 15 | 105 |
| Late | June 15 | 166 |

### Growing Season
- **Duration**: 130 days (fixed)
- **Data period**: 2010-2025 (16 years)
- **Temperature variable**: T2M (mean daily temperature at 2m height)

---

## APSIM Response Functions

### Thermal Time Response f(T)
```
Base temperature (Tb):    10°C
Optimum temperature (Topt): 30°C
Maximum temperature (Tmax): 40°C

Formula:
  If T < Tb or T > Tmax:  f(T) = 0
  If Tb ≤ T ≤ Topt:       f(T) = (T - Tb) / (Topt - Tb)
  If Topt < T ≤ Tmax:     f(T) = (Tmax - T) / (Tmax - Topt)
```

### Photoperiod Response f(PP)
```
Critical photoperiod 1 (PPcrit1): 13.09 hours
Critical photoperiod 2 (PPcrit2): 16.45 hours

Formula:
  If PP ≤ PPcrit1:        f(PP) = 1.0 (full response)
  If PPcrit1 < PP < PPcrit2: f(PP) = 1.0 - (PP - PPcrit1) / (PPcrit2 - PPcrit1)
  If PP ≥ PPcrit2:        f(PP) = 0.0 (no response)
```

---

## Data Source

**NASA POWER (Prediction Of Worldwide Energy Resources)**

| Parameter | Value |
|-----------|-------|
| Dataset | Agroclimatology |
| Community | Agricultural |
| Temperature Variable | T2M (Mean temperature at 2m height) |
| Temporal Resolution | Daily |
| Spatial Resolution | 4 km × 4 km |
| Time Period | 2010-2025 (16 years) |
| Source | https://power.larc.nasa.gov/ |

---

## Output Files

### Data File
- **`nasa_power_2010_2025_daily_averages.csv`** (16-year averaged daily data)
  - 520 records (4 scenarios × 130 days)
  - Columns: temperature (mean, min, max), photoperiod, scenario metadata

### Figure File
- **`figures/gxe_environmental_variation_nasa_2010_2025.tiff`** (publication-ready)
  - Format: TIFF, 300 dpi
  - Dimensions: 16 cm × 6 cm
  - 2-panel layout: f(T) and f(PP) with scatter overlay

---

## Troubleshooting

### Issue: "nasapower package not found"
**Solution:**
```r
install.packages("nasapower")
```

### Issue: "Data file not found"
**Solution:** Run `fetch_nasa_power_2010_2025.R` first to download the data.

### Issue: NASA POWER API timeout or failure
**Possible causes:**
- Firewall/proxy blocking API access
- Network connectivity issue
- NASA POWER API maintenance

**Workaround:** Use synthetic climate-realistic data instead
```r
source("create_temperature_data.py")  # Generate synthetic data
source("gxe_variation_figure_with_range.R")  # Use synthetic data for figure
```

### Issue: API rate limiting
**Solution:** The script includes 0.3s delays between requests. If still hitting limits, increase delays in the script.

---

## Customization

### Change Sowing Dates
Edit in `fetch_nasa_power_2010_2025.R`:
```r
sowing_dates <- list(
  "Early (Apr 15)" = 105,
  "Late (Jun 15)" = 166
  # Add or modify sowing dates here
)
```

### Change Growing Season Length
Edit in both scripts:
```r
growing_season_days <- 130  # Change to desired number of days
end_doy <- sow_doy + (growing_season_days - 1)
```

### Change Response Function Thresholds
Edit in `gxe_variation_figure_with_nasa_data.R`:
```r
thermal_response <- function(temp) {
  Tb <- 10      # Adjust base temperature
  Topt <- 30    # Adjust optimum
  Tmax <- 40    # Adjust maximum
  # ... rest of function
}
```

### Modify Figure Appearance
Edit theme and scale parameters:
```r
# Change colors for sowing dates
scale_colour_manual(values = c("Early" = "#YOUR_COLOR", "Late" = "#YOUR_COLOR"))

# Change figure dimensions
ggsave(..., width = 16, height = 6, ...)  # in cm
```

---

## Citation

**Data Source:**
> NASA POWER Project (2024). POWER: Prediction Of Worldwide Energy Resources. 
> NASA/LARC/DAAC. https://power.larc.nasa.gov/

**Method:**
> Spencer, J. W. (1971). Fourier series representation of the position of the sun. 
> Search, 2(5), 172.

---

## Contact & Support

For issues with NASA POWER data: https://power.larc.nasa.gov/
For R package issues: https://github.com/adamhsparks/nasapower
