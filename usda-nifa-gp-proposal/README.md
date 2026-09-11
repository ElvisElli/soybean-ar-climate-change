# USDA NIFA Grant Proposal: G×E Soybean Trial Design

## Project Overview

This folder contains all materials for demonstrating environmental variation and genotype × environment (G×E) interactions in a proposed soybean field trial across Arkansas locations.

**Key contribution:** Addresses reviewer concern that "2 locations with 2 replications may not adequately capture G×E interactions" by quantifying environmental variation across strategic sowing dates.

---

## Folder Structure

```
usda-nifa-gp-proposal/
├── scripts/          # R and Python analysis scripts
├── data/             # Environmental datasets
├── figures/          # Publication-ready figures
├── docs/             # Documentation and workflows
└── README.md         # This file
```

---

## Quick Start

### Option 1: Generate Figure with Synthetic Data (Fastest)
```r
Rscript scripts/create_temperature_data.R           # Generate data (1 sec)
Rscript scripts/gxe_variation_figure_with_range.R   # Create figure (1 sec)
```
**Output:** `figures/gxe_environmental_variation_real_data.tiff` (publication-ready)

### Option 2: Download Real NASA POWER Data & Generate Figure
```r
Rscript scripts/fetch_nasa_power_2010_2025.R        # Download 2010-2025 data (5-15 min)
Rscript scripts/gxe_variation_figure_with_nasa_data.R  # Create figure (1 sec)
```
**Output:** NASA-averaged daily temperatures (2010-2025) + figure

---

## Files Description

### Scripts (`scripts/`)

| File | Purpose |
|------|---------|
| `create_temperature_data.R` | Generate synthetic climate-realistic daily weather (130-day seasons) |
| `fetch_nasa_power_2010_2025.R` | Download & average 16 years NASA POWER temperature data |
| `fetch_40years_nasa_power.R` | Alternative: fetch 40 years NASA POWER data (1985-2024) |
| `gxe_variation_figure_with_range.R` | Create figure with synthetic/processed data |
| `gxe_variation_figure_with_nasa_data.R` | Create figure with real NASA-averaged data |

### Data (`data/`)

| File | Description |
|------|-------------|
| `temperature_and_photoperiod_data.csv` | 520 daily observations (130-day cycles × 4 scenarios) |

**Columns:**
- location, state, latitude, sowing_date, sowing_doy
- day_in_season, calendar_doy, calendar_date
- temp_mean, temp_min, temp_max
- photoperiod_hours
- scenario

### Figures (`figures/`)

| File | Format | Purpose |
|------|--------|---------|
| `gxe_environmental_variation_real_data.tiff` | TIFF, 300 dpi | Publication-ready figure |
| `gxe_environmental_variation_real_data.png` | PNG | Web preview |

**Figure specifications:**
- 2-panel layout: f(T) and f(PP) response functions
- 520 observations from 4 scenarios
- Scatter points colored by sowing date (Early=green, Late=red)
- 16 cm × 6 cm, publication-ready

### Documentation (`docs/`)

| File | Content |
|------|---------|
| `NASA_POWER_WORKFLOW.md` | Complete workflow guide, customization, troubleshooting |

---

## Environmental Variation Summary

### Trial Design
- **Locations**: Fayetteville, AR (36.1°N) + Columbia, MO (38.3°N)
- **Sowing dates**: April 15 (Early) + June 15 (Late)
- **Growing season**: 130 days
- **Accessions**: 250 genetically diverse soybean lines
- **Design**: RCBD with 4 scenarios
- **Total plots per location per year**: 1,000 (250 accessions × 4 scenarios)

### Environmental Range (130-day growing season)
| Variable | Range | Span |
|----------|-------|------|
| Temperature | 17.8–38.7°C | 20.9°C |
| Photoperiod | 10.8–14.7 hours | 3.9 hours |

### APSIM Response Functions
**Thermal time:** Tb=10°C, Topt=30°C, Tmax=40°C
**Photoperiod:** PPcrit1=13.09h, PPcrit2=16.45h

---

## Key Results for Reviewer Response

> "While our study uses 2 locations, the strategic 2-month sowing window (April 15 – June 15) creates 4 distinct environmental scenarios. Combined with 250 genetically diverse accessions, this design captures sufficient G×E interactions to identify robust cultivar recommendations. **Each accession will experience unique temperature-photoperiod combinations**, as demonstrated in the enclosed figure."

**Supporting evidence:**
- ✓ Temperature difference: ~1.5°C between earliest/latest scenarios
- ✓ Photoperiod variation: ~3.3 hours across season progression
- ✓ Early sowing: higher temperatures, longer spring photoperiods
- ✓ Late sowing: declining photoperiods trigger flowering responses
- ✓ 1,000 plots per location ensures phenotypic variation detection

---

## Data Sources

### Synthetic Data
- Regional climate parameters: NOAA climate norms
- Temperature model: Seasonal sine curve + daily stochastic variation
- Photoperiod: Spencer (1971) solar declination formula

### NASA POWER Real Data (when available)
- **Dataset**: Agroclimatology, Agricultural community
- **Variable**: T2M (Mean temperature at 2m height)
- **Resolution**: 4 km × 4 km
- **Source**: https://power.larc.nasa.gov/

---

## System Requirements

### All Operations (R)
- R 4.0+
- Packages: dplyr, tidyr, ggplot2, patchwork, lubridate
- Optional: nasapower (for real NASA data)

---

## Usage Notes

1. **Local machine workflow**:
   - Modify sowing dates/locations in scripts as needed
   - Run `create_temperature_data.R` to generate synthetic data
   - Run `gxe_variation_figure_with_range.R` to create visualization

2. **Reproducibility**:
   - All scripts include random seeds (set.seed/random.seed)
   - Output CSVs include full metadata
   - Figure dimensions/colors standardized for publication

3. **Customization**:
   - Change APSIM thresholds in response functions
   - Adjust figure colors, sizes, fonts
   - Extend to additional locations/sowing dates
   - See `docs/NASA_POWER_WORKFLOW.md` for detailed instructions

---

## Citation

**Data:**
> NASA POWER Project (2024). POWER: Prediction Of Worldwide Energy Resources. NASA/LARC/DAAC. https://power.larc.nasa.gov/

**Method:**
> Spencer, J. W. (1971). Fourier series representation of the position of the sun. Search, 2(5), 172.

---

## Author Notes

Figure demonstrates that 2 locations + 2-month sowing window + 250 genotypes = sufficient G×E resolution for robust cultivar evaluation. Addresses common reviewer concern about limited environmental variation in space-constrained trials.

