# R Project Setup Guide

## Opening the Project

### Option 1: RStudio (Recommended)
1. Download [RStudio](https://posit.co/download/rstudio-desktop/) if you don't have it
2. Open RStudio
3. **File** → **Open Project**
4. Navigate to `usda-nifa-gp-proposal.Rproj`
5. Click **Open**

RStudio will automatically:
- Set the working directory to the project root
- Load the `.Rprofile` (custom setup script)
- Display the welcome message

### Option 2: Command Line
```bash
cd usda-nifa-gp-proposal
Rscript scripts/create_temperature_data.R
Rscript scripts/gxe_variation_figure_with_range.R
```

### Option 3: R Console
```r
# Set working directory
setwd("path/to/usda-nifa-gp-proposal")

# Run scripts
source("scripts/create_temperature_data.R")
source("scripts/gxe_variation_figure_with_range.R")
```

---

## Required R Packages

Install these packages (one-time setup):

```r
install.packages(c(
  "dplyr",
  "tidyr", 
  "ggplot2",
  "patchwork",
  "lubridate"
))
```

**Optional** (for NASA POWER data download):
```r
install.packages("nasapower")
```

---

## Project Structure

```
usda-nifa-gp-proposal/
├── usda-nifa-gp-proposal.Rproj    ← Open this to start
├── .Rprofile                      ← Loads automatically in RStudio
├── .gitignore                     ← Git configuration
├── README.md                      ← Project overview
├── SETUP.md                       ← This file
│
├── scripts/                       ← All R scripts
│   ├── create_temperature_data.R           # Generate data (1 sec)
│   ├── gxe_variation_figure_with_range.R   # Create figure (1 sec)
│   ├── fetch_nasa_power_2010_2025.R        # Download real data (5-15 min)
│   ├── fetch_40years_nasa_power.R          # Alternative: 40-year data
│   └── gxe_variation_figure_with_nasa_data.R  # Figure with real data
│
├── data/                          ← Generated datasets
│   └── temperature_and_photoperiod_data.csv    (520 rows, 130-day cycles)
│
├── figures/                       ← Output figures
│   ├── gxe_environmental_variation_real_data.tiff  (publication-ready, 300 dpi)
│   └── gxe_environmental_variation_real_data.png   (web preview)
│
└── docs/                          ← Documentation
    └── NASA_POWER_WORKFLOW.md          (complete workflow guide)
```

---

## Quick Start Workflows

### Workflow 1: Synthetic Data (Fastest)
**Time**: ~2 seconds
```r
source("scripts/create_temperature_data.R")        # Step 1: Generate data
source("scripts/gxe_variation_figure_with_range.R") # Step 2: Create figure
```
**Output**: `figures/gxe_environmental_variation_real_data.tiff`

### Workflow 2: Real NASA POWER Data
**Time**: 5-15 minutes (includes download time)
```r
source("scripts/fetch_nasa_power_2010_2025.R")        # Step 1: Download 2010-2025 data
source("scripts/gxe_variation_figure_with_nasa_data.R") # Step 2: Create figure
```
**Output**: `figures/gxe_environmental_variation_nasa_2010_2025.tiff`

---

## Working in RStudio

### Console Commands
```r
# View data
View(read.csv("data/temperature_and_photoperiod_data.csv"))

# Check temperature range
data <- read.csv("data/temperature_and_photoperiod_data.csv")
summary(data$temp_mean)
summary(data$photoperiod_hours)

# List all files in project
list.files(recursive = TRUE)
```

### Project Files Tab
- Click **Files** pane in RStudio (bottom-right)
- Navigate folders and open files
- Scripts automatically use project working directory

---

## Customizing the Project

### Change Sowing Dates
Edit `scripts/create_temperature_data.R`:
```r
sowing_dates <- list(
  "Early (Apr 15)" = 105,
  "Late (Jun 15)" = 166
  # Add or modify dates here
)
```

### Change Response Function Thresholds
Edit `scripts/gxe_variation_figure_with_range.R`:
```r
thermal_response <- function(temp) {
  Tb <- 10      # Change base temperature
  Topt <- 30    # Change optimum
  Tmax <- 40    # Change maximum
  # ... rest of function
}
```

### Modify Figure Appearance
Edit the ggplot2 scales:
```r
scale_colour_manual(values = c("Early" = "#2ecc71", "Late" = "#e74c3c"))
ggsave(..., width = 16, height = 6, ...)  # Adjust dimensions
```

---

## Troubleshooting

### Error: "object 'dplyr' not found"
**Solution**: Install dplyr
```r
install.packages("dplyr")
```

### Error: "cannot open file 'scripts/...'"
**Solution**: Ensure working directory is project root
```r
setwd("path/to/usda-nifa-gp-proposal")
getwd()  # Verify
```

### Figure doesn't save
**Solution**: Create figures folder first
```r
dir.create("figures", showWarnings = FALSE)
```

---

## Git Integration

The project is version-controlled with Git. Recent commits:
```
16f69d9  Convert to R-only workflow and test in cloud
e5fbdec  Add USDA NIFA GP proposal: G×E environmental variation analysis
```

**Pushing changes:**
```bash
git add .
git commit -m "Your change description"
git push origin main
```

---

## Support & Documentation

- **Project README**: See `README.md`
- **NASA POWER Workflow**: See `docs/NASA_POWER_WORKFLOW.md`
- **GitHub Repository**: https://github.com/ElvisElli/soybean-ar-climate-change

---

## Session Info

Capture your R environment:
```r
sessionInfo()
```

Use this to track package versions and R version used for reproducibility.
