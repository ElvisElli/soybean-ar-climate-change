# USDA NIFA GP Proposal - R Project Profile
# Loads when RStudio opens this project

# Set working directory to project root
setwd(getwd())

# Print welcome message
cat("\n")
cat("================================================================================\n")
cat("  USDA NIFA Grant Proposal: G×E Soybean Trial Design\n")
cat("================================================================================\n")
cat("\nQuick Start:\n")
cat("  1. source('scripts/create_temperature_data.R')    # Generate synthetic data\n")
cat("  2. source('scripts/gxe_variation_figure_with_range.R')  # Create figure\n")
cat("\nFiles:\n")
cat("  • scripts/     - R analysis and visualization scripts\n")
cat("  • data/        - Generated datasets (CSV)\n")
cat("  • figures/     - Output figures (TIFF, PNG)\n")
cat("  • docs/        - Documentation and workflows\n")
cat("  • README.md    - Project overview\n")
cat("\n================================================================================\n\n")

# Set R options
options(
  stringsAsFactors = FALSE,
  digits = 3,
  width = 100
)

# Suppress package startup messages
suppressPackageStartupMessages({
  # Load common libraries if they're available
  if (requireNamespace("dplyr", quietly = TRUE)) {
    library(dplyr)
  }
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    library(ggplot2)
  }
})
