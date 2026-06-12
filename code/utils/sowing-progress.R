## ============================================================
## sowing-progress.R
## Soybean planting progress in Arkansas (USDA-NASS, 1990–2023)
##
## Fits a logistic curve to weekly % planted data for each year:
##   progress(DOY) = 1 / (1 + exp(-b * (DOY - c)))
##   where c = DOY at 50% planting (inflection point)
##         b = rate of progress
##
## Output:
##   figures/sowing-progress.tiff — progress curves by year + average
## ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(broom)
  library(readxl)
  library(ggplot2)
  library(lubridate)
})

source("code/utils/plot-theme.R")
dir.create("figures", showWarnings = FALSE)

## ── Load and clean USDA-NASS data ────────────────────────────────────────────
raw <- read_excel("data/raw/progress.xlsx")

## Normalise column names to lowercase
names(raw) <- toupper(trimws(names(raw)))

nass <- raw %>%
  filter(
    grepl("PLANTED", DATA_ITEM, ignore.case = TRUE),
    grepl("SOYBEANS", DATA_ITEM, ignore.case = TRUE),
    YEAR >= 1990,
    YEAR <= 2023
  ) %>%
  transmute(
    year  = as.integer(YEAR),
    week  = as.Date(WEEK_ENDING),
    DOY   = as.integer(strftime(week, "%j")),
    value = VALUE / 100        # convert % to proportion [0, 1]
  ) %>%
  filter(!is.na(value), !is.na(DOY))

cat(sprintf("[Progress] %d year-week records for soybeans (%d–%d)\n",
            nrow(nass), min(nass$year), max(nass$year)))

## ── Fit logistic model per year ───────────────────────────────────────────────
## Model: value ~ 1 / (1 + exp(-b * (DOY - c)))
##   c = inflection point = DOY at 50% planted
##   b = steepness of the curve
fit_logistic <- function(df) {
  tryCatch(
    nls(value ~ 1 / (1 + exp(-b * (DOY - c))),
        data  = df,
        start = list(b = 0.10, c = 135)),   # May-15 as starting guess for c
    error = function(e) NULL
  )
}

models <- nass %>%
  group_by(year) %>%
  nest() %>%
  mutate(
    model  = map(data, fit_logistic),
    failed = map_lgl(model, is.null)
  )

n_failed <- sum(models$failed)
if (n_failed > 0)
  message(sprintf("[Progress] %d year(s) failed to converge and will be skipped.", n_failed))

## ── Generate predicted curves (DOY 100–175) ───────────────────────────────────
DOY_seq <- 100:175

pred_curves <- models %>%
  filter(!failed) %>%
  mutate(
    params = map(model, tidy),
    pct_50 = map_dbl(params, ~ .x$estimate[.x$term == "c"]),  # DOY at 50%
    pred   = map(model, ~ {
      p <- coef(.x)
      data.frame(
        DOY      = DOY_seq,
        progress = 1 / (1 + exp(-p["b"] * (DOY_seq - p["c"]))) * 100
      )
    })
  ) %>%
  select(year, pct_50, pred) %>%
  unnest(pred)

## ── Average curve across all years ───────────────────────────────────────────
avg_curve <- pred_curves %>%
  group_by(DOY) %>%
  summarise(progress = mean(progress, na.rm = TRUE), .groups = "drop")

avg_pct50 <- mean(models$model %>%
                    keep(Negate(is.null)) %>%
                    map_dbl(~ coef(.x)["c"]))

cat(sprintf("[Progress] Average 50%% planting DOY: %.1f (~ %s)\n",
            avg_pct50,
            format(as.Date(paste0("2023-", round(avg_pct50)), "%Y-%j"), "%B %d")))

## ── Figure: progress curves by year + average ────────────────────────────────
##
## Each thin grey line = one year's predicted logistic curve
## Bold green line     = average curve across all years
## Dashed lines        = 10% and 50% progress thresholds
##

plot_progress <- ggplot() +
  ## Individual year curves (thin, light)
  geom_line(
    data = pred_curves,
    aes(x = DOY, y = progress, group = year),
    colour = "grey65", linewidth = 0.4, alpha = 0.7
  ) +
  ## Reference lines at 10% and 50% planted
  geom_hline(yintercept = c(10, 50), linetype = "dashed",
             colour = "grey30", linewidth = 0.5) +
  ## Average curve (bold)
  geom_line(
    data = avg_curve,
    aes(x = DOY, y = progress),
    colour = "#00BF7D", linewidth = 1.4
  ) +
  ## Annotate average 50% DOY
  annotate("text",
           x     = avg_pct50 + 2,
           y     = 53,
           label = sprintf("50%% avg: DOY %.0f\n(~%s)",
                           avg_pct50,
                           format(as.Date(paste0("2023-", round(avg_pct50)), "%Y-%j"), "%b %d")),
           hjust = 0, size = 3.2, colour = "#00BF7D") +
  temp +
  scale_x_continuous(
    name   = "Day of year",
    breaks = c(100, 110, 120, 130, 140, 150, 160, 170),
    labels = c("100\n(Apr 10)", "110\n(Apr 20)", "120\n(Apr 30)",
               "130\n(May 10)", "140\n(May 20)", "150\n(May 30)",
               "160\n(Jun 9)",  "170\n(Jun 19)")
  ) +
  scale_y_continuous(
    name   = "Soybean planting progress (%)",
    limits = c(0, 100),
    breaks = seq(0, 100, 20)
  ) +
  labs(caption = sprintf(
    "Arkansas soybeans, USDA-NASS 1990–2023 (n = %d years). Grey lines = individual years; green = average.",
    n_distinct(pred_curves$year)
  )) +
  theme(plot.caption = element_text(size = 8, colour = "grey40"))

ggsave("figures/sowing-progress.tiff",
       plot        = plot_progress,
       width       = 17, height = 13, units = "cm",
       dpi         = 600, compression = "lzw", bg = "white")

cat("[Progress] Saved: figures/sowing-progress.tiff\n")

## ── Console summary ───────────────────────────────────────────────────────────
cat("\nYearly 50% planting DOY:\n")
models %>%
  filter(!failed) %>%
  mutate(pct_50 = map_dbl(model, ~ coef(.x)["c"])) %>%
  select(year, pct_50) %>%
  mutate(date_approx = format(
    as.Date(paste0(year, "-", round(pct_50)), "%Y-%j"), "%b %d")) %>%
  arrange(year) %>%
  print(n = Inf)
