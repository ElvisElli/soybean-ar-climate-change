## ============================================================
## sowing-progress.R
## Soybean planting progress in Arkansas (USDA-NASS, 1990–present)
##
## Steps:
##   1. Download weekly % planted from USDA-NASS Quick Stats API
##      (requires a free API key from https://quickstats.nass.usda.gov/api)
##      Falls back to data/raw/progress.xlsx if download fails or key not set.
##   2. Fit logistic curve per year:
##        progress(DOY) = 100 / (1 + exp(-b * (DOY - c)))
##        c = DOY at 50% planted (inflection)
##        b = steepness
##   3. Produce three figures:
##        fig_progress_all.tiff    — all years + avg, 10% and 50% thresholds
##        fig_progress_recent.tiff — latest 5 years highlighted, 10% & 50% marked
##
## To enable auto-download:
##   1. Get a free API key at https://quickstats.nass.usda.gov/api
##   2. Set NASS_API_KEY below (or as environment variable NASS_API_KEY)
## ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(broom)
  library(readxl)
  library(ggplot2)
  library(lubridate)
  library(httr)
  library(jsonlite)
})

source("code/utils/plot-theme.R")
dir.create("figures", showWarnings = FALSE)

## ── Settings ─────────────────────────────────────────────────────────────────
NASS_API_KEY  <- Sys.getenv("D228A372-93ED-3BF7-9699-D2D0DDD3C88D", unset = "")  # set env var or paste key here
FALLBACK_FILE <- "data/raw/progress.xlsx"
YEAR_MIN      <- 1990
YEAR_MAX      <- as.integer(format(Sys.Date(), "%Y"))  # always current year
N_RECENT      <- 5   # years to highlight in Plot 2
THRESHOLDS    <- c(10, 50)  # % planted thresholds to annotate

## ── Step 1: Download or load data ────────────────────────────────────────────
download_nass <- function(api_key) {
  url <- "https://quickstats.nass.usda.gov/api/api_GET/"
  params <- list(
    key               = api_key,
    commodity_desc    = "SOYBEANS",
    statisticcat_desc = "PROGRESS",
    unit_desc         = "PCT PLANTED",
    state_alpha       = "AR",
    freq_desc         = "WEEKLY",
    format            = "JSON"
  )
  resp <- tryCatch(
    GET(url, query = params, timeout(30)),
    error = function(e) NULL
  )
  if (is.null(resp) || http_error(resp)) return(NULL)

  raw_json <- tryCatch(fromJSON(content(resp, "text", encoding = "UTF-8")),
                       error = function(e) NULL)
  if (is.null(raw_json) || is.null(raw_json$data)) return(NULL)

  df <- as.data.frame(raw_json$data) %>%
    transmute(
      year  = as.integer(year),
      week  = as.Date(week_ending),
      DOY   = as.integer(strftime(week, "%j")),
      value = as.numeric(Value) / 100
    ) %>%
    filter(!is.na(value), !is.na(DOY), year >= YEAR_MIN)
  df
}

nass <- NULL

if (nchar(NASS_API_KEY) > 0) {
  message("[Progress] Downloading from USDA-NASS Quick Stats API...")
  nass <- download_nass(NASS_API_KEY)
  if (!is.null(nass)) {
    message(sprintf("[Progress] Downloaded %d records (%d–%d).",
                    nrow(nass), min(nass$year), max(nass$year)))
    ## Save a local copy so future runs don't need the API
    saveRDS(nass, "data/raw/nass-progress-cache.rds")
  } else {
    message("[Progress] Download failed — falling back to local file.")
  }
}

## Try local cache first (faster than re-downloading)
if (is.null(nass) && file.exists("data/raw/nass-progress-cache.rds")) {
  nass <- readRDS("data/raw/nass-progress-cache.rds")
  message(sprintf("[Progress] Loaded from cache (%d–%d).", min(nass$year), max(nass$year)))
}

## Final fallback: Excel file
if (is.null(nass)) {
  message("[Progress] Loading from ", FALLBACK_FILE)
  raw <- read_excel(FALLBACK_FILE)
  names(raw) <- gsub(" ", "_", toupper(trimws(names(raw))))
  nass <- raw %>%
    filter(
      grepl("PLANTED",  DATA_ITEM, ignore.case = TRUE),
      grepl("SOYBEANS", DATA_ITEM, ignore.case = TRUE),
      YEAR >= YEAR_MIN, YEAR <= YEAR_MAX
    ) %>%
    transmute(
      year  = as.integer(YEAR),
      week  = as.Date(WEEK_ENDING),
      DOY   = as.integer(strftime(week, "%j")),
      value = VALUE / 100
    ) %>%
    filter(!is.na(value), !is.na(DOY))
}

cat(sprintf("[Progress] %d records | %d years (%d–%d)\n",
            nrow(nass), n_distinct(nass$year),
            min(nass$year), max(nass$year)))

## ── Step 2: Fit logistic model per year ──────────────────────────────────────
fit_logistic <- function(df) {
  tryCatch(
    nls(value ~ 1 / (1 + exp(-b * (DOY - c))),
        data  = df,
        start = list(b = 0.10, c = 135)),
    error = function(e) NULL
  )
}

## DOY at a given % threshold, inverted from logistic parameters
doy_at_pct <- function(b, c, pct) c - log(100 / pct - 1) / b

models <- nass %>%
  group_by(year) %>%
  nest() %>%
  mutate(
    model  = map(data, fit_logistic),
    failed = map_lgl(model, is.null)
  )

n_failed <- sum(models$failed)
if (n_failed > 0)
  message(sprintf("[Progress] %d year(s) did not converge and are excluded.", n_failed))

## Extract per-year parameters and threshold DOYs
params_df <- models %>%
  filter(!failed) %>%
  mutate(
    b      = map_dbl(model, ~ coef(.x)["b"]),
    c      = map_dbl(model, ~ coef(.x)["c"]),
    doy_10 = map2_dbl(b, c, ~ doy_at_pct(.x, .y, 10)),
    doy_50 = map2_dbl(b, c, ~ doy_at_pct(.x, .y, 50))
  ) %>%
  select(year, b, c, doy_10, doy_50)

## Generate predicted curves (DOY 95–175)
DOY_seq <- 95:175

pred_curves <- params_df %>%
  mutate(pred = map2(b, c, ~ data.frame(
    DOY      = DOY_seq,
    progress = 100 / (1 + exp(-.x * (DOY_seq - .y)))
  ))) %>%
  select(year, doy_10, doy_50, pred) %>%
  unnest(pred)

## Average curve and average threshold DOYs
avg_curve  <- pred_curves %>%
  group_by(DOY) %>%
  summarise(progress = mean(progress, na.rm = TRUE), .groups = "drop")

avg_doy_10 <- mean(params_df$doy_10, na.rm = TRUE)
avg_doy_50 <- mean(params_df$doy_50, na.rm = TRUE)

cat(sprintf("[Progress] Avg DOY at 10%% planted : %.1f (~%s)\n",
            avg_doy_10,
            format(as.Date(paste0("2023-", round(avg_doy_10)), "%Y-%j"), "%b %d")))
cat(sprintf("[Progress] Avg DOY at 50%% planted : %.1f (~%s)\n",
            avg_doy_50,
            format(as.Date(paste0("2023-", round(avg_doy_50)), "%Y-%j"), "%b %d")))

## ── Shared scale helpers ──────────────────────────────────────────────────────
doy_breaks  <- c(100, 110, 120, 130, 140, 150, 160, 170)
doy_labels  <- c("100\n(Apr 10)", "110\n(Apr 20)", "120\n(Apr 30)",
                 "130\n(May 10)", "140\n(May 20)", "150\n(May 30)",
                 "160\n(Jun 9)",  "170\n(Jun 19)")

## ── Figure 1: all years, 10% and 50% thresholds ──────────────────────────────
plot_all <- ggplot() +
  geom_line(
    data    = pred_curves,
    aes(x = DOY, y = progress, group = year),
    colour = "grey70", linewidth = 0.35, alpha = 0.7
  ) +
  geom_hline(yintercept = THRESHOLDS, linetype = "dashed",
             colour = "grey30", linewidth = 0.45) +
  geom_line(
    data    = avg_curve,
    aes(x = DOY, y = progress),
    colour = "#00BF7D", linewidth = 1.5
  ) +
  annotate("text",
           x = avg_doy_10 + 1.5, y = 13,
           label = sprintf("10%% avg: DOY %.0f (~%s)",
                           avg_doy_10,
                           format(as.Date(paste0("2023-", round(avg_doy_10)), "%Y-%j"), "%b %d")),
           hjust = 0, size = 3.0, colour = "grey20") +
  annotate("text",
           x = avg_doy_50 + 1.5, y = 53,
           label = sprintf("50%% avg: DOY %.0f (~%s)",
                           avg_doy_50,
                           format(as.Date(paste0("2023-", round(avg_doy_50)), "%Y-%j"), "%b %d")),
           hjust = 0, size = 3.0, colour = "#00BF7D") +
  temp +
  scale_x_continuous(name = "Day of year", breaks = doy_breaks, labels = doy_labels) +
  scale_y_continuous(name = "Soybean planting progress (%)",
                     limits = c(0, 100), breaks = seq(0, 100, 20)) +
  labs(caption = sprintf(
    "Arkansas soybeans, USDA-NASS %d–%d (n = %d years). Grey = individual years; green = average.",
    min(params_df$year), max(params_df$year), nrow(params_df)
  )) +
  theme(plot.caption = element_text(size = 7.5, colour = "grey40"))

ggsave("figures/fig_progress_all.tiff",
       plot = plot_all, width = 17, height = 13, units = "cm",
       dpi = 600, compression = "lzw", bg = "white")
cat("[Progress] Saved: figures/fig_progress_all.tiff\n")

## ── Figure 2: latest N years highlighted ─────────────────────────────────────
recent_years  <- sort(unique(params_df$year), decreasing = TRUE)[seq_len(N_RECENT)]
year_colours  <- setNames(
  scales::hue_pal()(N_RECENT),
  as.character(sort(recent_years))
)

pred_bg     <- filter(pred_curves, !year %in% recent_years)
pred_recent <- filter(pred_curves, year  %in% recent_years) %>%
  mutate(year_f = factor(year))

## Per-year threshold annotations for recent years
thresh_recent <- params_df %>%
  filter(year %in% recent_years) %>%
  mutate(year_f = factor(year))

plot_recent <- ggplot() +
  ## Background years (grey)
  geom_line(
    data    = pred_bg,
    aes(x = DOY, y = progress, group = year),
    colour = "grey80", linewidth = 0.3, alpha = 0.6
  ) +
  ## Threshold lines
  geom_hline(yintercept = THRESHOLDS, linetype = "dashed",
             colour = "grey35", linewidth = 0.45) +
  ## Recent year curves (coloured)
  geom_line(
    data = pred_recent,
    aes(x = DOY, y = progress, colour = year_f),
    linewidth = 1.1
  ) +
  ## 10% threshold markers (points on each curve)
  geom_point(
    data = thresh_recent,
    aes(x = doy_10, y = 10, colour = year_f),
    size = 2.5, shape = 21, fill = "white", stroke = 1.2
  ) +
  ## 50% threshold markers
  geom_point(
    data = thresh_recent,
    aes(x = doy_50, y = 50, colour = year_f),
    size = 2.5
  ) +
  ## Labels next to each 50% point
  geom_text(
    data = thresh_recent %>% arrange(doy_50) %>%
      mutate(nudge = ifelse(row_number() %% 2 == 0, 10, -10)),
    aes(x = doy_50 + nudge, y = 53, label = sprintf("%d\nDOY %.0f", year, doy_50),
        colour = year_f),
    size = 2.6, lineheight = 0.85
  ) +
  ## Labels next to each 10% point
  geom_text(
    data = thresh_recent %>% arrange(doy_10) %>%
      mutate(nudge = ifelse(row_number() %% 2 == 0, 8, -8)),
    aes(x = doy_10 + nudge, y = 13, label = sprintf("DOY %.0f", doy_10),
        colour = year_f),
    size = 2.4
  ) +
  temp +
  scale_colour_manual(
    values = year_colours,
    name   = "Year"
  ) +
  scale_x_continuous(name = "Day of year", breaks = doy_breaks, labels = doy_labels) +
  scale_y_continuous(name = "Soybean planting progress (%)",
                     limits = c(0, 100), breaks = seq(0, 100, 20)) +
  labs(caption = sprintf(
    "Arkansas soybeans, USDA-NASS. Bold lines = %d–%d; grey = %d–%d background. Open circles = 10%% planted; filled = 50%%.",
    min(recent_years), max(recent_years),
    min(params_df$year), max(params_df$year)
  )) +
  theme(
    legend.position  = "top",
    plot.caption     = element_text(size = 7.5, colour = "grey40")
  )

ggsave("figures/fig_progress_recent.tiff",
       plot = plot_recent, width = 17, height = 13, units = "cm",
       dpi = 600, compression = "lzw", bg = "white")
cat("[Progress] Saved: figures/fig_progress_recent.tiff\n")

## ── Console summary ───────────────────────────────────────────────────────────
cat(sprintf("\n── Summary: %d–%d ────────────────────────────────\n",
            min(params_df$year), max(params_df$year)))
params_df %>%
  transmute(
    year,
    doy_10,
    date_10 = format(as.Date(paste0(year, "-", round(doy_10)), "%Y-%j"), "%b %d"),
    doy_50,
    date_50 = format(as.Date(paste0(year, "-", round(doy_50)), "%Y-%j"), "%b %d")
  ) %>%
  print(n = Inf)

