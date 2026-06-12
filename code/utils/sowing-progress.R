## ============================================================
## sowing-progress.R
## Soybean planting progress in Arkansas (USDA-NASS, 1990–present)
##
## Fits logistic curve per year to weekly % planted data:
##   progress(DOY) = 100 / (1 + exp(-b * (DOY - c)))
##   c = DOY at 50% planted  |  b = steepness
##
## Data: auto-downloaded via USDA-NASS Quick Stats API, cached locally.
##       Falls back to data/raw/progress.xlsx if API is unavailable.
##
## Figures:
##   fig_progress_all.tiff    — all years (grey) + avg (green), 10% & 50% annotated
##   fig_progress_recent.tiff — last 5 years (same colour) + 5-yr avg (bold),
##                              arrows + text inside plot for 10% and 50%
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
NASS_API_KEY  <- "D228A372-93ED-3BF7-9699-D2D0DDD3C88D"
FALLBACK_FILE <- "data/raw/progress.xlsx"
CACHE_FILE    <- "data/raw/nass-progress-cache.rds"
YEAR_MIN      <- 1990
YEAR_MAX      <- as.integer(format(Sys.Date(), "%Y")) - 1L  # exclude current (incomplete) year
N_RECENT      <- 5
RECENT_COL    <- "#31a354"   # single colour for all recent-year lines
AVG_COL       <- "#00BF7D"   # average curve colour

## ── Step 1: Download or load data ────────────────────────────────────────────
download_nass <- function(api_key) {
  message("[Progress] Downloading from USDA-NASS Quick Stats API...")
  resp <- tryCatch(
    GET("https://quickstats.nass.usda.gov/api/api_GET/",
        query   = list(key               = api_key,
                       commodity_desc    = "SOYBEANS",
                       statisticcat_desc = "PROGRESS",
                       unit_desc         = "PCT PLANTED",
                       state_alpha       = "AR",
                       freq_desc         = "WEEKLY",
                       format            = "JSON"),
        timeout(30)),
    error = function(e) { message("[Progress] API error: ", e$message); NULL }
  )
  if (is.null(resp) || http_error(resp)) {
    message("[Progress] API returned status ", if (!is.null(resp)) status_code(resp) else "N/A")
    return(NULL)
  }
  j <- tryCatch(fromJSON(content(resp, "text", encoding = "UTF-8")),
                error = function(e) NULL)
  if (is.null(j) || is.null(j$data) || nrow(j$data) == 0) {
    message("[Progress] API returned empty data.")
    return(NULL)
  }
  df <- as.data.frame(j$data) %>%
    transmute(
      year  = as.integer(year),
      week  = as.Date(week_ending),
      DOY   = as.integer(strftime(week, "%j")),
      value = suppressWarnings(as.numeric(Value)) / 100
    ) %>%
    filter(!is.na(value), !is.na(DOY), year >= YEAR_MIN, year <= YEAR_MAX)
  message(sprintf("[Progress] Downloaded %d records (%d–%d).",
                  nrow(df), min(df$year), max(df$year)))
  df
}

nass <- download_nass(NASS_API_KEY)

if (!is.null(nass)) {
  saveRDS(nass, CACHE_FILE)  # cache for offline use
} else if (file.exists(CACHE_FILE)) {
  nass <- readRDS(CACHE_FILE)
  message(sprintf("[Progress] Loaded from cache (%d–%d).", min(nass$year), max(nass$year)))
} else {
  message("[Progress] Loading from ", FALLBACK_FILE)
  raw <- read_excel(FALLBACK_FILE)
  names(raw) <- gsub(" ", "_", toupper(trimws(names(raw))))
  nass <- raw %>%
    filter(grepl("PLANTED",  DATA_ITEM, ignore.case = TRUE),
           grepl("SOYBEANS", DATA_ITEM, ignore.case = TRUE),
           YEAR >= YEAR_MIN, YEAR <= YEAR_MAX) %>%
    transmute(year  = as.integer(YEAR),
              week  = as.Date(WEEK_ENDING),
              DOY   = as.integer(strftime(week, "%j")),
              value = VALUE / 100) %>%
    filter(!is.na(value), !is.na(DOY))
}

cat(sprintf("[Progress] %d records | %d years (%d–%d)\n",
            nrow(nass), n_distinct(nass$year), min(nass$year), max(nass$year)))

## ── Step 2: Fit logistic model per year ──────────────────────────────────────
fit_logistic <- function(df) {
  tryCatch(
    nls(value ~ 1 / (1 + exp(-b * (DOY - c))),
        data = df, start = list(b = 0.10, c = 135)),
    error = function(e) NULL
  )
}

doy_at_pct <- function(b, c, pct) c - log(100 / pct - 1) / b

models <- nass %>%
  group_by(year) %>%
  nest() %>%
  mutate(model  = map(data, fit_logistic),
         failed = map_lgl(model, is.null))

n_failed <- sum(models$failed)
if (n_failed > 0)
  message(sprintf("[Progress] %d year(s) did not converge and are excluded.", n_failed))

params_df <- models %>%
  filter(!failed) %>%
  mutate(b      = map_dbl(model, ~ coef(.x)["b"]),
         c      = map_dbl(model, ~ coef(.x)["c"]),
         doy_10 = map2_dbl(b, c, ~ doy_at_pct(.x, .y, 10)),
         doy_50 = map2_dbl(b, c, ~ doy_at_pct(.x, .y, 50))) %>%
  select(year, b, c, doy_10, doy_50)

DOY_seq <- 95:175

pred_curves <- params_df %>%
  mutate(pred = map2(b, c, ~ data.frame(
    DOY      = DOY_seq,
    progress = 100 / (1 + exp(-.x * (DOY_seq - .y)))
  ))) %>%
  select(year, doy_10, doy_50, pred) %>%
  unnest(pred)

avg_curve  <- pred_curves %>%
  group_by(DOY) %>%
  summarise(progress = mean(progress, na.rm = TRUE), .groups = "drop")

avg_doy_10 <- mean(params_df$doy_10, na.rm = TRUE)
avg_doy_50 <- mean(params_df$doy_50, na.rm = TRUE)

cat(sprintf("[Progress] Avg DOY at 10%% planted: %.1f (~%s)\n", avg_doy_10,
            format(as.Date(paste0("2023-", round(avg_doy_10)), "%Y-%j"), "%b %d")))
cat(sprintf("[Progress] Avg DOY at 50%% planted: %.1f (~%s)\n", avg_doy_50,
            format(as.Date(paste0("2023-", round(avg_doy_50)), "%Y-%j"), "%b %d")))

## ── Shared axis settings ─────────────────────────────────────────────────────
doy_breaks <- c(100, 110, 120, 130, 140, 150, 160, 170)
doy_labels <- c("100\n(Apr 10)", "110\n(Apr 20)", "120\n(Apr 30)",
                "130\n(May 10)", "140\n(May 20)", "150\n(May 30)",
                "160\n(Jun 9)",  "170\n(Jun 19)")

## ── Figure 1: all years + average, 10% and 50% annotated ────────────────────
## Text sits above its dashed line (10%: lower-left area; 50%: mid-right area)
plot_all <- ggplot() +
  geom_line(data = pred_curves,
            aes(x = DOY, y = progress, group = year),
            colour = "grey70", linewidth = 0.55, alpha = 0.7) +
  geom_hline(yintercept = c(10, 50), linetype = "dashed",
             colour = "grey40", linewidth = 0.45) +
  geom_line(data = avg_curve, aes(x = DOY, y = progress),
            colour = AVG_COL, linewidth = 1.5) +
  ## 10% label — placed right of the curve crossing, above the dashed line
  annotate("text", x = avg_doy_10 + 2, y = 15,
           label = sprintf("10%% avg: DOY %.0f (%s)", avg_doy_10,
                           format(as.Date(paste0("2023-", round(avg_doy_10)), "%Y-%j"), "%b %d")),
           hjust = 0, size = 3.0, colour = "black", lineheight = 0.9) +
  ## 50% label — placed right of the curve crossing, above the dashed line
  annotate("text", x = avg_doy_50 + 2, y = 55,
           label = sprintf("50%% avg: DOY %.0f (%s)", avg_doy_50,
                           format(as.Date(paste0("2023-", round(avg_doy_50)), "%Y-%j"), "%b %d")),
           hjust = 0, size = 3.0, colour = "black", lineheight = 0.9) +
  temp +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "grey90")) +
  scale_x_continuous(name = "Day of year", breaks = doy_breaks, labels = doy_labels,
                     expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(name = "Soybean planting progress (%)",
                     limits = c(0, 100), breaks = seq(0, 100, 20))

ggsave("figures/fig_progress_all.tiff", plot = plot_all,
       width = 16, height = 11, units = "cm",
       dpi = 600, compression = "lzw", bg = "white")
cat("[Progress] Saved: figures/fig_progress_all.tiff\n")

## ── Figure 2: last 5 years (same colour) + 5-yr average, arrows + labels ────
recent_years <- sort(unique(params_df$year), decreasing = TRUE)[seq_len(N_RECENT)]

pred_bg     <- filter(pred_curves, !year %in% recent_years)
pred_recent <- filter(pred_curves,  year %in% recent_years)

## 5-year average curve and thresholds
avg5_b      <- mean(filter(params_df, year %in% recent_years)$b)
avg5_c      <- mean(filter(params_df, year %in% recent_years)$c)
avg5_doy_10 <- doy_at_pct(avg5_b, avg5_c, 10)
avg5_doy_50 <- doy_at_pct(avg5_b, avg5_c, 50)
avg5_curve  <- data.frame(DOY      = DOY_seq,
                          progress = 100 / (1 + exp(-avg5_b * (DOY_seq - avg5_c))))

## Arrow annotation: 10% text above-right of crossing; 50% text above-right of crossing
## Both arrows point downward onto the average curve — positioned to avoid line overlap
arr_10_text_x <- avg5_doy_10 + 10
arr_10_text_y <- 25
arr_50_text_x <- avg5_doy_50 + 5
arr_50_text_y <- 63

plot_recent <- ggplot() +
  ## Background years (light grey)
  geom_line(data = pred_bg,
            aes(x = DOY, y = progress, group = year),
            colour = "grey75", linewidth = 0.3, alpha = 0.6) +
  ## Recent year curves — all same colour, thin
  geom_line(data = pred_recent,
            aes(x = DOY, y = progress, group = year),
            colour = RECENT_COL, linewidth = 0.45, alpha = 0.7) +
  ## 5-year average — bold
  geom_line(data = avg5_curve, aes(x = DOY, y = progress),
            colour = RECENT_COL, linewidth = 2.0) +
  ## Dashed threshold lines
  geom_hline(yintercept = c(10, 50), linetype = "dashed",
             colour = "grey40", linewidth = 0.4) +
  ## Arrow: 10% label → curve
  annotate("segment",
           x = arr_10_text_x, y = arr_10_text_y - 3,
           xend = avg5_doy_10 + 1, yend = 11,
           arrow = arrow(length = unit(0.13, "cm"), type = "closed"),
           colour = "black", linewidth = 0.45) +
  ## Arrow: 50% label → curve
  annotate("segment",
           x = arr_50_text_x, y = arr_50_text_y - 3,
           xend = avg5_doy_50 + 1, yend = 51,
           arrow = arrow(length = unit(0.13, "cm"), type = "closed"),
           colour = "black", linewidth = 0.45) +
  ## Label: 10%
  annotate("text",
           x = arr_10_text_x, y = arr_10_text_y,
           label = sprintf("10%% planted\nDOY %.0f (%s)", avg5_doy_10,
                           format(as.Date(paste0("2023-", round(avg5_doy_10)), "%Y-%j"), "%b %d")),
           hjust = 0, size = 2.9, colour = "black", lineheight = 0.95) +
  ## Label: 50%
  annotate("text",
           x = arr_50_text_x, y = arr_50_text_y,
           label = sprintf("50%% planted\nDOY %.0f (%s)", avg5_doy_50,
                           format(as.Date(paste0("2023-", round(avg5_doy_50)), "%Y-%j"), "%b %d")),
           hjust = 0, size = 2.9, colour = "black", lineheight = 0.95) +
  temp +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "grey90")) +
  scale_x_continuous(name = "Day of year", breaks = doy_breaks, labels = doy_labels,
                     expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(name = "Soybean planting progress (%)",
                     limits = c(0, 100), breaks = seq(0, 100, 20))

ggsave("figures/fig_progress_recent.tiff", plot = plot_recent,
       width = 16, height = 11, units = "cm",
       dpi = 600, compression = "lzw", bg = "white")
cat("[Progress] Saved: figures/fig_progress_recent.tiff\n")

## ── Console summary ───────────────────────────────────────────────────────────
cat(sprintf("\n── Per-year 50%% and 10%% planting DOY (%d–%d) ──\n",
            min(params_df$year), max(params_df$year)))
params_df %>%
  transmute(year,
            doy_10 = round(doy_10, 1),
            date_10 = format(as.Date(paste0(year, "-", round(doy_10)), "%Y-%j"), "%b %d"),
            doy_50 = round(doy_50, 1),
            date_50 = format(as.Date(paste0(year, "-", round(doy_50)), "%Y-%j"), "%b %d")) %>%
  arrange(desc(year)) %>%
  print(n = Inf)
