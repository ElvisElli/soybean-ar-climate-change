## ============================================================
## 04-ptq-analysis.R  —  Photothermal Quotient (PTQ) analysis
##
## Addresses reviewer comment on mechanistic explanation of yield
## responses. PTQ = mean daily Radn / (mean daily Tmean - Tbase)
## computed over the critical period: StartPodDAS → MaturityDAS
## (EarlyPodDevelopment → Maturing in APSIM phenology).
##
## Requires: simulated-scenarios-df.rds WITH StartPodDAS column
##           (re-run simulation with updated template first)
##
## Outputs:
##   data/outputs/ptq-results.rds        — PTQ per cell × year × scenario
##   figures/fig11 - ptq map.tiff        — spatial PTQ map across scenarios
##   figures/fig12 - ptq change map.tiff — PTQ change vs baseline map
##   figures/fig13 - ptq yield relationship.tiff — PTQ vs yield scatter
## ============================================================

suppressPackageStartupMessages({
  library(apsimx)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(ggplot2)
  library(sf)
  library(stars)
  library(cowplot)
  library(doParallel)
  library(foreach)
})

source("code/utils/plot-theme.R")
dir.create("figures",      showWarnings = FALSE)
dir.create("data/outputs", showWarnings = FALSE, recursive = TRUE)

## ── Settings ─────────────────────────────────────────────────────────────────
WEATHER_DIR <- "intermediate-data/weather"   # .met files named <cellid>.met
TBASE       <- 10                            # base temperature (°C), Zanon et al. 2016
N_CORES     <- max(1L, parallel::detectCores() - 2L)

## ── Load simulation results ───────────────────────────────────────────────────
sim <- readRDS("data/outputs/simulated-scenarios-df.rds")
sim <- sim[, nzchar(names(sim)), drop = FALSE]
sim <- as_tibble(sim) %>%
  rename(any_of(c(date = "Date")))

## Check StartPodDAS is present
if (!"StartPodDAS" %in% names(sim))
  stop("StartPodDAS not found in simulated-scenarios-df.rds.\n",
       "Re-run the simulation with the updated template first.")

## Sowing day-of-year per scenario: May-3 = DOY 123, May-15 = DOY 135
sim <- sim %>%
  mutate(
    year        = lubridate::year(date),
    sowing_doy  = ifelse(grepl("early_sowing", scenario), 123L, 135L),  # May-3=123, May-15=135
    ## Critical window as calendar DOY (handling year wrap is unnecessary — window stays in-season)
    pod_doy     = sowing_doy + StartPodDAS,
    mat_doy     = sowing_doy + MaturityDAS
  )

## ── Read .met files and compute PTQ ──────────────────────────────────────────
## Group simulation rows by cellid so each .met file is read only once.

cells <- unique(sim$cellid)
cat(sprintf("[PTQ] Computing PTQ for %d cells using %d cores...\n",
            length(cells), N_CORES))

cl <- makeCluster(N_CORES)
registerDoParallel(cl)

ptq_list <- foreach(
  cid        = cells,
  .packages  = c("apsimx", "dplyr"),
  .errorhandling = "pass"
) %dopar% {

  met_path <- file.path(WEATHER_DIR, paste0(cid, ".met"))
  if (!file.exists(met_path)) return(NULL)

  ## Read daily weather
  met <- tryCatch(
    read_apsim_met(met_path, verbose = FALSE),
    error = function(e) NULL
  )
  if (is.null(met)) return(NULL)

  met_df <- as.data.frame(met) %>%
    dplyr::select(year, day, radn, maxt, mint) %>%
    dplyr::mutate(tmean = (maxt + mint) / 2)

  ## Rows for this cell
  cell_rows <- dplyr::filter(sim, cellid == cid) %>%
    dplyr::select(cellid, x, y, scenario, co2, year, pod_doy, mat_doy,
                  Yield_kgha, StartPodDAS, MaturityDAS)

  ## For each row, subset met to critical window and compute PTQ
  result <- vector("list", nrow(cell_rows))
  for (k in seq_len(nrow(cell_rows))) {
    r   <- cell_rows[k, ]
    win <- dplyr::filter(met_df,
                         year == r$year,
                         day  >= r$pod_doy,
                         day  <= r$mat_doy)
    if (nrow(win) < 5) {
      result[[k]] <- dplyr::mutate(r,
        ptq              = NA_real_,
        critical_radn    = NA_real_,
        critical_tmean   = NA_real_,
        critical_n_days  = 0L)
    } else {
      result[[k]] <- dplyr::mutate(r,
        ptq              = mean(win$radn,  na.rm = TRUE) /
                           (mean(win$tmean, na.rm = TRUE) - TBASE),
        critical_radn    = mean(win$radn,  na.rm = TRUE),
        critical_tmean   = mean(win$tmean, na.rm = TRUE),
        critical_n_days  = nrow(win))
    }
  }
  dplyr::bind_rows(result)
}

stopCluster(cl)

ptq_df <- dplyr::bind_rows(Filter(Negate(is.null), ptq_list))
cat(sprintf("[PTQ] Done. %d rows, %.1f%% with valid PTQ.\n",
            nrow(ptq_df), mean(!is.na(ptq_df$ptq)) * 100))

saveRDS(ptq_df, "data/outputs/ptq-results.rds")
cat("[PTQ] Saved to data/outputs/ptq-results.rds\n\n")

## ── Shared spatial setup (mirrors 02-analysis.R) ─────────────────────────────
ark <- sf::st_read("data/raw/cropland/cb_2018_us_state_20m/cb_2018_us_state_20m.shp",
                   quiet = TRUE) %>%
  subset(STUSPS == "AR") %>%
  sf::st_transform(5070)

usa_counties <- sf::st_read(
  "data/raw/cropland/Elvis-Crop-Data/Arkansas_Counties_4269.shp", quiet = TRUE)

df2 <- stars::st_as_stars(sf::st_bbox(ark), dx = 2500, dy = 2500)

to_stars_grid <- function(df, dims) {
  s <- stars::st_as_stars(df, dims = dims, xy = c("x", "y"), proxy = TRUE)
  sf::st_crs(s) <- "epsg:4326"
  s <- sf::st_transform(s, 5070)
  stars::st_warp(s, df2, no_data_value = NA)
}

map_theme <- theme(
  axis.title      = element_blank(),
  axis.text       = element_blank(),
  legend.position  = "top",
  legend.direction = "horizontal",
  legend.title     = element_text(size = 11, hjust = 0),
  strip.text       = element_text(size = 10)
)

scenario_levels <- c("baseline", "climate_change", "longer_mat",
                     "early_sowing", "early_sowing_longer_mat")
scenario_labels <- c("Baseline", "2°C-increase", "Late-Maturing",
                     "Early Sowing", "LM & ES")

## ── Figure 11: PTQ spatial map ───────────────────────────────────────────────
## Mean PTQ per field (averaged across 40 years), CO2 = 350, all scenarios

p11_data <- ptq_df %>%
  filter(co2 == 350, !is.na(ptq)) %>%
  group_by(x, y, scenario) %>%
  summarise(ptq_mean = mean(ptq, na.rm = TRUE), .groups = "drop") %>%
  mutate(scenario = factor(scenario, levels = scenario_levels,
                           labels = scenario_labels))

df_p11 <- to_stars_grid(p11_data, dims = c("x", "y", "scenario"))

plot11 <- ggplot() +
  geom_sf(data = ark, fill = "grey50", color = "black") +
  geom_stars(data = df_p11, aes(fill = ptq_mean)) +
  geom_sf(data = usa_counties, fill = NA, colour = "black", linewidth = 0.5) +
  coord_sf(xlim = c(360000, 570000)) +
  facet_wrap(~ scenario, nrow = 1) +
  temp +
  scale_fill_viridis_c(
    option = "plasma", direction = -1, na.value = "grey50",
    name = "PTQ (MJ/m²/°C·day)") +
  map_theme

ggsave("figures/fig11 - ptq map.tiff", plot = plot11,
       width = 30, height = 10, units = "cm", dpi = 600,
       compression = "lzw", bg = "white")
cat("[PTQ] Saved figures/fig11 - ptq map.tiff\n")

## ── Figure 12: PTQ change vs baseline ────────────────────────────────────────
## Mean PTQ change (vs baseline) per field, showing how each strategy
## shifts the photothermal environment

ptq_baseline <- ptq_df %>%
  filter(scenario == "baseline", !is.na(ptq)) %>%
  group_by(x, y, year) %>%
  summarise(ptq_bl = mean(ptq, na.rm = TRUE), .groups = "drop")

p12_data <- ptq_df %>%
  filter(scenario != "baseline", co2 == 350, !is.na(ptq)) %>%
  left_join(ptq_baseline, by = c("x", "y", "year")) %>%
  mutate(ptq_chg = ptq - ptq_bl) %>%
  group_by(x, y, scenario) %>%
  summarise(ptq_chg_mean = mean(ptq_chg, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    scenario  = factor(scenario,
                       levels = scenario_levels[-1],
                       labels = scenario_labels[-1]),
    ptq_bin   = cut(ptq_chg_mean,
                    breaks = c(-2, -0.5, 0, 0.5, 1.5, 3),
                    labels = c("< -0.5", "-0.5 to 0", "0 to 0.5",
                               "0.5 to 1.5", "> 1.5"),
                    include.lowest = TRUE)
  )

df_p12 <- to_stars_grid(p12_data, dims = c("x", "y", "scenario"))

plot12 <- ggplot() +
  geom_sf(data = ark, fill = "grey50", color = "black") +
  geom_stars(data = df_p12, aes(fill = ptq_bin)) +
  geom_sf(data = usa_counties, fill = NA, colour = "black", linewidth = 0.5) +
  coord_sf(xlim = c(360000, 570000)) +
  facet_wrap(~ scenario, nrow = 1) +
  temp +
  scale_fill_manual(
    values = c("< -0.5"     = "#b10026",
               "-0.5 to 0"  = "#fc8d59",
               "0 to 0.5"   = "#e5f5f9",
               "0.5 to 1.5" = "#74c476",
               "> 1.5"      = "#005824"),
    na.value = "grey50", na.translate = FALSE,
    name = "PTQ change vs baseline\n(MJ/m²/°C·day)") +
  map_theme

ggsave("figures/fig12 - ptq change map.tiff", plot = plot12,
       width = 28, height = 10, units = "cm", dpi = 600,
       compression = "lzw", bg = "white")
cat("[PTQ] Saved figures/fig12 - ptq change map.tiff\n")

## ── Figure 13: PTQ vs yield scatter ──────────────────────────────────────────
## Shows the mechanistic link between PTQ and yield across scenarios

p13_data <- ptq_df %>%
  filter(co2 == 350, !is.na(ptq)) %>%
  mutate(scenario = factor(scenario, levels = scenario_levels,
                           labels = scenario_labels))

plot13 <- ggplot(p13_data, aes(x = ptq, y = Yield_kgha, colour = scenario)) +
  geom_point(size = 0.8, alpha = 0.15) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  temp +
  labs(x = "Photothermal Quotient (MJ/m²/°C·day)",
       y = "Seed yield (kg/ha)",
       colour = "Scenario") +
  scale_colour_manual(values = c("Baseline"    = "#1f78b4",
                                 "2°C-increase"= "#e31a1c",
                                 "Late-Maturing"= "#33a02c",
                                 "Early Sowing" = "#ff7f00",
                                 "LM & ES"      = "#6a3d9a")) +
  theme(legend.position = "top") +
  facet_wrap(~ scenario, nrow = 1)

ggsave("figures/fig13 - ptq yield relationship.tiff", plot = plot13,
       width = 30, height = 10, units = "cm", dpi = 600,
       compression = "lzw", bg = "white")
cat("[PTQ] Saved figures/fig13 - ptq yield relationship.tiff\n")

## ── Console stats for manuscript ─────────────────────────────────────────────
cat(paste(rep("─", 60), collapse = ""), "\n")
cat("PTQ SUMMARY (CO2 = 350, field × year means)\n\n")

ptq_df %>%
  filter(co2 == 350, !is.na(ptq)) %>%
  group_by(scenario) %>%
  summarise(
    mean_ptq   = round(mean(ptq, na.rm = TRUE), 2),
    sd_ptq     = round(sd(ptq,   na.rm = TRUE), 2),
    min_ptq    = round(min(ptq,  na.rm = TRUE), 2),
    max_ptq    = round(max(ptq,  na.rm = TRUE), 2),
    .groups    = "drop"
  ) %>%
  print()

cat("\nPTQ–YIELD REGRESSION SLOPES (kg/ha per unit PTQ, by scenario):\n")
ptq_df %>%
  filter(co2 == 350, !is.na(ptq)) %>%
  group_by(scenario) %>%
  summarise(
    slope = round(coef(lm(Yield_kgha ~ ptq))[["ptq"]], 0),
    r2    = round(summary(lm(Yield_kgha ~ ptq))$r.squared, 3),
    .groups = "drop"
  ) %>%
  print()

cat(paste(rep("─", 60), collapse = ""), "\n")
