## ============================================================
## 04-ptq-analysis.R  —  Photothermal Quotient (PTQ) analysis
##
## WHAT IS PTQ?
## ────────────
## The Photothermal Quotient (Zanon et al. 2016) captures the combined
## effect of solar radiation and temperature on seed yield:
##
##   PTQ = mean_daily_radiation (MJ/m²/day)
##         ────────────────────────────────────────────────
##         mean_daily_temperature (°C) − Tbase (10 °C)
##
## A higher PTQ means more radiation relative to heat accumulation, which
## is favourable for seed filling (more photosynthate, less heat stress).
##
## CRITICAL WINDOW (following Zanon et al. 2016):
##   From: EarlyPodDevelopment (StartPodDAS days after sowing)
##   To  : Maturing            (MaturityDAS days after sowing)
##   These DAS values come directly from the APSIM phenology output.
##
## WHY DAILY WEATHER FILES?
##   APSIM reports only season-averaged weather, so we re-read the daily
##   .met files and subset them to the exact critical window per field × year.
##
## OUTPUTS:
##   data/outputs/ptq-results.rds             — PTQ per cell × year × scenario
##   figures/fig11 - ptq map.tiff             — mean PTQ map across scenarios
##   figures/fig12 - ptq change map.tiff      — PTQ change vs baseline map
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
##
## LOCAL_DATA_CACHE: same optional local-SSD path used in 01-simulation.R.
## Set to NULL to auto-detect Box Drive (Windows) or intermediate-data/ (Linux).
LOCAL_DATA_CACHE <- "C:/temp/soybean-data"   # set to NULL if not using a cache

TBASE    <- 10    # base temperature (°C) — Zanon et al. 2016
N_CORES  <- max(1L, parallel::detectCores() - 2L)

## ── Auto-detect weather directory (mirrors 01-simulation.R) ──────────────────
## Priority: LOCAL_DATA_CACHE  >  Box Drive  >  repo intermediate-data/
detect_weather_dir <- function() {
  ## 1. Local cache
  if (!is.null(LOCAL_DATA_CACHE) && dir.exists(LOCAL_DATA_CACHE)) {
    p <- normalizePath(file.path(LOCAL_DATA_CACHE, "weather"), mustWork = FALSE)
    if (dir.exists(p)) {
      message("[PTQ] Weather: local cache — ", p)
      return(p)
    }
  }
  ## 2. Box Drive (Windows)
  if (.Platform$OS.type == "windows") {
    user_home  <- Sys.getenv("USERPROFILE", path.expand("~"))
    box_suffix <- file.path("_Projects", "Scale-Sims",
                            "soybean-ar-climate-change", "intermediate-data", "weather")
    box_mounts <- c("Box", "Box Sync", "Box Drive")
    candidates <- unlist(lapply(box_mounts,
                                function(m) file.path(user_home, m, box_suffix)))
    users_dir  <- dirname(user_home)
    if (dir.exists(users_dir)) {
      other_homes <- list.dirs(users_dir, recursive = FALSE)
      candidates  <- c(candidates,
                       unlist(lapply(other_homes, function(h)
                         unlist(lapply(box_mounts, function(m)
                           file.path(h, m, box_suffix))))))
    }
    found <- Filter(dir.exists, unique(candidates))
    if (length(found)) {
      message("[PTQ] Weather: Box Drive — ", found[[1]])
      return(normalizePath(found[[1]]))
    }
  }
  ## 3. Repo-relative paths (Linux / cloud / fallback)
  for (p in c("intermediate-data/weather", "data/raw/weather")) {
    if (dir.exists(p)) {
      message("[PTQ] Weather: repo path — ", normalizePath(p))
      return(normalizePath(p))
    }
  }
  stop("[PTQ] Weather directory not found.\n",
       "  Option A: copy weather/ to a local SSD and set LOCAL_DATA_CACHE at the top of this script.\n",
       "  Option B: ensure Box Drive is mounted under your user profile.\n",
       "  Option C: place .met files in intermediate-data/weather/ relative to the repo root.")
}

## normalizePath converts to an absolute path with OS-native separators.
## This is critical on Windows: parallel workers each have their own working
## directory and cannot resolve relative paths from the main session.
WEATHER_DIR <- normalizePath(detect_weather_dir(), mustWork = TRUE)
met_files   <- list.files(WEATHER_DIR, "\\.met$")
n_met       <- length(met_files)
message(sprintf("[PTQ] Found %d .met files in: %s", n_met, WEATHER_DIR))
if (n_met == 0)
  stop("[PTQ] No .met files found in: ", WEATHER_DIR)

## Show a few example filenames so we can confirm the naming convention
message("[PTQ] Example .met filenames: ",
        paste(head(met_files, 5), collapse = ", "))

## ── Load simulation results ───────────────────────────────────────────────────
message("[PTQ] Loading simulation results...")
sim <- readRDS("data/outputs/simulated-scenarios-df.rds")

## RDS is sometimes saved as a list of per-scenario data frames — bind if so.
if (is.list(sim) && !is.data.frame(sim))
  sim <- dplyr::bind_rows(Filter(Negate(is.null), sim))
sim <- as_tibble(sim) %>%
  rename(any_of(c(date = "Date")))

message(sprintf("[PTQ] Simulation data: %d rows × %d scenarios × %d cells",
                nrow(sim),
                length(unique(sim$scenario)),
                length(unique(sim$cellid))))

## Cross-check: do cellid values match .met filenames?
## .met files are expected to be named <cellid>.met (e.g. "12345.met").
## If the names don't match, every cell will silently return NULL.
met_ids_on_disk <- as.integer(sub("\\.met$", "", met_files))
sim_cellids     <- unique(sim$cellid)
n_match         <- sum(sim_cellids %in% met_ids_on_disk)
message(sprintf("[PTQ] cellid→.met match check: %d of %d simulation cells have a matching .met file",
                n_match, length(sim_cellids)))
if (n_match == 0)
  stop("[PTQ] No simulation cellids match any .met filename.\n",
       "  First 5 cellids in sim : ", paste(head(sim_cellids, 5), collapse = ", "), "\n",
       "  First 5 .met filenames : ", paste(head(met_files, 5), collapse = ", "), "\n",
       "  The .met files must be named <cellid>.met  (e.g. '12345.met').")
if (n_match < length(sim_cellids))
  message(sprintf("[PTQ] WARNING: %d cells have no .met file — they will be skipped.",
                  length(sim_cellids) - n_match))

## Check required columns
required_cols <- c("StartPodDAS", "MaturityDAS", "Yield_kgha",
                   "date", "scenario", "co2", "cellid", "x", "y")
missing_cols  <- setdiff(required_cols, names(sim))
if (length(missing_cols))
  stop("[PTQ] Missing required columns: ", paste(missing_cols, collapse = ", "), "\n",
       "  — 'StartPodDAS' missing means the simulation was run without it; re-run with\n",
       "    the updated APSIM template that reports [Soybean].Phenology.StartPodDAS.")

## ── Compute calendar DOY for critical window ──────────────────────────────────
##
## APSIM phenology outputs (StartPodDAS, MaturityDAS) are Days After Sowing.
## We convert to day-of-year so we can look them up in the daily .met files.
##
##   sowing_doy: May-3  (early_sowing scenarios) = DOY 123
##               May-15 (all others)              = DOY 135
##
##   pod_doy  = sowing_doy + StartPodDAS   ← start of critical window
##   mat_doy  = sowing_doy + MaturityDAS   ← end   of critical window
##
message("[PTQ] Converting phenology DAS → calendar DOY...")
sim <- sim %>%
  mutate(
    year        = lubridate::year(date),
    sowing_doy  = ifelse(grepl("early_sowing", scenario), 123L, 135L),
    pod_doy     = sowing_doy + StartPodDAS,
    mat_doy     = sowing_doy + MaturityDAS
  )

## Sanity check: pod_doy should always be before mat_doy
n_bad_window <- sum(sim$pod_doy >= sim$mat_doy, na.rm = TRUE)
if (n_bad_window > 0)
  warning(sprintf("[PTQ] %d rows have pod_doy >= mat_doy (bad phenology) — these will get NA PTQ.", n_bad_window))

## ── Compute PTQ from daily .met files ────────────────────────────────────────
##
## For each grid cell × year × scenario:
##   1. Read the cell's .met file once (avoids re-reading for each scenario).
##   2. Subset daily records to the critical window [pod_doy, mat_doy].
##   3. PTQ = mean(Radn) / (mean(Tmean) − Tbase)
##      where Tmean = (Tmax + Tmin) / 2 each day.
##   4. Rows with fewer than 5 days in the window get PTQ = NA.
##      (< 5 days likely signals a phenology/data error, not a real season.)
##
cells <- unique(sim$cellid)

## Pre-flight: verify at least one cell's .met file is reachable with the
## absolute path before launching the parallel cluster.
## This catches path problems (wrong directory, symlinks, network drives)
## without waiting 7 minutes for 4,651 workers to silently return NULL.
test_path <- file.path(WEATHER_DIR, paste0(cells[1], ".met"))
if (!file.exists(test_path))
  stop("[PTQ] Pre-flight check failed — cannot find .met file for cell ", cells[1], "\n",
       "  Expected path: ", test_path, "\n",
       "  WEATHER_DIR contains: ", paste(head(met_files, 3), collapse = ", "), " ...\n",
       "  Simulation cellids start with: ", paste(head(cells, 3), collapse = ", "), "\n",
       "  Check that LOCAL_DATA_CACHE points to the folder that contains the weather/ subfolder.")
message(sprintf("[PTQ] Pre-flight OK — %s exists.", basename(test_path)))

message(sprintf("[PTQ] Computing PTQ for %d cells using %d cores...", length(cells), N_CORES))
message(sprintf("[PTQ] Tbase = %d°C | critical window: EarlyPodDevelopment → Maturing", TBASE))
message("[PTQ] (This takes several minutes on the full grid — progress shown below)")

cl <- makeCluster(N_CORES)
registerDoParallel(cl)

ptq_list <- foreach(
  cid            = cells,
  .packages      = c("apsimx", "dplyr"),
  .export        = c("sim", "TBASE", "WEATHER_DIR"),
  .errorhandling = "pass"
) %dopar% {

  met_path <- file.path(WEATHER_DIR, paste0(cid, ".met"))
  if (!file.exists(met_path)) return(NULL)

  met <- tryCatch(
    read_apsim_met(met_path, verbose = FALSE),
    error = function(e) NULL
  )
  if (is.null(met)) return(NULL)

  ## Daily weather for this cell (all years)
  met_df <- as.data.frame(met) %>%
    dplyr::select(year, day, radn, maxt, mint) %>%
    dplyr::mutate(tmean = (maxt + mint) / 2)   # daily mean temperature

  ## All rows for this cell (all years × scenarios)
  cell_rows <- dplyr::filter(sim, cellid == cid) %>%
    dplyr::select(cellid, x, y, scenario, co2, year,
                  pod_doy, mat_doy, Yield_kgha, StartPodDAS, MaturityDAS)

  result <- vector("list", nrow(cell_rows))
  for (k in seq_len(nrow(cell_rows))) {
    r <- cell_rows[k, ]

    ## Subset met to the critical window for this year
    win <- dplyr::filter(met_df,
                         year == r$year,
                         day  >= r$pod_doy,
                         day  <= r$mat_doy)

    if (nrow(win) < 5) {
      ## Too few days — flag as NA rather than compute a meaningless value
      result[[k]] <- dplyr::mutate(r,
        ptq             = NA_real_,
        critical_radn   = NA_real_,   # mean daily radiation in window
        critical_tmean  = NA_real_,   # mean daily temperature in window
        window_days     = as.integer(nrow(win)))
    } else {
      mean_radn  <- mean(win$radn,  na.rm = TRUE)
      mean_tmean <- mean(win$tmean, na.rm = TRUE)
      result[[k]] <- dplyr::mutate(r,
        ptq            = mean_radn / (mean_tmean - TBASE),
        critical_radn  = mean_radn,
        critical_tmean = mean_tmean,
        window_days    = as.integer(nrow(win)))
    }
  }
  dplyr::bind_rows(result)
}

stopCluster(cl)

## ── Validate parallel output ──────────────────────────────────────────────────
## Filter to data frames only (error objects come through with .errorhandling = "pass")
ptq_ok  <- Filter(is.data.frame, ptq_list)
ptq_err <- Filter(function(x) inherits(x, "condition"), ptq_list)

message(sprintf("[PTQ] Cells processed: %d ok, %d errored, %d no .met file",
                length(ptq_ok),
                length(ptq_err),
                length(cells) - length(ptq_ok) - length(ptq_err)))
if (length(ptq_err))
  message("[PTQ] First error: ", conditionMessage(ptq_err[[1]]))

ptq_df <- dplyr::bind_rows(ptq_ok)

## Stop loudly if result is empty or malformed, rather than letting a downstream
## filter() silently resolve 'co2' to a stale global variable.
required_out <- c("x", "y", "scenario", "co2", "year", "ptq",
                  "Yield_kgha", "critical_radn", "critical_tmean", "window_days")
missing_out  <- setdiff(required_out, names(ptq_df))
if (nrow(ptq_df) == 0L || length(missing_out))
  stop("[PTQ] ptq_df is empty or missing columns: ",
       paste(missing_out, collapse = ", "), "\n",
       "  Make sure .met files exist in: ", WEATHER_DIR)

## ── PTQ inspection summary ───────────────────────────────────────────────────
n_valid  <- sum(!is.na(ptq_df$ptq))
n_total  <- nrow(ptq_df)
pct_ok   <- round(100 * n_valid / n_total, 1)
mean_win <- round(mean(ptq_df$window_days, na.rm = TRUE), 1)

message(sprintf("[PTQ] Results: %d rows total | %d valid (%.1f%%) | mean window = %.1f days",
                n_total, n_valid, pct_ok, mean_win))

## Negative PTQ would mean mean temperature < Tbase — biologically wrong for soybean
n_neg <- sum(ptq_df$ptq < 0, na.rm = TRUE)
if (n_neg > 0)
  warning(sprintf("[PTQ] %d rows have negative PTQ (Tmean < Tbase = %d°C) — check data.", n_neg, TBASE))

## Per-scenario PTQ summary (printed to console for quick sanity check)
cat("\n── PTQ quick check (CO2 = 350, mean across all site-years) ─────────────\n")
ptq_df %>%
  filter(co2 == 350, !is.na(ptq)) %>%
  group_by(scenario) %>%
  summarise(
    n          = n(),
    mean_ptq   = round(mean(ptq),            2),
    sd_ptq     = round(sd(ptq),              2),
    mean_radn  = round(mean(critical_radn),  1),
    mean_tmean = round(mean(critical_tmean), 1),
    mean_days  = round(mean(window_days),    0),
    .groups = "drop"
  ) %>%
  print()
cat("─────────────────────────────────────────────────────────────────────────\n\n")

saveRDS(ptq_df, "data/outputs/ptq-results.rds")
message("[PTQ] Saved: data/outputs/ptq-results.rds\n")

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
  axis.title       = element_blank(),
  axis.text        = element_blank(),
  legend.position  = "top",
  legend.direction = "horizontal",
  legend.title     = element_text(size = 11, hjust = 0),
  strip.text       = element_text(size = 10)
)

scenario_levels <- c("baseline", "climate_change", "longer_mat",
                     "early_sowing", "early_sowing_longer_mat")
scenario_labels <- c("Baseline", "2°C-increase", "Late-Maturing",
                     "Early Sowing", "LM & ES")

## ── Figure 11: mean PTQ map ───────────────────────────────────────────────────
## Mean PTQ per grid cell (averaged across 40 years), CO2 = 350, all scenarios.
## Higher values = more favourable photothermal environment during seed fill.
message("[PTQ] Building Figure 11: mean PTQ map...")

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
    name = "Mean PTQ (MJ m⁻² °C⁻¹ day⁻¹)") +
  map_theme

ggsave("figures/fig11 - ptq map.tiff", plot = plot11,
       width = 30, height = 10, units = "cm", dpi = 600,
       compression = "lzw", bg = "white")
message("[PTQ] Saved: figures/fig11 - ptq map.tiff")

## ── Figure 12: PTQ change vs baseline ────────────────────────────────────────
## Shows how each adaptation strategy shifts the photothermal environment
## relative to baseline. Positive = better photothermal conditions.
message("[PTQ] Building Figure 12: PTQ change vs baseline map...")

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
    scenario = factor(scenario,
                      levels = scenario_levels[-1],
                      labels = scenario_labels[-1]),
    ptq_bin  = cut(ptq_chg_mean,
                   breaks = c(-Inf, -0.5, 0, 0.5, 1.5, Inf),
                   labels = c("< −0.5", "−0.5 to 0", "0 to 0.5",
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
    values = c("< −0.5"     = "#b10026",
               "−0.5 to 0"  = "#fc8d59",
               "0 to 0.5"        = "#e5f5f9",
               "0.5 to 1.5"      = "#74c476",
               "> 1.5"           = "#005824"),
    na.value = "grey50", na.translate = FALSE,
    name = "PTQ change vs Baseline\n(MJ m⁻² °C⁻¹ day⁻¹)") +
  map_theme

ggsave("figures/fig12 - ptq change map.tiff", plot = plot12,
       width = 28, height = 10, units = "cm", dpi = 600,
       compression = "lzw", bg = "white")
message("[PTQ] Saved: figures/fig12 - ptq change map.tiff")

## ── Figure 13: PTQ vs yield scatter ──────────────────────────────────────────
## Mechanistic link: higher PTQ during seed fill → higher yield.
## Regression slope (kg/ha per PTQ unit) shown per scenario.
message("[PTQ] Building Figure 13: PTQ vs yield scatter...")

p13_data <- ptq_df %>%
  filter(co2 == 350, !is.na(ptq)) %>%
  mutate(scenario = factor(scenario, levels = scenario_levels,
                           labels = scenario_labels))

plot13 <- ggplot(p13_data, aes(x = ptq, y = Yield_kgha, colour = scenario)) +
  geom_point(size = 0.8, alpha = 0.15) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  temp +
  labs(x = "Photothermal Quotient (MJ m⁻² °C⁻¹ day⁻¹)",
       y = "Seed yield (kg/ha)",
       colour = "Scenario") +
  scale_colour_manual(values = c("Baseline"      = "#1f78b4",
                                 "2°C-increase" = "#e31a1c",
                                 "Late-Maturing" = "#33a02c",
                                 "Early Sowing"  = "#ff7f00",
                                 "LM & ES"       = "#6a3d9a")) +
  theme(legend.position = "top") +
  facet_wrap(~ scenario, nrow = 1)

ggsave("figures/fig13 - ptq yield relationship.tiff", plot = plot13,
       width = 30, height = 10, units = "cm", dpi = 600,
       compression = "lzw", bg = "white")
message("[PTQ] Saved: figures/fig13 - ptq yield relationship.tiff")

## ── Manuscript stats ──────────────────────────────────────────────────────────
cat(paste(rep("─", 60), collapse = ""), "\n")
cat("PTQ SUMMARY (CO2 = 350, all site-years)\n\n")

ptq_df %>%
  filter(co2 == 350, !is.na(ptq)) %>%
  group_by(scenario) %>%
  summarise(
    mean_ptq   = round(mean(ptq),  2),
    sd_ptq     = round(sd(ptq),    2),
    min_ptq    = round(min(ptq),   2),
    max_ptq    = round(max(ptq),   2),
    .groups    = "drop"
  ) %>%
  print()

cat("\nPTQ–YIELD REGRESSION SLOPES (kg/ha per PTQ unit, by scenario):\n")
ptq_df %>%
  filter(co2 == 350, !is.na(ptq)) %>%
  group_by(scenario) %>%
  summarise(
    slope   = round(coef(lm(Yield_kgha ~ ptq))[["ptq"]], 0),
    r2      = round(summary(lm(Yield_kgha ~ ptq))$r.squared, 3),
    .groups = "drop"
  ) %>%
  print()

cat(paste(rep("─", 60), collapse = ""), "\n")
message("[PTQ] Phase 5 complete.")
