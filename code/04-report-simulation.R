## ═══════════════════════════════════════════════════════════════════════════
## Simulation Run Report
## Generates reports/simulation-report.pdf — technical PDF with tables
## documenting what was simulated, per-scenario and per-chunk timing.
## Run from repo root: source("code/04-report-simulation.R")
## ═══════════════════════════════════════════════════════════════════════════

library(dplyr)
library(readr)
library(lubridate)
library(grid)
library(gridExtra)

dir.create("reports", showWarnings = FALSE, recursive = TRUE)

## ── Load inputs ──────────────────────────────────────────────────────────

log_path     <- "data/outputs/sim-run-log.csv"
results_path <- "data/outputs/simulated-scenarios-df.rds"
summary_path <- "data/outputs/run-summary.txt"

if (!file.exists(log_path))     stop("Run log not found: ", log_path)
if (!file.exists(results_path)) stop("Results file not found: ", results_path)

run_log <- read_csv(log_path, show_col_types = FALSE) %>%
  mutate(timestamp = as.POSIXct(timestamp))

results <- readRDS(results_path) %>% as_tibble()

## ── Summary stats ────────────────────────────────────────────────────────

total_cells      <- n_distinct(results$cellid)
total_scenarios  <- n_distinct(results$scenario)
total_rows       <- nrow(results)
total_years      <- n_distinct(lubridate::year(results$Date))
date_range       <- paste(min(lubridate::year(results$Date)),
                          max(lubridate::year(results$Date)), sep = "–")
total_time_min   <- sum(run_log$elapsed_sec, na.rm = TRUE) / 60
total_time_hr    <- total_time_min / 60
n_chunks         <- nrow(run_log)
total_cells_ok   <- sum(run_log$cells_ok,   na.rm = TRUE)
total_cells_fail <- sum(run_log$cells_fail, na.rm = TRUE)
pct_success      <- total_cells_ok / (total_cells_ok + total_cells_fail) * 100
run_start        <- format(min(run_log$timestamp), "%Y-%m-%d %H:%M")
run_end          <- format(max(run_log$timestamp), "%Y-%m-%d %H:%M")

## Per-scenario timing (from run_log, grouped by scenario start/end)
scenario_timing <- run_log %>%
  group_by(scenario) %>%
  summarise(
    sc_start      = format(min(timestamp), "%Y-%m-%d %H:%M"),
    sc_end        = format(max(timestamp), "%Y-%m-%d %H:%M"),
    chunks        = n(),
    cells_ok      = sum(cells_ok,   na.rm = TRUE),
    cells_fail    = sum(cells_fail, na.rm = TRUE),
    total_min     = round(sum(elapsed_sec, na.rm = TRUE) / 60, 1),
    sec_per_cell  = round(sum(elapsed_sec, na.rm = TRUE) /
                            max(sum(cells_ok, na.rm = TRUE), 1), 1),
    cells_per_hr  = round(sum(cells_ok, na.rm = TRUE) /
                            max(sum(elapsed_sec, na.rm = TRUE) / 3600, 1e-6), 0),
    errors        = sum(!is.na(errors) & errors != "", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(
    Scenario      = scenario,
    Start         = sc_start,
    End           = sc_end,
    Chunks        = chunks,
    OK            = cells_ok,
    Failed        = cells_fail,
    `Time (min)`  = total_min,
    `s/cell`      = sec_per_cell,
    `Cells/hr`    = cells_per_hr,
    `Chunks w/ errors` = errors
  )

## Per-chunk detail table
chunk_detail <- run_log %>%
  mutate(
    start_time  = format(timestamp, "%H:%M:%S"),
    elapsed_min = round(elapsed_sec / 60, 2),
    pct_ok      = round(cells_ok / pmax(cells_total, 1) * 100, 1)
  ) %>%
  select(scenario, chunk, cells_total, cells_ok, cells_fail,
         pct_ok, elapsed_min, start_time) %>%
  rename(
    Scenario      = scenario,
    Chunk         = chunk,
    Total         = cells_total,
    OK            = cells_ok,
    Failed        = cells_fail,
    `% OK`        = pct_ok,
    `Min`         = elapsed_min,
    `Start`       = start_time
  )

## Scenario definitions from results
sc_defs <- results %>%
  distinct(scenario, co2, cultivar, sowing) %>%
  arrange(scenario, co2) %>%
  mutate(Climate = ifelse(scenario == "baseline", "Current", "+2°C")) %>%
  rename(Scenario = scenario, `CO2 (ppm)` = co2,
         Cultivar = cultivar, `Sowing date` = sowing)

## Yield summary per scenario
yield_summary <- results %>%
  group_by(scenario, co2) %>%
  summarise(
    cells     = n_distinct(cellid),
    years     = n_distinct(lubridate::year(Date)),
    mean_kg   = round(mean(Yield_kgha, na.rm = TRUE), 0),
    sd_kg     = round(sd(Yield_kgha,   na.rm = TRUE), 0),
    min_kg    = round(min(Yield_kgha,  na.rm = TRUE), 0),
    max_kg    = round(max(Yield_kgha,  na.rm = TRUE), 0),
    .groups   = "drop"
  ) %>%
  rename(Scenario = scenario, `CO2 (ppm)` = co2,
         Cells = cells, Years = years,
         `Mean (kg/ha)` = mean_kg, `SD` = sd_kg,
         `Min (kg/ha)` = min_kg, `Max (kg/ha)` = max_kg)

## ── PDF helper ───────────────────────────────────────────────────────────

table_page <- function(df, title, subtitle = NULL, fontsize = 9) {
  tt <- ttheme_minimal(
    base_size   = fontsize,
    core        = list(fg_params = list(hjust = 0, x = 0.02)),
    colhead     = list(fg_params = list(fontface = "bold", hjust = 0, x = 0.02),
                       bg_params = list(fill = "#e8edf3"))
  )
  tbl   <- tableGrob(df, rows = NULL, theme = tt)
  title_grob <- textGrob(title, gp = gpar(fontsize = 14, fontface = "bold"), hjust = 0, x = 0)
  grobs <- list(title_grob)
  if (!is.null(subtitle)) {
    grobs <- c(grobs, list(textGrob(subtitle,
                                    gp = gpar(fontsize = 10, col = "grey40"),
                                    hjust = 0, x = 0)))
  }
  grobs <- c(grobs, list(tbl))
  heights <- unit.c(unit(0.06, "npc"),
                    if (!is.null(subtitle)) unit(0.04, "npc") else NULL,
                    unit(1, "null"))
  arrangeGrob(grobs = grobs, ncol = 1, heights = heights)
}

## ── Page 1: Cover page ───────────────────────────────────────────────────

cover_lines <- c(
  paste0("Project    : Soybean Arkansas Climate-Change Simulation"),
  paste0("Report     : Technical Simulation Run Report"),
  paste0("Generated  : ", format(Sys.time(), "%Y-%m-%d %H:%M")),
  "",
  "Run summary",
  paste(rep("─", 50), collapse = ""),
  sprintf("  Cells simulated   : %d",          total_cells),
  sprintf("  Scenarios         : %d",          total_scenarios),
  sprintf("  Years per cell    : %d  (%s)",    total_years, date_range),
  sprintf("  Total result rows : %s",          format(total_rows, big.mark = ",")),
  sprintf("  Chunks processed  : %d",          n_chunks),
  sprintf("  Cells OK          : %d",          total_cells_ok),
  sprintf("  Cells failed      : %d",          total_cells_fail),
  sprintf("  Overall success   : %.1f%%",      pct_success),
  sprintf("  Total run time    : %.1f min  (%.2f hr)", total_time_min, total_time_hr),
  sprintf("  Simulation start  : %s",          run_start),
  sprintf("  Simulation end    : %s",          run_end)
)

cover_page <- ggdraw <- {
  grid.newpage()
  grid.text("Soybean AR Climate-Change — Simulation Run Report",
            x = 0.5, y = 0.93, just = "centre",
            gp = gpar(fontsize = 18, fontface = "bold"))
  grid.text(paste(cover_lines, collapse = "\n"),
            x = 0.08, y = 0.73, just = c("left", "top"),
            gp = gpar(fontsize = 11, fontfamily = "mono"))
}

## ── Assemble PDF ─────────────────────────────────────────────────────────

out_pdf <- "reports/simulation-report.pdf"
pdf(out_pdf, width = 13, height = 9)

## Page 1 — cover
grid.newpage()
grid.text("Soybean AR Climate-Change — Simulation Run Report",
          x = 0.5, y = 0.93, just = "centre",
          gp = gpar(fontsize = 18, fontface = "bold"))
grid.text(paste(cover_lines, collapse = "\n"),
          x = 0.08, y = 0.72, just = c("left", "top"),
          gp = gpar(fontsize = 11, fontfamily = "mono"))

## Page 2 — scenario definitions
grid.draw(table_page(sc_defs,
  title    = "Scenario Definitions",
  subtitle = "Treatments simulated across the full grid"))

## Page 3 — per-scenario timing summary
grid.draw(table_page(scenario_timing,
  title    = "Simulation Timing by Scenario",
  subtitle = "Aggregated from per-chunk run log (data/outputs/sim-run-log.csv)",
  fontsize = 9))

## Page 4 — per-chunk detail (split into batches of 35 rows)
chunk_rows <- nrow(chunk_detail)
batch_size <- 35
n_batches  <- ceiling(chunk_rows / batch_size)
for (i in seq_len(n_batches)) {
  rows <- ((i - 1) * batch_size + 1):min(i * batch_size, chunk_rows)
  sub  <- if (n_batches > 1) paste0("(part ", i, " of ", n_batches, ")") else NULL
  grid.draw(table_page(chunk_detail[rows, ],
    title    = "Per-Chunk Timing Detail",
    subtitle = sub,
    fontsize = 8))
}

## Page 5 — yield summary
grid.draw(table_page(yield_summary,
  title    = "Simulated Yield Summary by Scenario",
  subtitle = "Mean, SD, min, and max yield (kg/ha) across all cells and years",
  fontsize = 9))

dev.off()

n_pages <- 3 + n_batches + 1
cat(sprintf("[DONE] Simulation report saved to %s (%d pages)\n", out_pdf, n_pages))
