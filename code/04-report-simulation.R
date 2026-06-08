## ═══════════════════════════════════════════════════════════════════════════
## Simulation Run Report
## Generates figures/simulation-report.pdf — a technical PDF documenting
## what was simulated, timing, coverage, and per-chunk diagnostics.
## Run from repo root: source("code/04-report-simulation.R")
## ═══════════════════════════════════════════════════════════════════════════

library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(cowplot)
library(scales)

dir.create("figures", showWarnings = FALSE, recursive = TRUE)

## ── Load inputs ──────────────────────────────────────────────────────────

log_path     <- "data/outputs/sim-run-log.csv"
results_path <- "data/outputs/simulated-scenarios-df.rds"
summary_path <- "data/outputs/run-summary.txt"
chunks_dir   <- "data/outputs/checkpoints"

if (!file.exists(log_path))     stop("Run log not found: ", log_path)
if (!file.exists(results_path)) stop("Results file not found: ", results_path)

run_log <- read_csv(log_path, show_col_types = FALSE) %>%
  mutate(timestamp = as.POSIXct(timestamp),
         scenario  = as.factor(scenario),
         pct_ok    = cells_ok / cells_total * 100,
         elapsed_min = elapsed_sec / 60)

results <- readRDS(results_path) %>% as_tibble()

chunk_files <- list.files(chunks_dir, pattern = "\\.rds$", full.names = TRUE)

## ── Summary stats ────────────────────────────────────────────────────────

total_cells     <- n_distinct(results$cellid)
total_scenarios <- n_distinct(results$scenario)
total_rows      <- nrow(results)
total_years     <- n_distinct(lubridate::year(results$Date))
date_range      <- paste(min(lubridate::year(results$Date)),
                         max(lubridate::year(results$Date)), sep = "–")
total_time_min  <- sum(run_log$elapsed_sec) / 60
n_chunks        <- nrow(run_log)
n_chunk_files   <- length(chunk_files)
total_cells_ok  <- sum(run_log$cells_ok)
total_cells_fail<- sum(run_log$cells_fail)
pct_success     <- total_cells_ok / (total_cells_ok + total_cells_fail) * 100
run_date        <- format(max(run_log$timestamp), "%Y-%m-%d %H:%M")

scenario_summary <- results %>%
  group_by(scenario, co2) %>%
  summarise(
    cells      = n_distinct(cellid),
    years      = n_distinct(lubridate::year(Date)),
    yield_mean = round(mean(Yield_kgha, na.rm = TRUE), 0),
    yield_sd   = round(sd(Yield_kgha,   na.rm = TRUE), 0),
    yield_min  = round(min(Yield_kgha,  na.rm = TRUE), 0),
    yield_max  = round(max(Yield_kgha,  na.rm = TRUE), 0),
    .groups    = "drop"
  )

chunk_summary <- run_log %>%
  group_by(scenario) %>%
  summarise(
    chunks       = n(),
    cells_ok     = sum(cells_ok),
    cells_fail   = sum(cells_fail),
    time_min     = round(sum(elapsed_sec) / 60, 2),
    time_per_cell_sec = round(sum(elapsed_sec) / sum(cells_ok), 1),
    .groups = "drop"
  )

## ── Theme ────────────────────────────────────────────────────────────────

rpt_theme <- theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(size = 10, color = "grey40"),
    panel.grid.minor = element_blank(),
    strip.text       = element_text(face = "bold"),
    legend.position  = "top"
  )

pal_scenario <- setNames(
  c("#1b7837", "#d73027", "#4575b4", "#e08214", "#762a83",
    "#35978f", "#8e0152", "#543005", "#003c30"),
  unique(as.character(run_log$scenario))
)

## ── Page 1: Title page ───────────────────────────────────────────────────

title_page <- ggdraw() +
  draw_label(
    "Soybean Arkansas Climate-Change Simulation",
    fontface = "bold", size = 20, x = 0.5, y = 0.82, hjust = 0.5
  ) +
  draw_label(
    "Technical Simulation Run Report",
    size = 14, color = "grey40", x = 0.5, y = 0.74, hjust = 0.5
  ) +
  draw_label(
    paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M")),
    size = 11, color = "grey50", x = 0.5, y = 0.65, hjust = 0.5
  ) +
  draw_label(
    paste0(
      "Cells simulated : ", total_cells, "\n",
      "Scenarios       : ", total_scenarios, "\n",
      "Years           : ", total_years, "  (", date_range, ")\n",
      "Total rows      : ", format(total_rows, big.mark = ","), "\n",
      "Success rate    : ", round(pct_success, 1), "%\n",
      "Total run time  : ", round(total_time_min, 1), " min\n",
      "Completed       : ", run_date
    ),
    size = 12, x = 0.5, y = 0.40, hjust = 0.5,
    fontfamily = "mono"
  )

## ── Page 2: Timing per chunk ─────────────────────────────────────────────

p_timing_chunk <- run_log %>%
  mutate(chunk_label = paste0("Sc", sc_idx, "-Ck", sprintf("%02d", chunk))) %>%
  ggplot(aes(x = chunk_label, y = elapsed_min, fill = scenario)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%.1f", elapsed_min)),
            vjust = -0.3, size = 3) +
  scale_fill_manual(values = pal_scenario, name = "Scenario") +
  labs(title    = "Elapsed time per chunk",
       subtitle = "Each bar = one parallel chunk",
       x = "Chunk", y = "Time (minutes)") +
  rpt_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## ── Page 3: Timing per scenario (total) ──────────────────────────────────

p_timing_sc <- chunk_summary %>%
  ggplot(aes(x = reorder(scenario, time_min), y = time_min, fill = scenario)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(time_min, " min\n(", time_per_cell_sec, " s/cell)")),
            hjust = -0.05, size = 3.5) +
  coord_flip() +
  scale_fill_manual(values = pal_scenario) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.35))) +
  labs(title    = "Total simulation time by scenario",
       subtitle = "Labels show total minutes and seconds per cell",
       x = NULL, y = "Time (minutes)") +
  rpt_theme +
  theme(legend.position = "none")

## ── Page 4: Cell coverage (ok vs fail) ───────────────────────────────────

p_coverage <- run_log %>%
  select(scenario, chunk, cells_ok, cells_fail) %>%
  pivot_longer(c(cells_ok, cells_fail), names_to = "status", values_to = "n") %>%
  mutate(status = recode(status, cells_ok = "Success", cells_fail = "Failed"),
         status = factor(status, levels = c("Failed", "Success"))) %>%
  ggplot(aes(x = factor(chunk), y = n, fill = status)) +
  geom_col(width = 0.7) +
  facet_wrap(~scenario, scales = "free_x") +
  scale_fill_manual(values = c("Success" = "#1a9850", "Failed" = "#d73027"),
                    name = "Cell status") +
  labs(title    = "Cell success vs failure by chunk",
       subtitle = paste0("Overall success rate: ", round(pct_success, 1), "%"),
       x = "Chunk", y = "Number of cells") +
  rpt_theme

## ── Page 5: Cumulative cells completed over time ──────────────────────────

p_cumulative <- run_log %>%
  arrange(timestamp) %>%
  mutate(cumulative_ok = cumsum(cells_ok),
         run_min = as.numeric(difftime(timestamp, min(timestamp), units = "mins"))) %>%
  ggplot(aes(x = run_min, y = cumulative_ok, colour = scenario, group = scenario)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_colour_manual(values = pal_scenario, name = "Scenario") +
  labs(title    = "Cumulative cells completed over time",
       subtitle = "Each point marks end of a chunk",
       x = "Elapsed time (minutes)", y = "Cumulative cells completed") +
  rpt_theme

## ── Page 6: Yield distribution by scenario ───────────────────────────────

p_yield_dist <- results %>%
  mutate(label = paste0(scenario, "\n(CO2=", co2, ")")) %>%
  ggplot(aes(x = Yield_kgha, y = label, fill = scenario)) +
  ggridges::geom_density_ridges(alpha = 0.7, scale = 0.9, colour = "white") +
  scale_fill_manual(values = pal_scenario) +
  labs(title    = "Simulated yield distribution by scenario",
       x = "Yield (kg/ha)", y = NULL) +
  rpt_theme +
  theme(legend.position = "none")

## ── Page 7: Yield by scenario — boxplot ──────────────────────────────────

p_yield_box <- results %>%
  mutate(label = paste0(scenario, " (CO2=", co2, ")")) %>%
  ggplot(aes(x = reorder(label, Yield_kgha, FUN = median),
             y = Yield_kgha, fill = scenario)) +
  geom_boxplot(outlier.size = 0.8, outlier.alpha = 0.4, width = 0.6) +
  coord_flip() +
  scale_fill_manual(values = pal_scenario) +
  labs(title = "Yield summary by scenario",
       x = NULL, y = "Yield (kg/ha)") +
  rpt_theme +
  theme(legend.position = "none")

## ── Page 8: Phenology by scenario ────────────────────────────────────────

pheno_cols <- c("EmergenceDAS", "FloweringDAS", "SeedFillingDAS", "MaturityDAS")
pheno_available <- intersect(pheno_cols, names(results))

if (length(pheno_available) >= 2) {
  p_phenology <- results %>%
    select(scenario, co2, all_of(pheno_available)) %>%
    pivot_longer(all_of(pheno_available), names_to = "stage", values_to = "DAS") %>%
    mutate(stage = factor(stage, levels = pheno_cols),
           label = paste0(scenario, " (CO2=", co2, ")")) %>%
    group_by(label, stage) %>%
    summarise(mean_DAS = mean(DAS, na.rm = TRUE),
              sd_DAS   = sd(DAS,   na.rm = TRUE), .groups = "drop") %>%
    ggplot(aes(x = stage, y = mean_DAS, colour = label, group = label)) +
    geom_line(linewidth = 0.8, alpha = 0.8) +
    geom_pointrange(aes(ymin = mean_DAS - sd_DAS, ymax = mean_DAS + sd_DAS),
                    size = 0.4) +
    labs(title    = "Phenology stages by scenario",
         subtitle = "Mean ± SD days after sowing",
         x = NULL, y = "Days after sowing",
         colour = "Scenario") +
    rpt_theme +
    theme(axis.text.x = element_text(angle = 20, hjust = 1))
} else {
  p_phenology <- ggdraw() +
    draw_label("Phenology columns not available in results", color = "grey50")
}

## ── Page 9: Weather summary ───────────────────────────────────────────────

p_weather <- results %>%
  filter(co2 == min(co2)) %>%
  select(scenario, SeasonMeanT, SeasonRain, SeasonRadn) %>%
  pivot_longer(c(SeasonMeanT, SeasonRain, SeasonRadn),
               names_to = "variable", values_to = "value") %>%
  mutate(variable = recode(variable,
    SeasonMeanT = "Mean Temp (°C)",
    SeasonRain  = "Season Rain (mm)",
    SeasonRadn  = "Solar Rad (MJ/m²)")) %>%
  ggplot(aes(x = scenario, y = value, fill = scenario)) +
  geom_boxplot(width = 0.6, outlier.size = 0.6, outlier.alpha = 0.3) +
  facet_wrap(~variable, scales = "free_y") +
  scale_fill_manual(values = pal_scenario) +
  labs(title    = "Weather inputs by scenario",
       subtitle = "CO2 = 350 ppm only",
       x = NULL, y = NULL) +
  rpt_theme +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 30, hjust = 1))

## ── Page 10: Scenario definition table ───────────────────────────────────

sc_table_data <- results %>%
  distinct(scenario, co2, cultivar, sowing) %>%
  arrange(scenario, co2) %>%
  mutate(climate = ifelse(scenario == "baseline", "Current", "+2°C"))

p_sc_table <- ggdraw() +
  draw_label("Scenario Definitions", fontface = "bold",
             x = 0.5, y = 0.95, hjust = 0.5, size = 14) +
  draw_label(
    paste0(
      sprintf("%-32s %-8s %-12s %-10s %-10s\n",
              "Scenario", "CO2", "Cultivar", "Sowing", "Climate"),
      paste(rep("─", 76), collapse = ""), "\n",
      paste(
        apply(sc_table_data, 1, function(r)
          sprintf("%-32s %-8s %-12s %-10s %-10s",
                  r["scenario"], r["co2"], r["cultivar"], r["sowing"], r["climate"])),
        collapse = "\n"
      )
    ),
    x = 0.05, y = 0.78, hjust = 0, size = 9.5, fontfamily = "mono"
  ) +
  draw_label(
    paste0(
      "Run details\n",
      paste(rep("─", 40), collapse = ""), "\n",
      sprintf("%-25s %s\n", "Cells simulated:",  total_cells),
      sprintf("%-25s %s\n", "Scenarios:",         total_scenarios),
      sprintf("%-25s %s\n", "Years per cell:",     total_years),
      sprintf("%-25s %s\n", "Date range:",         date_range),
      sprintf("%-25s %s\n", "Total result rows:",  format(total_rows, big.mark = ",")),
      sprintf("%-25s %.1f min\n", "Total run time:", total_time_min),
      sprintf("%-25s %d\n", "Chunks processed:",   n_chunks),
      sprintf("%-25s %.1f%%\n", "Success rate:",    pct_success),
      sprintf("%-25s %s\n", "Completed:",           run_date)
    ),
    x = 0.05, y = 0.40, hjust = 0, size = 9.5, fontfamily = "mono"
  )

## ── Assemble PDF ─────────────────────────────────────────────────────────

out_pdf <- "figures/simulation-report.pdf"
pdf(out_pdf, width = 12, height = 8)

print(title_page)
print(p_sc_table)
print(p_timing_chunk)
print(p_timing_sc)
print(p_coverage)
print(p_cumulative)
print(p_yield_dist)
print(p_yield_box)
print(p_phenology)
print(p_weather)

dev.off()

cat(sprintf("[DONE] Simulation report saved to %s (%d pages)\n", out_pdf, 10))
