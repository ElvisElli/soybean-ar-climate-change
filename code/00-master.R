## ═══════════════════════════════════════════════════════════════════════════
## Soybean Arkansas Climate-Change Simulation — Master Script
## ═══════════════════════════════════════════════════════════════════════════
##
## Usage (RStudio): open this file and press Source
## Usage (command line): Rscript code/00-master.R
##
## Phases:
##   1  Grid simulation     01-simulation.R
##   2  Data analysis       02-analysis.R
##   3  Inspection report   03-report-scientific.R
##   4  Simulation report   04-report-simulation.R
##
## FORCE_RERUN_SIM     TRUE = delete all checkpoints and re-run APSIM from scratch
## FORCE_RERUN_OUTPUT  TRUE = re-run phases 2-4 even if output files already exist
## ═══════════════════════════════════════════════════════════════════════════

## ── Settings ─────────────────────────────────────────────────────────────
FORCE_RERUN_SIM    <- FALSE  # TRUE = wipe checkpoints and re-run all APSIM simulations
FORCE_RERUN_OUTPUT <- TRUE   # TRUE = re-run analysis figures and PDF reports
RUN_SIM            <- TRUE   # Phase 1: APSIM grid simulation
RUN_ANALYSIS       <- TRUE   # Phase 2: data analysis + manuscript figures
RUN_REPORT         <- TRUE   # Phase 3: PDF inspection report (scientific)
RUN_SIM_REPORT     <- TRUE   # Phase 4: PDF simulation run report (technical)

## ── Helpers ──────────────────────────────────────────────────────────────
.ts <- function() format(Sys.time(), "%H:%M:%S")

banner <- function(phase, title) {
  width <- 72
  bar   <- paste(rep("═", width), collapse = "")
  cat("\n", bar, "\n", sep = "")
  cat(sprintf("  Phase %s │ %s │ %s\n", phase, .ts(), title))
  cat(bar, "\n\n", sep = "")
}

run_phase <- function(phase, title, output_check, script) {
  if (!FORCE_RERUN_OUTPUT && all(file.exists(output_check))) {
    cat(sprintf("[%s] Phase %s SKIPPED — outputs already exist: %s\n",
                .ts(), phase, paste(basename(output_check), collapse = ", ")))
    return(invisible(NULL))
  }
  banner(phase, title)
  t0 <- proc.time()[["elapsed"]]
  tryCatch(
    source(script, echo = FALSE, local = new.env(parent = globalenv())),
    error = function(e) {
      cat(sprintf("\n[%s] Phase %s FAILED: %s\n", .ts(), phase, e$message))
      stop(sprintf("Phase %s failed — stopping master script.", phase), call. = FALSE)
    }
  )
  elapsed <- proc.time()[["elapsed"]] - t0
  cat(sprintf("\n[%s] Phase %s DONE in %.1f min\n",
              .ts(), phase, elapsed / 60))
}

## ── Verify working directory ──────────────────────────────────────────────
if (!file.exists("code/00-master.R")) {
  stop("Run this script from the repo root, not from inside code/.\n",
       "  setwd('<repo-root>') then source('code/00-master.R')", call. = FALSE)
}

cat(sprintf("[%s] Master script started\n", .ts()))
cat(sprintf("  Working dir : %s\n", getwd()))
cat(sprintf("  R version   : %s\n", R.version$version.string))
cat(sprintf("  Platform    : %s\n\n", R.version$platform))

## ── Phase 1: Simulation ───────────────────────────────────────────────────
if (RUN_SIM) {
  if (FORCE_RERUN_SIM) {
    cat(sprintf("[%s] FORCE_RERUN_SIM = TRUE — deleting checkpoints for fresh run\n", .ts()))
    unlink("data/outputs/checkpoints", recursive = TRUE)
    unlink("data/outputs/sim-run-log.csv")
  }
  banner("1", "APSIM grid simulation")
  t0 <- proc.time()[["elapsed"]]
  tryCatch(
    source("code/01-simulation.R", echo = FALSE, local = new.env(parent = globalenv())),
    error = function(e) {
      cat(sprintf("\n[%s] Phase 1 FAILED: %s\n", .ts(), e$message))
      stop("Phase 1 failed — stopping master script.", call. = FALSE)
    }
  )
  cat(sprintf("\n[%s] Phase 1 DONE in %.1f min\n", .ts(),
              (proc.time()[["elapsed"]] - t0) / 60))
}

## ── Phase 2: Data analysis ────────────────────────────────────────────────
if (RUN_ANALYSIS) {
  run_phase(
    phase        = "2",
    title        = "Data analysis & manuscript figures",
    output_check = c("figures/fig01 - climate change without adaptation.tiff",
                     "figures/fig05 - adaptation strategies merged.tiff",
                     "figures/fig09 - phenology change maps.tiff",
                     "figures/fig10 - water use efficiency - sWUE.tiff"),
    script       = "code/02-analysis.R"
  )
}

## ── Phase 3: Inspection report ────────────────────────────────────────────
if (RUN_REPORT) {
  run_phase(
    phase        = "3",
    title        = "PDF inspection report",
    output_check = "reports/inspection-report.pdf",
    script       = "code/03-report-scientific.R"
  )
}

## ── Phase 4: Simulation run report ───────────────────────────────────────
if (RUN_SIM_REPORT) {
  run_phase(
    phase        = "4",
    title        = "Simulation run report (technical)",
    output_check = "reports/simulation-report.pdf",
    script       = "code/04-report-simulation.R"
  )
}

## ── Phase 5: PTQ analysis (requires StartPodDAS in simulation output) ────
## Check column names via RDS header only (avoids reading full 30 MB file)
.sim_rds <- "data/outputs/simulated-scenarios-df.rds"
RUN_PTQ  <- file.exists(.sim_rds) &&
            tryCatch("StartPodDAS" %in% names(readRDS(.sim_rds)),
                     error = function(e) FALSE)
if (RUN_PTQ) {
  run_phase(
    phase        = "5",
    title        = "Photothermal Quotient analysis",
    output_check = c("data/outputs/ptq-results.rds",
                     "figures/fig11 - ptq map.tiff"),
    script       = "code/04-ptq-analysis.R"
  )
} else {
  cat(sprintf("[%s] Phase 5 SKIPPED — StartPodDAS not in simulation output yet.\n",
              .ts()))
  cat("         Re-run simulation with updated template to enable PTQ analysis.\n\n")
}

## ── Summary ───────────────────────────────────────────────────────────────
cat(paste(rep("═", 72), collapse = ""), "\n")
cat(sprintf("[%s] All phases complete.\n", .ts()))
cat(sprintf("  Manuscript figures   : figures/\n"))
cat(sprintf("  Inspection report    : reports/inspection-report.pdf\n"))
cat(sprintf("  Simulation report    : reports/simulation-report.pdf\n"))
cat(sprintf("  PTQ results          : data/outputs/ptq-results.rds\n"))
cat(sprintf("  Simulation results   : data/outputs/simulated-scenarios-df.rds\n"))
cat(paste(rep("═", 72), collapse = ""), "\n\n")

## ── Final email notification (after PDFs are ready) ───────────────────────
if (RUN_SIM && exists("send_notification") && exists("sim_summary_for_notify")) {
  s <- sim_summary_for_notify
  send_notification(
    subject = sprintf("Soybean sim COMPLETE — %.0f min | %s",
                      s$total_elapsed, s$nodename),
    body = paste0(
      "**All scenarios complete — PDFs attached!**\n\n",
      "- Total rows : ", s$total_rows,      "\n",
      "- Scenarios  : ", s$n_scenarios,     "\n",
      "- Cells      : ", s$n_cells,         "\n",
      "- Total time : ", s$total_elapsed, " min\n",
      "- Output     : data/outputs/simulated-scenarios-df.rds\n\n",
      "Machine: ", s$nodename
    ),
    attachments = Filter(file.exists, c(
      "reports/simulation-report.pdf",
      "reports/inspection-report.pdf"
    ))
  )
}
