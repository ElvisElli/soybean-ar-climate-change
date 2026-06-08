## ═══════════════════════════════════════════════════════════════════════════
## Soybean Arkansas Climate-Change Simulation — Master Script
## ═══════════════════════════════════════════════════════════════════════════
##
## Usage (RStudio): open this file and press Source
## Usage (command line): Rscript code/00-master.R
##
## Phases:
##   1  Grid simulation     02-grid-simulation-scenarios-parallel.R
##   2  Data analysis       03-data-analysis-6-5-26.R
##   3  Inspection report   04-inspection-report.R
##   4  Simulation report   05-simulation-report.R
##
## Each phase is skipped automatically if its outputs already exist,
## unless FORCE_RERUN = TRUE.
## ═══════════════════════════════════════════════════════════════════════════

## ── Settings ─────────────────────────────────────────────────────────────
FORCE_RERUN   <- FALSE    # TRUE = re-run every phase even if outputs exist
RUN_SIM       <- TRUE     # Phase 1: APSIM grid simulation
RUN_ANALYSIS  <- TRUE     # Phase 2: data analysis + manuscript figures
RUN_REPORT    <- TRUE     # Phase 3: PDF inspection report (scientific)
RUN_SIM_REPORT <- TRUE    # Phase 4: PDF simulation run report (technical)

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
  if (!FORCE_RERUN && all(file.exists(output_check))) {
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
  run_phase(
    phase        = "1",
    title        = "APSIM grid simulation",
    output_check = "intermediate-data/simulated-scenarios-df.rds",
    script       = "code/02-grid-simulation-scenarios-parallel.R"
  )
}

## ── Phase 2: Data analysis ────────────────────────────────────────────────
if (RUN_ANALYSIS) {
  run_phase(
    phase        = "2",
    title        = "Data analysis & manuscript figures",
    output_check = c("plots/p1 - climate change without adaptation.tiff",
                     "plots/p5 - environmental characterization.tiff"),
    script       = "code/03-data-analysis-6-5-26.R"
  )
}

## ── Phase 3: Inspection report ────────────────────────────────────────────
if (RUN_REPORT) {
  run_phase(
    phase        = "3",
    title        = "PDF inspection report",
    output_check = "plots/inspection-report.pdf",
    script       = "code/04-inspection-report.R"
  )
}

## ── Phase 4: Simulation run report ───────────────────────────────────────
if (RUN_SIM_REPORT) {
  run_phase(
    phase        = "4",
    title        = "Simulation run report (technical)",
    output_check = "plots/simulation-report.pdf",
    script       = "code/05-simulation-report.R"
  )
}

## ── Summary ───────────────────────────────────────────────────────────────
cat(paste(rep("═", 72), collapse = ""), "\n")
cat(sprintf("[%s] All phases complete.\n", .ts()))
cat(sprintf("  Manuscript figures   : plots/\n"))
cat(sprintf("  Inspection report    : plots/inspection-report.pdf\n"))
cat(sprintf("  Simulation report    : plots/simulation-report.pdf\n"))
cat(sprintf("  Simulation results   : intermediate-data/simulated-scenarios-df.rds\n"))
cat(paste(rep("═", 72), collapse = ""), "\n\n")
