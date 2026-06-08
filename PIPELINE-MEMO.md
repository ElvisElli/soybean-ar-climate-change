# Pipeline Memo — Grid-Scale APSIM Simulation in R
*Soybean Arkansas Climate-Change Project | E. Elli | eelli@uark.edu*
*Last updated: 2026-06-08*

---

## Purpose

This memo captures every challenge, solution, and design decision made while
building this pipeline. Use it as a blueprint when starting a new grid-scale
APSIM simulation project.

---

## 1. Project Architecture

### What this pipeline does
- Runs APSIM Next Generation simulations across a spatial grid (~4,651 cells)
- Covers 40 years of weather (1985–2024) and multiple management scenarios
- Runs in parallel using R's PSOCK cluster (`doParallel` + `foreach`)
- Is fully resumable from checkpoints after any crash
- Produces manuscript figures, a scientific inspection PDF, and a technical run report

### Folder structure
```
repo-root/
├── code/
│   ├── 00-master.R                  # runs all phases in sequence
│   ├── 01-simulation.R              # Phase 1: APSIM grid simulation
│   ├── 02-analysis.R                # Phase 2: manuscript figures
│   ├── 03-report-scientific.R       # Phase 3: scientific PDF report
│   ├── 04-report-simulation.R       # Phase 4: technical run report
│   ├── utils/
│   │   ├── plot-theme.R             # shared ggplot2 theme
│   │   └── variables.R              # soil-fraction variable aggregation
│   └── diagnostics/
│       └── diagnose-windows.R       # step-by-step Windows diagnostic
├── data/
│   ├── raw/
│   │   ├── sim-grid.rds             # spatial grid
│   │   ├── scenarios/               # scenario Excel files
│   │   ├── weather/                 # .met files (gitignored, in Box)
│   │   └── soil/                    # .rds soil profiles (gitignored, in Box)
│   └── outputs/
│       ├── simulated-scenarios-df.rds  # full simulation results
│       ├── simulated-scenarios-df.csv
│       ├── sim-run-log.csv          # per-chunk timing log
│       ├── run-summary.txt          # end-of-run summary
│       └── checkpoints/             # per-chunk RDS files (gitignored)
├── templates/
│   └── soybean-mg4-baseline.apsimx  # APSIM simulation template
├── installers/
│   ├── apsim-7681.deb               # APSIM installer (Linux)
│   └── apsim-7681.exe               # APSIM installer (Windows)
├── figures/                         # all output figures and PDFs
├── raw-data/                        # shapefiles for spatial maps
├── paper/                           # manuscript files
├── CLAUDE.md                        # AI assistant instructions
└── PIPELINE-MEMO.md                 # this file
```

---

## 2. Environment Auto-Detection

**Problem:** The script needs to run on Windows (researcher's workstation) and
Linux (cloud/HPC) without any manual path editing.

**Solution:** A `detect_env()` function at the top of the simulation script
identifies the OS and sets all paths automatically.

```r
os <- tolower(Sys.info()[["sysname"]])   # "windows" or "linux"/"darwin"

if (os == "windows") {
  # APSIM exe — search LocalAppData AND Program Files
  search_roots <- c(file.path(Sys.getenv("LOCALAPPDATA"), "Programs"),
                    "C:/Program Files", "C:/Program Files (x86)")
  apsim_dirs <- sort(grep("APSIM",
    unlist(lapply(search_roots, function(d) {
      if (dir.exists(d)) list.dirs(d, recursive = FALSE) else character(0)
    })), value = TRUE, ignore.case = TRUE))
  apsim_exe <- utils::shortPathName(   # <-- critical: removes spaces from path
    file.path(tail(apsim_dirs, 1), "bin", "Models.exe"))

  # Box Drive data — scan all user profiles automatically
  box_suffix <- file.path("_Projects", "Scale-Sims", "<project>", "data")
  ...
} else {
  apsim_exe <- Sys.which("Models")     # APSIM installed system-wide on Linux
}
```

**Key lessons:**
- Always use `utils::shortPathName()` on Windows — `apsimx` rejects paths with spaces
  (e.g., `C:\Program Files` → `C:\PROGRA~1`)
- Search both `%LOCALAPPDATA%\Programs` and `C:\Program Files` — APSIM 2025+
  installs to `Program Files` by default
- Use `tail(apsim_dirs, 1)` to automatically pick the latest APSIM version

---

## 3. APSIM Template Version Compatibility

**Problem:** "File version is greater than the latest file version" error — APSIM
refuses to open a `.apsimx` file saved by a newer version.

**Solution:** Match the APSIM installation version to the version that created
the template. Keep the installer `.exe` / `.deb` in `installers/` so any
machine can install the correct version.

**Key lesson:** The APSIM build number is embedded in the `.apsimx` JSON. If you
get a version mismatch error, install the APSIM version that matches the template,
not the latest one.

---

## 4. Path Spaces — The #1 Source of Errors

The `apsimx` R package rejects any path containing a space.

| Problem path | Fix |
|---|---|
| `C:\Program Files\APSIM...\Models.exe` | `utils::shortPathName()` → `C:\PROGRA~1\...` |
| `processed data/` (folder name) | Rename to `templates/` or `data/outputs/apsim-work/` via `git mv` |
| `file.path(tempdir(), "x")` on Windows | `normalizePath(file.path(...), mustWork=FALSE)` |

**Rule:** Any path passed to `apsimx()` or `apsimx_options()` must never
contain a space. Always wrap with `utils::shortPathName()` on Windows.

---

## 5. Parallel Simulation Design

### Per-cell isolated directories (critical pattern)
Each cell gets its own subdirectory so workers never collide on `.db` files:

```r
cell_dir <- file.path(apsim_dir, paste0("cell-", j))
dir.create(cell_dir, recursive = TRUE, showWarnings = FALSE)
file.copy(template_path, file.path(cell_dir, "sim.apsimx"))
# ... inject soil, weather, parameters ...
sim <- apsimx("sim.apsimx", src.dir = cell_dir, cleanup = FALSE, silent = TRUE)
unlink(cell_dir, recursive = TRUE)   # clean up after reading results
```

**Why:** APSIM writes `sim.db` to `src.dir`. If two workers share a directory
they overwrite each other's `.db`. Isolated dirs eliminate all race conditions.

### PSOCK cluster setup
```r
cl <- makeCluster(n_cores, type = "PSOCK")
registerDoParallel(cl)

# Export everything workers need before the foreach loop
clusterExport(cl, c("ENV", "scenarios", "apsim_dir", "weather_path",
                     "soil_path", "prepare_soil", "extract_sim_columns", ...))
clusterEvalQ(cl, {
  library(apsimx); library(dplyr); library(DBI); library(RSQLite)
  if (file.exists(ENV$apsim_exe))
    apsimx_options(exe.path = ENV$apsim_exe)
})

stopCluster(cl)   # always stop at the end
```

### `<<-` scoping bug in foreach workers
**Problem:** Using `<<-` inside a plain `if` block inside a `foreach` worker
writes to the global environment instead of the worker's local scope.

**Fix:** Use `<-` in `if` blocks. Only use `<<-` inside `tryCatch(error=function(e){...})`
closures where it correctly assigns to the enclosing worker scope:

```r
apsimx_err <- NULL
sim <- tryCatch(
  apsimx("sim.apsimx", src.dir = cell_dir, ...),
  error = function(e) { apsimx_err <<- e$message; NULL }  # <<- OK here
)
```

---

## 6. Resume / Checkpoint System

### Chunk file naming
```
data/outputs/checkpoints/chunk_sc<sc>_ck<chunk>_<first_cellid>.rds
```
The scenario index, chunk index, and first cell ID are embedded in the filename.
On restart, completed chunks are detected by parsing existing filenames —
no database or lock file needed.

### Resume logic (sketch)
```r
done_files  <- list.files(checkpoint_dir, "chunk_sc%02d_.*\\.rds", ...)
done_cellids <- unlist(lapply(done_files, function(f) readRDS(f)$cellid))
cells_todo  <- setdiff(all_cells, done_cellids)
```

### Per-chunk progress log
`data/outputs/sim-run-log.csv` — written after every chunk with columns:
`scenario, sc_idx, co2, chunk, cells_total, cells_ok, cells_fail, elapsed_sec, errors, timestamp`

---

## 7. Test Mode

Always build a test mode controlled by flags at the top of the script:

```r
TEST_RUN         <- TRUE
TEST_N_CELLS     <- 1000         # will be capped to available data files
TEST_DATE_START  <- "2015-01-01"
TEST_DATE_END    <- "2015-12-31"
TEST_N_SCENARIOS <- 2
```

**Critical in cloud/partial-data environments:** filter cells to only those
with both a `.met` and `.rds` file before sampling:

```r
if (TEST_RUN) {
  met_ids  <- as.integer(sub("\\.met$", "", list.files(weather_path, "\\.met$")))
  soil_ids <- as.integer(sub("\\.rds$", "", list.files(soil_path,    "\\.rds$")))
  avail    <- intersect(met_ids, soil_ids)
  sim.grid1 <- sim.grid1[sim.grid1$cellid %in% avail, ]
  sim.grid1 <- sim.grid1[sample(nrow(sim.grid1), min(TEST_N_CELLS, nrow(sim.grid1))), ]
}
```

---

## 8. Soil Profile Preparation

Standard pattern used for every cell:

```r
prepare_soil <- function(soil_rds_path, KL_VEC, XF_VEC) {
  soil.result <- readRDS(soil_rds_path)
  soils <- soil.result[[1]][[1]]

  # Exponential KS decay — prevents unrealistic deep drainage
  KS_max <- max(soils$soil$KS, na.rm = TRUE)
  soils$soil$KS <- KS_max * exp(seq(0, log(1e-4), length.out = nrow(soils$soil)))

  soils <- apsimx:::fix_apsimx_soil_profile(soils, verbose = FALSE)
  soils$initialwater <- initialwater_parms(
    Depth = soils$soil$Depth, Thickness = soils$soil$Thickness,
    InitialValues = soils$soil$DUL)
  soils$crops <- c("Soybean", "Wheat", "Maize")

  n_lay <- nrow(soils$soil)
  list(soils = soils,
       KL    = KL_VEC[seq_len(min(n_lay, length(KL_VEC)))],
       XF    = XF_VEC[seq_len(min(n_lay, length(XF_VEC)))])
}
```

**Key:** Always truncate `KL` and `XF` vectors to the actual layer count —
APSIM errors if they are longer than the number of soil layers.

---

## 9. Email Notifications

Uses `blastula` + Gmail App Password + system keyring (one-time setup per machine):

```r
# One-time setup in RStudio console:
blastula::create_smtp_creds_key(
  id = "gmail", user = "your@gmail.com", provider = "gmail"
)
# Enter Gmail App Password when prompted
# Get App Password: Google Account → Security → 2-Step Verification → App passwords
```

```r
# In the script:
send_notification <- function(subject, body) {
  if (!NOTIFY) return(invisible(NULL))
  tryCatch({
    email <- blastula::compose_email(body = blastula::md(body))
    blastula::smtp_send(email, from = NOTIFY_FROM, to = NOTIFY_TO,
                        subject = subject,
                        credentials = blastula::creds_key("gmail"))
  }, error = function(e) message("[NOTIFY] Failed — ", e$message))
}
```

**Note:** SMS via carrier email gateways (e.g., `@vtext.com`) is unreliable.
Send to a regular email instead (`eelli@uark.edu`).

---

## 10. Master Script Pattern

A `00-master.R` that runs all phases in sequence with smart skipping:

```r
FORCE_RERUN    <- FALSE
RUN_SIM        <- TRUE
RUN_ANALYSIS   <- TRUE
RUN_REPORT     <- TRUE
RUN_SIM_REPORT <- TRUE

run_phase <- function(phase, title, output_check, script) {
  if (!FORCE_RERUN && all(file.exists(output_check))) {
    cat(sprintf("Phase %s SKIPPED — outputs exist\n", phase)); return()
  }
  source(script, echo = FALSE, local = new.env(parent = globalenv()))
}

run_phase("1", "APSIM simulation",   "data/outputs/simulated-scenarios-df.rds",
          "code/01-simulation.R")
run_phase("2", "Data analysis",      "figures/p1 - climate change without adaptation.tiff",
          "code/02-analysis.R")
run_phase("3", "Inspection report",  "figures/inspection-report.pdf",
          "code/03-report-scientific.R")
run_phase("4", "Simulation report",  "figures/simulation-report.pdf",
          "code/04-report-simulation.R")
```

**Key:** Each phase checks for its own output file before running. Set
`FORCE_RERUN <- TRUE` to redo everything. Toggle individual phases with flags.

---

## 11. Output Reports

| Script | Output | Contents |
|--------|--------|----------|
| `03-data-analysis-6-5-26.R` | `figures/*.tiff` | All manuscript figures |
| `04-inspection-report.R` | `figures/inspection-report.pdf` | All paper figures + 8 reviewer inspection plots |
| `05-simulation-report.R` | `figures/simulation-report.pdf` | 10-page technical run report |

### Simulation report pages (05)
1. Title page — cells, scenarios, years, success rate, total time
2. Scenario definitions table + run details
3. Elapsed time per chunk
4. Total time per scenario + seconds per cell
5. Cell success vs failure per chunk
6. Cumulative cells completed over time
7. Yield distribution by scenario (ridge density)
8. Yield summary by scenario (boxplot)
9. Phenology stages by scenario (mean ± SD)
10. Weather inputs by scenario

### Inspection report figures (04)
- All paper figures (recreated)
- `insp_a` Seed-filling duration ridge density
- `insp_b` Phenology dot-range timeline
- `insp_c` Yield CV boxplot by scenario
- `insp_d` Spatial CV map
- `insp_e` Water balance stacked bar
- `insp_f` Yield vs radiation scatter
- `insp_g` Blooming temperature boxplot
- `insp_h` Radiation use efficiency proxy

---

## 12. Diagnostics Script

`code/diagnose-windows.R` — run this first on any new Windows machine to trace
exactly what APSIM does step by step. It:
- Checks all paths exist
- Detects APSIM exe
- Loads soil profile
- Builds one cell directory
- Runs APSIM with `cleanup = FALSE, silent = FALSE`
- Reads `_Messages` table from the `.db` on failure to show the exact APSIM error

**Adapt it for any new project by changing the 5 path variables at the top.**

---

## 13. .gitignore Essentials

```
intermediate-data/apsim-work/    # temp cell working dirs
data/outputs/checkpoints/    # checkpoint RDS files
data/raw/weather/       # .met files (large, in Box)
data/raw/soil/          # .rds soil profiles (large, in Box)
```

Track: `sim-run-log.csv`, `run-summary.txt`, `simulated-scenarios-df.rds`,
`simulated-scenarios-df.csv`, `figures/`.

---

## 14. Required R Packages

**Simulation:** `apsimx`, `doParallel`, `foreach`, `parallel`, `dplyr`,
`readr`, `readxl`, `DBI`, `RSQLite`

**Analysis:** `ggplot2`, `sf`, `viridis`, `stars`, `dplyr`, `tidyr`,
`lubridate`, `data.table`, `lme4`, `emmeans`, `ggridges`, `cowplot`, `gghalves`, `scales`

**Notifications:** `blastula`, `keyring`

---

## 15. Checklist for a New Similar Project

- [ ] Create repo with same folder structure
- [ ] Write `CLAUDE.md` describing the project for AI assistance
- [ ] Place `.apsimx` template in `templates/ (APSIM template) and installers/ (exe/deb)` (no spaces in path)
- [ ] Note the APSIM build number used to save the template; store installer in `templates/ (APSIM template) and installers/ (exe/deb)`
- [ ] Copy and adapt `detect_env()` with project-specific Box path suffix
- [ ] Copy `prepare_soil()` — adjust `KL_VEC`, `XF_VEC`, and crop list
- [ ] Copy checkpoint/resume logic — only change the chunk filename prefix
- [ ] Copy `00-master.R` and update script names and output check paths
- [ ] Copy `diagnose-windows.R` and update the 5 path variables at the top
- [ ] Set up `.gitignore` for large data dirs
- [ ] Run `diagnose-windows.R` first on any new Windows machine
- [ ] Set `TEST_RUN <- TRUE` for initial validation
- [ ] Set `TEST_RUN <- FALSE` for full run
