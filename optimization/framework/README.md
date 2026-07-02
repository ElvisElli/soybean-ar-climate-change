# Soybean calibration framework

A self-contained, config-driven framework for calibrating APSIM Next Gen
soybean cultivar parameters against multi-environment observations. It is
built to be **portable** (download the repo, run one command, no edits),
**declarative** (add or remove a parameter by editing one list), and
**parallel** (every site runs concurrently).

---

## Quick start

```bash
Rscript optimization/framework/run.R
```

That's it. The script finds the project root from its own location,
auto-detects the APSIM `Models` binary (Windows / Linux / macOS), cleans
the observed data, and calibrates every group. Results land in
`optimization/framework/results/`.

**Requirements:** R with `apsimx`, `parallel`, `jsonlite`, `ggplot2`; a
working APSIM Next Gen install; and the `soil/` + `weather/` folders in
`optimization/simulations/` (same inputs the grid simulation uses).

---

## What it produces

```
results/
  <group>-coefficients.csv         fitted vs starting values, per group
  all-coefficients.csv             everything combined
  <group>-calibrated-phen.rds      predicted vs observed phenology (calibrated)
  <group>-uncalibrated-phen.rds    ... at starting values
  <group>-calibrated-yield.rds     predicted vs observed yield
  <group>-uncalibrated-yield.rds
  optimization-progress.csv        every objective evaluation (drives the plot)
  calibrated-cultivars.apsimx      a template copy with NEW ready-to-use
                                   cultivars (EarlyMG4_cal, ...) holding the
                                   calibrated parameters
  figures/
    phenology-1to1.png             observed vs predicted DOY, by group
    yield-1to1.png                 observed vs predicted yield, by group
    progress.png                   objective error per evaluation, by stage
```

`calibrated-cultivars.apsimx` is the deliverable for downstream use: open
it in APSIM (or point the grid sim at it) and select `EarlyMG4_cal`, etc.

---

## The one file you edit: `config.R`

Everything project-specific lives in `config.R`. The engine in `modules/`
never needs editing.

### Add or remove a calibrated parameter

1. Add (or delete) one entry in `PARAMETERS`. Each entry gives the unique
   APSIM Command substring, how many scalar values it has (`n`), and a
   `render()` that formats the value(s) for APSIM.
2. Add (or delete) the matching column(s) in `START_VALUES` (an `n=2`
   parameter contributes `<name>1` and `<name>2`).
3. Reference it in whichever `STAGES` entry should optimize it.

The optimizer vector, simplex, edits, coefficient table, and the
ready-to-use cultivar file all resize automatically. The engine validates
the `PARAMETERS` ⇄ `START_VALUES` layout on startup and stops with a clear
message if they disagree.

### Staged (sequential) calibration

`STAGES` defines an ordered plan. Each stage optimizes only its own
parameters while holding all others fixed at their current best, scored
against a stage-appropriate target, then carries the result forward. The
default plan:

| Stage | Parameters | Fit against |
|-------|-----------|-------------|
| `vegetative`   | `veg`, `early_flowering`, `veg_photoperiod` | VE, R1 dates |
| `reproductive` | `early_grain`, `late_grain`, `photoperiod`  | R3, R5, R7, R8 dates |
| `yield`        | `rue`, `area_largest_leaf`                  | final yield |

This mirrors standard crop-model practice: fix vegetative timing first,
then reproductive timing, then yield — so later fits can't distort earlier
ones. To calibrate everything at once instead, collapse `STAGES` into a
single entry listing all parameters.

### Quality control (`QC`)

The observed dataset has known problems, handled here and reported line by
line at runtime:

- **Implausible planting dates.** 25–70 % of records carry Feb/Mar/Dec
  planting dates that are agronomically impossible for the Northern-
  hemisphere US sites and wreck the phenology match. `planting_doy_min/max`
  keep only a sensible window (default Apr 1 – Jul 1, matching the range in
  the paper's own Figure S1). This is the single biggest driver of fit
  quality — leaving it off roughly doubles RMSE.
- **Replicate duplication.** Replicate plots collapse to the same
  site-year-planting; `dedup_replicates` averages them to one point per
  group-site-year-planting-stage so the scatter isn't silently over-plotted.
- **Southern-hemisphere sites** (`drop_southern`): DOY seasons are
  inverted, so drop `lat < 0`.
- **R6 stage** (`fit_drop_stages`): APSIM's end-of-grain-fill stage is
  structurally mismatched to the observed R6 (both this work and the paper
  show ~35–42 d error there); excluded from fit statistics but still plotted.

> Note: `id` is built as `mg2-site-year-planting` because the same
> site/year/planting hosts several maturity groups — an id without the
> group would merge different groups during de-duplication.

---

## How it works (module tour)

| Module | Responsibility |
|--------|----------------|
| `run.R` | entrypoint: locate root, load everything, loop over groups, save, plot |
| `modules/setup.R` | libraries, project root, APSIM executable auto-detection |
| `modules/data.R` | load observations, apply QC, build the stage dictionary |
| `modules/template.R` | copy the production template; patch the report and clone one cultivar per group (calibration-only, never touches the shared template) |
| `modules/simulate.R` | run one site through APSIM; fan a group's sites across a PSOCK cluster |
| `modules/optimize.R` | parameter mapping, per-stage objective, the staged optimizer |
| `modules/outputs.R` | save coefficients + comparison data; write the ready-to-use cultivars |
| `modules/figures.R` | phenology & yield 1:1 plots and the progress plot |

### Calibration-only template edits

Three edits are applied to a *local copy* of the production grid-sim
template, never the shared file:

1. The daily report is given phenology-stage variables and renamed
   `PhenologyReport` (its original name `Report` collided with the
   EndOfYear report's results table).
2. One cultivar per group is cloned from its production parent so each
   group's parameters can move independently.
3. The "harvest by min temp" manager is neutralized per run — its rolling
   min-temp trigger otherwise forces instant maturity when the 90-day
   pre-sowing buffer still carries winter temperatures at an early sowing.

---

## Performance

- One PSOCK cluster per group, sized to `min(cores, #sites)`, **reused**
  across all stages and the final comparison runs.
- Per-worker soil/weather reads are memoized (constant across the thousands
  of evaluations a group needs).
- `cores` defaults to physical cores − 1. On a many-core machine the early
  groups (~130 sites) parallelize almost linearly.

**Future speedup (not yet implemented):** the dominant cost is launching one
APSIM process per site per evaluation. Batching all of a group's sites into a
single multi-simulation `.apsimx` file — which APSIM runs internally in
parallel from one process launch — would cut per-evaluation overhead
substantially. It's a larger change (programmatic per-site Simulation
cloning with per-simulation soil/weather) and is left as the next
optimization.

---

## Resuming

The run is resumable at the group level: a group whose
`<group>-coefficients.csv` already exists is skipped and reused. Delete that
file to recalibrate just that group.
