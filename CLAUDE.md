# CLAUDE.md — soybean-ar-climate-change

Guidance for Claude Code when working in this repository.

## Project purpose

Grid-scale APSIM Next Generation soybean simulations across Arkansas cropland (~4,651 fields),
40-year weather record (1985–2024), multiple climate-change and adaptation scenarios.
The results feed a manuscript studying the synergistic effect of maturity-group shift (MG4→MG5)
and earlier planting (May 15 → May 3) under a +2 °C warming scenario.

## Running the simulation

```r
# From repo root in RStudio:
source("code/00-master.R")

# Or from command line (Windows):
Rscript code\01-simulation.R
```

The script is **fully resumable** — re-run any time; completed chunks are skipped automatically.

| Setting | Default | Notes |
|---|---|---|
| `CHUNK_SIZE` | 50 | cells per parallel task |
| `DATE_START/END` | 1985–2024 | simulation clock |
| Cores | `nCores - 2` | leaves 2 free for OS |

## How to use this environment productively

1. Start with `source("code/00-master.R")` so each phase runs in order and auto-skips completed outputs.
2. Use the resumable workflow in `code/01-simulation.R` — if a run stops, restart instead of rerunning from scratch.
3. Keep large weather/soil data outside git (as already configured) and only version results needed for analysis/reporting.
4. Use `code/diagnostics/diagnose-windows.R` first on a new machine to quickly catch path/APSIM setup issues.
5. Keep iteration fast: run simulation first, then analysis/report scripts after new outputs are generated.

## Environment auto-detection

The script identifies the machine at startup — no manual configuration needed:

| Environment | APSIM exe | Weather/soil | Tmp dir |
|---|---|---|---|
| Windows (any user) | Latest APSIM under `%LOCALAPPDATA%\Programs` | Scans all Box mount points across user profiles | `C:\temp\apsim-proc` |
| Linux / cloud | Auto-detected | `intermediate-data/weather` + `intermediate-data/soil` | `/tmp/apsim-proc` |

To add a new Windows machine: nothing needed — the Box scan and APSIM auto-detection handle it.

## Pipeline scripts

| Script | Purpose |
|---|---|
| `code/01-simulation.R` | Run APSIM across grid × scenarios (parallel) |
| `code/02-analysis.R` | Generate all manuscript figures |
| `code/variables.R` | Soil-fraction weighted variable aggregation |
| `code/utils/plot-theme.R` | Shared ggplot2 theme |

## Key intermediate data files

| File | Description |
|---|---|
| `data/raw/sim-grid.rds` | Spatial grid (x, y, cellid, cultivated flag) |
| `data/outputs/simulated-scenarios-df.rds` | Full simulation results (~30 MB) |
| `data/raw/scenarios/soy-scenarios-10-24.xlsx` | Scenario definitions |
| `data/outputs/checkpoints/` | Per-chunk checkpoint RDS files (gitignored) |
| `data/outputs/sim-run-log.csv` | Per-chunk progress log (timing, errors) |
| `data/outputs/run-summary.txt` | End-of-run inspection report |
| `data/raw/weather/` | `.met` files (in Box, gitignored) |
| `data/raw/soil/` | `.rds` soil profiles (in Box, gitignored) |

## Scenarios

Defined in `data/raw/scenarios/soy-scenarios-10-24.xlsx`:

| Scenario | Cultivar | Sowing | CO₂ (ppm) | Climate |
|---|---|---|---|---|
| baseline | MG4 | May-15 | 350 | current |
| climate_change | MG4 | May-15 | 350 | +2°C |
| climate_change (CO2) | MG4 | May-15 | 540 | +2°C |
| early_sowing | MG4 | May-3 | 350 | +2°C |
| early_sowing (CO2) | MG4 | May-3 | 540 | +2°C |
| longer_mat | MG5 | May-15 | 350 | +2°C |
| longer_mat (CO2) | MG5 | May-15 | 540 | +2°C |
| early_sowing_longer_mat | MG5 | May-3 | 350/540 | +2°C |

## APSIM template

`processed data/_soybean-10-24-25.apsimx` — soybean simulation with ClimateController node
(shifts temperature by `delta_T` from `EnableDate`), CO2 node, and a sowing manager for
MG-specific cultivars.

## Output columns (simulated-scenarios-df.rds)

Scenario/treatment: `cultivar`, `sowing`, `scenario`, `climate.control`, `co2`, `rowSpacing`
Spatial: `x`, `y`, `cellid` (+ any other sim.grid columns)
Temporal: `date`
Phenology (DAS): `EmergenceDAS`, `FloweringDAS`, `SeedFillingDAS`, `MaturityDAS`
Yield: `Yield_kgha`, `biomass_kgha`, `CumRadiationInterceptionOnGreen`
Weather: `SeasonRain`, `SeasonRadn`, `SeasonMaxt`, `SeasonMint`, `SeasonMeanT`, `BloomingSeasonMaxt`
Physiology: `Silking_RUE_Temp`, `Silking_Supply_Demand_Ratio`
Soil water: `swhc_6in`, `swhc_12in`, `swhc_24in`
Water balance: `Crop_ET`, `WDrainage`, `WRunoff`, `sWUE`

## Crash recovery

- **Per-chunk RDS checkpoints** in `data/outputs/checkpoints/` survive crashes.
- **Filename pattern** `chunk_sc<sc>_ck<chunk>_<first_cellid>.rds` encodes scenario + chunk index.
- Re-running the script resumes from the last completed chunk automatically.
- Progress log at `data/outputs/sim-run-log.csv` tracks cells attempted/ok/failed per chunk.

## Paper

In `paper/` — manuscript, supplementary material, and reviewer response for:
*"Synergistic maturity group and planting date shifts sustain soybean yields under warming"*
Submitted to *Agricultural & Environmental Letters* (AEL-2026-04-0047-RL).

Reviewer requests that need additional work:
1. New figure: seed-filling duration across scenarios (`SeedFillingDAS` already in results)
2. Mechanistic analysis: photothermal quotient during seed-fill
3. Additional scenarios: potentially earlier sowing dates or MG6

## Required R packages

`apsimx`, `doParallel`, `foreach`, `dplyr`, `readr`, `readxl`, `parallel`,
`ggplot2`, `sf`, `viridis`, `stars`, `tidyr`, `data.table`, `lme4`, `emmeans`,
`ggridges`, `cowplot`, `gghalves`
