# Repository Guidance

## Scope

- Treat this repo as a small R/Quarto project with a large local data cache.
- Prefer targeted reads of `scripts/*.R`, `reporte.qmd`, and `README.md`.
- Do not recurse into `data/` or `output/` unless the task explicitly requires it.

## High-cost Paths

- `data/` contains multi-GB ENIGHUR source files and duplicated vendor extracts.
- `output/` contains generated figures and tables.

## Safe Commands

- Fast smoke check: `Rscript scripts/check.R`
- Main analysis run: `Rscript scripts/01_ingresos_hogares.R`
- Quarto render: `quarto render reporte.qmd`

## Working Rules

- Default to the smoke check before suggesting or running the full analysis script.
- Avoid commands that scan the full repo tree when a file-targeted command is enough.
- Assume raw data files under `data/` are local inputs, not code that needs review.
- Do not install packages as part of normal verification; fail fast and tell the user to install the missing packages explicitly.

## Project Layout

- `scripts/01_ingresos_hogares.R`: main analysis script that loads the slim 2012 inputs and 2025 working `.RData`, then writes figures.
- `scripts/utils.R`: shared plotting helpers.
- `scripts/packages.R`: package loading checks.
- `scripts/check.R`: cheap verification entrypoint that does not load the large datasets.
- `reporte.qmd`: report outline and Quarto entrypoint.
