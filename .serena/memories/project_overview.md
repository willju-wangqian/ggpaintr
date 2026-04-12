# ggpaintr — Project Overview

R package that converts ggplot-like formula strings into Shiny apps.
Placeholder tokens in formulas (`var`, `text`, `num`, `expr`, `upload`) become Shiny input widgets automatically.

## Tech Stack
- Language: R
- Key deps: ggplot2, shiny, shinyWidgets, rlang, assertthat, cli, purrr, bslib
- Dev tools: devtools, roxygen2, testthat (edition 3), pkgdown

## Codebase Structure
- `R/paintr-*.R` — source files
- `tests/testthat/test-*.R` — tests
- `tests/testthat/helper-*.R` — test helpers
- `tests/testthat/fixtures/` — test fixtures
- `vignettes/` — pkgdown vignettes
- `man/` — auto-generated roxygen docs (never edit by hand)
- `NAMESPACE` — auto-generated (never edit by hand)
- `dev/` — developer notes and task prompts
- `.claude/specs/` — decision specs (read SPECS.md index first)

## Key Source Files
- `R/paintr-parse.R` — formula parsing
- `R/paintr-runtime.R` — Shiny reactive logic
- `R/paintr-app.R` / `R/paintr-app-bslib.R` — app entry points
- `R/paintr-ui.R` — UI generation
- `R/paintr-placeholders.R` — placeholder registry
- `R/paintr-copy.R` — UI text / copy rules
- `R/paintr-data.R` / `R/paintr-upload.R` — data handling
- `R/paintr-utils.R` — internal helpers
