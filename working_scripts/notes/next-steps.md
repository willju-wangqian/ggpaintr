# Next Steps

## Purpose

This file tracks the prioritized follow-up work for the maintained package.

## Current priorities

1. Phase-3 extensibility follow-up
   - decide whether to layer Shiny modules on top of the current id-based
     `ggpaintr_*` integration helpers rather than replacing them
   - decide whether to expose any higher-level helper constructors for common
     custom-placeholder patterns without expanding the low-level registry
     contract too early
   - review whether custom-placeholder export should stay limited to inline hook
     definitions or gain stronger static validation or documented helper
     patterns
   - keep `ggpaintr_*` as the supported public surface while de-emphasizing
     deeper `paintr_*` internals in docs  
     Reference: `R/paintr-app.R:191-567`, `R/paintr-placeholders.R:44-131`,
     `R/paintr-placeholders.R:457-509`, `_pkgdown.yml:7-48`

2. Documentation and submission workflow
   - keep `README.Rmd` as the editable README source and re-knit `README.md`
     after README source changes  
     Reference: `README.Rmd:1-5`
   - keep pkgdown reference pages aligned with the intended supported
     `ggpaintr_*` surface, including the placeholder-registry vignette and
     article navigation  
     Reference: `_pkgdown.yml:7-48`
   - keep manual workbook/checklist examples aligned with the supported custom
     Shiny integration and placeholder-registry paths  
     Reference: `tests/manual/manual-test-ggpaintr.Rmd:602-841`,
     `tests/manual/manual-checklist-ggpaintr.md:58-79`

3. Historical material cleanup
   - decide whether tracked exploratory material under `preconsideration/`
     should remain as explicit repo history or move into a clearer archive path
   - keep non-maintained historical material clearly separated from the
     maintained package surface even if it stays tracked
   - keep archive content, exploratory utilities, and the active package path
     visually distinct for fresh-session re-entry

4. Copy-rule ergonomics
   - review whether the current defaults and examples cover enough common
     parameters, layer-specific unnamed arguments, and custom-placeholder copy
     defaults
   - decide whether additional aliases or layer-specific overrides are needed
     without expanding the public API beyond named-list `copy_rules`
   - keep automated and manual copy-rule coverage in sync if wording changes  
     Reference: `R/paintr-copy.R:12-146`,
     `tests/testthat/test-copy-rules.R`,
     `tests/testthat/test-placeholder-registry.R:64-100`

## Near-term exit conditions

- the current `ggpaintr_*` integration surface remains documented and covered
  by focused automated tests
- durable notes reflect the current exported surface, current runtime
  semantics, and latest verification status
- the placeholder registry remains a stable contributor-facing extensibility
  path without breaking the existing phase-1 integration API
- `NAMESPACE`, `man/`, and `docs/` are regenerated from the active source
- `README.md` is regenerated from `README.Rmd` after README source edits
- the package passes local automated tests
- the local `--as-cran --no-manual` check is green
- the pkgdown site reflects the intended public/internal boundary
- the manual workbook and checklist stay aligned with current copy-rule and
  exported-app behavior, including custom placeholders
- the submission notes reflect the latest check results
