# Next Steps

## Purpose

This file tracks the prioritized follow-up work for the maintained package.

## Current priorities

1. Phase-2 extensibility
   - define a contributor-facing extension path for adding new
     placeholder/widget types without editing unrelated parser, UI, runtime,
     and export code
   - decide whether to layer Shiny modules on top of the current id-based
     phase-1 helpers rather than replacing them
   - keep `ggpaintr_*` as the supported integration surface while hiding or
     de-emphasizing deeper `paintr_*` internals in reference docs
   - decide whether exported apps should stay on the current
     `ggpaintr_server()` template or later gain an optional binder-based export
     path
   - build phase 2 on the current phase-1 surface in `R/paintr-app.R`,
     `NAMESPACE`, and `_pkgdown.yml`  
     Reference: `R/paintr-app.R:21-535`, `NAMESPACE:3-23`,
     `_pkgdown.yml:7-42`

2. Column-name robustness
   - improve `var` handling for spaces and other non-syntactic names
   - align static-data and upload-data behavior
   - add focused tests for spaced and non-standard names

3. Documentation and submission workflow
   - refresh `cran-comments.md` with the latest clean `--as-cran --no-manual`
     result
   - keep `README.Rmd` as the editable README source and re-knit `README.md`
     after README source changes  
     Reference: `README.Rmd:1-5`
   - keep pkgdown reference pages aligned with the intended supported
     `ggpaintr_*` integration surface  
     Reference: `_pkgdown.yml:7-42`

4. Copy-rule ergonomics
   - review whether the current defaults and examples cover enough common
     parameters and layer-specific unnamed arguments
   - decide whether additional aliases or layer-specific overrides are needed
     without expanding the public API beyond named-list `copy_rules`
   - keep automated and manual copy-rule coverage in sync if wording changes

5. Non-package cleanup
   - decide whether old exploratory `working_scripts/` and
     `tests/manual/shiny_export_trial/` files that still mention `paintr2`
     should be renamed, archived, or left as historical utilities
   - keep archive content and non-active historical material clearly separated
     from the maintained package surface

## Near-term exit conditions

- the phase-1 `ggpaintr_*` integration surface remains documented and covered by
  focused automated tests
- phase 2 picks a contributor-facing extensibility path without breaking the
  current id-based phase-1 API
- `NAMESPACE`, `man/`, and `docs/` are regenerated from the active source
- `README.md` is regenerated from `README.Rmd` after README source edits
- the package passes local automated tests
- the local `--as-cran --no-manual` check is green
- the pkgdown site reflects the intended public/internal boundary
- the manual workbook and checklist stay aligned with current copy-rule and
  exported-app behavior
- the submission notes reflect the latest check results
