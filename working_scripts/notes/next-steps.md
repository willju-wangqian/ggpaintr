# Next Steps

## Purpose

This file tracks the prioritized follow-up work for the maintained package.

## Current priorities

1. Tighten internal documentation output
   - decide whether internal roxygen topics should remain in `man/` or move to
     `@noRd`
   - review whether pkgdown should continue surfacing internal helpers such as
     `register_var_ui_outputs()` and whether exported copy helpers should stay
     prominent in user-facing docs
   - keep exported reference docs small and user-facing

2. Column-name robustness
   - improve `var` handling for spaces and other non-syntactic names
   - align static-data and upload-data behavior
   - add focused tests for spaced and non-standard names

3. Export-template usability
   - preserve the exported app as an understandable, editable, extensible
     starting point rather than hiding its `ui` or `server` structure
   - keep multiline `input_formula` readable in generated app source
   - keep `copy_rules <- NULL` discoverable in default exports while preserving
     non-default custom-copy parity through compact `custom_copy_rules`
     overrides

4. Copy-rule ergonomics
   - review whether the current defaults and examples cover enough common
     parameters and layer-specific unnamed arguments
   - decide whether additional aliases or layer-specific overrides are needed
     without expanding the public API beyond named-list `copy_rules`
   - keep automated and manual copy-rule coverage in sync if wording changes

5. Submission cleanup
   - refresh `cran-comments.md` with the latest `--as-cran --no-manual` result
     and note that the remaining local note is the environment time-verification
     note
   - review examples, vignette timing, and package metadata against CRAN policy

6. Non-package cleanup
   - decide whether old exploratory `working_scripts/` and
     `tests/manual/shiny_export_trial/` files that still mention `paintr2`
     should be renamed, archived, or left as historical utilities
   - keep archive content and non-active historical material clearly separated
     from the maintained package surface

## Near-term exit conditions

- `NAMESPACE`, `man/`, and `docs/` are regenerated from the active source
- the package passes local automated tests
- the local `--as-cran --no-manual` check is green aside from at most the
  environment-only time note
- the pkgdown site reflects the intended public/internal boundary
- the manual workbook and checklist stay aligned with current copy-rule and
  exported-app behavior
- the submission notes reflect the latest check results
