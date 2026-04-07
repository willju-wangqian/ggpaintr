# Next Steps

## Purpose

This file tracks the prioritized follow-up work for the maintained package.

## Current priorities

1. Tighten internal documentation output
   - decide whether internal roxygen topics should remain in `man/` or move to
     `@noRd`
   - keep exported reference docs small and user-facing
   - review whether pkgdown should continue surfacing internal helpers such as
     `register_var_ui_outputs()`

2. Column-name robustness
   - improve `var` handling for spaces and other non-syntactic names
   - align static-data and upload-data behavior
   - add focused tests for spaced and non-standard names

3. Export-template usability
   - decide whether the exported `ui` / `server` template should expose more
     guidance around extending `paintr_state$runtime()`
   - keep the exported app readable while preserving the package-backed runtime

4. Better labels for unnamed arguments
   - improve fallback labels for placeholders inside unnamed arguments
   - add tests for nested and repeated-placeholder calls

5. Submission cleanup
   - refresh `cran-comments.md` with the latest clean check result
   - review examples, vignette timing, and package metadata against CRAN policy

6. Non-package cleanup
   - decide whether old exploratory `working_scripts/` and `tests/manual/shiny_export_trial/` files that still mention `paintr2` should be renamed, archived, or left as historical utilities
   - keep archive content and non-active historical material clearly separated from the maintained package surface

## Near-term exit conditions

- `NAMESPACE`, `man/`, and `docs/` are regenerated from the active source
- the package passes local automated tests
- the package gets through a clean `R CMD check`
- the pkgdown site reflects the intended public/internal boundary
- the submission notes reflect the latest check results
