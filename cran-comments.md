## Test environments

- Local macOS (Darwin 24.6.0, `R 4.2.3`, `aarch64-apple-darwin20`)

## R CMD check results

- `R CMD check --as-cran --no-manual ggpaintr_0.1.0.tar.gz`
  completed with:
  - 0 errors
  - 0 warnings
  - 1 note
- The remaining note:
  - `unable to verify current time`
  - this is an environment/timestamp check note, not a package issue

## Resubmission

- Unified API prefix: all exported functions now use the `ggpaintr_*`
  prefix for naming consistency.
- Migrated error signaling from `base::stop()` to `rlang::abort()`.
- Added `@examples` to all exported functions.
- Removed legacy `preconsideration/` directory from tracking.
