# Boot scaffolding: see .claude/rules/testing.md "Browser e2e (shinytest2)".
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
# EXCEPT: ADR 0024 — shared-section variant. Single-formula `shared='ds'`
# means the source partition assigns ppUpload to the formula's inline
# shared section (shared_partition → local_keys_by_formula). Canonical id
# `shared_ds`; companion id `shared_ds_name`. The spec= mechanism dispatches
# via the same source_companion role (no scope-keyword filter in
# apply_spec_at_boot for source_companion rows). Asserts the shared
# variant inherits the entry-point semantics for free.
ptr_app(
  "ppUpload(shared='ds') |> ggplot(aes(x = ppVar(shared='col'))) + geom_bar()",
  spec = list(shared_ds_name = "mtcars")
)
