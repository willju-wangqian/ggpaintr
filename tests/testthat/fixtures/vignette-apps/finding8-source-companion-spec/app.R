# Boot scaffolding (NOT vignette code): load the in-development package into
# this child app process so the e2e test exercises dev source, not a stale
# system install. See `.claude/rules/testing.md` "Browser e2e (shinytest2)" /
# project memory `shinytest2-appdir-pkgload`.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
# EXCEPT: this fixture is NOT a vignette mirror — it is the FINDING #8
# regression from dev/notes/placeholder-role-coverage2.html. spec= for a
# `source_companion` row (the bare-name textInput beside the fileInput in
# ppUpload's tagList) must OVERRIDE `node$default` on the companion's
# first render so that the downstream binder picks the spec-supplied
# frame (mtcars), not the formula's default (iris).
#
# Pre-fix bug: apply_spec_at_boot() writes state$spec_seed[[<companion-id>]]
# AND marks the row in `seeded`, which short-circuits the onFlushed
# updateTextInput dispatch. Meanwhile ptr_builtin_upload_build_ui()
# initial-renders the companion textInput from `node$default %||% ""` only
# — no `state$spec_seed[[<companion-id>]]` read exists ANYWHERE in the
# server-side tree (the existing spec_seed reads at R/paintr-server.R:1745,
# 1815, 1881, 2107, 2406 all use raw_id = node$id, not the companion id).
# Net: companion textInput first-renders with "iris" (node$default),
# ignoring the spec entry. Both `mtcars` and `iris` are in scope so the
# resolve path can lock onto either one — the assertion discriminates by
# checking the ppVar picker for "cyl" (mtcars-only column).
ptr_app(
  "ppUpload(iris) |> ggplot(aes(x = ppVar('col'))) + geom_bar()",
  spec = list(ggplot_0_ppUpload_NA_name = "mtcars")
)
