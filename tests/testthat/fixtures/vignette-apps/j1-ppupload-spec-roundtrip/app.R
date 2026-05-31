# Boot scaffolding (NOT vignette code): load the in-development package into
# this child app process so the e2e test exercises dev source, not a stale
# system install. See `.claude/rules/testing.md` "Browser e2e (shinytest2)" /
# project memory `shinytest2-appdir-pkgload`.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
# EXCEPT: NOT vignette-paired -- this fixture exists for J1 stage 2
# (spec-roundtrip-bound-names-fallback.R :25 + :94 absorbed). It exercises
# the snapshot-fallback contract from PLAN-06 / ADR 0025 §6: after a real
# upload, the upload observer writes `state$bound_names[[key]](node$auto_name)`;
# if the shortcut textbox is empty, the snapshot-write loop inside
# `ptr_setup_runtime()` overwrites the empty `snapshot[[shortcut_id]]` with
# the auto-name from bound_names, so `state$spec()` carries the auto-name
# at the shortcut-id key. When the user types into the shortcut textbox,
# the typed value wins over the bound_names fallback in the same loop.
ptr_app("ppUpload |> ggplot(aes(x = ppVar(mpg), y = ppVar(hp))) + geom_point()")
