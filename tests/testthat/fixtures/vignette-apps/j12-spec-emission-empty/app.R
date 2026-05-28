# J12 journey scaffolding (NOT a vignette chunk): smallest fixture for the
# empty-spec placeholder line. Absorbs the testServer-level assertions
# previously pinned at test-ptr-spec-emission.R:200 (DOM-explicit) and
# :133 (DOM-equivalent via the same panel observation) -- both deleted as
# part of the J12 browser-faithfulness merge -- see dev/audit/audit-test-
# fidelity-v8-j12-browser-faithfulness-2026-05-27-2337.html.
#
# The formula is TRULY placeholder-free (no ppVar / ppText / ppNum) so
# the runtime fires on Update click with no overrides: state$spec() is
# empty, format_spec_for_panel emits the ADR-0022 placeholder line "No
# overrides yet" rather than a `ptr_spec <- list(...)` block.
#
# NOTE: `app-basic` is NOT placeholder-free (it has `aes(ppVar, ppVar,
# color=ppVar) + labs(title=ppText)`), so even on an empty boot its
# `labs_1_ppText_NA = ""` lands in state$spec() and produces a spec
# block. That is why this J12-specific empty fixture exists rather than
# reusing app-basic.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)

ptr_app("ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()")
