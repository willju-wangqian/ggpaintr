# J12 journey scaffolding (NOT a vignette chunk): one fixture absorbs the
# renderUI-emission half of J12 (stages 4/5/6 in v8 audit). Absorbs the
# testServer-level assertions previously pinned at
# test-adr12b-renderui-emission.R:23, :61, :97 (file deleted as part of
# the J12 browser-faithfulness merge — see dev/audit/audit-test-fidelity-
# v8-j12-browser-faithfulness-2026-05-27-2337.html).
#
# The combined formula exercises three ADR 0012 / PLAN-01 contracts in one
# boot:
#   * stage 4 (renderui-emission.R:23): bare ppNum at ggplot aes pos 2 →
#     numericInput emitted by build_ui with `value = NA_real_`, stringified
#     in HTML as `value="NA"`. Observable at `#ggplot_1_2_ppNum_NA`.
#   * stage 5 (renderui-emission.R:61): `ppVar(mpg)` source at ggplot aes
#     pos 1 → source widget goes through renderUI (post-PLAN-01) so binds
#     after first flush. Observable via the input registry.
#   * stage 6 (renderui-emission.R:97): geom_text's `aes(label=ppText)` and
#     `size=ppNum` → value-side renderUI registers a `_ui` slot for each.
#     Observable via DOM id existence.
#
# bare_ids stay distinct across the formula (`ggplot_1_1_ppVar_NA`,
# `ggplot_1_2_ppNum_NA`, `geom_text_1_3_ppText_NA_ui`,
# `geom_text_2_ppNum_NA_ui`) so the three stage assertions do not collide.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)

ptr_app(
  "ggplot(mtcars, aes(ppVar(mpg), ppNum)) + geom_text(aes(label = ppText), size = ppNum)"
)
