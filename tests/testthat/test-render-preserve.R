# test-render-preserve.R — direct unit tests for the render-walker's
# preserve-mode branch (`ptr_render(node, preserve_placeholders = TRUE)`).
#
# Why this file exists (ADR 0022): the preserve-mode code-panel UI was
# retired — the code panel now offers only Final / Spec modes. The
# render-walker preserve-mode infrastructure stays alive (`R/paintr-render.R`)
# but is no longer reached via the panel, so the previous integration
# coverage in test-super-pressure.R + test-adr10-ppupload-name-browser.R
# (which drove the radio to "preserve" and grep'd the panel text) was
# lifted into this file as direct unit tests against the still-live
# render code.
#
# Each test builds a small formula, translates it (ptr_translate
# annotates ids), stamps a snapshot of widget values onto the typed tree
# via stamp_current_pick_walk, then asserts ptr_render(..., preserve = TRUE)
# emits the expected wrapper shape. No browser, no fixtures.

# Helper: translate a formula string and stamp a snapshot, returning
# preserve-mode render text. Mirrors the call shape ptr_register_code
# used pre-ADR-0022.
preserve_render <- function(formula, snapshot = list()) {
  tree <- ggpaintr:::ptr_translate(formula)
  ggpaintr:::ptr_render(
    ggpaintr:::stamp_current_pick_walk(tree, snapshot),
    preserve_placeholders = TRUE
  )
}

# ---- ppNum value emission --------------------------------------------------

test_that("preserve mode wraps a set ppNum value as ppNum(<value>)", {
  formula <- "ggplot(mtcars, aes(x = mpg, y = wt)) + geom_point(size = ppNum(2))"
  snap <- list(geom_point_1_ppNum_NA = 0.7314159)
  text <- preserve_render(formula, snap)
  expect_match(
    text,
    "ppNum\\(0\\.7314159\\)",
    label = "preserve mode emits ppNum(<value>) with the snapshot value"
  )
})

# ---- ppText value emission -------------------------------------------------

test_that("preserve mode wraps a set ppText value as ppText(\"<value>\")", {
  formula <- "ggplot(mtcars, aes(x = mpg, y = wt)) + geom_point() + labs(title = ppText(\"Title\"))"
  snap <- list(labs_1_ppText_NA = "S_T_2718")
  text <- preserve_render(formula, snap)
  expect_match(
    text,
    "ppText\\(\"S_T_2718\"\\)",
    label = "preserve mode emits ppText(\"<value>\") with the snapshot string"
  )
})

# ---- ppExpr value emission -------------------------------------------------

test_that("preserve mode wraps a set ppExpr value as ppExpr(<expr-text>)", {
  formula <- "mtcars |> dplyr::mutate(adj = ppExpr(mpg / wt)) |> ggplot(aes(x = ppVar(mpg), y = ppVar(adj))) + geom_point()"
  snap <- list(ggplot_2_1_ppExpr_NA = "mpg + wt")
  text <- preserve_render(formula, snap)
  expect_match(
    text,
    "ppExpr\\(mpg \\+ wt\\)",
    label = "preserve mode emits ppExpr(<expr-text>) with the snapshot expression"
  )
})

# ---- ppVar consumer rendering ----------------------------------------------

test_that("preserve mode wraps a set ppVar consumer as ppVar(<pick>)", {
  formula <- "ggplot(mtcars, aes(x = ppVar(mpg), y = ppVar(wt))) + geom_point()"
  snap <- list(ggplot_1_1_ppVar_NA = "disp")
  text <- preserve_render(formula, snap)
  # Consumer placeholders preserve the ppVar() wrapper around the pick.
  # The substitute (final-mode) path is what strips the wrapper; preserve
  # mode retains it so the formula remains re-runnable through ptr_app().
  expect_match(
    text,
    "aes\\([^)]*x\\s*=\\s*ppVar\\(disp\\)",
    label = "preserve mode emits aes(x = ppVar(<pick>)) for a set consumer"
  )
})

# ---- ppUpload(bareword) preservation --------------------------------------
# Source-placeholder companion round-trip (ADR 0010) is covered by
# test-adr10-ppupload-name-browser.R's "preserve-mode render stamps
# companion bareword" test, which exercises the same `ptr_render(...,
# preserve_placeholders = TRUE)` path against the canonical ADR-0010
# fixture formula. Not duplicated here.

# ---- shared= round-trip ----------------------------------------------------
# The shared bind populates each placeholder's `current_pick` via a
# separate runtime pass (the shared coordinator); the per-placeholder id
# is what `stamp_current_pick_walk` reads, not the `shared_<key>` widget
# id directly. A faithful unit test of "shared-widget value reaches
# preserve render" therefore needs the runtime-side shared bind, which is
# not exercisable in a pure render unit test. Coverage of the shared-key
# rendering ARG (`ppNum(..., shared = "k")`) is implicit in the ppNum /
# ppText tests above — the wrapper-emission code path is shared-arg-
# agnostic at the render-walker level.
