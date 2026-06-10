# Runtime draw-gate contracts (ADR 0028 verified baseline).
#
# Pins three as-is behaviors of the existing runtime against real browser
# sessions (fixtures under fixtures/vignette-apps/adr28-*):
#   - a reactive pipeline head (`rx() |> ggplot(...)`) boots, seeds the ppVar
#     pickers, and draws — no core support needed;
#   - `draw_trigger` is a click-counter contract (`clicked()`: numeric scalar
#     >= 1) — a reactive carrying a data frame invalidates the runtime
#     observer but never triggers a draw;
#   - `ptr_options(gate_draw = FALSE)` live mode re-renders on any reactive
#     the formula reads, including the pipeline head, with no Update click.
#
# The expected values come from the documented contracts (draw_trigger
# roxygen, ptr_options gate_draw docs, CONTEXT.md "Draw" term), not from
# running the implementation. The live-mode test doubles as the positive
# control for the "plot html changed" detector: the same shrink click that
# MUST NOT change the plot under the counter-gate fixture MUST change it in
# live mode, proving the no-change assertion has teeth.

plot_html <- function(app) app$get_html("#p-ptr_plot")

shrink_and_settle <- function(app) {
  app$click("shrink")
  app$wait_for_idle(timeout = 15 * 1000)
}

test_that("reactive pipeline head boots and seeds pickers", {
  app <- boot_vignette_app("adr28-gate-counter-trigger")

  # Boots and draws: the host shows a rendered ggplot with no inline error.
  expect_host_settled(app, "p-ptr_plot", "ggplot", "p-ptr_error")

  # Seeds: both ppVar pickers carry the formula defaults from the reactive
  # head's columns (wt / mpg), not blanks.
  expect_input_eventually(app, "p-ggplot_1_1_ppVar_NA", "wt")
  expect_input_eventually(app, "p-ggplot_1_2_ppVar_NA", "mpg")
})

test_that("draw_trigger ignores non-counter values", {
  app <- boot_vignette_app("adr28-gate-counter-trigger")
  expect_host_settled(app, "p-ptr_plot", "ggplot", "p-ptr_error")

  before <- plot_html(app)
  expect_true(nzchar(before %||% ""))

  # Changing the reactive carried by draw_trigger (a data frame, not a click
  # counter) must NOT redraw: clicked() rejects non-numeric values, and no
  # Update click happened. The identical-html assertion discriminates — the
  # same shrink on the live-mode fixture below produces different html.
  shrink_and_settle(app)
  wait_for_output(app, "p-ptr_plot")
  expect_identical(plot_html(app), before)
})

test_that("gate_draw=FALSE redraws on reactive pipeline-head change", {
  app <- boot_vignette_app("adr28-gate-live-reactive-head")
  expect_host_settled(app, "p-ptr_plot", "ggplot", "p-ptr_error")

  # Live mode drops the Update button entirely.
  expect_no_dom_id(app, "p-ptr_update_plot")

  before <- plot_html(app)
  expect_true(nzchar(before %||% ""))

  # Changing the pipeline head's data re-renders with no click: the live
  # observer holds a reactive dependency on rx() through the formula eval.
  app$click("shrink")
  after <- poll_html(
    app, "#p-ptr_plot",
    function(h) !is.null(h) && nzchar(h) && !identical(h, before)
  )
  expect_false(identical(after, before))
  expect_match(after %||% "", "<img", fixed = TRUE)
})
