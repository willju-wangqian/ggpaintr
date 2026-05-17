# UI-shape tests for the coordinator panel trio introduced in Step 01 and
# kept partition-correct in Step 02: `ptr_shared()` builds the spec,
# `ptr_shared_panel()` is the self-contained L2 panel, `ptr_ui_shared_panel()`
# the bare L3 fragment. Both render exactly the CROSS-FORMULA (panel) keys
# (single-formula / disjoint keys are formula-local and are NOT shown here
# -- the owning module renders them). Partition + bare-vs-bundled coverage
# lives in test-shared-partition.R; this file covers the remaining panel
# behaviours (structural markers, draw-all gating, the `shared_ui` builder
# escape hatch, value placeholders).

two_consumer <- c(
  'ggplot(mtcars, aes(x = var(shared = "col"), y = mpg)) + geom_point()',
  'ggplot(mtcars, aes(x = var(shared = "col"), y = hp)) + geom_line()'
)

render_html <- function(tag) {
  as.character(htmltools::renderTags(tag)$html)
}

test_that("S-UI.2 ptr_shared_panel renders a wellPanel with the canonical shared inputId", {
  html <- render_html(ptr_shared_panel(ptr_shared(two_consumer)))
  expect_match(html, "well", fixed = TRUE)
  expect_match(html, "ptr-shared-panel", fixed = TRUE)
  # `var(shared = ...)` is a data-consumer; its widget is a uiOutput
  # placeholder the server fills via `ptr_bind_shared_consumer_uis()`.
  expect_match(html, "shared_col_ui", fixed = TRUE)
})

test_that("S-UI.2b ptr_shared_panel stays bare .ptr-app (no --page canvas)", {
  # The shared panel is a region fragment placed in a host layout (its own
  # row above the plots); it must size to its contents, not opt into the
  # full-viewport `ptr-app--page` backdrop.
  html <- render_html(ptr_shared_panel(ptr_shared(two_consumer)))
  expect_match(html, 'class="ptr-app"', fixed = TRUE)
  expect_false(grepl("ptr-app--page", html, fixed = TRUE))
})

test_that("S-UI.3 draw-all button is gated on >= 2 formulas", {
  one <- ptr_shared_panel(ptr_shared(
    'ggplot(mtcars, aes(x = var(shared = "k"), y = mpg)) + geom_point()'
  ))
  two <- ptr_shared_panel(ptr_shared(c(
    'ggplot(mtcars, aes(x = var(shared = "k"), y = mpg)) + geom_point()',
    'ggplot(mtcars, aes(x = var(shared = "k"), y = hp)) + geom_line()'
  )))
  expect_false(grepl("ptr_shared_draw_all", render_html(one), fixed = TRUE))
  expect_true(grepl("ptr_shared_draw_all", render_html(two), fixed = TRUE))
})

test_that("S-UI.4 ptr_shared_errors uiOutput is always present", {
  html <- render_html(ptr_shared_panel(ptr_shared(two_consumer)))
  expect_match(html, "ptr_shared_errors", fixed = TRUE)
})

test_that("S-UI.5 embedder-supplied builder wins; receives canonical id", {
  seen_id <- NULL
  obj <- ptr_shared(
    two_consumer,
    shared_ui = list(
      col = function(id) {
        seen_id <<- id
        shiny::tags$div(id = id, "custom widget")
      }
    )
  )
  html <- render_html(ptr_ui_shared_panel(obj))
  expect_equal(seen_id, "shared_col")
  expect_match(html, "custom widget", fixed = TRUE)
})

test_that("S-UI.6 shared_ui referencing an unknown key aborts at ptr_shared()", {
  expect_error(
    ptr_shared(
      two_consumer,
      shared_ui = list(other = function(id) shiny::tags$div(id = id))
    ),
    "not used in any plot formula"
  )
})

test_that("S-UI.7 value-shared placeholder renders an auto widget at canonical id", {
  obj <- ptr_shared(c(
    'ggplot(mtcars) + geom_point(aes(x = mpg, y = hp), alpha = num(shared = "a"))',
    'ggplot(mtcars) + geom_line(aes(x = mpg, y = hp), alpha = num(shared = "a"))'
  ))
  html <- render_html(ptr_shared_panel(obj))
  expect_match(html, "shared_a", fixed = TRUE)
})

test_that("S-UI.8 disjoint single-formula keys are formula-local, not in the panel", {
  # Replaces the old union semantic: a key used by only ONE formula is
  # formula-local and is NOT surfaced in the cross-formula panel.
  obj <- ptr_shared(c(
    'ggplot(mtcars, aes(x = var(shared = "col"), y = mpg)) + geom_point()',
    'ggplot(mtcars) + geom_point(aes(x = mpg, y = hp), alpha = num(shared = "a"))'
  ))
  expect_length(obj$panel_keys, 0L)
  html <- render_html(ptr_shared_panel(obj))
  expect_false(grepl("shared_col", html, fixed = TRUE))
  expect_false(grepl("shared_a", html, fixed = TRUE))
})
