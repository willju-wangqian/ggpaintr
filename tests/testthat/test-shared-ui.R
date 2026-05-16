# UI-shape tests for `ptr_shared_ui()` -- the page-level shared widget
# panel used by multi-instance embedding. Renders one wellPanel hosting
# one widget per shared key declared across the supplied formulas.

test_that("S-UI.1 ptr_shared_ui errors when no formula declares a shared key", {
  expect_error(
    ptr_shared_ui("ggplot(mtcars, aes(x = var, y = var)) + geom_point()"),
    "declare no `shared"
  )
})

test_that("S-UI.2 ptr_shared_ui renders a wellPanel with the canonical shared inputId", {
  tag <- ptr_shared_ui(
    'ggplot(mtcars, aes(x = var(shared = "col"), y = mpg)) + geom_point()'
  )
  html <- as.character(htmltools::renderTags(tag)$html)
  expect_match(html, "well", fixed = TRUE)
  expect_match(html, "ptr-shared-panel", fixed = TRUE)
  # `var(shared = ...)` is a data-consumer; its widget is a uiOutput
  # placeholder that the server fills via `ptr_bind_shared_consumer_uis()`.
  expect_match(html, "shared_col_ui", fixed = TRUE)
})

test_that("S-UI.2b ptr_shared_ui stays bare .ptr-app (no --page canvas)", {
  # The shared panel is a region fragment placed in a host layout (its own
  # row above the plots); it must size to its contents, not opt into the
  # full-viewport `ptr-app--page` backdrop.
  tag <- ptr_shared_ui(
    'ggplot(mtcars, aes(x = var(shared = "col"), y = mpg)) + geom_point()'
  )
  html <- as.character(htmltools::renderTags(tag)$html)
  expect_match(html, 'class="ptr-app"', fixed = TRUE)
  expect_false(grepl("ptr-app--page", html, fixed = TRUE))
})

test_that("S-UI.3 draw-all button is gated on length(formulas) >= 2", {
  one <- ptr_shared_ui(
    'ggplot(mtcars, aes(x = var(shared = "k"), y = mpg)) + geom_point()'
  )
  two <- ptr_shared_ui(c(
    'ggplot(mtcars, aes(x = var(shared = "k"), y = mpg)) + geom_point()',
    'ggplot(mtcars, aes(x = var(shared = "k"), y = hp)) + geom_line()'
  ))
  one_html <- as.character(htmltools::renderTags(one)$html)
  two_html <- as.character(htmltools::renderTags(two)$html)
  expect_false(grepl("ptr_shared_draw_all", one_html, fixed = TRUE))
  expect_true(grepl("ptr_shared_draw_all", two_html, fixed = TRUE))
})

test_that("S-UI.4 ptr_shared_errors uiOutput is always present", {
  tag <- ptr_shared_ui(
    'ggplot(mtcars, aes(x = var(shared = "col"), y = mpg)) + geom_point()'
  )
  html <- as.character(htmltools::renderTags(tag)$html)
  expect_match(html, "ptr_shared_errors", fixed = TRUE)
})

test_that("S-UI.5 embedder-supplied builder wins; receives canonical id", {
  seen_id <- NULL
  tag <- ptr_shared_ui(
    'ggplot(mtcars, aes(x = var(shared = "col"), y = mpg)) + geom_point()',
    shared_ui = list(
      col = function(id) {
        seen_id <<- id
        shiny::tags$div(id = id, "custom widget")
      }
    )
  )
  expect_equal(seen_id, "shared_col")
  html <- as.character(htmltools::renderTags(tag)$html)
  expect_match(html, "custom widget", fixed = TRUE)
})

test_that("S-UI.6 shared_ui referencing an unknown key aborts", {
  expect_error(
    ptr_shared_ui(
      'ggplot(mtcars, aes(x = var(shared = "col"), y = mpg)) + geom_point()',
      shared_ui = list(other = function(id) shiny::tags$div(id = id))
    ),
    "not used in any plot formula"
  )
})

test_that("S-UI.7 value-shared placeholder renders an auto widget at canonical id", {
  tag <- ptr_shared_ui(
    'ggplot(mtcars) + geom_point(aes(x = mpg, y = hp), alpha = num(shared = "a"))'
  )
  html <- as.character(htmltools::renderTags(tag)$html)
  expect_match(html, "shared_a", fixed = TRUE)
})

test_that("S-UI.8 union of shared keys across formulas is rendered", {
  tag <- ptr_shared_ui(c(
    'ggplot(mtcars, aes(x = var(shared = "col"), y = mpg)) + geom_point()',
    'ggplot(mtcars) + geom_point(aes(x = mpg, y = hp), alpha = num(shared = "a"))'
  ))
  html <- as.character(htmltools::renderTags(tag)$html)
  expect_match(html, "shared_col_ui", fixed = TRUE)
  expect_match(html, "shared_a", fixed = TRUE)
})
