# W4 (D6): empty shared panel guard — shared_panel_body_tag / ptr_shared_panel /
# ptr_ui_shared_panel return NULL when panel_keys is empty; ptr_shared_server
# is a safe no-op in the same case.

render_html <- function(tag) {
  if (is.null(tag)) return(NULL)
  paste(as.character(htmltools::renderTags(tag)$html), collapse = "\n")
}

# Two disjoint formula-local ppVar(shared=) keys -> panel_keys = character(0)
f_local_1 <- 'ggplot(iris, aes(x = ppVar(shared = "ax1"), y = Sepal.Width)) + geom_point()'
f_local_2 <- 'ggplot(iris, aes(x = ppVar(shared = "ax2"), y = Petal.Length)) + geom_point()'

# Cross-formula key -> panel_keys = "col"
f_cross_1 <- 'ggplot(mtcars, aes(x = ppVar(shared = "col"), y = mpg)) + geom_point()'
f_cross_2 <- 'ggplot(mtcars, aes(x = ppVar(shared = "col"), y = hp)) + geom_line()'

test_that("SC.1 ptr_shared_panel returns NULL when panel_keys is empty", {
  obj <- ptr_shared(c(f_local_1, f_local_2))
  expect_equal(obj$panel_keys, character(0))
  expect_null(ptr_shared_panel(obj))
})

test_that("SC.2 ptr_ui_shared_panel returns NULL when panel_keys is empty", {
  obj <- ptr_shared(c(f_local_1, f_local_2))
  expect_null(ptr_ui_shared_panel(obj))
})

test_that("SC.3 ptr_shared_panel returns NULL: no ptr-shared-panel in DOM", {
  obj <- ptr_shared(c(f_local_1, f_local_2))
  html <- render_html(ptr_shared_panel(obj))
  expect_null(html)
})

test_that("SC.4 ptr_shared_panel renders panel when panel_keys is non-empty", {
  obj <- ptr_shared(c(f_cross_1, f_cross_2))
  expect_true(length(obj$panel_keys) > 0L)
  html <- render_html(ptr_shared_panel(obj))
  expect_match(html, "ptr-shared-panel", fixed = TRUE)
})

test_that("SC.5 ptr_shared_server constructs safely with empty panel_keys", {
  obj <- ptr_shared(c(f_local_1, f_local_2))
  expect_equal(obj$panel_keys, character(0))
  shiny::testServer(function(input, output, session) {
    st <- ptr_shared_server(obj)
    expect_s3_class(st, "ptr_shared_state")
    expect_equal(names(st$shared), character(0))
  }, {})
})
