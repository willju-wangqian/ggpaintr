# Step 01 (#S) -- cross-formula shared partition + the ptr_shared()
# coordinator and its two panel renderers. The partition is net-new:
# nothing before this counted cross-formula key occurrence.

f1 <- 'ggplot(mtcars) + geom_point(aes(x = mpg, y = hp), alpha = ppNum(shared = "B"), size = ppNum(shared = "A"))'
f2 <- 'ggplot(mtcars) + geom_point(aes(x = mpg, y = hp), alpha = ppNum(shared = "B"), size = ppNum(shared = "C"))'
f_single <- 'ggplot(mtcars) + geom_point(aes(x = mpg, y = hp), alpha = ppNum(shared = "A"), size = ppNum(shared = "A"))'

count_matches <- function(html, pattern) {
  m <- gregexpr(pattern, html, fixed = TRUE)[[1]]
  if (length(m) == 1L && m[[1]] == -1L) 0L else length(m)
}

dep_names <- function(tag) {
  deps <- htmltools::renderTags(tag)$dependencies
  vapply(deps, function(d) d$name, character(1))
}

test_that("S-P.1 a key shared across two formulas goes to the panel", {
  obj <- ptr_shared(formulas = list(f1, f2))

  expect_s3_class(obj, "ptr_shared_spec")
  expect_equal(obj$panel_keys, c("B"))
  expect_equal(obj$local_keys_by_formula[[1]], c("A"))
  expect_equal(obj$local_keys_by_formula[[2]], c("C"))
})

test_that("S-P.2 a single formula yields no panel keys", {
  obj <- ptr_shared(formulas = list(f_single))

  expect_length(obj$panel_keys, 0L)
  expect_equal(obj$local_keys_by_formula[[1]], c("A"))
})

test_that("S-P.2b ptr_shared() is deterministic for the same formulas", {
  a <- ptr_shared(formulas = list(f1, f2))
  b <- ptr_shared(formulas = list(f1, f2))
  expect_equal(a$panel_keys, b$panel_keys)
  expect_equal(a$local_keys_by_formula, b$local_keys_by_formula)
})

test_that("S-P.3 the panel UI renders only cross-formula keys, in one .ptr-app", {
  obj <- ptr_shared(formulas = list(f1, f2))
  html <- as.character(htmltools::renderTags(ptr_shared_panel(obj))$html)

  expect_match(html, "shared_B", fixed = TRUE)
  expect_false(grepl("shared_A", html, fixed = TRUE))
  expect_false(grepl("shared_C", html, fixed = TRUE))
  expect_equal(count_matches(html, 'class="ptr-app"'), 1L)
})

test_that("S-P.4 the bare panel omits the shell and the asset bundle", {
  obj <- ptr_shared(formulas = list(f1, f2))
  bare <- ptr_ui_shared_panel(obj)
  html <- as.character(htmltools::renderTags(bare)$html)

  expect_match(html, "shared_B", fixed = TRUE)
  expect_false(grepl("ptr-app", html, fixed = TRUE))
  expect_false("ggpaintr" %in% dep_names(bare))
  expect_false("ggpaintr-layer" %in% dep_names(bare))

  # ... while the self-contained panel DOES carry the bundle.
  expect_true("ggpaintr" %in% dep_names(ptr_shared_panel(obj)))
})
