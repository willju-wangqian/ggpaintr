# W2 (#B2) -- ptr_ui() gains a `shared=` parameter, forwarded
# verbatim to ptr_ui_controls(shared=). NULL (default) => single-instance
# (every shared key renders inline); a ptr_shared_spec => the coordinator's
# cross-formula panel keys (obj$panel_keys) are excluded from the inline
# section because they belong to the one standalone ptr_shared_panel().

f1 <- 'ggplot(iris, aes(x = var(shared = "ax1"), y = var(shared = "ax1"), color = Species)) + geom_point(size = num(shared = "sz"))'
f2 <- 'ggplot(iris, aes(x = var(shared = "ax2"), y = Sepal.Width, color = Species)) + geom_point(size = num(shared = "sz"))'

# shiny::tabsetPanel() stamps a fresh random data-tabsetid (and matching
# `#tab-<n>-*` anchors) on every call, so two structurally identical UIs are
# never raw-string-equal. Normalise that one nondeterministic token before a
# byte-stability comparison; nothing else in the shell is random.
norm_tabsetid <- function(x) {
  x <- gsub('tabsetid="[0-9]+"', 'tabsetid="N"', x)
  gsub("tab-[0-9]+", "tab-N", x)
}

test_that("B2.1 ptr_ui has shared = NULL in its formals", {
  fmls <- formals(ptr_ui)
  expect_true("shared" %in% names(fmls))
  expect_null(fmls$shared)
})

test_that("B2.2 shared = NULL (default) renders every shared key inline", {
  ui <- as.character(ptr_ui(f1, "plot_1"))
  # formula-local consumer key (var) AND value key (num) both inline
  expect_true(grepl("plot_1-shared_ax1_ui", ui, fixed = TRUE))
  expect_true(grepl("shared_sz", ui, fixed = TRUE))
})

test_that("B2.2b default forwarding is a no-op vs explicit shared = NULL", {
  # Pre-change ptr_ui hardcoded ptr_ui_controls(shared = NULL); the
  # post-change default is shared = NULL forwarded verbatim. The two must be
  # byte-identical modulo Shiny's random tabsetid -> regression-stable for
  # the single-instance no-arg call.
  a <- norm_tabsetid(as.character(ptr_ui(f1, "plot_1")))
  b <- norm_tabsetid(as.character(ptr_ui(f1, "plot_1", shared = NULL)))
  expect_identical(a, b)
})

test_that("B2.3 shared = obj excludes obj$panel_keys from the inline section", {
  obj <- ptr_shared(
    list(f1, f2),
    shared_ui = list(sz = function(id) shiny::sliderInput(id, "Size", 1, 6, 3))
  )
  expect_equal(obj$panel_keys, "sz")

  ui <- as.character(ptr_ui(f1, "plot_1", shared = obj))
  # formula-local key still renders inline ...
  expect_true(grepl("plot_1-shared_ax1_ui", ui, fixed = TRUE))
  # ... the cross-formula panel key does NOT (owned by ptr_shared_panel())
  expect_false(grepl("shared_sz", ui, fixed = TRUE))
})

test_that("B2.3b non-ptr_shared_spec non-NULL shared errors via ptr_ui_controls", {
  expect_error(
    ptr_ui(f1, "plot_1", shared = list(1)),
    "ptr_shared_spec"
  )
})

test_that("B2.4 shared = does not change argument order of existing params", {
  # Constraint: shared = NULL is appended; existing params keep position.
  expect_identical(
    names(formals(ptr_ui)),
    c("formula", "id", "ui_text", "checkbox_defaults",
      "expr_check", "css", "shared")
  )
})
