# ptr_ui() / ptr_ui_controls() expression-input parity with ptr_app()/ptr_server().
#
# Contract (ADR 0009, extended to the L2/L3 UI entry points): `formula` is
# captured with rlang::enexpr() at the public boundary, so a string, an
# unquoted ggplot expression, and a `!!`-spliced `expr(...)` that all DENOTE
# the same formula must produce the SAME UI. The oracle is the string form:
# the expression forms are *defined* to equal their deparsed string, so the
# string-built UI is an independent reference, not the output of the code path
# under test for the expression input.
#
# Teeth: if capture were missing (the pre-change is.character() assertion) the
# expression cases ERROR; if capture deparsed wrong, the HTML diverges.

# Canonical pieces: a string formula and the byte-equivalent quoted expression.
FSTR <- "ggplot(mtcars, aes(x = ppVar, y = ppVar)) + geom_point(size = ppNum)"
FEXPR <- rlang::expr(
  ggplot(mtcars, aes(x = ppVar, y = ppVar)) + geom_point(size = ppNum)
)

# `data-tabsetid` / `tab-<n>` are a process-global counter: any two UI builds
# differ there (even two string builds). Normalize it so equivalence is about
# the *formula-derived* structure, not build ordinality (cf. test-module-ui.R).
norm_ui <- function(x) {
  x <- as.character(x)
  x <- gsub('tabsetid="[0-9]+"', 'tabsetid="N"', x)
  gsub("tab-[0-9]+", "tab-N", x)
}

test_that("ptr_ui: an unquoted ggplot expression equals the string form", {
  a <- norm_ui(ptr_ui(
    ggplot(mtcars, aes(x = ppVar, y = ppVar)) + geom_point(size = ppNum), "p"
  ))
  b <- norm_ui(ptr_ui(FSTR, "p"))
  expect_identical(a, b)
})

test_that("ptr_ui: a `!!`-spliced expr() variable equals the string form", {
  a <- norm_ui(ptr_ui(!!FEXPR, "p"))
  b <- norm_ui(ptr_ui(FSTR, "p"))
  expect_identical(a, b)
})

test_that("ptr_ui: a bare expr() symbol equals the string form", {
  # ptr_app/ptr_server contract: a bare symbol bound to a language object
  # resolves in `envir`. Mirror it here.
  f <- FEXPR
  a <- norm_ui(ptr_ui(f, "p"))
  b <- norm_ui(ptr_ui(FSTR, "p"))
  expect_identical(a, b)
})

test_that("ptr_ui_controls: a `!!`-spliced expr() variable equals the string form", {
  a <- norm_ui(ptr_ui_controls(!!FEXPR, "p"))
  b <- norm_ui(ptr_ui_controls(FSTR, "p"))
  expect_identical(a, b)
})

test_that("ptr_ui: `!!plots[[i]]` (subscript splice) builds each module", {
  # The subscript case is exactly why `!!` is canonical: a bare `plots[[1]]`
  # holding a quoted expr cannot flow through the string-builder branch, but
  # `!!plots[[1]]` unquotes to the call before capture.
  plots <- list(
    rlang::expr(ggplot(iris, aes(x = ppVar(shared = "ax1"), y = Sepal.Width)) + geom_point()),
    rlang::expr(ggplot(iris, aes(x = ppVar(shared = "ax2"), y = Petal.Width)) + geom_point())
  )
  ui1 <- norm_ui(ptr_ui(!!plots[[1]], "plot_1"))
  ui2 <- norm_ui(ptr_ui(!!plots[[2]], "plot_2"))
  expect_match(ui1, "plot_1-ptr_layer_select")
  expect_match(ui2, "plot_2-ptr_layer_select")
  # Equal to the deparsed-string build of the same element.
  s1 <- rlang::expr_text(plots[[1]])
  expect_identical(ui1, norm_ui(ptr_ui(s1, "plot_1")))
})

test_that("ptr_ui / ptr_ui_controls: string form (fallback) still emits the ids", {
  # Regression guard: green-by-design before AND after; teeth against an impl
  # that mangles the string path while adding expression support.
  ui <- as.character(ptr_ui(FSTR, "m1"))
  expect_match(ui, "m1-ptr_plot")
  expect_match(ui, "m1-ptr_layer_select")
  ctl <- as.character(ptr_ui_controls(FSTR, "x"))
  expect_match(ctl, "x-ptr_update_plot")
})

test_that("ptr_ui: a built ggplot object is rejected (must quote it)", {
  # Negative space, mirroring the capture contract: an evaluated ggplot object
  # is not a formula. (`ptr_capture_formula` rejects non-string/non-language.)
  gobj <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg)) + ggplot2::geom_point()
  expect_error(ptr_ui(!!gobj, "p"))
})
