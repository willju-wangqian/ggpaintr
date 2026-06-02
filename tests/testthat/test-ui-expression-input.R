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
  # `!!plots[[1]]` unquotes to the call before capture. A bare `plots[[1]]`
  # now ALSO works (resolved by the subscript branch of ptr_capture_formula --
  # see the dedicated tests below); `!!` remains available and equivalent.
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

# ---- Subscript WITHOUT `!!` (the upgrade) -------------------------------
# Contract (extends ADR 0009 to the subscript case): a subscript / extraction
# call (`[[`, `$`) whose value is a quoted ggplot expression is resolved at the
# boundary and deparsed -- exactly like a bare symbol bound to a language
# object -- so `ptr_ui(plots[[1]])` no longer requires `!!plots[[1]]`. A string
# element still passes through; a value that is neither string nor language
# (a number, or a sub-list from single-bracket `[`) is still rejected.
#
# The substantive change lives in `ptr_capture_formula()`, so the primary
# tests assert THAT function directly: the oracle is the hand-written deparsed
# string, derived from the contract (resolve the subscript, then deparse) and
# independent of the code path under test. Teeth: on the pre-change capture a
# `[[`/`$` call flowed into the string-only branch and ABORTED on a language
# value ("got a language of length 3"), so the language cases failed.
cap_formula <- function(formula, envir = parent.frame()) {
  ggpaintr:::ptr_capture_formula(rlang::enexpr(formula), envir)
}

test_that("ptr_capture_formula: a `[[` subscript of a quoted expr deparses to its formula", {
  plots <- list(
    rlang::expr(ggplot(mtcars, aes(x = ppVar, y = ppVar)) + geom_point()),
    rlang::expr(ggplot(iris, aes(x = ppVar)) + geom_line())
  )
  expect_identical(cap_formula(plots[[1]]),
                   "ggplot(mtcars, aes(x = ppVar, y = ppVar)) + geom_point()")
  expect_identical(cap_formula(plots[[2]]),
                   "ggplot(iris, aes(x = ppVar)) + geom_line()")
})

test_that("ptr_capture_formula: a `$` extraction of a quoted expr deparses to its formula", {
  holder <- list(body = rlang::expr(ggplot(mtcars, aes(x = ppVar)) + geom_point()))
  expect_identical(cap_formula(holder$body),
                   "ggplot(mtcars, aes(x = ppVar)) + geom_point()")
})

test_that("ptr_capture_formula: a `[[` subscript of a STRING element returns the string", {
  # The string-element contract is preserved (force-eval to the scalar).
  strs <- list("ggplot(mtcars, aes(x = ppVar)) + geom_bar()")
  expect_identical(cap_formula(strs[[1]]),
                   "ggplot(mtcars, aes(x = ppVar)) + geom_bar()")
})

test_that("ptr_capture_formula: a subscript resolving to a non-formula value is rejected", {
  # Negative space: the subscript branch accepts a string OR a language object
  # only. A numeric `[[` element, and a single-bracket `[` that yields a
  # sub-LIST (not the element), must both still abort -- guards the
  # `else if (!is.null(evaled))` rejection from being widened away.
  nums  <- list(1, 2)
  plots <- list(rlang::expr(ggplot(mtcars) + geom_point()))
  expect_error(cap_formula(nums[[1]]),
               "single string or an unquoted ggplot expression")
  expect_error(cap_formula(plots[1]),  # `[` -> length-1 list, not language
               "single string or an unquoted ggplot expression")
})

test_that("ptr_ui: a bare `plots[[i]]` subscript builds the same module as the `!!` splice", {
  # Integration: bare subscript and the proven-canonical `!!` splice converge
  # because both deparse the same call. (No `shared =` here -- the inline
  # shared panel carries process-global state that makes cross-call HTML
  # equivalence order-dependent; the capture contract is what this exercises.)
  plots <- list(
    rlang::expr(ggplot(mtcars, aes(x = ppVar, y = ppVar)) + geom_point(size = ppNum)),
    rlang::expr(ggplot(iris, aes(x = ppVar, y = ppVar)) + geom_line())
  )
  expect_identical(norm_ui(ptr_ui(plots[[1]], "plot_1")),
                   norm_ui(ptr_ui(!!plots[[1]], "plot_1")))
  expect_identical(norm_ui(ptr_ui_controls(plots[[2]], "plot_2")),
                   norm_ui(ptr_ui_controls(!!plots[[2]], "plot_2")))
})

test_that("ptr_ui: a built ggplot object is rejected (must quote it)", {
  # Negative space, mirroring the capture contract: an evaluated ggplot object
  # is not a formula. (`ptr_capture_formula` rejects non-string/non-language.)
  gobj <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg)) + ggplot2::geom_point()
  expect_error(ptr_ui(!!gobj, "p"))
})
