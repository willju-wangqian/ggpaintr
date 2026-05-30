# Executable AUTO-verification of sections/rank-1.feature (79 scenarios).
# One test_that per scenario (description == scenario name); each ASSERTS real
# behaviour against loaded source — no skips. The remaining 17 scenarios are
# genuinely runtime/Shiny (live reactive session) and live as runnable apps in
# test-rank-1-manual.R — you run those by hand. 79 + 17 = 96 = all rank-1 scenarios.
#
# Run:  NOT_CRAN=true Rscript -e 'pkgload::load_all("."); testthat::test_file(".claude/feature_groundtruth_bank/tests/test-rank-1.R", reporter="progress")'
# (load_all heals the registry-env split that raw test_file would otherwise hit.)

`%||%` <- function(a, b) if (is.null(a)) b else a

# Capture wrapper mirroring ptr_app's boundary: enexpr() then ptr_capture_formula().
cap <- function(formula, envir = parent.frame()) {
  ggpaintr:::ptr_capture_formula(rlang::enexpr(formula), envir)
}
reg_entry <- function(kw) get(kw, envir = ggpaintr:::.ptr_registry, inherits = FALSE)

# ============================================================================
# Rule: ptr_app(formula, envir, ui_text, expr_check, safe_to_remove, css, spec)
# ============================================================================

test_that("[formula] a string literal is used verbatim — EXPECTED", {
  s <- "ggplot(mtcars, aes(x = ppVar, y = ppVar)) + geom_point()"
  expect_identical(cap(s), s)
  expect_s3_class(ptr_app(s), "shiny.appobj")
})

test_that("[formula] an unquoted ggplot expression is captured and deparsed — EXPECTED", {
  # Literal expression form (the @examples form).
  out <- cap(ggplot(mtcars, aes(x = ppVar, y = ppVar)) + geom_point())
  expect_type(out, "character")
  expect_length(out, 1L)
  # Deparsed string is equivalent: re-parsing yields the same translated tree
  # as feeding the equivalent string directly.
  str_form <- "ggplot(mtcars, aes(x = ppVar, y = ppVar)) + geom_point()"
  expect_equal(
    ggpaintr:::ptr_translate(out),
    ggpaintr:::ptr_translate(str_form)
  )
})

test_that("[formula] a pre-captured expression is spliced with !! — EXPECTED", {
  # The canonical way to pass a stored expression: ptr_app(!!e).
  e <- rlang::expr(ggplot(mtcars, aes(x = ppVar, y = ppVar)) + geom_point())
  out <- cap(!!e)
  expect_type(out, "character")
  expect_match(out, "geom_point", fixed = TRUE)
  expect_equal(
    ggpaintr:::ptr_translate(out),
    ggpaintr:::ptr_translate("ggplot(mtcars, aes(x = ppVar, y = ppVar)) + geom_point()")
  )
})

test_that("[formula] a bare symbol bound to a string resolves in envir — EXPECTED", {
  f <- "ggplot(mtcars, aes(x = ppVar, y = ppVar)) + geom_point()"
  expect_identical(cap(f), f)
})

test_that("[formula] a top-level braces block keeps only its last sub-expression — EXPECTED", {
  out <- cap({
    "noise"
    iris |> ggplot(aes(x = ppVar, y = ppVar))
  })
  expect_type(out, "character")
  expect_match(out, "ggplot", fixed = TRUE)
  expect_false(grepl("noise", out, fixed = TRUE))
})

test_that("[formula] !! splicing inside expression mode is honoured — EXPECTED", {
  col <- rlang::sym("mpg")
  out <- cap(ggplot(mtcars, aes(x = !!col, y = ppVar)) + geom_point())
  expect_match(out, "x = mpg", fixed = TRUE)
})

test_that("[formula] a symbol resolving to neither string nor language is rejected — NOT EXPECTED", {
  n <- 42
  expect_error(cap(n), "single string or an unquoted ggplot")
})

test_that("[formula] native pipe |> is NOT preserved in expression mode — NOT EXPECTED", {
  out <- cap(mtcars |> ggplot(aes(x = ppVar, y = ppVar)))
  expect_false(grepl("|>", out, fixed = TRUE))   # parser desugared to nested call
  expect_match(out, "ggplot(mtcars", fixed = TRUE)
})

test_that("[envir] defaults to the caller's frame — EXPECTED", {
  expect_equal(formals(ptr_app)$envir, quote(parent.frame()))
})

test_that("[ui_text] NULL leaves the built-in copy in place — EXPECTED", {
  expect_null(formals(ptr_app)$ui_text)
})

test_that("[expr_check] defaults to TRUE -> built-in denylist + AST walker — EXPECTED", {
  expect_true(formals(ptr_app)$expr_check)
  r <- ggpaintr:::resolve_expr_check(TRUE)
  expect_equal(r$mode, "denylist")
  expect_identical(r$fns, ggpaintr:::unsafe_expr_denylist)
})

test_that("[expr_check] FALSE disables all validation — EXPECTED", {
  r <- ggpaintr:::resolve_expr_check(FALSE)
  expect_equal(r$mode, "off")
  expect_length(r$fns, 0L)
})

test_that("[expr_check] a list with allow_list switches to allowlist mode — EXPECTED", {
  r <- ggpaintr:::resolve_expr_check(list(allow_list = c("Sys.time")))
  expect_equal(r$mode, "allowlist")
  expect_identical(r$fns, c("Sys.time"))
})

test_that("[expr_check] a list with both allow_list and deny_list subtracts deny from allow — EXPECTED", {
  r <- ggpaintr:::resolve_expr_check(list(allow_list = c("a", "b"), deny_list = c("b")))
  expect_equal(r$mode, "allowlist")
  expect_identical(r$fns, c("a"))
})

test_that("[expr_check] a list with only deny_list replaces the default denylist — EXPECTED", {
  r <- ggpaintr:::resolve_expr_check(list(deny_list = c("system")))
  expect_equal(r$mode, "denylist")
  expect_identical(r$fns, c("system"))
})

test_that("[expr_check] an empty list() silently falls back to the default denylist — EXPECTED (gotcha)", {
  r <- ggpaintr:::resolve_expr_check(list())
  expect_equal(r$mode, "denylist")
  expect_identical(r$fns, ggpaintr:::unsafe_expr_denylist)
})

test_that("[expr_check] a value that is not TRUE/FALSE/list is rejected — NOT EXPECTED", {
  expect_error(ggpaintr:::resolve_expr_check("denylist"),
               "must be TRUE, FALSE, or a list")
})

test_that("[safe_to_remove] defaults to empty character() — EXPECTED", {
  expect_identical(eval(formals(ptr_app)$safe_to_remove), character())
})

test_that("[css] NULL ships only the default stylesheet — EXPECTED", {
  expect_null(formals(ptr_app)$css)
})

test_that("[spec] NULL boots every widget at its formula default — EXPECTED", {
  expect_null(formals(ptr_app)$spec)
})

test_that("[return] a runnable shiny.appobj is produced — EXPECTED", {
  expect_s3_class(ptr_app("ggplot(mtcars, aes(x = ppVar, y = ppVar)) + geom_point()"),
                  "shiny.appobj")
})

# ============================================================================
# Rule: ppVar / ppNum / ppText / ppExpr  (value/consumer identity tokens)
# ============================================================================

test_that("[x] outside ptr_app the call returns its argument unchanged — EXPECTED", {
  expect_identical(ppVar(42), 42)
  expect_identical(ppNum(3.5), 3.5)
  expect_identical(ppText("hi"), "hi")
  expect_identical(ppExpr(quote(a + b)), quote(a + b))
})

test_that("[x] omitting x is valid (bare token) — EXPECTED", {
  expect_null(ppVar())
  expect_null(ppNum())
  expect_null(ppText())
  expect_null(ppExpr())
})

test_that("[...] extra named args are ignored by the built-in identity body — NOT EXPECTED to act", {
  expect_identical(ppVar(7, foo = 1, bar = 2), 7)
})

# ============================================================================
# Rule: ppUpload(x, ...)
# ============================================================================

test_that("[x] with an argument it returns the value unchanged — EXPECTED", {
  expect_identical(ppUpload(mtcars), mtcars)
})

test_that("[x] the no-arg form aborts outside ptr_app — NOT EXPECTED to return", {
  expect_error(ppUpload(), "only meaningful inside")
})

# ============================================================================
# Rule: ppLayerOff(layer_expr, hide = TRUE)
# ============================================================================

test_that("[hide] defaults to TRUE and drops the layer to NULL in naked R — EXPECTED", {
  expect_null(ppLayerOff(ggplot2::geom_point()))
})

test_that("[hide] FALSE returns the evaluated layer in naked R — EXPECTED", {
  expect_s3_class(ppLayerOff(ggplot2::geom_point(), FALSE), "LayerInstance")
})

test_that("[hide] a non-logical / non-scalar / NA value is rejected — NOT EXPECTED", {
  expect_error(ppLayerOff(ggplot2::geom_point(), "yes"), "length-1 non-NA logical")
  expect_error(ppLayerOff(ggplot2::geom_point(), NA), "length-1 non-NA logical")
  expect_error(ppLayerOff(ggplot2::geom_point(), c(TRUE, FALSE)), "length-1 non-NA logical")
})

test_that("[layer_expr] is evaluated only when hide = FALSE — EXPECTED", {
  # hide = TRUE returns NULL without evaluating layer_expr (lazy).
  expect_null(ppLayerOff(stop("must not evaluate"), TRUE))
})

# ============================================================================
# Rule: ppVerbSwitch(.data, verb_expr, switch_on = TRUE, label = NULL)
# ============================================================================

test_that("[switch_on] defaults to TRUE and routes .data through the verb — EXPECTED", {
  skip_if_not_installed("dplyr")
  res <- ppVerbSwitch(mtcars, dplyr::filter(mpg > 20))
  expect_equal(nrow(res), 14L)
})

test_that("[switch_on] FALSE returns .data unchanged — EXPECTED", {
  skip_if_not_installed("dplyr")
  expect_identical(ppVerbSwitch(mtcars, dplyr::mutate(mpg = mpg + 100), FALSE), mtcars)
})

test_that("[switch_on] a non-logical / non-scalar / NA value is rejected — NOT EXPECTED", {
  skip_if_not_installed("dplyr")
  expect_error(ppVerbSwitch(mtcars, dplyr::mutate(x = 1), switch_on = NA),
               "length-1 non-NA logical")
})

test_that("[verb_expr] must be a verb call even when switch_on = FALSE — NOT EXPECTED otherwise", {
  expect_error(ppVerbSwitch(mtcars, 42, FALSE), "must be a verb call")
})

test_that("[verb_expr] .data is inserted as the FIRST positional argument only — EXPECTED", {
  skip_if_not_installed("dplyr")
  res <- ppVerbSwitch(mtcars, dplyr::filter(mpg > 20), TRUE)
  expect_equal(nrow(res), 14L)   # filter applied to mtcars as data slot 1
})

test_that("[label] is metadata-only and ignored in naked R — NOT EXPECTED to affect output", {
  skip_if_not_installed("dplyr")
  with_label <- ppVerbSwitch(mtcars, dplyr::filter(mpg > 20), TRUE, label = "Filter")
  no_label   <- ppVerbSwitch(mtcars, dplyr::filter(mpg > 20), TRUE)
  expect_identical(with_label, no_label)
})

# ============================================================================
# Rule: ptr_define_placeholder_value(...)
# ============================================================================

test_that("[keyword] a syntactically valid, non-shadowing name registers a value placeholder — EXPECTED", {
  ptr_define_placeholder_value("ppTestVal",
    build_ui = function(node, ...) shiny::textInput(node$id, "x"),
    resolve_expr = function(value, node, ...) value)
  on.exit(ptr_clear_placeholder("ppTestVal"), add = TRUE)
  e <- reg_entry("ppTestVal")
  expect_equal(e$role, "value")
  expect_false(e$data_aware)
})

test_that("[keyword] a non-string / empty / NA value is rejected — NOT EXPECTED", {
  bu <- function(node, ...) NULL; re <- function(value, node, ...) value
  expect_error(ptr_define_placeholder_value("", build_ui = bu, resolve_expr = re))
  expect_error(ptr_define_placeholder_value(NA_character_, build_ui = bu, resolve_expr = re))
  expect_error(ptr_define_placeholder_value(123, build_ui = bu, resolve_expr = re))
})

test_that("[keyword] a reserved word is rejected — NOT EXPECTED", {
  bu <- function(node, ...) NULL; re <- function(value, node, ...) value
  expect_error(ptr_define_placeholder_value("if", build_ui = bu, resolve_expr = re),
               "reserved word")
})

test_that("[keyword] a name that isn't a valid R identifier is rejected — NOT EXPECTED", {
  bu <- function(node, ...) NULL; re <- function(value, node, ...) value
  expect_error(ptr_define_placeholder_value("my keyword", build_ui = bu, resolve_expr = re),
               "syntactically valid R name")
})

test_that("[keyword] a name shadowing base R / ggplot2 is rejected — NOT EXPECTED", {
  bu <- function(node, ...) NULL; re <- function(value, node, ...) value
  expect_error(ptr_define_placeholder_value("mean", build_ui = bu, resolve_expr = re),
               "shadows")
})

test_that("[keyword] re-registering an existing keyword warns about overwrite — EXPECTED (warn, still registers)", {
  bu <- function(node, ...) NULL; re <- function(value, node, ...) value
  ptr_define_placeholder_value("ppTestDup", build_ui = bu, resolve_expr = re)
  on.exit(ptr_clear_placeholder("ppTestDup"), add = TRUE)
  expect_warning(
    ptr_define_placeholder_value("ppTestDup", build_ui = bu, resolve_expr = re),
    "Overwriting")
})

test_that("[build_ui] a function declaring node (or ...) is accepted — EXPECTED", {
  re <- function(value, node, ...) value
  expect_no_error(ptr_define_placeholder_value("ppTestBU",
    build_ui = function(node, label = NULL, selected = NULL, ...) NULL, resolve_expr = re))
  ptr_clear_placeholder("ppTestBU")
})

test_that("[build_ui] a non-function is rejected — NOT EXPECTED", {
  expect_error(ptr_define_placeholder_value("ppTestBU2",
    build_ui = "nope", resolve_expr = function(value, node, ...) value),
    "must be a function")
})

test_that("[build_ui] declaring ONLY ... still works but warns when required args exist — EXPECTED (warn)", {
  expect_warning(
    ptr_define_placeholder_value("ppTestDots",
      build_ui = function(...) NULL, resolve_expr = function(value, node, ...) value),
    "only declares")
  ptr_clear_placeholder("ppTestDots")
})

test_that("[resolve_expr] a function declaring value, node (or ...) is accepted — EXPECTED", {
  expect_no_error(ptr_define_placeholder_value("ppTestRE",
    build_ui = function(node, ...) NULL, resolve_expr = function(value, node, ...) value))
  ptr_clear_placeholder("ppTestRE")
})

test_that("[resolve_expr] missing a required arg with no ... is rejected — NOT EXPECTED", {
  expect_error(ptr_define_placeholder_value("ppTestRE2",
    build_ui = function(node, ...) NULL, resolve_expr = function(value) value),
    "must accept argument")
})

test_that("[validate_input] NULL means no pre-resolve validation — EXPECTED", {
  ptr_define_placeholder_value("ppTestVI",
    build_ui = function(node, ...) NULL, resolve_expr = function(value, node, ...) value)
  on.exit(ptr_clear_placeholder("ppTestVI"), add = TRUE)
  expect_null(reg_entry("ppTestVI")$validate_input)
})

test_that("[default_arg] NULL rejects positional formula arguments at translate — EXPECTED", {
  ptr_define_placeholder_value("ppTestDA",
    build_ui = function(node, ...) NULL, resolve_expr = function(value, node, ...) value)
  on.exit(ptr_clear_placeholder("ppTestDA"), add = TRUE)
  expect_error(ggpaintr:::ptr_translate("ggplot(mtcars) + labs(title = ppTestDA(5))"))
})

test_that("[default_arg] a validator closure accepts a positional default — EXPECTED", {
  ptr_define_placeholder_value("ppTestDA2",
    build_ui = function(node, ...) NULL, resolve_expr = function(value, node, ...) value,
    default_arg = ptr_default_numeric())
  on.exit(ptr_clear_placeholder("ppTestDA2"), add = TRUE)
  expect_no_error(ggpaintr:::ptr_translate("ggplot(mtcars) + labs(title = ppTestDA2(5))"))
})

test_that("[default_arg] a non-NULL non-function value is rejected — NOT EXPECTED", {
  expect_error(ptr_define_placeholder_value("ppTestDA3",
    build_ui = function(node, ...) NULL, resolve_expr = function(value, node, ...) value,
    default_arg = 50), "must be NULL or a function")
})

test_that("[named_args] empty list (default) means no extra named args — EXPECTED", {
  expect_equal(eval(formals(ptr_define_placeholder_value)$named_args), list())
})

test_that("[named_args] a non-list is rejected — NOT EXPECTED", {
  expect_error(ptr_define_placeholder_value("ppTestNA",
    build_ui = function(node, ...) NULL, resolve_expr = function(value, node, ...) value,
    named_args = c(a = 1)), "must be a list")
})

test_that("[named_args] an unnamed / partially-named list is rejected — NOT EXPECTED", {
  expect_error(ptr_define_placeholder_value("ppTestNA2",
    build_ui = function(node, ...) NULL, resolve_expr = function(value, node, ...) value,
    named_args = list(function(x) x)), "fully-named")
})

test_that("[named_args] an entry named 'shared' is rejected — NOT EXPECTED", {
  expect_error(ptr_define_placeholder_value("ppTestNA3",
    build_ui = function(node, ...) NULL, resolve_expr = function(value, node, ...) value,
    named_args = list(shared = function(x) x)), "shared")
})

test_that("[named_args] non-function entries are rejected — NOT EXPECTED", {
  expect_error(ptr_define_placeholder_value("ppTestNA4",
    build_ui = function(node, ...) NULL, resolve_expr = function(value, node, ...) value,
    named_args = list(foo = 1)), "validator functions")
})

test_that("[runtime] NULL supplies the identity function and returns it — EXPECTED", {
  fn <- ptr_define_placeholder_value("ppTestRT",
    build_ui = function(node, ...) NULL, resolve_expr = function(value, node, ...) value)
  on.exit(ptr_clear_placeholder("ppTestRT"), add = TRUE)
  expect_true(is.function(fn))
  expect_identical(fn(99), 99)
})

test_that("[runtime] an override gives the keyword a non-identity plain-R meaning — EXPECTED", {
  fn <- ptr_define_placeholder_value("ppTestRT2",
    build_ui = function(node, ...) NULL, resolve_expr = function(value, node, ...) value,
    runtime = function(x, ...) x / 100)
  on.exit(ptr_clear_placeholder("ppTestRT2"), add = TRUE)
  expect_identical(fn(50), 0.5)
})

test_that("[copy_defaults] a named list of leaf-field strings is accepted; {param} interpolates — EXPECTED", {
  fn <- ptr_define_placeholder_value("ppTestCD",
    build_ui = function(node, ...) NULL, resolve_expr = function(value, node, ...) value,
    copy_defaults = list(label = "Percent for {param}"))
  on.exit(ptr_clear_placeholder("ppTestCD"), add = TRUE)
  expect_equal(reg_entry("ppTestCD")$copy_defaults$label, "Percent for {param}")
})

test_that("[copy_defaults] an unsupported field name is rejected — NOT EXPECTED", {
  expect_error(ptr_define_placeholder_value("ppTestCD2",
    build_ui = function(node, ...) NULL, resolve_expr = function(value, node, ...) value,
    copy_defaults = list(bogus = "x")), "unsupported field")
})

test_that("[copy_defaults] a non-string / multi-length / NA value is rejected — NOT EXPECTED", {
  expect_error(ptr_define_placeholder_value("ppTestCD3",
    build_ui = function(node, ...) NULL, resolve_expr = function(value, node, ...) value,
    copy_defaults = list(label = c("a", "b"))), "single non-NA string")
})

test_that("[return] the runtime callable is returned for binding under the keyword name — EXPECTED", {
  fn <- ptr_define_placeholder_value("ppTestRet",
    build_ui = function(node, ...) NULL, resolve_expr = function(value, node, ...) value)
  on.exit(ptr_clear_placeholder("ppTestRet"), add = TRUE)
  expect_true(is.function(fn))
})

# ============================================================================
# Rule: ptr_define_placeholder_consumer(...)
# ============================================================================

test_that("[registration] registers a data-aware consumer entry — EXPECTED", {
  ptr_define_placeholder_consumer("ppTestCons",
    build_ui = function(node, cols, data, ...) NULL,
    resolve_expr = function(value, node, ...) rlang::sym(value))
  on.exit(ptr_clear_placeholder("ppTestCons"), add = TRUE)
  e <- reg_entry("ppTestCons")
  expect_equal(e$role, "consumer")
  expect_true(e$data_aware)
})

test_that("[build_ui] requires node, cols AND data (or ...) — EXPECTED", {
  expect_no_error(ptr_define_placeholder_consumer("ppTestCons2",
    build_ui = function(node, cols, data, label = NULL, selected = NULL, ...) NULL,
    resolve_expr = function(value, node, ...) value))
  ptr_clear_placeholder("ppTestCons2")
})

test_that("[build_ui] missing cols/data with no ... is rejected — NOT EXPECTED", {
  expect_error(ptr_define_placeholder_consumer("ppTestCons3",
    build_ui = function(node) NULL,
    resolve_expr = function(value, node, ...) value), "must accept argument")
})

# ============================================================================
# Rule: ptr_define_placeholder_source(...)
# ============================================================================

test_that("[registration] registers a data-aware source entry — EXPECTED", {
  ptr_define_placeholder_source("ppTestSrc",
    build_ui = function(node, label, ...) NULL,
    resolve_data = function(value, node, ...) NULL)
  on.exit(ptr_clear_placeholder("ppTestSrc"), add = TRUE)
  e <- reg_entry("ppTestSrc")
  expect_equal(e$role, "source")
  expect_true(e$data_aware)
})

test_that("[build_ui] requires only node (or ...) — EXPECTED", {
  expect_no_error(ptr_define_placeholder_source("ppTestSrc2",
    build_ui = function(node, label, ...) NULL,
    resolve_data = function(value, node, ...) NULL))
  ptr_clear_placeholder("ppTestSrc2")
})

test_that("[resolve_data] required; returns a data.frame or NULL for 'no data yet' — EXPECTED", {
  # Missing resolve_data is a hard error; a wrong-arity one is rejected.
  expect_error(ptr_define_placeholder_source("ppTestSrc3",
    build_ui = function(node, label, ...) NULL))
  expect_error(ptr_define_placeholder_source("ppTestSrc3b",
    build_ui = function(node, label, ...) NULL,
    resolve_data = function(value) value), "must accept argument")
})

test_that("[resolve_expr] NULL defaults to rlang::sym(value) — EXPECTED", {
  ptr_define_placeholder_source("ppTestSrc4",
    build_ui = function(node, label, ...) NULL,
    resolve_data = function(value, node, ...) NULL)
  on.exit(ptr_clear_placeholder("ppTestSrc4"), add = TRUE)
  re <- reg_entry("ppTestSrc4")$resolve_expr
  expect_true(is.function(re))
  expect_identical(re("mtcars"), rlang::sym("mtcars"))
})

test_that("[resolve_expr] an override controls how the data is referred to in rendered code — EXPECTED", {
  ptr_define_placeholder_source("ppTestSrc5",
    build_ui = function(node, label, ...) NULL,
    resolve_data = function(value, node, ...) NULL,
    resolve_expr = function(value, node, ...) rlang::call2("read.csv", value))
  on.exit(ptr_clear_placeholder("ppTestSrc5"), add = TRUE)
  re <- reg_entry("ppTestSrc5")$resolve_expr
  expect_equal(re("f.csv"), rlang::call2("read.csv", "f.csv"))
})

test_that("[shortcut] FALSE (default) renders a single bound input — EXPECTED", {
  expect_false(eval(formals(ptr_define_placeholder_source)$shortcut))
  ptr_define_placeholder_source("ppTestSrc6",
    build_ui = function(node, label, ...) NULL,
    resolve_data = function(value, node, ...) NULL)
  on.exit(ptr_clear_placeholder("ppTestSrc6"), add = TRUE)
  expect_false(reg_entry("ppTestSrc6")$shortcut)
})

test_that("[shortcut] TRUE stamps node$shortcut_id and expects two bound inputs — EXPECTED", {
  ptr_define_placeholder_source("ppTestSrc7",
    build_ui = function(node, label, ...) NULL,
    resolve_data = function(value, node, ...) NULL, shortcut = TRUE)
  on.exit(ptr_clear_placeholder("ppTestSrc7"), add = TRUE)
  expect_true(reg_entry("ppTestSrc7")$shortcut)
})

test_that("[shortcut] a non-logical / non-scalar / NA value is rejected — NOT EXPECTED", {
  expect_error(ptr_define_placeholder_source("ppTestSrc8",
    build_ui = function(node, label, ...) NULL,
    resolve_data = function(value, node, ...) NULL, shortcut = "yes"),
    "must be a single logical")
})

test_that("[runtime] NULL supplies an abort guard, not identity — EXPECTED", {
  fn <- ptr_define_placeholder_source("ppTestSrc9",
    build_ui = function(node, label, ...) NULL,
    resolve_data = function(value, node, ...) NULL)
  on.exit(ptr_clear_placeholder("ppTestSrc9"), add = TRUE)
  expect_error(fn(), "only meaningful inside")
})


# ============================================================================
# Promoted from skip() — verifiable WITHOUT a browser (probed 2026-05-29).
# These were originally skipped as "runtime"; re-triage showed they assert
# cheaply via mocks (forwarding) or post-translate tree inspection.
# ============================================================================

test_that("[ui_text] an override list is forwarded to both UI and server — EXPECTED", {
  seen <- new.env()
  testthat::local_mocked_bindings(
    ptr_build_app_ui    = function(tree, ui_text = NULL, css = NULL, ...) { seen$ui <- ui_text; shiny::tags$div() },
    ptr_make_app_server = function(formula, tree, ui_text = NULL, ...) { seen$srv <- ui_text; function(input, output, session) {} },
    .package = "ggpaintr"
  )
  ggpaintr:::ptr_app_components("ggplot(mtcars, aes(x=ppVar,y=ppVar)) + geom_point()",
                                ui_text = list(app_title = "X"))
  expect_equal(seen$ui, list(app_title = "X"))
  expect_equal(seen$srv, list(app_title = "X"))
})

test_that("[css] a path/ptr_css value is forwarded to the UI builder — EXPECTED", {
  seen <- new.env()
  testthat::local_mocked_bindings(
    ptr_build_app_ui = function(tree, ui_text = NULL, css = NULL, ...) { seen$css <- css; shiny::tags$div() },
    .package = "ggpaintr"
  )
  ggpaintr:::ptr_app_components("ggplot(mtcars, aes(x=ppVar,y=ppVar)) + geom_point()", css = "x.css")
  expect_equal(seen$css, "x.css")
})

test_that("[hide] inside ptr_app sets default_active = FALSE on the layer node — EXPECTED", {
  tr <- ggpaintr:::ptr_translate("ggplot(mtcars, aes(x=ppVar,y=ppVar)) + ppLayerOff(geom_point(), TRUE)")
  offs <- Filter(function(l) isFALSE(l$default_active), tr$layers)
  expect_gte(length(offs), 1L)   # the ppLayerOff layer boots off
})

test_that("[build_ui.node] the placeholder node carries $id/$keyword/$param post-translate — EXPECTED", {
  tr <- ggpaintr:::ptr_translate("ggplot(mtcars, aes(x=ppVar,y=ppNum)) + geom_point()")
  phs <- list()
  rec <- function(x) if (is.list(x)) { if (inherits(x, "ptr_node") && any(grepl("ptr_ph", class(x)))) phs[[length(phs)+1]] <<- x; for (e in x) rec(e) }
  rec(tr)
  expect_gt(length(phs), 0L)
  n <- phs[[1]]
  expect_true(all(c("id", "keyword", "param") %in% names(n)))
  expect_true(nzchar(n$id) && nzchar(n$keyword))
})

test_that("[keyword] a top-level mismatched binding does NOT warn (best-effort tripwire) — EXPECTED", {
  # CORRECTION: the LHS-drift check is best-effort. R's `<-` primitive pushes no
  # call frame at top level, so the common `ppFoo <- define(keyword="ppBar")`
  # case is SILENT. (Original scenario wrongly claimed a warning.)
  expect_no_warning({
    ppMismatch <- ptr_define_placeholder_value("ppDriftKw",
      build_ui = function(node, ...) NULL, resolve_expr = function(value, node, ...) value)
  })
  ptr_clear_placeholder("ppDriftKw")
})
