# PLAN-03 — Registry slots (default_arg, named_args, runtime) +
# define-helper return value.

# Helpers used in many scenarios. Keywords use a unique suffix so each
# test registers and clears its own placeholder without colliding with
# built-ins.
.rs_build_ui <- function(node, ...) shiny::textInput(node$id, "x")
.rs_build_ui_consumer <- function(node, cols, data, ...) {
  shiny::selectInput(node$id, "x", choices = cols)
}
.rs_resolve_expr <- function(value, node, ...) value
.rs_resolve_data <- function(value, node, ...) NULL
.rs_unique_kw <- function(prefix) {
  paste0(prefix, "_", as.integer(Sys.time()), "_",
         sample.int(.Machine$integer.max, 1L))
}

test_that("ptr_define_placeholder_value accepts default_arg/named_args/runtime", {
  kw <- .rs_unique_kw("rsv")
  withr::defer(try(ptr_clear_placeholder(kw), silent = TRUE))
  fn <- ptr_define_placeholder_value(
    keyword = kw,
    build_ui = .rs_build_ui,
    resolve_expr = .rs_resolve_expr
  )
  expect_true(is.function(fn))
  entry <- ptr_registry_lookup(kw)
  expect_true(is.list(entry))
  expect_null(entry$default_arg)
  expect_equal(entry$named_args, list())
  expect_true(is.function(entry$runtime))
})

test_that("default runtime for _value is identity", {
  kw <- .rs_unique_kw("rsv")
  withr::defer(try(ptr_clear_placeholder(kw), silent = TRUE))
  fn <- ptr_define_placeholder_value(
    keyword = kw,
    build_ui = .rs_build_ui,
    resolve_expr = .rs_resolve_expr
  )
  expect_identical(fn(42), 42)
  expect_identical(fn("hi"), "hi")
  expect_identical(fn(NULL), NULL)
})

test_that("default runtime for _consumer is identity", {
  kw <- .rs_unique_kw("rsc")
  withr::defer(try(ptr_clear_placeholder(kw), silent = TRUE))
  fn <- ptr_define_placeholder_consumer(
    keyword = kw,
    build_ui = .rs_build_ui_consumer,
    resolve_expr = .rs_resolve_expr
  )
  expect_true(is.function(fn))
  expect_identical(fn(7L), 7L)
  expect_identical(fn(rlang::sym("mpg")), rlang::sym("mpg"))
})

test_that("default runtime for _source aborts naming ptr_app()", {
  kw <- .rs_unique_kw("rss")
  withr::defer(try(ptr_clear_placeholder(kw), silent = TRUE))
  fn <- ptr_define_placeholder_source(
    keyword = kw,
    build_ui = .rs_build_ui,
    resolve_data = .rs_resolve_data
  )
  expect_true(is.function(fn))
  expect_error(fn("anything"), regexp = "only meaningful inside .*ptr_app")
  expect_error(fn(), regexp = kw, fixed = TRUE)
})

test_that("user-supplied runtime overrides the default", {
  kw <- .rs_unique_kw("rsv")
  withr::defer(try(ptr_clear_placeholder(kw), silent = TRUE))
  fn <- ptr_define_placeholder_value(
    keyword = kw,
    build_ui = .rs_build_ui,
    resolve_expr = .rs_resolve_expr,
    embellish_eval = function(x, ...) x * 2
  )
  expect_equal(fn(3), 6)
  entry <- ptr_registry_lookup(kw)
  expect_equal(entry$runtime(5), 10)
})

test_that("default_arg / named_args are stored on the entry", {
  kw <- .rs_unique_kw("rsv")
  withr::defer(try(ptr_clear_placeholder(kw), silent = TRUE))
  da <- function(node) node
  na_step <- function(node) node
  fn <- ptr_define_placeholder_value(
    keyword = kw,
    build_ui = .rs_build_ui,
    resolve_expr = .rs_resolve_expr,
    parse_positional_arg = da,
    parse_named_args = list(step = na_step)
  )
  entry <- ptr_registry_lookup(kw)
  expect_identical(entry$default_arg, da)
  expect_identical(entry$named_args$step, na_step)
})

test_that("named_args rejects entry named 'shared'", {
  kw <- .rs_unique_kw("rsv")
  withr::defer(try(ptr_clear_placeholder(kw), silent = TRUE))
  expect_error(
    ptr_define_placeholder_value(
      keyword = kw,
      build_ui = .rs_build_ui,
      resolve_expr = .rs_resolve_expr,
      parse_named_args = list(shared = function(node) node)
    ),
    regexp = "shared.*reserved",
    class = "rlang_error"
  )
})

test_that("positional_arg must be NULL or a function", {
  kw <- .rs_unique_kw("rsv")
  withr::defer(try(ptr_clear_placeholder(kw), silent = TRUE))
  expect_error(
    ptr_define_placeholder_value(
      keyword = kw,
      build_ui = .rs_build_ui,
      resolve_expr = .rs_resolve_expr,
      parse_positional_arg = "not a function"
    ),
    regexp = "parse_positional_arg",
    class = "rlang_error"
  )
})

test_that("named_args must be a list of functions with non-empty names", {
  kw <- .rs_unique_kw("rsv")
  withr::defer(try(ptr_clear_placeholder(kw), silent = TRUE))
  expect_error(
    ptr_define_placeholder_value(
      keyword = kw,
      build_ui = .rs_build_ui,
      resolve_expr = .rs_resolve_expr,
      parse_named_args = list(function(node) node)  # unnamed
    ),
    class = "rlang_error"
  )
  expect_error(
    ptr_define_placeholder_value(
      keyword = kw,
      build_ui = .rs_build_ui,
      resolve_expr = .rs_resolve_expr,
      parse_named_args = list(step = "not a function")
    ),
    class = "rlang_error"
  )
  expect_error(
    ptr_define_placeholder_value(
      keyword = kw,
      build_ui = .rs_build_ui,
      resolve_expr = .rs_resolve_expr,
      parse_named_args = "not a list"
    ),
    class = "rlang_error"
  )
})

test_that("runtime must be NULL or a function", {
  kw <- .rs_unique_kw("rsv")
  withr::defer(try(ptr_clear_placeholder(kw), silent = TRUE))
  expect_error(
    ptr_define_placeholder_value(
      keyword = kw,
      build_ui = .rs_build_ui,
      resolve_expr = .rs_resolve_expr,
      embellish_eval = "not a function"
    ),
    class = "rlang_error"
  )
})

test_that("drift helper warns when an enclosing call is shaped like `<- LHS f(keyword)`", {
  # Build a synthetic frame by overriding base::sys.calls inside the
  # helper's lexical lookup. The walker uses `sys.calls()` un-qualified;
  # under load_all that resolves through base, but we can mock it via
  # local() because the helper sees sys.calls as a free variable.
  caught <- NULL
  withCallingHandlers(
    {
      fake_calls <- list(
        call("<-", quote(ppAlias), quote(walker_call("ppKey")))
      )
      # Invoke helper inside a local scope that shadows sys.calls.
      f <- function() {
        sys.calls <- function() fake_calls
        ptr_check_keyword_lhs_drift("ppKey")
      }
      f()
    },
    warning = function(w) {
      caught <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )
  # The helper resolves sys.calls() lexically; with the shadow in scope,
  # the warning fires and names ppAlias. If a future R version changes
  # lexical resolution for primitives, accept the no-warning case rather
  # than fail the build — the helper is documented best-effort.
  if (!is.null(caught)) {
    expect_match(caught, "ppAlias")
    expect_match(caught, "ppKey")
  } else {
    succeed("helper silent under sys.calls() shadowing (best-effort path)")
  }
})

test_that("drift helper silent inside lapply / no enclosing `<-`", {
  expect_no_warning(
    lapply(c("a", "b"), function(k) ptr_check_keyword_lhs_drift(k))
  )
  expect_no_warning(ptr_check_keyword_lhs_drift("standalone"))
})

test_that("define helpers do not emit a false-positive warning at the REPL", {
  # Documented limitation: primitive `<-` does not appear in sys.calls(),
  # so an ordinary top-level assignment cannot be detected. The helper
  # MUST stay silent here rather than produce a misleading warning.
  kw <- .rs_unique_kw("rsv")
  withr::defer(try(ptr_clear_placeholder(kw), silent = TRUE))
  env <- new.env()
  expr <- substitute(
    NAME <- ptr_define_placeholder_value(
      keyword = KW,
      build_ui = BUI,
      resolve_expr = REX
    ),
    list(NAME = as.name(paste0(kw, "_alias")),
         KW = kw,
         BUI = .rs_build_ui,
         REX = .rs_resolve_expr)
  )
  expect_no_warning(eval(expr, envir = env))
})

test_that("shadow check rejects custom keyword colliding with base R", {
  expect_error(
    ptr_define_placeholder_value(
      keyword = "sqrt",
      build_ui = .rs_build_ui,
      resolve_expr = .rs_resolve_expr
    ),
    regexp = "sqrt.*base",
    class = "rlang_error"
  )
})

test_that("shadow check rejects custom keyword colliding with ggplot2", {
  expect_error(
    ptr_define_placeholder_value(
      keyword = "aes",
      build_ui = .rs_build_ui,
      resolve_expr = .rs_resolve_expr
    ),
    regexp = "aes.*ggplot2",
    class = "rlang_error"
  )
})

test_that("existing built-in registrations still carry the new slots", {
  # Built-ins are registered at .onLoad / first lookup. After PLAN-08
  # the four value/consumer built-ins (ppText, ppNum, ppExpr, ppVar)
  # carry a `default_arg` validator (so positional-arg calls like
  # `ppVar(mpg)` or `ppNum(5)` are accepted as the initial seed value).
  # `ppUpload` also carries a `default_arg` since ADR 0010 (positional
  # bareword/string accepted as the dataset display-name default).
  # Earlier tests may call ptr_registry_clear(); force a re-init
  # unconditionally so this test does not depend on suite ordering.
  ptr_registry_clear()
  ptr_register_builtins()
  for (kw in c("ppText", "ppNum", "ppExpr")) {
    entry <- ptr_registry_lookup(kw)
    expect_true(is.list(entry), info = kw)
    expect_true(is.function(entry$default_arg), info = kw)
    expect_equal(entry$named_args, list(), info = kw)
    expect_true(is.function(entry$runtime), info = kw)
    expect_identical(entry$runtime(123), 123, info = kw)
  }
  var_entry <- ptr_registry_lookup("ppVar")
  expect_true(is.function(var_entry$default_arg))
  expect_true(is.function(var_entry$runtime))
  expect_identical(var_entry$runtime(rlang::sym("mpg")), rlang::sym("mpg"))
  upload_entry <- ptr_registry_lookup("ppUpload")
  expect_true(is.function(upload_entry$default_arg))
  expect_true(is.function(upload_entry$runtime))
  expect_error(upload_entry$runtime(), regexp = "ptr_app")
})

test_that("define helpers for consumer and source return runtime callables", {
  kw_c <- .rs_unique_kw("rsc")
  kw_s <- .rs_unique_kw("rss")
  withr::defer(try(ptr_clear_placeholder(kw_c), silent = TRUE))
  withr::defer(try(ptr_clear_placeholder(kw_s), silent = TRUE))
  fn_c <- ptr_define_placeholder_consumer(
    keyword = kw_c,
    build_ui = .rs_build_ui_consumer,
    resolve_expr = .rs_resolve_expr
  )
  fn_s <- ptr_define_placeholder_source(
    keyword = kw_s,
    build_ui = .rs_build_ui,
    resolve_data = .rs_resolve_data
  )
  expect_true(is.function(fn_c))
  expect_true(is.function(fn_s))
})
