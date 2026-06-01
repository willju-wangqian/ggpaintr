# Tests for PLAN-03 (ADR 0027 D2/D5/D4/D6): atomic placeholder-argument rename.
# Derived from the plan's Interface Contract + BDD ONLY (implementation-blind).
# The rename must NOT silently change semantics: the behavior-preservation guards
# assert the NEW API yields IDENTICAL observable behavior (entry slots, one-positional
# cap, identity default, built-ins still resolve). RED until PLAN-03 ships, EXCEPT the
# two cases tagged [characterization] which are GREEN-NOW guards that must stay green.

# ---- RED-first: the rename itself ------------------------------------------

test_that("parse_positional_arg is accepted and maps to entry$default_arg [PLAN-03 BDD; behavior-preservation]", {
  kw <- "t_p2t_newname"
  withr::defer(try(ptr_clear_placeholder(kw), silent = TRUE))
  ptr_define_placeholder_value(
    keyword = kw,
    build_ui = function(node, ...) NULL,
    resolve_expr = function(value, node, ...) value,
    parse_positional_arg = ptr_arg_numeric()
  )
  entry <- ggpaintr:::ptr_registry_lookup(kw)
  expect_identical(entry$default_arg(quote(5)), 5)
})

test_that("the old positional_arg name is rejected [PLAN-03 BDD: rename is a hard break]", {
  expect_error(
    ptr_define_placeholder_value(
      keyword = "t_p2t_oldname",
      build_ui = function(node, ...) NULL,
      resolve_expr = function(value, node, ...) value,
      positional_arg = ptr_arg_numeric()
    ),
    "unused argument"
  )
})

test_that("a custom embellish_eval yields a string in plain R [PLAN-03 BDD: becomes-possible #2]", {
  kw <- "ppCode_p2t"
  withr::defer(try(ptr_clear_placeholder(kw), silent = TRUE))
  ppCode <- ptr_define_placeholder_value(
    keyword = kw,
    build_ui = function(node, ...) NULL,
    resolve_expr = function(value, node, ...) as.character(value),
    embellish_eval = function(x, ...) as.character(x)
  )
  expect_identical(ppCode(1), "1")
})

test_that("bare multi-positional colvars(mpg, hp) still aborts [PLAN-03 BDD: rejected-by-design #1; D6 cap unchanged]", {
  kw <- "colvars_p2t_cap"
  withr::defer(try(ptr_clear_placeholder(kw), silent = TRUE))
  ptr_define_placeholder_consumer(
    keyword = kw,
    build_ui = function(node, cols, data, ...)
      shiny::selectInput(node$id, "Columns", choices = cols, multiple = TRUE),
    resolve_expr = function(value, node, ...) rlang::call2("c", !!!rlang::syms(value)),
    parse_positional_arg = ptr_arg_symbol(vector = TRUE)
  )
  f <- paste0("mtcars |> dplyr::select(", kw,
              "(mpg, hp)) |> ggplot(aes(ppVar(mpg), ppVar(hp))) + geom_point()")
  expect_error(ptr_translate(f), "accepts at most one positional argument")
})

test_that("ptr_arg_numeric_vector is removed by the rename plan [PLAN-03 SC]", {
  expect_false(exists("ptr_arg_numeric_vector",
                      where = asNamespace("ggpaintr"), inherits = FALSE))
})

# ---- behavior-preservation via the renamed API (RED until rename ships) -----

test_that("embellish_eval defaults to the identity runtime [PLAN-03 BDD: default wiring; behavior-preservation]", {
  # Registration with no embellish_eval: entry$runtime must be the identity,
  # i.e. identical observable behavior to the pre-rename default.
  kw <- "t_p2t_default"
  withr::defer(try(ptr_clear_placeholder(kw), silent = TRUE))
  ptr_define_placeholder_value(
    keyword = kw,
    build_ui = function(node, ...) NULL,
    resolve_expr = function(value, node, ...) value
  )
  entry <- ggpaintr:::ptr_registry_lookup(kw)
  expect_identical(entry$runtime(5L), 5L)
})

# ---- characterization (GREEN-NOW regression guards) ------------------------

test_that("built-in placeholders still register and resolve [PLAN-03 SC; characterization]", {
  # ppVar/ppNum must remain recognised placeholders after the built-in
  # registration call sites are swept to the new arg names.
  expect_no_error(ptr_translate("ggplot(mtcars, aes(ppVar(mpg), ppVar(hp))) + geom_point()"))
  expect_no_error(ptr_translate("ggplot(mtcars, aes(x = ppVar(mpg))) + geom_point(alpha = ppNum)"))
})
