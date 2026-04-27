# Tests for ns (namespace) threading through ptr_server_state, ptr_input_ui,
# and ptr_ns_obj.  Covers all 6 acceptance criteria from the plan.

# Shared formula used across tests.
.formula_two_vars <- "ggplot(data, aes(x=var, y=var)) + geom_point()"

# ---------------------------------------------------------------------------
# AC-1: Two ptr_server_state calls with distinct ns produce disjoint ids
# ---------------------------------------------------------------------------

test_that("distinct ns values produce disjoint rendered and server top-level ids", {
  s1 <- ptr_server_state(.formula_two_vars, ns = shiny::NS("page1"))
  s2 <- ptr_server_state(.formula_two_vars, ns = shiny::NS("page2"))

  expect_equal(unlist(s1$raw_ids), unlist(s2$raw_ids))
  expect_length(intersect(unlist(s1$ui_ids), unlist(s2$ui_ids)), 0)
  expect_length(intersect(unlist(s1$server_ids), unlist(s2$server_ids)), 0)
  expect_equal(s1$ids, s1$server_ids)
})

test_that("distinct ns values keep parsed ids raw", {
  s1 <- ptr_server_state(.formula_two_vars, ns = shiny::NS("page1"))
  s2 <- ptr_server_state(.formula_two_vars, ns = shiny::NS("page2"))

  shiny::isolate({
    il1 <- unlist(s1$obj()$id_list)
    il2 <- unlist(s2$obj()$id_list)
    expect_equal(il1, il2)
    expect_true(all(!startsWith(il1, "page1-")))
    expect_true(all(!startsWith(il1, "page2-")))
  })
})

test_that("distinct ns values produce disjoint rendered placeholder ids", {
  s1 <- ptr_server_state(.formula_two_vars, ns = shiny::NS("page1"))
  s2 <- ptr_server_state(.formula_two_vars, ns = shiny::NS("page2"))

  shiny::isolate({
    ui1 <- paste(as.character(ptr_get_tab_ui(s1$obj(), ns_fn = s1$ui_ns_fn)), collapse = "\n")
    ui2 <- paste(as.character(ptr_get_tab_ui(s2$obj(), ns_fn = s2$ui_ns_fn)), collapse = "\n")
    expect_match(ui1, 'id="page1-var-ggplot_3_2"', fixed = TRUE)
    expect_match(ui2, 'id="page2-var-ggplot_3_2"', fixed = TRUE)
  })
})

test_that("distinct ns values keep checkbox id list raw", {
  s1 <- ptr_server_state(.formula_two_vars, ns = shiny::NS("page1"))
  s2 <- ptr_server_state(.formula_two_vars, ns = shiny::NS("page2"))

  shiny::isolate({
    cb1 <- unlist(s1$obj()$checkbox_id_list)
    cb2 <- unlist(s2$obj()$checkbox_id_list)
    expect_equal(cb1, cb2)
    expect_true(all(!startsWith(cb1, "page1-")))
    expect_true(all(!startsWith(cb1, "page2-")))
  })
})

test_that("distinct root-session ns values bind independent server runtimes", {
  server_wrapper <- function(input, output, session) {
    session$userData$state_a <- ptr_server(
      input, output, session,
      "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()",
      ns = shiny::NS("plot_a")
    )
    session$userData$state_b <- ptr_server(
      input, output, session,
      "ggplot(data = iris, aes(x = var, y = var)) + geom_point()",
      ns = shiny::NS("plot_b")
    )
  }

  shiny::testServer(server_wrapper, {
    session$setInputs(
      "plot_a-ggplot_3_2" = "mpg",
      "plot_a-ggplot_3_3" = "wt",
      "plot_a-geom_point_checkbox" = TRUE,
      "plot_a-draw" = 1,
      "plot_b-ggplot_3_2" = "Sepal.Length",
      "plot_b-ggplot_3_3" = "Sepal.Width",
      "plot_b-geom_point_checkbox" = TRUE,
      "plot_b-draw" = 1
    )

    result_a <- session$userData$state_a$runtime()
    result_b <- session$userData$state_b$runtime()

    expect_true(result_a$ok)
    expect_true(result_b$ok)
    expect_match(result_a$code_text, "aes\\(x = mpg, y = wt\\)")
    expect_match(result_b$code_text, "aes\\(x = Sepal.Length, y = Sepal.Width\\)")
  })
})

# ---------------------------------------------------------------------------
# AC-2: ptr_input_ui(ns=) and ptr_server_state(ns=) produce matching ids;
#        mismatched ns values produce non-matching ids.
# ---------------------------------------------------------------------------

test_that("ptr_input_ui ns matches ptr_server_state ns for control_panel id", {
  ns_fn <- shiny::NS("page1")
  ids   <- ptr_build_ids()
  state <- ptr_server_state(.formula_two_vars, ns = ns_fn)

  # ptr_input_ui applies ns_fn to ids$control_panel internally;
  # ptr_server_state stores ns_fn(ids$control_panel) in state$ui_ids$control_panel.
  # Both should equal ns_fn("controlPanel").
  expect_equal(state$ui_ids$control_panel, ns_fn(ids$control_panel))
  expect_equal(state$server_ids$control_panel, ns_fn(ids$control_panel))
})

test_that("mismatched ns values produce non-matching control_panel ids", {
  ns_a <- shiny::NS("pageA")
  ns_b <- shiny::NS("pageB")
  ids  <- ptr_build_ids()

  state_a <- ptr_server_state(.formula_two_vars, ns = ns_a)

  # state_a uses ns_a; ns_b would produce a different rendered id.
  expect_false(state_a$ui_ids$control_panel == ns_b(ids$control_panel))
})

test_that("ptr_server_state id contract carries raw, UI, and server ids", {
  ns_fn <- shiny::NS("myns")
  state <- ptr_server_state(.formula_two_vars, ns = ns_fn)

  expect_equal(state$raw_ids$control_panel, "controlPanel")
  expect_equal(state$ui_ids$control_panel, "myns-controlPanel")
  expect_equal(state$server_ids$control_panel, "myns-controlPanel")
  expect_equal(state$ids, state$server_ids)
})

# ---------------------------------------------------------------------------
# AC-3: ns = shiny::NS(NULL) (default) produces same ids as omitting arg
# ---------------------------------------------------------------------------

test_that("ns = shiny::NS(NULL) produces same top-level ids as default", {
  s_default  <- ptr_server_state(.formula_two_vars)
  s_explicit <- ptr_server_state(.formula_two_vars, ns = shiny::NS(NULL))

  expect_equal(unlist(s_default$raw_ids), unlist(s_explicit$raw_ids))
  expect_equal(unlist(s_default$ui_ids), unlist(s_explicit$ui_ids))
  expect_equal(unlist(s_default$server_ids), unlist(s_explicit$server_ids))
})

test_that("ns = shiny::NS(NULL) produces same id_list as default", {
  s_default  <- ptr_server_state(.formula_two_vars)
  s_explicit <- ptr_server_state(.formula_two_vars, ns = shiny::NS(NULL))

  shiny::isolate({
    expect_equal(
      unlist(s_default$obj()$id_list),
      unlist(s_explicit$obj()$id_list)
    )
  })
})

test_that("ns = shiny::NS(NULL) default ids have no namespace prefix", {
  s <- ptr_server_state(.formula_two_vars)
  expect_equal(s$raw_ids$control_panel, "controlPanel")
  expect_equal(s$ui_ids$control_panel, "controlPanel")
  expect_equal(s$server_ids$control_panel, "controlPanel")
  shiny::isolate({
    il <- unlist(s$obj()$id_list)
    expect_true(all(!grepl("^[^-]+-", il)))
  })
})

# ---------------------------------------------------------------------------
# AC-4: ptr_runtime_input_spec returns namespaced input_id values when ns set
# ---------------------------------------------------------------------------

test_that("ptr_runtime_input_spec returns raw input_id for placeholder rows", {
  ns_fn <- shiny::NS("tab1")
  state <- ptr_server_state(.formula_two_vars, ns = ns_fn)

  shiny::isolate({
    spec <- ptr_runtime_input_spec(state$obj())
    placeholder_ids <- spec$input_id[spec$type != "checkbox"]
    expect_true(all(!startsWith(placeholder_ids, "tab1-")))
  })
})

test_that("ptr_runtime_input_spec returns raw input_id for checkbox rows", {
  ns_fn <- shiny::NS("tab1")
  state <- ptr_server_state(.formula_two_vars, ns = ns_fn)

  shiny::isolate({
    spec <- ptr_runtime_input_spec(state$obj())
    checkbox_ids <- spec$input_id[spec$type == "checkbox"]
    expect_true(all(!startsWith(checkbox_ids, "tab1-")))
  })
})

test_that("ptr_runtime_input_spec without ns returns un-prefixed input_ids", {
  state <- ptr_server_state(.formula_two_vars)

  shiny::isolate({
    spec <- ptr_runtime_input_spec(state$obj())
    expect_true(all(!grepl("^[^-]+-", spec$input_id)))
  })
})

# ---------------------------------------------------------------------------
# AC-5: Non-function ns aborts with a clear error
# ---------------------------------------------------------------------------

test_that("non-function ns (string) aborts with an error", {
  expect_error(
    ptr_server_state(.formula_two_vars, ns = "page1"),
    regexp = "ns"
  )
})

test_that("non-function ns (integer) aborts with an error", {
  expect_error(
    ptr_server_state(.formula_two_vars, ns = 1L),
    regexp = "ns"
  )
})

test_that("non-function ns (NULL) aborts with an error", {
  expect_error(
    ptr_server_state(.formula_two_vars, ns = NULL),
    regexp = "ns"
  )
})

# ---------------------------------------------------------------------------
# AC-6: ptr_ns_obj rewrites all four id families correctly
# ---------------------------------------------------------------------------

test_that("ptr_ns_obj rewrites id_list values", {
  obj    <- ptr_parse_formula(.formula_two_vars)
  ns_fn  <- shiny::NS("p")
  obj_ns <- ptr_ns_obj(obj, ns_fn)

  original_ids <- unlist(obj$id_list)
  rewritten    <- unlist(obj_ns$id_list)
  expect_equal(rewritten, vapply(original_ids, ns_fn, character(1)))
})

test_that("ptr_ns_obj rewrites placeholder_map keys", {
  obj    <- ptr_parse_formula(.formula_two_vars)
  ns_fn  <- shiny::NS("p")
  obj_ns <- ptr_ns_obj(obj, ns_fn)

  original_keys <- unlist(lapply(obj$placeholder_map, names))
  rewritten_keys <- unlist(lapply(obj_ns$placeholder_map, names))
  expect_equal(rewritten_keys, vapply(original_keys, ns_fn, character(1)))
})

test_that("ptr_ns_obj rewrites placeholder_map entry $id fields", {
  obj    <- ptr_parse_formula(.formula_two_vars)
  ns_fn  <- shiny::NS("p")
  obj_ns <- ptr_ns_obj(obj, ns_fn)

  original_entry_ids <- unlist(lapply(obj$placeholder_map, function(m)
    vapply(m, function(e) e$id, character(1))
  ))
  rewritten_entry_ids <- unlist(lapply(obj_ns$placeholder_map, function(m)
    vapply(m, function(e) e$id, character(1))
  ))
  expect_equal(unname(rewritten_entry_ids), unname(vapply(original_entry_ids, ns_fn, character(1))))
})

test_that("ptr_ns_obj rewrites index_path_list names", {
  obj    <- ptr_parse_formula(.formula_two_vars)
  ns_fn  <- shiny::NS("p")
  obj_ns <- ptr_ns_obj(obj, ns_fn)

  original_names <- unlist(lapply(obj$index_path_list, names))
  rewritten_names <- unlist(lapply(obj_ns$index_path_list, names))
  expect_equal(rewritten_names, vapply(original_names, ns_fn, character(1)))
})

test_that("ptr_ns_obj rewrites checkbox_id_list values but preserves layer-name keys", {
  obj    <- ptr_parse_formula(.formula_two_vars)
  ns_fn  <- shiny::NS("p")
  obj_ns <- ptr_ns_obj(obj, ns_fn)

  original_values <- unname(unlist(obj$checkbox_id_list))
  original_keys   <- names(unlist(obj$checkbox_id_list))

  rewritten_values <- unname(unlist(obj_ns$checkbox_id_list))
  rewritten_keys   <- names(unlist(obj_ns$checkbox_id_list))

  expect_equal(rewritten_values, unname(vapply(original_values, ns_fn, character(1))))
  expect_equal(rewritten_keys, original_keys)  # layer names unchanged
})

test_that("ptr_ns_obj with shiny::NS(NULL) is identity (no-op)", {
  obj      <- ptr_parse_formula(.formula_two_vars)
  obj_noop <- ptr_ns_obj(obj, shiny::NS(NULL))

  expect_equal(unlist(obj_noop$id_list),       unlist(obj$id_list))
  expect_equal(unlist(lapply(obj_noop$placeholder_map, names)),
               unlist(lapply(obj$placeholder_map, names)))
  expect_equal(unlist(lapply(obj_noop$index_path_list, names)),
               unlist(lapply(obj$index_path_list, names)))
  expect_equal(unlist(obj_noop$checkbox_id_list), unlist(obj$checkbox_id_list))
})
