# PLAN-07 -- node$default seeds widget initial values, and shared
# coordinators surface the first-occurrence default to the shared widget.
#
# Coverage:
#   * per-layer value widgets (num / text / expr) seed from node$default
#   * per-layer var consumer (server-side renderUI flow) seeds from
#     node$default via the orchestrator (`invoke_build_ui`)
#   * shared coordinator picks occurrences[[1L]]$default
#     (first-occurrence-wins) and surfaces it onto the rendered widget
#   * NULL default preserves today's empty-initial behaviour
#   * substitute walk is unchanged by node$default
#
# All assertions inspect the rendered shiny.tag's `value` (or `selected`)
# attribute on the literal HTML, not just the tag's existence -- the
# project's "e2e assertion-weakness" lens applies.

# ---- helpers ---------------------------------------------------------------

.input_value_attr <- function(ui, id) {
  rendered <- as.character(ui)
  pat <- paste0("<input[^>]*id=\"", id, "\"[^>]*value=\"([^\"]*)\"")
  m <- regmatches(rendered, regexec(pat, rendered))[[1L]]
  if (length(m) < 2L) return(NA_character_)
  m[[2L]]
}

.textarea_value <- function(ui, id) {
  rendered <- as.character(ui)
  # <textarea id="..." ...>VALUE</textarea>
  pat <- paste0("<textarea[^>]*id=\"", id, "\"[^>]*>([^<]*)</textarea>")
  m <- regmatches(rendered, regexec(pat, rendered))[[1L]]
  if (length(m) < 2L) return(NA_character_)
  m[[2L]]
}

# ---- per-layer value widgets ----------------------------------------------

test_that("num build_ui seeds numericInput value from node$default", {
  node <- ptr_ph_value(
    id = "p_num", keyword = "num", expr = quote(num(5)), default = 5
  )
  ui <- build_ui_for(node, layer_name = "geom_point")
  expect_equal(.input_value_attr(ui, "p_num"), "5")
})

test_that("num build_ui leaves value empty when node$default is NULL", {
  node <- ptr_ph_value(
    id = "p_num2", keyword = "num", expr = quote(num())
  )
  ui <- build_ui_for(node, layer_name = "geom_point")
  # Today's behaviour: numericInput renders value="NA" -- but the copy-
  # driven placeholder rewrite blanks that to "" when empty_text is set.
  # Either is acceptable as "no seed"; reject anything that smells like
  # a real number (anything other than "NA" or "").
  val <- .input_value_attr(ui, "p_num2")
  expect_true(is.na(val) || val %in% c("NA", ""))
})

test_that("text build_ui seeds textInput value from node$default", {
  node <- ptr_ph_value(
    id = "p_txt", keyword = "text", expr = quote(text("hello")),
    default = "hello"
  )
  ui <- build_ui_for(node, layer_name = "geom_point")
  expect_equal(.input_value_attr(ui, "p_txt"), "hello")
})

test_that("text build_ui leaves value empty when node$default is NULL", {
  node <- ptr_ph_value(
    id = "p_txt2", keyword = "text", expr = quote(text())
  )
  ui <- build_ui_for(node, layer_name = "geom_point")
  expect_equal(.input_value_attr(ui, "p_txt2"), "")
})

test_that("expr build_ui seeds textAreaInput value from node$default (string)", {
  node <- ptr_ph_value(
    id = "p_expr", keyword = "expr", expr = quote(expr("mpg * 2")),
    default = "mpg * 2"
  )
  ui <- build_ui_for(node, layer_name = "geom_point")
  expect_equal(.textarea_value(ui, "p_expr"), "mpg * 2")
})

test_that("expr build_ui deparses a language default into textarea source", {
  node <- ptr_ph_value(
    id = "p_expr_lang", keyword = "expr",
    expr = quote(expr(mpg * 2)),
    default = quote(mpg * 2)
  )
  ui <- build_ui_for(node, layer_name = "geom_point")
  expect_equal(.textarea_value(ui, "p_expr_lang"), "mpg * 2")
})

test_that("expr build_ui leaves textarea empty when node$default is NULL", {
  node <- ptr_ph_value(
    id = "p_expr2", keyword = "expr", expr = quote(expr())
  )
  ui <- build_ui_for(node, layer_name = "geom_point")
  expect_equal(.textarea_value(ui, "p_expr2"), "")
})

# ---- per-layer var consumer (orchestrator path) ----------------------------

test_that("var hook seeds picker selection from node$default via orchestrator", {
  node <- ptr_ph_data_consumer(
    id = "p_var", keyword = "var", expr = quote(var(mpg)),
    default = "mpg"
  )
  # Mirror `ptr_setup_consumer_uis`'s call into the orchestrator: cols
  # populated, no persisted input (selected = character(0)). The
  # orchestrator must fall back to node$default.
  picker <- invoke_build_ui(
    node,
    ui_text = NULL,
    layer_name = NULL,
    ns_fn = identity,
    extra = list(cols = c("mpg", "hp", "disp"), data = mtcars,
                 selected = character(0))
  )
  rendered <- as.character(picker)
  # pickerInput marks the selected <option> with `selected`.
  expect_match(rendered, "<option[^>]*selected[^>]*>mpg</option>")
})

test_that("persisted picker selection wins over node$default", {
  node <- ptr_ph_data_consumer(
    id = "p_var2", keyword = "var", expr = quote(var(mpg)),
    default = "mpg"
  )
  picker <- invoke_build_ui(
    node,
    ui_text = NULL,
    layer_name = NULL,
    ns_fn = identity,
    extra = list(cols = c("mpg", "hp", "disp"), data = mtcars,
                 selected = "hp")
  )
  rendered <- as.character(picker)
  expect_match(rendered, "<option[^>]*selected[^>]*>hp</option>")
  # mpg must NOT carry `selected`.
  expect_false(grepl("<option[^>]*selected[^>]*>mpg</option>", rendered))
})

test_that("NULL node$default + no persisted input leaves picker empty", {
  node <- ptr_ph_data_consumer(
    id = "p_var3", keyword = "var", expr = quote(var())
  )
  picker <- invoke_build_ui(
    node,
    ui_text = NULL,
    layer_name = NULL,
    ns_fn = identity,
    extra = list(cols = c("mpg", "hp"), data = mtcars,
                 selected = character(0))
  )
  rendered <- as.character(picker)
  expect_false(grepl("<option[^>]*selected[^>]*>", rendered))
})

test_that("var default not in cols is silently dropped (C1)", {
  # Edge-case resolution C1 from the plan: a default referring to a
  # column not present in cols is dropped silently (no warning).
  node <- ptr_ph_data_consumer(
    id = "p_var_missing", keyword = "var", expr = quote(var(absent)),
    default = "absent"
  )
  expect_no_warning(
    picker <- invoke_build_ui(
      node,
      ui_text = NULL,
      layer_name = NULL,
      ns_fn = identity,
      extra = list(cols = c("mpg", "hp"), data = mtcars,
                   selected = character(0))
    )
  )
  rendered <- as.character(picker)
  expect_false(grepl("<option[^>]*selected[^>]*>absent</option>", rendered))
})

# ---- shared coordinator default seeding ------------------------------------

test_that("shared_widget_default reads occurrences[[1L]]$default", {
  occ <- list(
    list(default = "mpg"),
    list(default = "disp")
  )
  expect_equal(shared_widget_default(occ), "mpg")
})

test_that("shared_widget_default returns NULL when first occurrence has none", {
  # First occurrence wins -- even when first is NULL and a later
  # occurrence has a default, the answer is NULL.
  occ <- list(
    list(default = NULL),
    list(default = "disp")
  )
  expect_null(shared_widget_default(occ))
})

test_that("shared_widget_default returns NULL on empty input", {
  expect_null(shared_widget_default(list()))
})

test_that("shared_widget_default does not warn on conflicting defaults (B2)", {
  occ <- list(
    list(default = "mpg"),
    list(default = "disp")
  )
  expect_no_warning(shared_widget_default(occ))
})

# ---- substitute walk is unchanged ------------------------------------------

test_that("substitute walk ignores node$default; runtime snapshot wins", {
  # The substitute walk reads runtime values from `input_snapshot` only.
  # `node$default` participates exclusively at boot via the UI seed
  # path; substitute must not consult it. We exercise that directly on
  # a hand-built value node so the assertion is unambiguous.
  node <- ptr_ph_value(
    id = "p_sub", keyword = "num", expr = quote(num(99)), default = 99
  )
  snapshot <- list(p_sub = 7)
  result <- ptr_substitute(node, input_snapshot = snapshot)
  # The result must be a literal carrying the snapshot value (7), NOT
  # the node$default (99). `ptr_literal` stores the resolved value on
  # `$expr` (numeric scalar, not a language object).
  expect_true(inherits(result, "ptr_literal"))
  expect_equal(as.numeric(result$expr), 7)
})

test_that("substitute walk treats missing input as missing, NOT default", {
  # Same shape, but no snapshot entry for the node id. The walk must
  # NOT fall back to node$default -- the resolved form must be the
  # "missing" sentinel so the prune step can drop the arg.
  node <- ptr_ph_value(
    id = "p_sub2", keyword = "num", expr = quote(num(99)), default = 99
  )
  result <- ptr_substitute(node, input_snapshot = list())
  expect_true(inherits(result, "ptr_missing"))
})

# ---- named_args pass-through -----------------------------------------------

test_that("invoke_build_ui passes node$named_args to the hook", {
  # Register a temporary placeholder whose hook records its arguments.
  # Snapshot + restore the registry around the test so we don't lose
  # other keywords (clear() drops everything, then re-register builtins).
  withr::defer({
    ptr_registry_clear()
    ptr_register_builtins()
  })
  seen <- new.env(parent = emptyenv())
  seen$args <- NULL
  ptr_define_placeholder_value(
    keyword = "ppFakeNa",
    build_ui = function(node, label = NULL, copy = NULL,
                        selected = NULL, named_args = list(), ...) {
      seen$args <- list(
        selected = selected,
        named_args = named_args
      )
      shiny::textInput(node$id, label = label %||% "x", value = "")
    },
    resolve_expr = function(value, node, ...) NULL
  )
  node <- ptr_ph_value(
    id = "p_fake", keyword = "ppFakeNa", expr = quote(ppFakeNa()),
    default = "seed-me", named_args = list(step = 0.5, hint = "go")
  )
  invisible(build_ui_for(node, layer_name = "geom_point"))
  expect_equal(seen$args$selected, "seed-me")
  expect_equal(seen$args$named_args, list(step = 0.5, hint = "go"))
})
