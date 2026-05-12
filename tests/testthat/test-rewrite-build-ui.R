# Helpers — pull a placeholder node out of a parsed tree by keyword.
.ph_by_keyword <- function(tree, kw) {
  hits <- find_nodes(tree, function(n) {
    is_ptr_placeholder(n) && identical(n$keyword, kw)
  })
  if (!length(hits)) rlang::abort(paste0("no placeholder with keyword=", kw))
  hits[[1L]]
}

# Helpers — collect tag attribute (recursive)
.find_tags <- function(tag, has_class = NULL, has_id = NULL) {
  out <- list()
  visit <- function(x) {
    if (inherits(x, "shiny.tag")) {
      cls <- x$attribs$class %||% ""
      id <- x$attribs$id %||% ""
      class_match <- is.null(has_class) || grepl(has_class, cls, fixed = TRUE)
      id_match <- is.null(has_id) || identical(id, has_id)
      if (class_match && id_match) out[[length(out) + 1L]] <<- x
      for (child in x$children) visit(child)
    } else if (inherits(x, "shiny.tag.list") || is.list(x)) {
      for (el in x) visit(el)
    }
  }
  visit(tag)
  out
}

# ---- P6.1 / P6.2 — value placeholders ----

test_that("P6.1 text placeholder UI renders textInput", {
  tree <- ptr_translate("ggplot(mtcars) + geom_point(color = text)")
  node <- .ph_by_keyword(tree, "text")
  ui <- build_ui_for(node, layer_name = "geom_point")
  text_inputs <- .find_tags(ui, has_id = node$id)
  expect_true(length(text_inputs) > 0L)
})

test_that("P6.2 num placeholder UI renders a numeric-style input", {
  tree <- ptr_translate("ggplot(mtcars) + geom_point(size = num)")
  node <- .ph_by_keyword(tree, "num")
  ui <- build_ui_for(node, layer_name = "geom_point")
  num_inputs <- .find_tags(ui, has_id = node$id)
  expect_true(length(num_inputs) > 0L)
})

# ---- P6.3 / P6.4 — var consumer ----

test_that("P6.3 var consumer static UI emits a uiOutput container", {
  tree <- ptr_translate("ggplot(mtcars, aes(x = var)) + geom_point()")
  node <- .ph_by_keyword(tree, "var")
  ui <- build_ui_for(node, layer_name = "ggplot")
  rendered <- as.character(ui)
  expect_match(rendered, "shiny-html-output")
  expect_match(rendered, paste0("id=\"", consumer_output_id(node$id), "\""), fixed = TRUE)
})

test_that("P6.3 registry build_ui renders pickerInput populated with cols", {
  tree <- ptr_translate("ggplot(mtcars, aes(x = var)) + geom_point()")
  node <- .ph_by_keyword(tree, "var")
  entry <- ptr_registry_lookup("var")
  picker <- entry$build_ui(node, cols = c("mpg", "hp"), label = "Pick a column")
  rendered <- as.character(picker)
  expect_match(rendered, "selectpicker")
  expect_match(rendered, "mpg")
  expect_match(rendered, "hp")
})

test_that("P6.4 var consumer with empty cols still renders an empty container", {
  tree <- ptr_translate("ggplot(mtcars, aes(x = var)) + geom_point()")
  node <- .ph_by_keyword(tree, "var")
  ui <- build_ui_for(node, layer_name = "ggplot")
  rendered <- as.character(ui)
  expect_match(rendered, paste0("id=\"", consumer_output_id(node$id), "\""), fixed = TRUE)
})

# ---- P6.5 — upload source paired widgets ----

test_that("P6.5 upload UI emits paired widgets (file + name)", {
  tree <- ptr_translate("upload |> ggplot()")
  node <- .ph_by_keyword(tree, "upload")
  ui <- build_ui_for(node, layer_name = "ggplot")
  expect_true(length(.find_tags(ui, has_id = node$id)) > 0L)
  expect_true(length(.find_tags(ui, has_id = node$companion_id)) > 0L)
})

# ---- P6.13 — copy resolution ----

test_that("P6.13 copy resolution applies custom label", {
  tree <- ptr_translate("ggplot(mtcars) + geom_point(size = num)")
  node <- .ph_by_keyword(tree, "num")
  ui <- build_ui_for(
    node,
    layer_name = "geom_point",
    ui_text = list(params = list(size = list(num = list(label = "Pick a number"))))
  )
  rendered <- as.character(ui)
  expect_match(rendered, "Pick a number")
})

# ---- P6.15 — ptr_user_expr emits no UI ----

test_that("P6.15 ptr_user_expr emits no UI", {
  node <- ptr_user_expr(quote(theme_minimal()))
  ui <- build_ui_for(node)
  expect_null(ui)
})

test_that("ptr_literal / ptr_missing emit no UI", {
  expect_null(build_ui_for(ptr_literal(1)))
  expect_null(build_ui_for(ptr_missing()))
})

# ---- ns_fn rendering — ids are namespaced before passed to hook ----

test_that("build_ui_for renders id through ns_fn", {
  tree <- ptr_translate("ggplot(mtcars) + geom_point(color = text)")
  node <- .ph_by_keyword(tree, "text")
  ns <- shiny::NS("module1")
  ui <- build_ui_for(node, layer_name = "geom_point", ns_fn = ns)
  rendered <- as.character(ui)
  expect_match(rendered, fixed = TRUE, ns(node$id))
})

# ---- Phase 1 — copy fidelity: help / placeholder / empty_text reach widgets ----

test_that("text widget renders placeholder + help from resolved copy", {
  tree <- ptr_translate("ggplot(mtcars) + geom_point(color = text)")
  node <- .ph_by_keyword(tree, "text")
  ui <- build_ui_for(
    node, layer_name = "geom_point",
    ui_text = list(params = list(color = list(text = list(
      label = "L", help = "H-help", placeholder = "P-ph"
    ))))
  )
  rendered <- as.character(ui)
  expect_match(rendered, 'placeholder="P-ph"', fixed = TRUE)
  expect_match(rendered, "help-block")
  expect_match(rendered, "H-help")
})

test_that("text widget carries the restored default placeholder", {
  tree <- ptr_translate("ggplot(mtcars) + geom_point(color = text)")
  node <- .ph_by_keyword(tree, "text")
  ui <- build_ui_for(node, layer_name = "geom_point")
  expect_match(as.character(ui), "quotes are added automatically")
  expect_equal(
    ptr_default_ui_text()$defaults$text$placeholder,
    "Plain text - quotes are added automatically"
  )
})

test_that("expr widget renders placeholder + help from resolved copy", {
  tree <- ptr_translate("ggplot(mtcars) + geom_point() + facet_wrap(expr)")
  node <- .ph_by_keyword(tree, "expr")
  ui <- build_ui_for(
    node, layer_name = "facet_wrap",
    ui_text = list(layers = list(facet_wrap = list(expr = list(
      `__unnamed__` = list(placeholder = "~ Species", help = "facet help")
    ))))
  )
  rendered <- as.character(ui)
  expect_match(rendered, "textarea")
  expect_match(rendered, "~ Species")
  expect_match(rendered, "facet help")
})

test_that("num widget renders help only (no placeholder attr)", {
  tree <- ptr_translate("ggplot(mtcars) + geom_point(size = num)")
  node <- .ph_by_keyword(tree, "num")
  ui <- build_ui_for(
    node, layer_name = "geom_point",
    ui_text = list(params = list(size = list(num = list(help = "0 to 1"))))
  )
  rendered <- as.character(ui)
  expect_match(rendered, "help-block")
  expect_match(rendered, "0 to 1")
})

test_that("var picker honors copy$empty_text via noneSelectedText", {
  tree <- ptr_translate("ggplot(mtcars, aes(x = var)) + geom_point()")
  node <- .ph_by_keyword(tree, "var")
  entry <- ptr_registry_lookup("var")

  default_copy <- ptr_resolve_ui_text("control", keyword = "var", param = "x")
  picker_default <- entry$build_ui(node, cols = c("mpg", "hp"),
                                   label = "Pick a column", copy = default_copy)
  expect_match(as.character(picker_default), "Choose one column")

  custom_copy <- ptr_resolve_ui_text(
    "control", keyword = "var", param = "x",
    ui_text = list(params = list(x = list(var = list(empty_text = "Pick me"))))
  )
  picker_custom <- entry$build_ui(node, cols = c("mpg", "hp"),
                                  label = "Pick a column", copy = custom_copy)
  expect_match(as.character(picker_custom), "Pick me")
})

test_that("var default copy carries empty_text", {
  expect_equal(ptr_default_ui_text()$defaults$var$empty_text, "Choose one column")
})

test_that("a third-party build_ui hook without `...`/`copy` is not passed copy", {
  withr::defer({
    if (exists("ptr_registry_clear")) ptr_registry_clear()
    ptr_register_builtins()
  })
  ptr_define_placeholder_value(
    keyword = "plain_widget",
    build_ui = function(node, label) shiny::textInput(node$id, label),
    resolve_expr = function(value, node) value
  )
  tree <- ptr_translate("ggplot(mtcars) + geom_point(color = plain_widget)")
  node <- .ph_by_keyword(tree, "plain_widget")
  expect_silent(ui <- build_ui_for(node, layer_name = "geom_point"))
  expect_true(length(.find_tags(ui, has_id = node$id)) > 0L)
})

# ---- P6.14 — custom placeholder hook invoked ----

test_that("P6.14 custom placeholder UI invokes registered build_ui", {
  withr::defer({
    if (exists("ptr_registry_clear")) ptr_registry_clear()
    ptr_register_builtins()
  })
  hook_called <- FALSE
  ptr_define_placeholder_value(
    keyword = "scale_factor",
    build_ui = function(node, label = NULL, ...) {
      hook_called <<- TRUE
      shiny::numericInput(node$id, label = label, value = 1)
    },
    resolve_expr = function(value, node, ...) as.numeric(value)
  )
  tree <- ptr_translate("ggplot(mtcars) + geom_point(size = scale_factor)")
  node <- .ph_by_keyword(tree, "scale_factor")
  ui <- build_ui_for(node, layer_name = "geom_point")
  expect_true(hook_called)
  expect_true(length(.find_tags(ui, has_id = node$id)) > 0L)
})
