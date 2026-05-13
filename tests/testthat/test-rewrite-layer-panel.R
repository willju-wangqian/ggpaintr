# Helpers shared with test-rewrite-build-ui.R
.find_tags2 <- function(tag, has_class = NULL, has_id = NULL) {
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

.layer_by_name <- function(tree, nm) {
  for (l in tree$layers) {
    if (identical(l$name, nm)) return(l)
  }
  NULL
}

# ---- P6.6 / P6.7 — Data sub-tab presence gated on pipeline placeholders ----

test_that("P6.6 layer panel includes Data sub-tab when pipeline placeholders present", {
  tree <- ptr_translate("mtcars |> head(num) |> ggplot(aes(x = var))")
  layer <- .layer_by_name(tree, "ggplot")
  panel <- build_ui_for(layer)
  # Look for tabsetPanel containing two tabPanels (Data + Controls)
  rendered <- as.character(panel)
  expect_match(rendered, "Data")
  expect_match(rendered, "Controls")
})

test_that("P6.7 layer panel omits Data sub-tab when no pipeline placeholders", {
  tree <- ptr_translate("ggplot(mtcars, aes(x = var)) + geom_point()")
  layer <- .layer_by_name(tree, "ggplot")
  panel <- build_ui_for(layer)
  rendered <- as.character(panel)
  # No Data sub-tab should be emitted; controls flat
  # We assert by checking no nested tabsetPanel structure (heuristic)
  expect_no_match(rendered, ">Data<")  # tab title surrounded by tags
})

# ---- P6.8 / P6.12 — checkbox emission ----

test_that("P6.8 non-ggplot layer panel includes a checkbox", {
  tree <- ptr_translate("ggplot(mtcars) + geom_point()")
  layer <- .layer_by_name(tree, "geom_point")
  panel <- build_ui_for(layer)
  cb_id <- "geom_point_checkbox"
  expect_true(length(.find_tags2(panel, has_id = cb_id)) > 0L)
})

test_that("P6.12 ggplot layer has no checkbox", {
  tree <- ptr_translate("ggplot(mtcars) + geom_point()")
  layer <- .layer_by_name(tree, "ggplot")
  panel <- build_ui_for(layer)
  expect_equal(length(.find_tags2(panel, has_id = "ggplot_checkbox")), 0L)
})

# ---- P6.9 / P6.10 — checkbox default state ----

test_that("P6.9 checkbox defaults to TRUE when key absent", {
  tree <- ptr_translate("ggplot(mtcars) + geom_point()")
  layer <- .layer_by_name(tree, "geom_point")
  panel <- build_ui_for(
    layer,
    checkbox_defaults = c(geom_smooth = FALSE)  # no geom_point key
  )
  rendered <- as.character(panel)
  expect_match(rendered, 'checked="checked"', fixed = TRUE)
})

test_that("P6.10 checkbox defaults to FALSE when key set FALSE", {
  tree <- ptr_translate("ggplot(mtcars) + geom_point()")
  layer <- .layer_by_name(tree, "geom_point")
  panel <- build_ui_for(
    layer,
    checkbox_defaults = c(geom_point = FALSE)
  )
  rendered <- as.character(panel)
  expect_no_match(rendered, 'checked="checked"', fixed = TRUE)
})

# ---- P6.11 — content div carries ptr-layer-disabled when FALSE ----

test_that("P6.11 layer toggle FALSE adds ptr-layer-disabled class", {
  tree <- ptr_translate("ggplot(mtcars) + geom_point()")
  layer <- .layer_by_name(tree, "geom_point")
  panel <- build_ui_for(
    layer,
    checkbox_defaults = c(geom_point = FALSE)
  )
  disabled_divs <- .find_tags2(panel, has_class = "ptr-layer-disabled")
  expect_true(length(disabled_divs) > 0L)
})

test_that("active layer content div has ptr-layer-content but not -disabled", {
  tree <- ptr_translate("ggplot(mtcars) + geom_point()")
  layer <- .layer_by_name(tree, "geom_point")
  panel <- build_ui_for(layer)  # default = TRUE
  content_divs <- .find_tags2(panel, has_class = "ptr-layer-content")
  disabled_divs <- .find_tags2(panel, has_class = "ptr-layer-disabled")
  expect_true(length(content_divs) > 0L)
  expect_equal(length(disabled_divs), 0L)
})

# ---- ns_fn threading ----

test_that("layer panel namespaces ids via ns_fn", {
  tree <- ptr_translate("ggplot(mtcars) + geom_point()")
  layer <- .layer_by_name(tree, "geom_point")
  ns <- shiny::NS("module1")
  panel <- build_ui_for(layer, ns_fn = ns)
  expect_true(length(.find_tags2(panel, has_id = ns("geom_point_checkbox"))) > 0L)
})

# ---- Phase 4.1 — pipeline-stage placeholder labels name the verb ----

test_that("pipeline-stage placeholder label names the verb via {param}", {
  tree <- ptr_translate("mtcars |> head(num) |> ggplot(aes(x = var))")
  layer <- .layer_by_name(tree, "ggplot")
  panel <- build_ui_for(layer)
  # Default copy is "Enter a number for {param}"; for an unnamed positional arg
  # the verb fills {param}, giving "...for head()" -- and we do NOT also append
  # " in head()" on top of that.
  expect_match(as.character(panel), "Enter a number for head\\(\\)")
  expect_no_match(as.character(panel), "head\\(\\) in head\\(\\)")
})

test_that("unnamed-arg pipeline placeholder uses 'verb()' as the copy param key", {
  tree <- ptr_translate("mtcars |> head(num) |> ggplot(aes(x = var))")
  layer <- .layer_by_name(tree, "ggplot")
  panel <- build_ui_for(
    layer,
    ui_text = list(params = list(`head()` = list(num = list(label = "How many rows"))))
  )
  expect_match(as.character(panel), "How many rows in head\\(\\)")
})

test_that("named-arg pipeline placeholder keeps its param key and gets the suffix", {
  tree <- ptr_translate("mtcars |> transform(n = num) |> ggplot(aes(x = var))")
  layer <- .layer_by_name(tree, "ggplot")
  panel <- build_ui_for(layer)
  expect_match(as.character(panel), "in transform\\(\\)")
})

test_that("placeholder nested in a sub-expression names the stage verb, not the inner call", {
  # `text` is the RHS of `Species == text`, itself an argument to `subset()`.
  # The label/copy key must report the stage verb `subset()`, never `==()`.
  tree <- ptr_translate("iris |> subset(Species == text) |> ggplot(aes(x = var))")
  layer <- .layer_by_name(tree, "ggplot")
  panel <- as.character(build_ui_for(layer))
  expect_match(panel, "Enter a value for subset\\(\\)")
  expect_no_match(panel, "==\\(\\)")
})

# ---- Phase 3 — layer-disabled visual cue ----

test_that("app shells inject the ptr_set_class handler + disabled-panel CSS", {
  f <- "ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()"
  for (ui in list(ptr_app_components(f)$ui, ptr_module_ui("m", f))) {
    rendered <- as.character(ui)
    expect_match(rendered, "ptr_set_class")
    expect_match(rendered, "ptr-layer-disabled")
  }
  # bslib path injects ptr_layer_assets() the same way; the shinyApp object
  # hides its $ui, so assert the helper itself renders the handler + CSS.
  assets <- as.character(ptr_layer_assets())
  expect_match(assets, "ptr_set_class")
  expect_match(assets, "ptr-layer-disabled")
})

test_that("toggling a layer include-checkbox sends ptr_set_class", {
  e <- list2env(list(mtcars = mtcars), parent = globalenv())
  sent <- new.env(parent = emptyenv())
  sent$msgs <- list()
  server <- function(input, output, session) {
    session$userData$state <- ptr_server(
      input, output, session,
      "ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()",
      envir = e
    )
  }
  shiny::testServer(server, {
    state <- session$userData$state
    session$sendCustomMessage <- function(type, message) {
      sent$msgs[[length(sent$msgs) + 1L]] <- list(type = type, message = message)
    }
    spec <- state$input_spec
    ck <- spec$input_id[spec$role == "layer_checkbox"][[1]]
    content_id <- layer_panel_content_id(
      spec$layer_name[spec$role == "layer_checkbox"][[1]]
    )

    do.call(session$setInputs, stats::setNames(list(FALSE), ck))
    session$flushReact()
    off_msgs <- Filter(function(m) identical(m$type, "ptr_set_class") &&
                         identical(m$message$id, content_id), sent$msgs)
    expect_true(length(off_msgs) >= 1L)
    expect_true(isTRUE(off_msgs[[length(off_msgs)]]$message$add))
    expect_equal(off_msgs[[length(off_msgs)]]$message$cls, "ptr-layer-disabled")

    do.call(session$setInputs, stats::setNames(list(TRUE), ck))
    session$flushReact()
    on_msgs <- Filter(function(m) identical(m$type, "ptr_set_class") &&
                        identical(m$message$id, content_id), sent$msgs)
    expect_false(isTRUE(on_msgs[[length(on_msgs)]]$message$add))
  })
})
