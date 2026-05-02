# Tests for ptr_get_layer_switcher_ui — picker + hidden tabsetPanel + per-layer
# panels with data/controls sub-tabs and the layer-toggle checkbox.

ui_str <- function(ui) paste(as.character(ui), collapse = "\n")

test_that("layer switcher returns NULL for non-ptr_obj input", {
  expect_null(ptr_get_layer_switcher_ui(NULL))
  expect_null(ptr_get_layer_switcher_ui(list()))
})

test_that("layer switcher renders pickerInput populated with all layer names", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point() + geom_smooth()"
  )
  rendered <- ui_str(ptr_get_layer_switcher_ui(obj))

  expect_match(rendered, 'id="ptr_layer_select"', fixed = TRUE)
  expect_match(rendered, '>Layer<', fixed = TRUE)
  for (layer in c("ggplot", "geom_point", "geom_smooth")) {
    expect_match(
      rendered,
      paste0('value="', layer, '"'),
      fixed = TRUE
    )
  }
})

test_that("layer switcher hidden tabset has type='hidden' and one panel per layer", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point() + geom_smooth()"
  )
  rendered <- ui_str(ptr_get_layer_switcher_ui(obj))

  expect_match(rendered, 'id="ptr_layer_tabset"', fixed = TRUE)
  expect_match(rendered, 'class="tab-content"', fixed = TRUE)
  for (layer in c("ggplot", "geom_point", "geom_smooth")) {
    expect_match(
      rendered,
      paste0('data-value="', layer, '"'),
      fixed = TRUE
    )
  }
})

test_that("layer with pipeline + non-pipeline placeholders gets Data + Controls sub-tabs", {
  obj <- ptr_parse_formula(
    "mtcars |> head(num) |> ggplot(aes(x = var, y = var)) + geom_point()"
  )
  rendered <- ui_str(ptr_get_layer_switcher_ui(obj))

  expect_match(rendered, 'data-value="Data"', fixed = TRUE)
  expect_match(rendered, 'data-value="Controls"', fixed = TRUE)
  # Update-data button only inside Data sub-tab
  expect_match(rendered, ptr_update_data_input_id("ggplot"), fixed = TRUE)
})

test_that("layer with non-pipeline placeholders only renders flat controls (no sub-tabs)", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )
  rendered <- ui_str(ptr_get_layer_switcher_ui(obj))

  expect_no_match(rendered, 'data-value="Data"')
  expect_no_match(rendered, 'data-value="Controls"')
  expect_no_match(rendered, "Update data")
})

test_that("layer switcher keeps ggplot panel without a layer-toggle checkbox", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )
  rendered <- ui_str(ptr_get_layer_switcher_ui(obj))

  expect_no_match(rendered, "ggplot_checkbox")
  expect_match(rendered, "geom_point_checkbox", fixed = TRUE)
})

test_that("layer-toggle checkbox renders at top of each non-ggplot layer panel", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var)) + geom_point() + geom_smooth()"
  )
  rendered <- ui_str(ptr_get_layer_switcher_ui(obj))

  expect_match(rendered, "Include this layer in the plot", fixed = TRUE)
  expect_match(
    rendered,
    paste0('id="', ptr_checkbox_input_id("geom_point"), '"'),
    fixed = TRUE
  )
  expect_match(
    rendered,
    paste0('id="', ptr_checkbox_input_id("geom_smooth"), '"'),
    fixed = TRUE
  )
})

test_that("layer toggle default = FALSE adds ptr-layer-disabled class to content div at render", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var)) + geom_point()"
  )
  rendered <- ui_str(ptr_get_layer_switcher_ui(
    obj,
    checkbox_defaults = c(geom_point = FALSE)
  ))

  expect_match(
    rendered,
    'class="ptr-layer-content ptr-layer-disabled"',
    fixed = TRUE
  )
})

test_that("layer toggle default = TRUE leaves ptr-layer-content class clean", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var)) + geom_point()"
  )
  rendered <- ui_str(ptr_get_layer_switcher_ui(obj))

  expect_match(rendered, 'class="ptr-layer-content"', fixed = TRUE)
  expect_no_match(rendered, "ptr-layer-disabled\"")
})

test_that("ui_text overrides apply to layer_picker, data_subtab, controls_subtab labels", {
  obj <- ptr_parse_formula(
    "mtcars |> head(num) |> ggplot(aes(x = var, y = var)) + geom_point()"
  )
  rendered <- ui_str(ptr_get_layer_switcher_ui(
    obj,
    ui_text = list(
      shell = list(
        layer_picker = list(label = "Choose layer"),
        data_subtab = list(label = "Pipeline"),
        controls_subtab = list(label = "Aesthetics")
      )
    )
  ))

  expect_match(rendered, ">Choose layer<", fixed = TRUE)
  expect_match(rendered, 'data-value="Pipeline"', fixed = TRUE)
  expect_match(rendered, 'data-value="Aesthetics"', fixed = TRUE)
  expect_no_match(rendered, ">Layer<")
  expect_no_match(rendered, 'data-value="Data"')
  expect_no_match(rendered, 'data-value="Controls"')
})

test_that("layer switcher namespaces the picker and tabset ids via ns_fn", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var)) + geom_point()"
  )
  rendered <- ui_str(ptr_get_layer_switcher_ui(obj, ns_fn = shiny::NS("modA")))

  expect_match(rendered, 'id="modA-ptr_layer_select"', fixed = TRUE)
  expect_match(rendered, 'id="modA-ptr_layer_tabset"', fixed = TRUE)
})

test_that("layer switcher emits the inline opacity CSS for ptr-layer-disabled", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var)) + geom_point()"
  )
  rendered <- ui_str(ptr_get_layer_switcher_ui(obj))
  expect_match(rendered, ".ptr-layer-disabled", fixed = TRUE)
  expect_match(rendered, "opacity:", fixed = TRUE)
})
