ui_text <- function(ui) {
  paste(as.character(ui), collapse = "\n")
}

test_that("copy rules validate supported sections and leaf fields", {
  expect_error(
    ptr_validate_ui_text(list(bad_section = list())),
    "unsupported top-level sections"
  )

  expect_error(
    ptr_validate_ui_text(list(shell = list(title = list(bad_field = "nope")))),
    "unsupported fields"
  )
})

test_that("copy rules normalize aliases and merge precedence field by field", {
  rules <- ptr_merge_ui_text(
    list(
      defaults = list(var = list(empty_text = "Pick one column")),
      params = list(colour = list(var = list(label = "Choose a colour column"))),
      layers = list(
        ggplot = list(
          var = list(
            color = list(label = "Choose a layer-specific color column")
          )
        )
      )
    )
  )

  color_copy <- ptr_resolve_ui_text(
    "control",
    keyword = "var",
    layer_name = "ggplot",
    param = "colour",
    ui_text = rules
  )

  expect_identical(color_copy$label, "Choose a layer-specific color column")
  expect_identical(color_copy$empty_text, "Pick one column")
})

test_that("copy rule compaction keeps only custom diffs with canonical keys", {
  compact_rules <- ptr_compact_ui_text(
    list(
      shell = list(
        title = list(label = "Exploratory Plot Builder")
      ),
      params = list(
        colour = list(var = list(label = "Choose a colour column"))
      ),
      layers = list(
        facet_wrap = list(
          expr = list(
            `__unnamed__` = list(label = "Split the plot by")
          )
        )
      )
    )
  )

  expect_identical(
    compact_rules,
    list(
      shell = list(
        title = list(label = "Exploratory Plot Builder")
      ),
      params = list(
        color = list(var = list(label = "Choose a colour column"))
      ),
      layers = list(
        facet_wrap = list(
          expr = list(
            `__unnamed__` = list(label = "Split the plot by")
          )
        )
      )
    )
  )
})

test_that("copy rule compaction collapses default-equivalent overrides", {
  expect_null(
    ptr_compact_ui_text(
      list(
        shell = list(
          title = list(label = "ggpaintr Plot Builder")
        )
      )
    )
  )
})

test_that("copy rules provide readable fallbacks and seeded defaults", {
  fallback_copy <- ptr_resolve_ui_text(
    "control",
    keyword = "num",
    layer_name = "geom_histogram",
    param = "bin_size"
  )
  alpha_copy <- ptr_resolve_ui_text(
    "control",
    keyword = "num",
    layer_name = "geom_point",
    param = "alpha"
  )
  facet_copy <- ptr_resolve_ui_text(
    "control",
    keyword = "expr",
    layer_name = "facet_wrap",
    param = list(NULL)
  )

  expect_identical(fallback_copy$label, "Enter a number for bin size")
  expect_identical(alpha_copy$label, "Transparency")
  expect_match(alpha_copy$help, "0 and 1", fixed = TRUE)
  expect_identical(facet_copy$label, "Facet by")
  expect_identical(facet_copy$placeholder, "~ Species")
})

test_that("app shell copy uses defaults and runtime overrides", {
  default_components <- ptr_app_components(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )
  default_text <- ui_text(default_components$ui)

  expect_match(default_text, "ggpaintr Plot Builder", fixed = TRUE)
  expect_match(default_text, "Update plot", fixed = TRUE)

  custom_components <- ptr_app_components(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()",
    ui_text = list(
      shell = list(
        title = list(label = "Exploratory Plot Builder"),
        draw_button = list(label = "Render plot")
      )
    )
  )
  custom_text <- ui_text(custom_components$ui)

  expect_match(custom_text, "Exploratory Plot Builder", fixed = TRUE)
  expect_match(custom_text, "Render plot", fixed = TRUE)
})

test_that("ui builders use resolved copy for uploads and common controls", {
  upload_text <- ui_text(generate_ui_upload("ggplot_2"))
  var_text <- ui_text(generate_ui_var(c("x", "y"), "ggplot_3_2", "x", layer_name = "ggplot"))
  text_input <- ui_text(generate_ui_text("labs_2", "title", layer_name = "labs"))
  num_input <- ui_text(generate_ui_num("geom_point_2", "alpha", layer_name = "geom_point"))
  expr_input <- ui_text(generate_ui_expr("facet_grid_2", list(NULL), layer_name = "facet_grid"))
  checkbox <- ptr_layer_checkbox_tag("geom_point")

  expect_match(upload_text, "Choose a data file", fixed = TRUE)
  expect_match(upload_text, "Optional dataset name", fixed = TRUE)
  expect_match(upload_text, "sales_data", fixed = TRUE)
  expect_match(upload_text, "Accepted formats: .csv, .tsv, .rds, .xlsx, .xls, .json.", fixed = TRUE)

  expect_match(var_text, "Choose the x-axis column", fixed = TRUE)
  expect_match(var_text, "Choose one column", fixed = TRUE)

  expect_match(text_input, "Plot title", fixed = TRUE)

  expect_match(num_input, "Transparency", fixed = TRUE)
  expect_match(num_input, "0 and 1", fixed = TRUE)

  expect_match(expr_input, "Facet layout", fixed = TRUE)
  expect_match(expr_input, "Species ~ .", fixed = TRUE)
  expect_no_match(expr_input, "argument 1")

  expect_match(ui_text(checkbox), "Include this layer in the plot", fixed = TRUE)
})

test_that("tab UI does not expose parser-style unnamed argument labels", {
  obj <- ptr_parse_formula(
    "ggplot(data = iris, aes(x = var, y = var)) + facet_wrap(expr)"
  )

  tab_text <- ui_text(ptr_get_layer_switcher_ui(obj))

  expect_match(tab_text, "Facet by", fixed = TRUE)
  expect_no_match(tab_text, "argument 1")
})

test_that("ptr_resolve_ui_text resolves all mapped components", {
  components <- names(ptr_ui_text_component_paths())
  for (comp in components) {
    result <- ptr_resolve_ui_text(comp)
    expect_type(result, "list")
    expect_true("label" %in% names(result))
  }
})

test_that("ptr_resolve_ui_text errors on unknown component", {
  expect_error(
    ptr_resolve_ui_text("nonexistent_component"),
    "Unknown copy component"
  )
})

test_that("ptr_ui_text_component_paths keys are exhaustive", {
  paths <- ptr_ui_text_component_paths()
  expected <- c(
    "title", "draw_button", "update_data_button",
    "layer_picker", "data_subtab", "controls_subtab",
    "upload_file", "upload_name", "layer_checkbox"
  )
  expect_setequal(names(paths), expected)
})

test_that("ptr_default_ui_text exposes update_data_button copy under shell", {
  defaults <- ptr_default_ui_text()
  expect_identical(defaults$shell$update_data_button$label, "Update data")
})

test_that("ptr_resolve_ui_text resolves update_data_button label", {
  default_copy <- ptr_resolve_ui_text("update_data_button")
  expect_identical(default_copy$label, "Update data")

  custom_copy <- ptr_resolve_ui_text(
    "update_data_button",
    ui_text = list(
      shell = list(update_data_button = list(label = "Refresh dataset"))
    )
  )
  expect_identical(custom_copy$label, "Refresh dataset")
})

# --- Improvements: alias, warn-on-unknown, generic label ---

test_that("alias: size resolves to same copy as linewidth, both yield label 'Size'", {
  size_copy <- ptr_resolve_ui_text(
    "control",
    keyword = "num",
    param = "size"
  )
  linewidth_copy <- ptr_resolve_ui_text(
    "control",
    keyword = "num",
    param = "linewidth"
  )

  expect_equal(size_copy$label, "Size")
  expect_equal(linewidth_copy$label, "Size")
  expect_identical(size_copy, linewidth_copy)
})

test_that("alias: size resolves to same var copy as linewidth", {
  size_var <- ptr_resolve_ui_text(
    "control",
    keyword = "var",
    param = "size"
  )
  linewidth_var <- ptr_resolve_ui_text(
    "control",
    keyword = "var",
    param = "linewidth"
  )

  expect_equal(size_var$label, "Choose the size column")
  expect_identical(size_var, linewidth_var)
})

test_that("linewidth$num label is the generic 'Size'", {
  defaults <- ptr_default_ui_text()
  expect_equal(defaults$params$linewidth$num$label, "Size")
})

test_that("warn-on-unknown fires when misspelled param key supplied with known_param_keys", {
  expect_warning(
    ptr_merge_ui_text(
      ui_text = list(params = list(colur = list(var = list(label = "X")))),
      known_param_keys = c("color", "x", "y")
    ),
    regexp = "colur"
  )
})

test_that("warn-on-unknown does NOT fire when known_param_keys = NULL (back-compat)", {
  expect_no_warning(
    ptr_merge_ui_text(
      ui_text = list(params = list(colur = list(var = list(label = "X"))))
    )
  )
})

test_that("warn-on-unknown does NOT fire when known_param_keys is empty but no params supplied", {
  expect_no_warning(
    ptr_merge_ui_text(
      ui_text = list(shell = list(title = list(label = "My App"))),
      known_param_keys = c("color", "x", "y")
    )
  )
})

test_that("alias normalization: user-supplied 'size' key does NOT warn when known_param_keys = c('linewidth')", {
  # size normalizes to linewidth before the unknown-key check, so no warning
  expect_no_warning(
    ptr_merge_ui_text(
      ui_text = list(params = list(size = list(num = list(label = "My size")))),
      known_param_keys = c("linewidth", "x", "y")
    )
  )
})
