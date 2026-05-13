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
  rules <- ptr_ui_text(
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
    "title", "draw_button", "draw_all_button",
    "layer_picker", "data_subtab", "controls_subtab",
    "upload_file", "upload_name", "layer_checkbox"
  )
  expect_setequal(names(paths), expected)
})

# --- Improvements: alias normalization, generic label ---

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
