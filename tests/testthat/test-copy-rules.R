ui_text <- function(ui) {
  paste(as.character(ui), collapse = "\n")
}

test_that("copy rules validate supported sections and leaf fields", {
  expect_error(
    paintr_validate_copy_rules(list(bad_section = list())),
    "unsupported top-level sections"
  )

  expect_error(
    paintr_validate_copy_rules(list(shell = list(title = list(bad_field = "nope")))),
    "unsupported fields"
  )
})

test_that("copy rules normalize aliases and merge precedence field by field", {
  rules <- paintr_effective_copy_rules(
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

  color_copy <- paintr_resolve_copy(
    "control",
    keyword = "var",
    layer_name = "ggplot",
    param = "colour",
    copy_rules = rules
  )

  expect_identical(color_copy$label, "Choose a layer-specific color column")
  expect_identical(color_copy$empty_text, "Pick one column")
})

test_that("copy rule compaction keeps only custom diffs with canonical keys", {
  compact_rules <- paintr_compact_copy_rules(
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
    paintr_compact_copy_rules(
      list(
        shell = list(
          title = list(label = "ggpaintr Plot Builder")
        )
      )
    )
  )
})

test_that("copy rules provide readable fallbacks and seeded defaults", {
  fallback_copy <- paintr_resolve_copy(
    "control",
    keyword = "num",
    layer_name = "geom_histogram",
    param = "bin_size"
  )
  alpha_copy <- paintr_resolve_copy(
    "control",
    keyword = "num",
    layer_name = "geom_point",
    param = "alpha"
  )
  facet_copy <- paintr_resolve_copy(
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
  default_components <- paintr_app_components(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )
  default_text <- ui_text(default_components$ui)

  expect_match(default_text, "ggpaintr Plot Builder", fixed = TRUE)
  expect_match(default_text, "Update plot", fixed = TRUE)
  expect_match(default_text, "Export Shiny app", fixed = TRUE)

  custom_components <- paintr_app_components(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()",
    copy_rules = list(
      shell = list(
        title = list(label = "Exploratory Plot Builder"),
        draw_button = list(label = "Render plot")
      )
    )
  )
  custom_text <- ui_text(custom_components$ui)

  expect_match(custom_text, "Exploratory Plot Builder", fixed = TRUE)
  expect_match(custom_text, "Render plot", fixed = TRUE)
  expect_match(custom_text, "Export Shiny app", fixed = TRUE)
})

test_that("ui builders use resolved copy for uploads and common controls", {
  upload_text <- ui_text(generate_ui_upload("ggplot+2"))
  var_text <- ui_text(generate_ui_var(c("x", "y"), "ggplot+3+2", "x", layer_name = "ggplot"))
  text_input <- ui_text(generate_ui_text("labs+2", "title", layer_name = "labs"))
  num_input <- ui_text(generate_ui_num("geom_point+2", "alpha", layer_name = "geom_point"))
  expr_input <- ui_text(generate_ui_expr("facet_grid+2", list(NULL), layer_name = "facet_grid"))
  checkbox <- ui_insert_checkbox(list(), "geom_point")[[1]]

  expect_match(upload_text, "Choose a data file", fixed = TRUE)
  expect_match(upload_text, "Optional dataset name", fixed = TRUE)
  expect_match(upload_text, "sales_data", fixed = TRUE)
  expect_match(upload_text, "Accepted formats: .csv and .rds.", fixed = TRUE)

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
  obj <- paintr_formula(
    "ggplot(data = iris, aes(x = var, y = var)) + facet_wrap(expr)"
  )

  tab_text <- ui_text(paintr_get_tab_ui(obj))

  expect_match(tab_text, "Facet by", fixed = TRUE)
  expect_no_match(tab_text, "argument 1")
})
