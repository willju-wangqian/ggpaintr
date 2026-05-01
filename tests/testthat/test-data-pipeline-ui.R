ui_text <- function(ui) {
  paste(as.character(ui), collapse = "\n")
}

test_that("data tabset renders when formula has data-pipeline placeholder", {
  obj <- ptr_parse_formula(
    "mtcars |> head(num) |> ggplot(aes(x = var, y = var)) + geom_point()"
  )

  data_tab <- ptr_get_data_tab_ui(obj)
  expect_false(is.null(data_tab))

  rendered <- ui_text(data_tab)
  expect_match(rendered, "data-value=\"ggplot\"", fixed = TRUE)

  num_id <- obj$data_pipeline_info[["ggplot"]]$placeholder_ids[[1]]
  expect_match(rendered, num_id, fixed = TRUE)

  expect_match(
    rendered,
    paste0("id=\"", ptr_update_data_input_id("ggplot"), "\""),
    fixed = TRUE
  )
  expect_match(rendered, "Update data", fixed = TRUE)
})

test_that("data tabset is absent when formula has no data pipeline", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var)) + geom_point()"
  )

  expect_null(ptr_get_data_tab_ui(obj))
})

test_that("data tabset honours custom update_data_button label via ui_text", {
  obj <- ptr_parse_formula(
    "mtcars |> head(num) |> ggplot(aes(x = var, y = var)) + geom_point()"
  )

  data_tab <- ptr_get_data_tab_ui(
    obj,
    ui_text = list(
      shell = list(update_data_button = list(label = "Refresh dataset"))
    )
  )

  rendered <- ui_text(data_tab)
  expect_match(rendered, "Refresh dataset", fixed = TRUE)
  expect_no_match(rendered, "Update data")
})

test_that("data tabset renders one sub-tab per data-pipeline layer", {
  obj <- ptr_parse_formula(paste(
    "mtcars |> head(num) |> ggplot(aes(x = var, y = var))",
    "+ geom_point(data = mtcars |> dplyr::filter(num > 0))",
    sep = " "
  ))

  expect_true(length(obj$data_pipeline_info) >= 2L)
  expect_true(all(c("ggplot", "geom_point") %in% names(obj$data_pipeline_info)))

  data_tab <- ptr_get_data_tab_ui(obj)
  rendered <- ui_text(data_tab)

  expect_match(rendered, "data-value=\"ggplot\"", fixed = TRUE)
  expect_match(rendered, "data-value=\"geom_point\"", fixed = TRUE)
  expect_match(
    rendered,
    paste0("id=\"", ptr_update_data_input_id("ggplot"), "\""),
    fixed = TRUE
  )
  expect_match(
    rendered,
    paste0("id=\"", ptr_update_data_input_id("geom_point"), "\""),
    fixed = TRUE
  )
})

# ---------------------------------------------------------------------------
# Regression: data-pipeline placeholders must NOT also be rendered in the
# regular layer-control tab. They live in the Data tab only — duplicating
# them produced Shiny "Duplicate input IDs" warnings and confused users
# typing into the wrong copy.
# ---------------------------------------------------------------------------

test_that("ptr_build_ui_list omits data-pipeline placeholders from layer-control UI", {
  obj <- ptr_parse_formula(
    "mtcars |> subset(mpg > num) |> head(num) |> ggplot(aes(x = var, y = var)) + geom_point(size = num) + geom_smooth(data = iris |> subset(Species != text))"
  )
  ui_list <- ptr_build_ui_list(obj)

  pipeline_ids <- unlist(lapply(
    obj$data_pipeline_info,
    function(info) info$placeholder_ids
  ), use.names = FALSE)
  expect_true(length(pipeline_ids) > 0L)

  # The non-pipeline `num` for `geom_point(size = num)` should still be present
  # in the geom_point control list — only pipeline-bound placeholders are skipped.
  size_ids <- setdiff(obj$id_list[["geom_point"]], pipeline_ids)
  expect_true(length(size_ids) >= 1L)

  rendered <- paste(unlist(lapply(ui_list, function(layer_ui) {
    as.character(layer_ui)
  })), collapse = "\n")

  for (id in pipeline_ids) {
    expect_false(
      grepl(id, rendered, fixed = TRUE),
      info = sprintf("Pipeline placeholder id '%s' leaked into layer-control UI", id)
    )
  }

  for (id in size_ids) {
    expect_true(
      grepl(id, rendered, fixed = TRUE),
      info = sprintf("Non-pipeline placeholder id '%s' missing from layer-control UI", id)
    )
  }
})
