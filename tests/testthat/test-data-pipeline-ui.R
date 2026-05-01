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
