test_that("ptr_runtime_input_spec returns placeholder and checkbox rows in order", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point() + labs(title = text)"
  )

  spec <- ptr_runtime_input_spec(obj)

  expect_s3_class(spec, "data.frame")
  expect_identical(
    names(spec),
    c("input_id", "role", "layer_name", "keyword", "param_key", "source_id")
  )
  expect_identical(
    spec$input_id,
    c(
      "ggplot+3+2",
      "ggplot+3+3",
      "labs+2",
      "geom_point+checkbox",
      "labs+checkbox"
    )
  )
  expect_identical(
    spec$role,
    c("placeholder", "placeholder", "placeholder", "layer_checkbox", "layer_checkbox")
  )
  expect_identical(
    spec$layer_name,
    c("ggplot", "ggplot", "labs", "geom_point", "labs")
  )
  expect_identical(
    spec$keyword,
    c("var", "var", "text", NA_character_, NA_character_)
  )
  expect_identical(
    spec$param_key,
    c("x", "y", "title", NA_character_, NA_character_)
  )
  expect_identical(
    spec$source_id,
    c("ggplot+3+2", "ggplot+3+3", "labs+2", NA_character_, NA_character_)
  )
})

test_that("ptr_runtime_input_spec includes derived upload name inputs", {
  obj <- ptr_parse_formula(
    "ggplot(data = upload, aes(x = var, y = var)) + geom_point()"
  )

  spec <- ptr_runtime_input_spec(obj)

  expect_identical(
    spec$input_id,
    c(
      "ggplot+2",
      "ggplot+2+name",
      "ggplot+3+2",
      "ggplot+3+3",
      "geom_point+checkbox"
    )
  )
  expect_identical(
    spec$role,
    c("placeholder", "upload_name", "placeholder", "placeholder", "layer_checkbox")
  )
  expect_identical(
    spec$keyword,
    c("upload", "upload", "var", "var", NA_character_)
  )
  expect_identical(
    spec$source_id,
    c("ggplot+2", "ggplot+2", "ggplot+3+2", "ggplot+3+3", NA_character_)
  )
})

test_that("ptr_runtime_input_spec preserves resolved duplicate layer names", {
  obj <- ptr_parse_formula(
    paste(
      "ggplot(data = mtcars, aes(x = mpg, y = disp)) +",
      "geom_point(color = text) +",
      "geom_point(size = num)"
    )
  )

  spec <- ptr_runtime_input_spec(obj)

  expect_identical(
    spec$layer_name,
    c("geom_point-1", "geom_point-2", "geom_point-1", "geom_point-2")
  )
  expect_identical(
    spec$input_id,
    c(
      "geom_point-1+2",
      "geom_point-2+2",
      "geom_point-1+checkbox",
      "geom_point-2+checkbox"
    )
  )
})

test_that("ptr_runtime_input_spec surfaces custom placeholder keywords", {
  registry <- ptr_merge_placeholders(
    list(date = make_test_date_placeholder())
  )
  obj <- ptr_parse_formula(test_date_formula, placeholders = registry)

  spec <- ptr_runtime_input_spec(obj)
  date_row <- spec[which(spec$keyword %in% "date"), , drop = FALSE]

  expect_identical(nrow(date_row), 1L)
  expect_identical(date_row$role, "placeholder")
  expect_identical(date_row$layer_name, "geom_vline")
  expect_identical(date_row$param_key, "xintercept")
  expect_identical(date_row$source_id, date_row$input_id)
})
