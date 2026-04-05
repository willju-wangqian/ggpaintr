test_that("generate_shiny writes a syntactically valid app script", {
  obj <- paintr_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )
  out_file <- tempfile(fileext = ".R")

  generate_shiny(obj, list(), out_file, style = FALSE)

  expect_true(file.exists(out_file))
  expect_no_error(parse(file = out_file))

  app_text <- paste(readLines(out_file), collapse = "\n")
  expect_match(app_text, "input_formula <- ")
  expect_match(app_text, "ggpaintr:::paintr_formula", fixed = TRUE)
  expect_match(app_text, "ggpaintr:::paintr_complete_expr", fixed = TRUE)
})

test_that("generate_shiny preserves upload-aware runtime code", {
  obj <- paintr_formula(
    "ggplot(data = upload, aes(x = var, y = var)) + geom_point()"
  )
  out_file <- tempfile(fileext = ".R")

  generate_shiny(obj, list(), out_file, style = FALSE)

  app_text <- paste(readLines(out_file), collapse = "\n")
  expect_match(app_text, "ggplot\\(data = upload")
  expect_match(app_text, "ggpaintr:::output_embed_var", fixed = TRUE)
})
