test_that("upload helpers read csv and rds fixtures", {
  csv_input <- mock_upload_input(fixture_path("simple_numeric.csv"), "simple numeric.csv")
  rds_input <- mock_upload_input(fixture_path("simple_numeric.rds"), "simple_numeric.rds")

  csv_data <- ggpaintr_read_uploaded_data(csv_input)
  rds_data <- ggpaintr_read_uploaded_data(rds_input)

  expect_s3_class(csv_data, "data.frame")
  expect_s3_class(rds_data, "data.frame")
  expect_identical(names(csv_data), c("x", "y", "group"))
  expect_identical(names(rds_data), c("x", "y", "group"))
})

test_that("upload helpers normalize .rds columns and coerce list-like uploads", {
  spaced_path <- tempfile(fileext = ".rds")
  spaced_data <- data.frame(left = 1:3, right = 4:6, check.names = FALSE)
  names(spaced_data) <- c("first column", "second-column")
  saveRDS(spaced_data, spaced_path)

  normalized_rds <- ggpaintr_read_uploaded_data(
    mock_upload_input(spaced_path, "spaced columns.rds")
  )
  coerced_list <- ggpaintr_read_uploaded_data(
    mock_upload_input(fixture_path("non_tabular.rds"), "non_tabular.rds")
  )

  expect_s3_class(normalized_rds, "data.frame")
  expect_identical(names(normalized_rds), c("first_column", "second_column"))
  expect_s3_class(coerced_list, "data.frame")
  expect_identical(names(coerced_list), c("alpha", "beta"))
  expect_identical(as.list(coerced_list[1, ]), list(alpha = 1, beta = 2))
})

test_that("upload metadata uses custom names or normalized file names", {
  input_default <- list(
    "ggplot+2" = mock_upload_input(fixture_path("simple_numeric.csv"), "simple numeric.csv"),
    "ggplot+2+name" = ""
  )
  info_default <- ggpaintr_resolve_upload_info(input_default, "ggplot+2")

  input_custom <- list(
    "ggplot+2" = mock_upload_input(fixture_path("simple_numeric.csv"), "simple numeric.csv"),
    "ggplot+2+name" = "custom dataset"
  )
  info_custom <- ggpaintr_resolve_upload_info(input_custom, "ggplot+2")

  expect_identical(info_default$object_name, "simple_numeric")
  expect_identical(info_custom$object_name, "custom_dataset")
  expect_match(info_default$code_text, 'simple_numeric <- read.csv\\("simple numeric.csv"\\)')
})

test_that("non-coercible uploads fail with a tabular-data validation error", {
  non_coercible_path <- tempfile(fileext = ".rds")
  saveRDS(function(x) x, non_coercible_path)

  input_bad <- list(
    "ggplot+2" = mock_upload_input(non_coercible_path, "non_coercible.rds"),
    "ggplot+2+name" = ""
  )

  expect_error(
    ggpaintr_resolve_upload_info(input_bad, "ggplot+2"),
    "Uploaded data is not usable as tabular data for ggpaintr"
  )
})

test_that("unsupported upload extensions error clearly", {
  input_bad <- list(
    "ggplot+2" = mock_upload_input(fixture_path("bad_extension.txt"), "bad_extension.txt"),
    "ggplot+2+name" = ""
  )

  expect_error(
    ggpaintr_resolve_upload_info(input_bad, "ggplot+2"),
    "Please upload a .csv or .rds file."
  )
})

test_that("register_var_ui_outputs waits for uploaded data and populates choices after upload", {
  obj <- ggpaintr_formula(
    "ggplot(data = upload, aes(x = var, y = var)) + geom_point()"
  )

  output <- list2env(list(), parent = emptyenv())

  before_upload <- register_var_ui_outputs(list(), output, obj)
  expect_length(before_upload, 0)

  input_after <- list(
    "ggplot+2" = mock_upload_input(fixture_path("simple_numeric.csv"), "simple_numeric.csv"),
    "ggplot+2+name" = "",
    "ggplot+3+2" = "x",
    "ggplot+3+3" = "y"
  )
  after_upload <- register_var_ui_outputs(input_after, output, obj)

  expect_named(after_upload, c("ggplot+3+2", "ggplot+3+3"))
})

test_that("register_var_ui_outputs exposes normalized names for uploaded rds data", {
  spaced_path <- tempfile(fileext = ".rds")
  spaced_data <- data.frame(left = 1:3, right = 4:6, check.names = FALSE)
  names(spaced_data) <- c("first column", "second-column")
  saveRDS(spaced_data, spaced_path)

  obj <- ggpaintr_formula(
    "ggplot(data = upload, aes(x = var, y = var)) + geom_point()"
  )
  output <- list2env(list(), parent = emptyenv())
  input_after <- list(
    "ggplot+2" = mock_upload_input(spaced_path, "spaced columns.rds"),
    "ggplot+2+name" = "",
    "ggplot+3+2" = "first_column",
    "ggplot+3+3" = "second_column"
  )

  after_upload <- register_var_ui_outputs(input_after, output, obj)
  ui_text <- paste(as.character(after_upload[["ggplot+3+2"]]), collapse = "\n")

  expect_named(after_upload, c("ggplot+3+2", "ggplot+3+3"))
  expect_match(ui_text, "first_column", fixed = TRUE)
  expect_match(ui_text, "second_column", fixed = TRUE)
})

test_that("register_var_ui_outputs produces distinct widgets for each var placeholder", {
  obj <- ggpaintr_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )

  output <- list2env(list(), parent = emptyenv())
  input <- list(
    "ggplot+3+2" = "mpg",
    "ggplot+3+3" = "disp"
  )

  result <- register_var_ui_outputs(input, output, obj)

  expect_named(result, c("ggplot+3+2", "ggplot+3+3"))

  ui_x <- paste(as.character(result[["ggplot+3+2"]]), collapse = "\n")
  ui_y <- paste(as.character(result[["ggplot+3+3"]]), collapse = "\n")

  expect_match(ui_x, "x-axis", fixed = TRUE)
  expect_match(ui_y, "y-axis", fixed = TRUE)
  expect_false(identical(ui_x, ui_y))
})

test_that("bad upload does not crash the app session", {
  formula <- "ggplot(data = upload, aes(x = var, y = var)) + geom_point()"

  server_wrapper <- function(input, output, session) {
    session$userData$paintr_state <- ggpaintr_server(
      input, output, session, formula
    )
  }

  shiny::testServer(server_wrapper, {
    session$setInputs(
      "ggplot+2" = mock_upload_input(fixture_path("bad_extension.txt"), "bad_extension.txt"),
      "ggplot+2+name" = "",
      draw = 1
    )

    runtime_result <- session$userData$paintr_state$runtime()
    expect_false(runtime_result$ok)
    expect_match(runtime_result$message, "Input error", fixed = TRUE)
  })
})
