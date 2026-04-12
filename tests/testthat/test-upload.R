test_that("upload helpers read csv and rds fixtures", {
  csv_input <- mock_upload_input(fixture_path("simple_numeric.csv"), "simple numeric.csv")
  rds_input <- mock_upload_input(fixture_path("simple_numeric.rds"), "simple_numeric.rds")

  csv_data <- ptr_read_uploaded_data(csv_input)
  rds_data <- ptr_read_uploaded_data(rds_input)

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

  normalized_rds <- ptr_read_uploaded_data(
    mock_upload_input(spaced_path, "spaced columns.rds")
  )
  coerced_list <- ptr_read_uploaded_data(
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
  info_default <- ptr_resolve_upload_info(input_default, "ggplot+2")

  input_custom <- list(
    "ggplot+2" = mock_upload_input(fixture_path("simple_numeric.csv"), "simple numeric.csv"),
    "ggplot+2+name" = "custom dataset"
  )
  info_custom <- ptr_resolve_upload_info(input_custom, "ggplot+2")

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
    ptr_resolve_upload_info(input_bad, "ggplot+2"),
    "Uploaded data is not usable as tabular data for ggpaintr"
  )
})

test_that("unsupported upload extensions error clearly", {
  input_bad <- list(
    "ggplot+2" = mock_upload_input(fixture_path("bad_extension.txt"), "bad_extension.txt"),
    "ggplot+2+name" = ""
  )

  expect_error(
    ptr_resolve_upload_info(input_bad, "ggplot+2"),
    "Please upload a .csv or .rds file."
  )
})

test_that("register_var_ui_outputs waits for uploaded data and populates choices after upload", {
  obj <- ptr_parse_formula(
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

  obj <- ptr_parse_formula(
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
  obj <- ptr_parse_formula(
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

# --- ptr_resolve_upload_expr ---------------------------------------------

test_that("ptr_resolve_upload_expr returns a symbol for a valid name", {
  result <- ptr_resolve_upload_expr("my_data", list(), list())
  expect_true(is.symbol(result))
  expect_equal(rlang::as_string(result), "my_data")
})

test_that("ptr_resolve_upload_expr returns a symbol for a name starting with dot", {
  result <- ptr_resolve_upload_expr(".data", list(), list())
  expect_true(is.symbol(result))
  expect_equal(rlang::as_string(result), ".data")
})

test_that("ptr_resolve_upload_expr aborts on name starting with digit", {
  expect_error(
    ptr_resolve_upload_expr("123bad", list(), list()),
    "invalid object name"
  )
})

test_that("ptr_resolve_upload_expr aborts on name with spaces", {
  expect_error(
    ptr_resolve_upload_expr("has space", list(), list()),
    "invalid object name"
  )
})

test_that("ptr_resolve_upload_expr aborts on injection attempt with semicolon", {
  expect_error(
    ptr_resolve_upload_expr("x; system('bad')", list(), list()),
    "invalid object name"
  )
})

test_that("ptr_resolve_upload_expr returns ptr_missing_expr for empty string", {
  result <- ptr_resolve_upload_expr("", list(), list())
  expect_s3_class(result, "ptr_missing_expr")
})

test_that("ptr_resolve_upload_expr returns ptr_missing_expr for NULL", {
  result <- ptr_resolve_upload_expr(NULL, list(), list())
  expect_s3_class(result, "ptr_missing_expr")
})

test_that("bad upload does not crash the app session", {
  formula <- "ggplot(data = upload, aes(x = var, y = var)) + geom_point()"

  server_wrapper <- function(input, output, session) {
    session$userData$paintr_state <- ptr_server(
      input, output, session, formula
    )
  }

  suppressWarnings(shiny::testServer(server_wrapper, {
    session$setInputs(
      "ggplot+2" = mock_upload_input(fixture_path("bad_extension.txt"), "bad_extension.txt"),
      "ggplot+2+name" = "",
      draw = 1
    )

    runtime_result <- session$userData$paintr_state$runtime()
    expect_false(runtime_result$ok)
    expect_match(runtime_result$message, "Input error", fixed = TRUE)
  }))
})

# --- F2: BOM stripping -------------------------------------------------------

test_that("F2: UTF-8 BOM is stripped from CSV column names", {
  bom_file <- withr::local_tempfile(fileext = ".csv")
  # Write BOM + CSV content as raw bytes
  bom <- as.raw(c(0xEF, 0xBB, 0xBF))
  csv_bytes <- chartr("\n", "\n", "a,b\n1,2\n3,4")
  writeBin(c(bom, charToRaw(csv_bytes)), bom_file)

  file_info <- mock_upload_input(bom_file, "bom_data.csv")
  result <- suppressWarnings(ptr_read_uploaded_data(file_info))

  expect_identical(names(result)[1], "a")
})

test_that("F2: plain UTF-8 CSV (no BOM) reads correctly", {
  plain_file <- withr::local_tempfile(fileext = ".csv")
  writeLines("a,b\n1,2\n3,4", plain_file)

  file_info <- mock_upload_input(plain_file, "plain_data.csv")
  result <- ptr_read_uploaded_data(file_info)

  expect_identical(names(result), c("a", "b"))
})

# --- F3: empty-file guards ---------------------------------------------------

test_that("F3: CSV with header only (no data rows) errors 'contains no rows'", {
  header_only <- withr::local_tempfile(fileext = ".csv")
  writeLines("a,b,c", header_only)

  file_info <- mock_upload_input(header_only, "header_only.csv")
  expect_error(ptr_read_uploaded_data(file_info), "contains no rows")
})

test_that("F3: completely empty CSV errors with a readable message", {
  empty_file <- withr::local_tempfile(fileext = ".csv")
  writeLines("", empty_file)

  file_info <- mock_upload_input(empty_file, "empty.csv")
  # read.csv signals "no lines available" before row/column guards run,
  # so the tryCatch converts it to "Could not read ... as a csv file"
  expect_error(
    ptr_read_uploaded_data(file_info),
    "contains no rows|contains no columns|Could not read.*as a csv file"
  )
})

test_that("F3: RDS zero-row data.frame errors 'contains no rows'", {
  rds_file <- withr::local_tempfile(fileext = ".rds")
  saveRDS(data.frame(a = integer(0)), rds_file)

  file_info <- mock_upload_input(rds_file, "zero_rows.rds")
  expect_error(ptr_read_uploaded_data(file_info), "contains no rows")
})

test_that("F3: RDS zero-column data.frame errors 'contains no columns'", {
  rds_file <- withr::local_tempfile(fileext = ".rds")
  saveRDS(data.frame(row.names = 1:3), rds_file)

  file_info <- mock_upload_input(rds_file, "zero_cols.rds")
  expect_error(ptr_read_uploaded_data(file_info), "contains no columns")
})

test_that("F3: RDS zero-row matrix errors 'contains no rows'", {
  rds_file <- withr::local_tempfile(fileext = ".rds")
  saveRDS(matrix(numeric(0), nrow = 0, ncol = 2), rds_file)

  file_info <- mock_upload_input(rds_file, "zero_row_matrix.rds")
  expect_error(ptr_read_uploaded_data(file_info), "contains no rows")
})

# --- F4: reader errors -------------------------------------------------------

test_that("F4: RDS file with .csv extension errors on bad content", {
  rds_as_csv <- withr::local_tempfile(fileext = ".csv")
  saveRDS(data.frame(x = 1:3), rds_as_csv)

  # read.csv parses RDS bytes as text and yields 0 rows, triggering the
  # empty-data guard rather than a parse error — either message is acceptable.
  file_info <- mock_upload_input(rds_as_csv, "actually_rds.csv")
  suppressWarnings(expect_error(
    ptr_read_uploaded_data(file_info),
    "contains no rows|Could not read.*as a csv file"
  ))
})

test_that("F4: CSV file with .rds extension errors 'Could not read ... as an RDS file'", {
  csv_as_rds <- withr::local_tempfile(fileext = ".rds")
  writeLines("a,b\n1,2", csv_as_rds)

  file_info <- mock_upload_input(csv_as_rds, "actually_csv.rds")
  expect_error(
    ptr_read_uploaded_data(file_info),
    "Could not read.*as an RDS file"
  )
})

# --- F5: reserved-word default names -----------------------------------------

test_that("F5: 'if.csv' produces default name 'if_'", {
  expect_equal(ptr_upload_default_name("if.csv"), "if_")
})

test_that("F5: 'for.csv' produces default name 'for_'", {
  expect_equal(ptr_upload_default_name("for.csv"), "for_")
})

test_that("F5: 'NULL.csv' produces default name 'NULL_'", {
  expect_equal(ptr_upload_default_name("NULL.csv"), "NULL_")
})

test_that("F5: 'normal.csv' produces default name 'normal' (happy path)", {
  expect_equal(ptr_upload_default_name("normal.csv"), "normal")
})
