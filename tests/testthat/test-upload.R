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
    "ggplot_2" = mock_upload_input(fixture_path("simple_numeric.csv"), "simple numeric.csv"),
    "ggplot_2_shortcut" = ""
  )
  info_default <- ptr_resolve_upload_info(input_default, "ggplot_2")

  input_custom <- list(
    "ggplot_2" = mock_upload_input(fixture_path("simple_numeric.csv"), "simple numeric.csv"),
    "ggplot_2_shortcut" = "custom dataset"
  )
  info_custom <- ptr_resolve_upload_info(input_custom, "ggplot_2")

  expect_identical(info_default$object_name, "simple_numeric")
  expect_identical(info_custom$object_name, "custom_dataset")
  expect_match(info_default$code_text, 'simple_numeric <- read.csv\\("simple numeric.csv"\\)')
})

test_that("non-coercible uploads fail with a tabular-data validation error", {
  non_coercible_path <- tempfile(fileext = ".rds")
  saveRDS(function(x) x, non_coercible_path)

  input_bad <- list(
    "ggplot_2" = mock_upload_input(non_coercible_path, "non_coercible.rds"),
    "ggplot_2_shortcut" = ""
  )

  expect_error(
    ptr_resolve_upload_info(input_bad, "ggplot_2"),
    "Uploaded data is not usable as tabular data for ggpaintr"
  )
})

test_that("unsupported upload extensions error clearly", {
  input_bad <- list(
    "ggplot_2" = mock_upload_input(fixture_path("bad_extension.txt"), "bad_extension.txt"),
    "ggplot_2_shortcut" = ""
  )

  expect_error(
    ptr_resolve_upload_info(input_bad, "ggplot_2"),
    "Please upload a .csv, .tsv, .rds, .xlsx, .xls, or .json file."
  )
})

# --- TSV / Excel / JSON readers ---------------------------------------------

test_that("TSV uploads parse with header and rows", {
  tsv_file <- withr::local_tempfile(fileext = ".tsv")
  writeLines(c("a\tb\tgroup", "1\t2\tx", "3\t4\ty"), tsv_file)

  result <- ptr_read_uploaded_data(mock_upload_input(tsv_file, "data.tsv"))

  expect_s3_class(result, "data.frame")
  expect_identical(names(result), c("a", "b", "group"))
  expect_equal(nrow(result), 2L)
})

test_that("TSV upload code_text uses read.delim", {
  tsv_file <- withr::local_tempfile(fileext = ".tsv")
  writeLines(c("a\tb", "1\t2"), tsv_file)

  input <- list(
    "ggplot_2" = mock_upload_input(tsv_file, "data.tsv"),
    "ggplot_2_shortcut" = ""
  )
  info <- ptr_resolve_upload_info(input, "ggplot_2")

  expect_match(info$code_text, 'data <- read.delim\\("data.tsv"\\)')
})

test_that("Excel uploads parse and normalize column names", {
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")

  xlsx_file <- withr::local_tempfile(fileext = ".xlsx")
  df <- data.frame(check.names = FALSE,
                   `first column` = c(1, 2),
                   `second-column` = c("A", "B"))
  names(df) <- c("first column", "second-column")
  writexl::write_xlsx(df, xlsx_file)

  result <- ptr_read_uploaded_data(mock_upload_input(xlsx_file, "spaced columns.xlsx"))

  expect_s3_class(result, "data.frame")
  expect_identical(names(result), c("first_column", "second_column"))
  expect_equal(nrow(result), 2L)
})

test_that("Excel upload code_text uses readxl::read_excel", {
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")

  xlsx_file <- withr::local_tempfile(fileext = ".xlsx")
  writexl::write_xlsx(data.frame(a = 1:2, b = 3:4), xlsx_file)

  input <- list(
    "ggplot_2" = mock_upload_input(xlsx_file, "report.xlsx"),
    "ggplot_2_shortcut" = ""
  )
  info <- ptr_resolve_upload_info(input, "ggplot_2")

  expect_match(info$code_text, 'report <- readxl::read_excel\\("report.xlsx"\\)')
})

test_that("Excel upload errors clearly when readxl is missing", {
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ...) !identical(package, "readxl"),
    .package = "base"
  )

  xlsx_file <- withr::local_tempfile(fileext = ".xlsx")
  writeLines("not really xlsx", xlsx_file)

  expect_error(
    ptr_read_uploaded_data(mock_upload_input(xlsx_file, "missing_dep.xlsx")),
    "requires the 'readxl' package"
  )
})

test_that("JSON array-of-records parses and normalizes column names", {
  skip_if_not_installed("jsonlite")

  json_file <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(
    data.frame(check.names = FALSE,
               `first column` = c(1, 2),
               `second-column` = c("A", "B")) |>
      setNames(c("first column", "second-column")),
    json_file
  )

  result <- ptr_read_uploaded_data(mock_upload_input(json_file, "records.json"))

  expect_s3_class(result, "data.frame")
  expect_identical(names(result), c("first_column", "second_column"))
  expect_equal(nrow(result), 2L)
})

test_that("JSON nested objects are flattened", {
  skip_if_not_installed("jsonlite")

  json_file <- withr::local_tempfile(fileext = ".json")
  writeLines(
    '[{"a":1,"b":{"x":10,"y":20}},{"a":2,"b":{"x":11,"y":21}}]',
    json_file
  )

  result <- ptr_read_uploaded_data(mock_upload_input(json_file, "nested.json"))

  expect_s3_class(result, "data.frame")
  expect_identical(sort(names(result)), c("a", "b.x", "b.y"))
})

test_that("JSON nested arrays produce a clear error", {
  skip_if_not_installed("jsonlite")

  json_file <- withr::local_tempfile(fileext = ".json")
  writeLines(
    '[{"a":1,"tags":["r","shiny"]},{"a":2,"tags":["viz"]}]',
    json_file
  )

  expect_error(
    ptr_read_uploaded_data(mock_upload_input(json_file, "nested_arrays.json")),
    "nested array/object columns"
  )
})

test_that("JSON top-level object (not array) produces a clear error", {
  skip_if_not_installed("jsonlite")

  json_file <- withr::local_tempfile(fileext = ".json")
  writeLines('{"a": 1, "b": 2}', json_file)

  expect_error(
    ptr_read_uploaded_data(mock_upload_input(json_file, "object.json")),
    "must be an array of objects"
  )
})

test_that("JSON upload code_text uses jsonlite::fromJSON", {
  skip_if_not_installed("jsonlite")

  json_file <- withr::local_tempfile(fileext = ".json")
  writeLines('[{"a":1,"b":2}]', json_file)

  input <- list(
    "ggplot_2" = mock_upload_input(json_file, "records.json"),
    "ggplot_2_shortcut" = ""
  )
  info <- ptr_resolve_upload_info(input, "ggplot_2")

  expect_match(info$code_text, 'records <- jsonlite::fromJSON\\("records.json"\\)')
})

test_that("JSON upload errors clearly when jsonlite is missing", {
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ...) !identical(package, "jsonlite"),
    .package = "base"
  )

  json_file <- withr::local_tempfile(fileext = ".json")
  writeLines('[{"a":1}]', json_file)

  expect_error(
    ptr_read_uploaded_data(mock_upload_input(json_file, "missing_dep.json")),
    "requires the 'jsonlite' package"
  )
})




# --- ptr_resolve_upload_expr ---------------------------------------------









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

test_that("ptr_upload_autoname derives a default only when the companion is blank", {
  # Drives the upload observer in ptr_setup_pipelines(): a blank
  # dataset-name companion is auto-filled from the uploaded filename so
  # the code panel can render `data = <name>`; a name the user typed is
  # never overwritten.
  expect_equal(ptr_upload_autoname(NULL, "my_penguins.csv"), "my_penguins")
  expect_equal(ptr_upload_autoname("", "data set.tsv"), "data_set")
  expect_null(ptr_upload_autoname("keep_me", "other.csv"))
  expect_null(ptr_upload_autoname(NULL, NULL))
  expect_null(ptr_upload_autoname(NULL, ""))
  expect_null(ptr_upload_autoname(NULL, character(0)))
})

# --- Phase 1.5 / 1.6: upload widget accept filter + copy-driven labels -------

.upload_node_from <- function(formula = "ppUpload |> ggplot()") {
  tree <- ptr_translate(formula)
  hits <- find_nodes(tree, function(n) {
    is_ptr_placeholder(n) && identical(n$keyword, "ppUpload")
  })
  hits[[1L]]
}

test_that("ppUpload build_ui restores the accept filter on the file input", {
  node <- .upload_node_from()
  ui <- .source_widget(node)
  rendered <- as.character(ui)
  expect_match(rendered, 'type="file"')
  expect_match(rendered, "accept=\".csv,.tsv,.rds,.xlsx,.xls,.json\"", fixed = TRUE)
})

test_that("upload labels/help are driven by upload_file / upload_name copy", {
  node <- .upload_node_from()
  ui <- .source_widget(
    node,
    ui_text = list(upload = list(
      file = list(label = "Pick your CSV", help = "file help here"),
      name = list(label = "Name it", placeholder = "eg sales", help = "name help here")
    ))
  )
  rendered <- as.character(ui)
  expect_match(rendered, "Pick your CSV")
  expect_match(rendered, "file help here")
  expect_match(rendered, "Name it")
  expect_match(rendered, 'placeholder="eg sales"', fixed = TRUE)
  expect_match(rendered, "name help here")
})

test_that("upload uses the resolved default copy when no override is given", {
  node <- .upload_node_from()
  ui <- .source_widget(node)
  rendered <- as.character(ui)
  expect_match(rendered, "Choose a data file")
  expect_match(rendered, "Optional dataset name")
})

test_that("the removed shell$update_data_button copy key is still rejected", {
  expect_error(
    ptr_validate_ui_text(list(shell = list(update_data_button = list(label = "x")))),
    "unsupported"
  )
})
