test_that("upload helpers read csv and rds fixtures", {
  csv_input <- mock_upload_input(fixture_path("simple_numeric.csv"), "simple numeric.csv")
  rds_input <- mock_upload_input(fixture_path("simple_numeric.rds"), "simple_numeric.rds")

  csv_data <- paintr_read_uploaded_data(csv_input)
  rds_data <- paintr_read_uploaded_data(rds_input)

  expect_s3_class(csv_data, "data.frame")
  expect_s3_class(rds_data, "data.frame")
  expect_identical(names(csv_data), c("x", "y", "group"))
  expect_identical(names(rds_data), c("x", "y", "group"))
})

test_that("upload metadata uses custom names or normalized file names", {
  input_default <- list(
    "ggplot+2" = mock_upload_input(fixture_path("simple_numeric.csv"), "simple numeric.csv"),
    "ggplot+2+name" = ""
  )
  info_default <- paintr_resolve_upload_info(input_default, "ggplot+2")

  input_custom <- list(
    "ggplot+2" = mock_upload_input(fixture_path("simple_numeric.csv"), "simple numeric.csv"),
    "ggplot+2+name" = "custom dataset"
  )
  info_custom <- paintr_resolve_upload_info(input_custom, "ggplot+2")

  expect_identical(info_default$object_name, "simple_numeric")
  expect_identical(info_custom$object_name, "custom_dataset")
  expect_match(info_default$code_text, 'simple_numeric <- read.csv\\("simple numeric.csv"\\)')
})

test_that("unsupported upload extensions error clearly", {
  input_bad <- list(
    "ggplot+2" = mock_upload_input(fixture_path("bad_extension.txt"), "bad_extension.txt"),
    "ggplot+2+name" = ""
  )

  expect_error(
    paintr_resolve_upload_info(input_bad, "ggplot+2"),
    "Please upload a .csv or .rds file."
  )
})

test_that("output_embed_var waits for uploaded data and populates choices after upload", {
  obj <- paintr_formula(
    "ggplot(data = upload, aes(x = var, y = var)) + geom_point()"
  )

  output <- list2env(list(), parent = emptyenv())

  before_upload <- output_embed_var(list(), output, obj)
  expect_length(before_upload, 0)

  input_after <- list(
    "ggplot+2" = mock_upload_input(fixture_path("simple_numeric.csv"), "simple_numeric.csv"),
    "ggplot+2+name" = "",
    "ggplot+3+2" = "x",
    "ggplot+3+3" = "y"
  )
  after_upload <- output_embed_var(input_after, output, obj)

  expect_named(after_upload, c("ggplot+3+2", "ggplot+3+3"))
})
