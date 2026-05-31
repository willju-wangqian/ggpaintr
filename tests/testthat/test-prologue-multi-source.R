# test-prologue-multi-source.R -- shinytest2 driver for ADR 0025 §4 /
# PLAN-04. Worked observable: a formula with two bare-data ppUpload sources
# (`geom_point(data = ppUpload(), ...) + geom_line(data = ppUpload(), ...)`)
# emits exactly two prologue lines, one per active source, in declaration
# (formula) order.
#
# (The ADR example mentions a 2-formula `ptr_app_grid` as one
# multi-source shape; this fixture exercises the per-state ordering
# claim directly with two bare-data sources sharing one code panel.
# The two-coordinator shape is covered by
# `test-prologue-two-coordinators-shared-key.R`.)

test_that("multi-source app emits one prologue line per active upload in declaration order", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")

  app <- boot_vignette_app("prologue-multi-source")

  point_id <- "geom_point_0_ppUpload_NA"
  line_id  <- "geom_line_0_ppUpload_NA"
  expect_dom_id(app, point_id)
  expect_dom_id(app, line_id)

  csv_path <- testthat::test_path("fixtures", "mtcars.csv")
  tsv_path <- testthat::test_path("fixtures", "mtcars.tsv")
  upload_file(app, geom_point_0_ppUpload_NA = csv_path)
  app$wait_for_idle(timeout = 15 * 1000)
  upload_file(app, geom_line_0_ppUpload_NA = tsv_path)
  app$wait_for_idle(timeout = 15 * 1000)
  draw(app, "ptr_update_plot")

  code_text <- app$get_value(output = "ptr_code") %||% ""
  expect_true(nzchar(code_text), label = "code panel text is non-empty")

  lines <- strsplit(code_text, "\n", fixed = TRUE)[[1]]
  prologue_lines <- grep("^[A-Za-z0-9_.]+ <- (read\\.|readRDS|readxl::)", lines, value = TRUE)
  expect_equal(length(prologue_lines), 2L,
               label = "exactly two prologue lines")

  # Declaration order: f1 = geom_point (point source) first. ADR 0025 §3:
  # an empty-textbox upload binds under the system auto-name
  # `df_<hash(node$id)>`, so the prologue LHS is that symbol (not the raw
  # node id). Computed in-test the same way paintr-ids.R::ptr_hash() does.
  pt_auto <- paste0("df_", substr(rlang::hash("geom_point_0_ppUpload_NA"), 1L, 6L))
  ln_auto <- paste0("df_", substr(rlang::hash("geom_line_0_ppUpload_NA"), 1L, 6L))
  expect_match(prologue_lines[[1]],
               paste0("^", pt_auto, " <- read\\.csv\\(\"mtcars\\.csv\"\\)$"),
               label = "first prologue line is the geom_point source (declaration order)")
  expect_match(prologue_lines[[2]],
               paste0("^", ln_auto, " <- read\\.delim\\(\"mtcars\\.tsv\"\\)$"),
               label = "second prologue line is the geom_line source")
})

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a
