# Regression test for the bad-extension wording fix
# (manual checklist "Upload checks" item 8).
#
# Before the fix, uploading a file whose extension wasn't in the supported
# set would silently swallow `ptr_read_uploaded_data()`'s
# `unsupported upload` abort in `ptr_setup_pipelines()`'s
# `tryCatch(..., error = function(e) NULL)`. The inline error the user
# saw was the *downstream* "object '<name>' not found", produced when the
# plot tried to evaluate against a symbol that had no binding.
#
# After the fix, those tryCatch sites push the abort message into
# `state$resolve_errors[[id]]` and `ptr_register_error()` surfaces it
# verbatim. This test pins both invariants:
#   1. `state$resolve_errors` captures the message after a `.txt` upload.
#   2. `state$resolve_errors` clears on a subsequent good `.csv` upload.

with_temp_file <- function(content, ext) {
  path <- tempfile(fileext = paste0(".", ext))
  writeLines(content, path)
  path
}

test_that("uploading an unsupported extension surfaces the abort in state$resolve_errors", {
  skip_if_not_installed("shiny")
  suppressWarnings(ggpaintr:::ptr_register_builtins())

  bad_path <- with_temp_file("a,b\n1,2\n", "txt")
  formula <- "ggplot(data = upload, aes(x = a, y = b)) + geom_point()"

  shiny::testServer(function(input, output, session) {
    st <- ptr_server(input, output, session, formula, expr_check = FALSE)
    session$userData$state <- st
  }, {
    st <- session$userData$state
    src <- find_nodes(st$tree(), is_ptr_ph_data_source)[[1L]]

    # Simulate a Shiny fileInput value for an unsupported extension.
    file_info <- data.frame(
      name = "bad_extension.txt",
      size = file.info(bad_path)$size,
      type = "text/plain",
      datapath = bad_path,
      stringsAsFactors = FALSE
    )
    do.call(session$setInputs, stats::setNames(list(file_info), src$id))
    session$flushReact()

    errs <- unlist(st$resolve_errors())
    expect_true(length(errs) >= 1L,
                info = "expected at least one captured resolve error")
    expect_match(
      paste(errs, collapse = " | "),
      "Please upload a .csv, .tsv, .rds, .xlsx, .xls, or .json file.",
      info = "expected the unsupported-format abort message verbatim"
    )

    # A subsequent successful upload clears the per-source error slot.
    good_path <- with_temp_file("a,b\n1,2\n", "csv")
    file_info2 <- data.frame(
      name = "good.csv",
      size = file.info(good_path)$size,
      type = "text/csv",
      datapath = good_path,
      stringsAsFactors = FALSE
    )
    do.call(session$setInputs, stats::setNames(list(file_info2), src$id))
    session$flushReact()

    expect_equal(length(unlist(st$resolve_errors())), 0L,
                 info = "successful upload should clear the resolve error")
  })
})

test_that("clearing the file input clears the resolve error slot", {
  skip_if_not_installed("shiny")
  suppressWarnings(ggpaintr:::ptr_register_builtins())

  bad_path <- with_temp_file("a,b\n1,2\n", "txt")
  formula <- "ggplot(data = upload, aes(x = a, y = b)) + geom_point()"

  shiny::testServer(function(input, output, session) {
    st <- ptr_server(input, output, session, formula, expr_check = FALSE)
    session$userData$state <- st
  }, {
    st <- session$userData$state
    src <- find_nodes(st$tree(), is_ptr_ph_data_source)[[1L]]
    file_info <- data.frame(
      name = "bad_extension.txt",
      size = file.info(bad_path)$size,
      type = "text/plain",
      datapath = bad_path,
      stringsAsFactors = FALSE
    )
    do.call(session$setInputs, stats::setNames(list(file_info), src$id))
    session$flushReact()
    expect_true(length(unlist(st$resolve_errors())) >= 1L)

    # Setting the upload to NULL is what Shiny does on `reset`.
    do.call(session$setInputs, stats::setNames(list(NULL), src$id))
    session$flushReact()
    expect_equal(length(unlist(st$resolve_errors())), 0L,
                 info = "clearing the file input should clear the error slot")
  })
})
