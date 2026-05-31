#' Coerce a String into a Syntactic, Reserved-Word-Safe R Object Name
#'
#' Single source of truth for turning a user- or file-supplied binding name
#' into a syntactic R name: collapse runs of non-`[alnum_]` to `_`, trim
#' leading/trailing `_`, substitute `fallback` if the result is empty, suffix
#' `_` for R reserved words, then `make.names()`. Both the file-derived
#' default name and the typed-companion (shortcut) name flow through here so
#' the two binding-name paths cannot drift.
#'
#' @param x A single character string (the raw name).
#' @param fallback Name to use when normalization leaves an empty string.
#'
#' @return A syntactic R object name.
#' @noRd
ptr_make_valid_name <- function(x, fallback = "uploaded_data") {
  x <- gsub("[^[:alnum:]_]+", "_", x)
  x <- gsub("^_+|_+$", "", x)

  if (identical(x, "")) {
    x <- fallback
  }

  if (x %in% ptr_reserved_words()) {
    x <- paste0(x, "_")
  }
  make.names(x)
}

#' Derive a Default Object Name from an Uploaded File
#'
#' @param file_name The uploaded filename.
#'
#' @return A syntactic R object name.
#' @noRd
ptr_upload_default_name <- function(file_name) {
  ptr_make_valid_name(tools::file_path_sans_ext(basename(file_name)))
}

#' Read Uploaded Paintr Data
#'
#' @param file_info A Shiny `fileInput()` value.
#'
#' @return A normalized tabular object or `NULL` when no file was supplied.
#' @noRd
ptr_read_uploaded_data <- function(file_info) {
  if (is.null(file_info) || is.null(file_info$datapath) || is.null(file_info$name)) {
    return(NULL)
  }

  ext <- tolower(tools::file_ext(file_info$name))
  reader <- switch(
    ext,
    csv  = ptr_read_csv_upload,
    tsv  = ptr_read_tsv_upload,
    rds  = ptr_read_rds_upload,
    xlsx = ptr_read_excel_upload,
    xls  = ptr_read_excel_upload,
    json = ptr_read_json_upload,
    rlang::abort(ptr_unsupported_upload_message())
  )

  result <- reader(file_info)
  ptr_check_upload_dimensions(result, file_info$name)
  result
}

#' Standard error message for unsupported upload extensions.
#' @noRd
ptr_unsupported_upload_message <- function() {
  "Please upload a .csv, .tsv, .rds, .xlsx, .xls, or .json file."
}

#' Validate non-empty rows and columns on a parsed upload.
#' @noRd
ptr_check_upload_dimensions <- function(data, file_name) {
  if (ncol(data) == 0L) {
    rlang::abort(paste0("Uploaded file '", file_name, "' contains no columns."))
  }
  if (nrow(data) == 0L) {
    rlang::abort(paste0("Uploaded file '", file_name, "' contains no rows."))
  }
  invisible(data)
}

#' Per-format upload readers.
#' @noRd
ptr_read_csv_upload <- function(file_info) {
  result <- tryCatch(
    utils::read.csv(file_info$datapath, fileEncoding = "UTF-8-BOM"),
    error = function(e) {
      rlang::abort(paste0(
        "Could not read '", file_info$name, "' as a csv file: ",
        conditionMessage(e)
      ))
    }
  )
  ptr_normalize_column_names(result)
}

ptr_read_tsv_upload <- function(file_info) {
  result <- tryCatch(
    utils::read.delim(file_info$datapath, fileEncoding = "UTF-8-BOM"),
    error = function(e) {
      rlang::abort(paste0(
        "Could not read '", file_info$name, "' as a tsv file: ",
        conditionMessage(e)
      ))
    }
  )
  ptr_normalize_column_names(result)
}

ptr_read_rds_upload <- function(file_info) {
  result <- tryCatch(
    readRDS(file_info$datapath),
    error = function(e) {
      rlang::abort(paste0(
        "Could not read '", file_info$name, "' as an RDS file: ",
        conditionMessage(e)
      ))
    }
  )
  ptr_normalize_tabular_data(result, source = "Uploaded data")
}

ptr_read_excel_upload <- function(file_info) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    rlang::abort(
      "Reading Excel uploads requires the 'readxl' package. Install it with install.packages(\"readxl\")."
    )
  }
  result <- tryCatch(
    readxl::read_excel(file_info$datapath),
    error = function(e) {
      rlang::abort(paste0(
        "Could not read '", file_info$name, "' as an Excel file: ",
        conditionMessage(e)
      ))
    }
  )
  ptr_normalize_tabular_data(result, source = "Uploaded data")
}

ptr_read_json_upload <- function(file_info) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    rlang::abort(
      "Reading JSON uploads requires the 'jsonlite' package. Install it with install.packages(\"jsonlite\")."
    )
  }
  parsed <- tryCatch(
    jsonlite::fromJSON(file_info$datapath, flatten = TRUE),
    error = function(e) {
      rlang::abort(paste0(
        "Could not read '", file_info$name, "' as a JSON file: ",
        conditionMessage(e)
      ))
    }
  )
  if (!is.data.frame(parsed)) {
    rlang::abort(paste0(
      "Uploaded JSON '", file_info$name,
      "' must be an array of objects (records). ",
      "Top-level objects, scalars, or arrays of arrays are not supported."
    ))
  }
  list_cols <- vapply(parsed, is.list, logical(1))
  if (any(list_cols)) {
    bad <- paste(names(parsed)[list_cols], collapse = ", ")
    rlang::abort(paste0(
      "Uploaded JSON '", file_info$name,
      "' has nested array/object columns (", bad,
      "). Flatten or remove them before uploading."
    ))
  }
  ptr_normalize_tabular_data(parsed, source = "Uploaded data")
}

#' Resolve Uploaded Dataset Metadata
#'
#' @param input A Shiny input-like object.
#' @param upload_id The upload placeholder id.
#' @param strict Whether missing uploads should error.
#'
#' @return A list with `data`, `object_name`, `file_name`, and `code_text`, or
#'   `NULL` when `strict = FALSE` and no upload was supplied.
#' @noRd
ptr_resolve_upload_info <- function(input, upload_id, strict = FALSE) {
  file_info <- input[[upload_id]]

  if (is.null(file_info) || is.null(file_info$datapath) || is.null(file_info$name)) {
    if (strict) {
      rlang::abort(paste0("Upload required for input '", upload_id, "'."))
    }
    return(NULL)
  }

  data_obj <- ptr_read_uploaded_data(file_info)
  object_name <- input[[paste0(upload_id, "_shortcut")]]
  object_name <- trimws(if (is.null(object_name)) "" else object_name)

  if (identical(object_name, "")) {
    object_name <- ptr_upload_default_name(file_info$name)
  } else {
    object_name <- ptr_make_valid_name(object_name)
  }

  ext <- tolower(tools::file_ext(file_info$name))
  read_fun <- reader_fn_name_for_ext(ext)
  if (is.na(read_fun)) rlang::abort(ptr_unsupported_upload_message())

  list(
    data = data_obj,
    object_name = object_name,
    file_name = file_info$name,
    code_text = paste0(object_name, " <- ", read_fun, "(\"", file_info$name, "\")")
  )
}

# ADR 0025 §4 / PLAN-04: single source of truth for extension -> reader-fn
# name dispatch. The mapping mirrors `ptr_read_uploaded_data()` (which picks
# the actual reader function) at name-only resolution, so the code-panel
# prologue (`emit_upload_prologue()` in paintr-server.R) and the
# `ptr_resolve_upload_info()` `code_text` field stay in lockstep with the
# real reader choice without duplicating the table.
#
# Returns `NA_character_` for unknown extensions; the prologue caller
# silently omits the line for that entry, while `ptr_resolve_upload_info()`
# aborts with `ptr_unsupported_upload_message()`.
reader_fn_name_for_ext <- function(ext) {
  if (!is.character(ext) || length(ext) != 1L) return(NA_character_)
  switch(
    tolower(ext),
    csv  = "read.csv",
    tsv  = "read.delim",
    rds  = "readRDS",
    xlsx = "readxl::read_excel",
    xls  = "readxl::read_excel",
    json = "jsonlite::fromJSON",
    NA_character_
  )
}

