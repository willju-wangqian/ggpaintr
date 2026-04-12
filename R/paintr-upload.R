#' Build the Dataset-Name Input Id for an Upload
#'
#' @param id A placeholder id.
#'
#' @return A related id string for the dataset-name control.
#' @noRd
ptr_upload_name_id <- function(id) {
  paste0(id, "+name")
}

#' Derive a Default Object Name from an Uploaded File
#'
#' @param file_name The uploaded filename.
#'
#' @return A syntactic R object name.
#' @noRd
ptr_upload_default_name <- function(file_name) {
  file_stem <- tools::file_path_sans_ext(basename(file_name))
  file_stem <- gsub("[^[:alnum:]_]+", "_", file_stem)
  file_stem <- gsub("^_+|_+$", "", file_stem)

  if (identical(file_stem, "")) {
    file_stem <- "uploaded_data"
  }

  if (file_stem %in% ptr_reserved_words()) {
    file_stem <- paste0(file_stem, "_")
  }
  make.names(file_stem)
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
  if (ext == "csv") {
    result <- tryCatch(
      utils::read.csv(file_info$datapath, fileEncoding = "UTF-8-BOM"),
      error = function(e) {
        rlang::abort(paste0(
          "Could not read '", file_info$name, "' as a csv file: ",
          conditionMessage(e)
        ))
      }
    )
    if (ncol(result) == 0L) {
      rlang::abort(paste0("Uploaded file '", file_info$name, "' contains no columns."))
    }
    if (nrow(result) == 0L) {
      rlang::abort(paste0("Uploaded file '", file_info$name, "' contains no rows."))
    }
    return(ptr_normalize_column_names(result))
  }

  if (ext == "rds") {
    result <- tryCatch(
      readRDS(file_info$datapath),
      error = function(e) {
        rlang::abort(paste0(
          "Could not read '", file_info$name, "' as an RDS file: ",
          conditionMessage(e)
        ))
      }
    )
    normalized <- ptr_normalize_tabular_data(result, source = "Uploaded data")
    if (ncol(normalized) == 0L) {
      rlang::abort(paste0("Uploaded file '", file_info$name, "' contains no columns."))
    }
    if (nrow(normalized) == 0L) {
      rlang::abort(paste0("Uploaded file '", file_info$name, "' contains no rows."))
    }
    return(normalized)
  }

  rlang::abort("Please upload a .csv or .rds file.")
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
  object_name <- input[[ptr_upload_name_id(upload_id)]]
  object_name <- trimws(if (is.null(object_name)) "" else object_name)

  if (identical(object_name, "")) {
    object_name <- ptr_upload_default_name(file_info$name)
  } else {
    object_name <- gsub("[[:space:]]+", "_", object_name)
    object_name <- make.names(object_name)
  }

  ext <- tolower(tools::file_ext(file_info$name))
  read_fun <- switch(
    ext,
    csv = "read.csv",
    rds = "readRDS",
    rlang::abort("Please upload a .csv or .rds file.")
  )

  list(
    data = data_obj,
    object_name = object_name,
    file_name = file_info$name,
    code_text = paste0(object_name, " <- ", read_fun, "(\"", file_info$name, "\")")
  )
}

#' Clone an Evaluation Environment and Inject Uploads
#'
#' @param ptr_obj A `ptr_obj`.
#' @param input A Shiny input-like object.
#' @param envir A parent environment.
#'
#' @return An evaluation environment containing uploaded datasets and any other
#'   placeholder-provided objects.
#' @noRd
ptr_prepare_eval_env <- function(ptr_obj, input, envir = parent.frame()) {
  eval_env <- rlang::env_clone(envir)
  context <- ptr_define_placeholder_context(ptr_obj, ui_text = NULL, envir = envir)

  for (keyword in names(ptr_obj$placeholders)) {
    spec <- ptr_obj$placeholders[[keyword]]
    metas <- ptr_flatten_placeholder_map(ptr_obj, keyword = keyword)

    if (length(metas) == 0 || is.null(spec$prepare_eval_env)) {
      next
    }

    prepared_env <- spec$prepare_eval_env(input, metas, eval_env, context)
    if (!is.null(prepared_env)) {
      eval_env <- prepared_env
    }
  }

  eval_env
}
