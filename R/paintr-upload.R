#' Build the Dataset-Name Input Id for an Upload
#'
#' @param id A placeholder id.
#'
#' @return A related id string for the dataset-name control.
#' @noRd
ggpaintr_upload_name_id <- function(id) {
  paste0(id, "+name")
}

#' Derive a Default Object Name from an Uploaded File
#'
#' @param file_name The uploaded filename.
#'
#' @return A syntactic R object name.
#' @noRd
ggpaintr_upload_default_name <- function(file_name) {
  file_stem <- tools::file_path_sans_ext(basename(file_name))
  file_stem <- gsub("[^[:alnum:]_]+", "_", file_stem)
  file_stem <- gsub("^_+|_+$", "", file_stem)

  if (identical(file_stem, "")) {
    file_stem <- "uploaded_data"
  }

  make.names(file_stem)
}

#' Read Uploaded Paintr Data
#'
#' @param file_info A Shiny `fileInput()` value.
#'
#' @return A normalized tabular object or `NULL` when no file was supplied.
#' @noRd
ggpaintr_read_uploaded_data <- function(file_info) {
  if (is.null(file_info) || is.null(file_info$datapath) || is.null(file_info$name)) {
    return(NULL)
  }

  ext <- tolower(tools::file_ext(file_info$name))
  if (ext == "csv") {
    return(ggpaintr_normalize_column_names(utils::read.csv(file_info$datapath)))
  }

  if (ext == "rds") {
    return(ggpaintr_normalize_tabular_data(
      readRDS(file_info$datapath),
      source = "Uploaded data"
    ))
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
ggpaintr_resolve_upload_info <- function(input, upload_id, strict = FALSE) {
  file_info <- input[[upload_id]]

  if (is.null(file_info) || is.null(file_info$datapath) || is.null(file_info$name)) {
    if (strict) {
      rlang::abort(paste0("Upload required for input '", upload_id, "'."))
    }
    return(NULL)
  }

  data_obj <- ggpaintr_read_uploaded_data(file_info)
  object_name <- input[[ggpaintr_upload_name_id(upload_id)]]
  object_name <- trimws(if (is.null(object_name)) "" else object_name)

  if (identical(object_name, "")) {
    object_name <- ggpaintr_upload_default_name(file_info$name)
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
#' @param ggpaintr_obj A `ggpaintr_obj`.
#' @param input A Shiny input-like object.
#' @param envir A parent environment.
#'
#' @return An evaluation environment containing uploaded datasets and any other
#'   placeholder-provided objects.
#' @noRd
ggpaintr_prepare_eval_env <- function(ggpaintr_obj, input, envir = parent.frame()) {
  eval_env <- rlang::env_clone(envir)
  context <- ggpaintr_placeholder_context(ggpaintr_obj, copy_rules = NULL, envir = envir)

  for (keyword in names(ggpaintr_obj$placeholders)) {
    spec <- ggpaintr_obj$placeholders[[keyword]]
    metas <- ggpaintr_flatten_placeholder_map(ggpaintr_obj, keyword = keyword)

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
