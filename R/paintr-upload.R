#' Build the Dataset-Name Input Id for an Upload
#'
#' @param id A placeholder id.
#'
#' @return A related id string for the dataset-name control.
#' @keywords internal
paintr_upload_name_id <- function(id) {
  paste0(id, "+name")
}

#' Derive a Default Object Name from an Uploaded File
#'
#' @param file_name The uploaded filename.
#'
#' @return A syntactic R object name.
#' @keywords internal
paintr_upload_default_name <- function(file_name) {
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
#' @return The uploaded object or `NULL`.
#' @keywords internal
paintr_read_uploaded_data <- function(file_info) {
  if (is.null(file_info) || is.null(file_info$datapath) || is.null(file_info$name)) {
    return(NULL)
  }

  ext <- tolower(tools::file_ext(file_info$name))
  if (ext == "csv") {
    return(utils::read.csv(file_info$datapath))
  }

  if (ext == "rds") {
    return(readRDS(file_info$datapath))
  }

  stop("Please upload a .csv or .rds file.", call. = FALSE)
}

#' Resolve Uploaded Dataset Metadata
#'
#' @param input A Shiny input-like object.
#' @param upload_id The upload placeholder id.
#' @param strict Whether missing uploads should error.
#'
#' @return A list with data, object name, file name, and generated code text.
#' @keywords internal
paintr_resolve_upload_info <- function(input, upload_id, strict = FALSE) {
  file_info <- input[[upload_id]]

  if (is.null(file_info) || is.null(file_info$datapath) || is.null(file_info$name)) {
    if (strict) {
      stop(paste0("Upload required for input '", upload_id, "'."), call. = FALSE)
    }
    return(NULL)
  }

  data_obj <- paintr_read_uploaded_data(file_info)
  object_name <- input[[paintr_upload_name_id(upload_id)]]
  object_name <- trimws(if (is.null(object_name)) "" else object_name)

  if (identical(object_name, "")) {
    object_name <- paintr_upload_default_name(file_info$name)
  } else {
    object_name <- gsub("[[:space:]]+", "_", object_name)
    object_name <- make.names(object_name)
  }

  ext <- tolower(tools::file_ext(file_info$name))
  read_fun <- switch(
    ext,
    csv = "read.csv",
    rds = "readRDS",
    stop("Please upload a .csv or .rds file.", call. = FALSE)
  )

  list(
    data = data_obj,
    object_name = object_name,
    file_name = file_info$name,
    code_text = paste0(object_name, " <- ", read_fun, "(\"", file_info$name, "\")")
  )
}

#' Return Uploaded Data Only
#'
#' @param input A Shiny input-like object.
#' @param upload_id The upload placeholder id.
#'
#' @return The uploaded object or `NULL`.
#' @keywords internal
paintr_get_uploaded_data <- function(input, upload_id) {
  upload_info <- paintr_resolve_upload_info(input, upload_id, strict = FALSE)
  if (is.null(upload_info)) {
    return(NULL)
  }

  upload_info$data
}

#' Clone an Evaluation Environment and Inject Uploads
#'
#' @param paintr_obj A `paintr_obj`.
#' @param input A Shiny input-like object.
#' @param envir A parent environment.
#'
#' @return An evaluation environment containing uploaded datasets.
#' @keywords internal
paintr_prepare_eval_env <- function(paintr_obj, input, envir = parent.frame()) {
  eval_env <- rlang::env_clone(envir)
  upload_ids <- character()

  for (expr_name in names(paintr_obj[["keywords_list"]])) {
    keyword_list <- paintr_obj[["keywords_list"]][[expr_name]]
    upload_matches <- vapply(keyword_list, detect_keywords, character(1)) == "upload"
    upload_ids <- c(upload_ids, names(keyword_list)[upload_matches])
  }

  for (upload_id in upload_ids) {
    upload_info <- paintr_resolve_upload_info(input, upload_id, strict = FALSE)
    if (is.null(upload_info)) {
      next
    }

    assign(upload_info$object_name, upload_info$data, envir = eval_env)
  }

  eval_env
}

