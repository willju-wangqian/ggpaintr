#' Normalize Dataset Column Names for `ggpaintr`
#'
#' Normalize incoming column names so `var` placeholders can require exact,
#' syntactic, unique column-name matches at runtime.
#'
#' @param data A data frame or an object coercible with `as.data.frame()`.
#'
#' @return A tabular object with `ggpaintr`-safe column names. Existing
#'   `data.frame` subclasses keep their class. Names are made syntactic, unique,
#'   and safe against reserved-word collisions. Non-`data.frame` inputs return
#'   the `data.frame` created by `as.data.frame()`.
#' @examples
#' messy <- data.frame(
#'   check.names = FALSE,
#'   "first column" = 1:3,
#'   "if" = 4:6
#' )
#'
#' clean <- ggpaintr_normalize_column_names(messy)
#' names(clean)
#' @export
ggpaintr_normalize_column_names <- function(data) {
  ggpaintr_normalize_tabular_data(data, source = "Data")
}

#' Coerce an Object to Tabular Data for `ggpaintr`
#'
#' @param data An arbitrary R object.
#' @param source A short label for error messages.
#'
#' @return A `data.frame`-like object with normalized column names.
#' @noRd
ggpaintr_normalize_tabular_data <- function(data, source = "Data") {
  if (inherits(data, "data.frame")) {
    names(data) <- ggpaintr_normalize_column_name_vector(names(data), ncol(data))
    return(data)
  }

  data_frame <- tryCatch(
    as.data.frame(data),
    error = function(e) NULL
  )

  if (is.null(data_frame)) {
    rlang::abort(
      paste0(
        source,
        " is not usable as tabular data for ggpaintr. ",
        "Provide a data.frame or an object coercible with as.data.frame()."
      )
    )
  }

  names(data_frame) <- ggpaintr_normalize_column_name_vector(
    names(data_frame),
    ncol(data_frame)
  )
  data_frame
}

#' Normalize a Vector of Column Names
#'
#' @param x A character vector of names.
#' @param n Expected column count.
#'
#' @return A normalized character vector with syntactic, unique names.
#' @noRd
ggpaintr_normalize_column_name_vector <- function(x, n = length(x)) {
  if (is.null(x)) {
    x <- character()
  }

  x <- as.character(x)
  x[is.na(x)] <- ""

  if (length(x) < n) {
    x <- c(x, rep("", n - length(x)))
  }

  if (length(x) > n) {
    x <- x[seq_len(n)]
  }

  x <- gsub("[^[:alnum:]_.]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x[x == ""] <- "col"

  valid_starts <- grepl("(^[[:alpha:]])|(^\\.(?![0-9]))", x, perl = TRUE)
  x[!valid_starts] <- paste0("X", x[!valid_starts])

  reserved <- ggpaintr_reserved_words()
  x[x %in% reserved] <- paste0(x[x %in% reserved], "_")

  make.unique(x, sep = "_")
}

#' Reserved Words That Should Not Be Used as Column Names
#'
#' @return A character vector of reserved words.
#' @noRd
ggpaintr_reserved_words <- function() {
  c(
    "if",
    "else",
    "repeat",
    "while",
    "function",
    "for",
    "in",
    "next",
    "break",
    "TRUE",
    "FALSE",
    "NULL",
    "Inf",
    "NaN",
    "NA",
    "NA_integer_",
    "NA_real_",
    "NA_complex_",
    "NA_character_"
  )
}
