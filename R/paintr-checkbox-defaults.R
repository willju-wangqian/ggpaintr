#' Strip a duplicate-instance suffix from a layer name.
#'
#' Layer names are deduplicated by `handle_duplicate_names()` with the rule
#' `"<base>-<n>"` for the 2nd, 3rd, ... occurrence. Strips that suffix.
#' @noRd
ptr_strip_dedup_suffix <- function(x) sub("-[0-9]+$", "", x)

#' Resolve user-supplied checkbox defaults into a per-layer logical vector.
#'
#' Validates input, expands group keys against the formula's deduped layer
#' names, pads short vectors with `TRUE`, warns on too-long vectors, warns on
#' unknown keys. Returns a length-N named logical vector keyed by the
#' non-`ggplot` entries of `names(expr_list)`, in formula order, with no `NA`.
#'
#' @param checkbox_defaults `NULL` or a named list (see `ptr_server_state()`).
#' @param expr_list The parsed formula's expression list (used only for
#'   `names()`).
#' @return Named logical vector.
#' @noRd
ptr_resolve_checkbox_defaults <- function(checkbox_defaults, expr_list) {
  layer_names <- setdiff(names(expr_list), "ggplot")
  fallback <- ptr_default_layer_state()
  result <- stats::setNames(rep(fallback, length(layer_names)), layer_names)

  if (is.null(checkbox_defaults) || length(checkbox_defaults) == 0L) {
    return(result)
  }

  ptr_validate_checkbox_defaults_shape(checkbox_defaults)
  ptr_validate_checkbox_defaults_values(checkbox_defaults)

  consumed_layers <- character(0)
  unknown_keys <- character(0)
  long_vector_msgs <- character(0)

  user_keys <- names(checkbox_defaults)

  # Pass 1 — direct hits. A key is "direct" only when it carries a dedup
  # suffix (e.g. `geom_point-2`) AND matches a layer name verbatim. Bare base
  # names always go to group expansion, even when there's only one instance.
  is_suffixed <- grepl("-[0-9]+$", user_keys)
  direct_keys <- user_keys[is_suffixed & user_keys %in% layer_names]
  for (key in direct_keys) {
    result[[key]] <- checkbox_defaults[[key]][[1L]]
    consumed_layers <- c(consumed_layers, key)
  }

  # Pass 2 — group expansion. Remaining user keys are matched to all
  # not-yet-consumed layers whose stripped base equals the user key.
  remaining_keys <- setdiff(user_keys, direct_keys)
  for (key in remaining_keys) {
    # A suffixed key that didn't land in pass 1 means the user named a
    # non-existent layer instance (e.g. `geom_point-99`). Don't silently
    # collapse it into the base group — surface as unknown.
    if (grepl("-[0-9]+$", key)) {
      unknown_keys <- c(unknown_keys, key)
      next
    }

    available <- setdiff(layer_names, consumed_layers)
    bases <- ptr_strip_dedup_suffix(available)
    group <- available[bases == key]

    if (length(group) == 0L) {
      unknown_keys <- c(unknown_keys, key)
      next
    }

    value_vec <- checkbox_defaults[[key]]
    if (length(value_vec) > length(group)) {
      long_vector_msgs <- c(
        long_vector_msgs,
        sprintf(
          "`checkbox_defaults$%s` has %d values but the layer group has %d. Extra values dropped.",
          key, length(value_vec), length(group)
        )
      )
      value_vec <- value_vec[seq_along(group)]
    } else if (length(value_vec) < length(group)) {
      value_vec <- c(value_vec, rep(fallback, length(group) - length(value_vec)))
    }

    result[group] <- value_vec
    consumed_layers <- c(consumed_layers, group)
  }

  if (length(unknown_keys) > 0L) {
    if (length(layer_names) == 0L) {
      cli::cli_warn(c(
        "`checkbox_defaults` was provided but the formula has no non-ggplot layers to configure.",
        "x" = "Unknown: {.val {unknown_keys}}"
      ))
    } else {
      cli::cli_warn(c(
        "Some `checkbox_defaults` names don't match any layer in the formula:",
        "x" = "Unknown: {.val {unknown_keys}}",
        "i" = "Available layers: {.val {layer_names}}"
      ))
    }
  }
  if (length(long_vector_msgs) > 0L) {
    cli::cli_warn(long_vector_msgs)
  }

  result
}

#' @noRd
ptr_validate_checkbox_defaults_shape <- function(checkbox_defaults) {
  if (!is.list(checkbox_defaults) || is.data.frame(checkbox_defaults)) {
    rlang::abort(sprintf(
      "`checkbox_defaults` must be a named list, not a %s.",
      paste(class(checkbox_defaults), collapse = "/")
    ))
  }
  nms <- names(checkbox_defaults)
  if (is.null(nms) || any(!nzchar(nms))) {
    unnamed_pos <- if (is.null(nms)) seq_along(checkbox_defaults) else which(!nzchar(nms))
    rlang::abort(sprintf(
      "`checkbox_defaults` must be fully named. Found unnamed entries at positions: %s.",
      paste(unnamed_pos, collapse = ", ")
    ))
  }
  dup_names <- unique(nms[duplicated(nms)])
  if (length(dup_names) > 0L) {
    rlang::abort(sprintf(
      "`checkbox_defaults` has duplicate names: %s. Each layer key must appear once.",
      paste(dup_names, collapse = ", ")
    ))
  }
  invisible(TRUE)
}

#' @noRd
ptr_validate_checkbox_defaults_values <- function(checkbox_defaults) {
  for (key in names(checkbox_defaults)) {
    value <- checkbox_defaults[[key]]
    if (!is.logical(value)) {
      rlang::abort(sprintf(
        "`checkbox_defaults$%s` must be logical, not %s.",
        key, paste(class(value), collapse = "/")
      ))
    }
    if (length(value) == 0L) {
      rlang::abort(sprintf(
        "`checkbox_defaults$%s` has length 0. Use TRUE / FALSE or omit the key.",
        key
      ))
    }
    if (any(is.na(value))) {
      rlang::abort(sprintf(
        "`checkbox_defaults$%s` contains NA. Use TRUE / FALSE only.",
        key
      ))
    }
  }
  invisible(TRUE)
}
