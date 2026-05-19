#' Controllable-region UI helpers (internal)
#'
#' Single source of truth for the `.ptr-stage` DOM shape used wherever a
#' checkbox above a region greys out that region and removes its
#' contribution at eval time. Three callers emit this DOM today
#' (`build_pipeline_stage_ui()`, `wrap_shared_widgets_with_stage_blocks()`,
#' the orphan-stage block in `ptr_shared_ui()`); they all go through these
#' helpers so the DOM contract stays in one place.
#'
#' Contract (must not drift, see plan 2026-05-14-controllable-region):
#' - Outer wrapper: `<div class="ptr-stage" id="<ns>(<region_id>_stage_block)">`.
#' - Head: `<div class="ptr-stage-head">` containing a `checkboxInput()` whose
#'   `inputId` is `<ns>(<region_id>)`.
#' - Fields: `<div class="ptr-stage-fields">` wrapping the body.
#' - Continuation: bare `<div class="ptr-stage-fields">` with no head, used
#'   when a region's body is split across non-contiguous emit sites.
#'
#' Observer wiring is intentionally *not* part of this abstraction. Callers
#' continue to use whichever observer shape matches their reactive context
#' (`ptr_setup_stage_enabled`, `ptr_setup_shared_stage_enabled`, or the
#' shared-panel CSS-class observer in `ptr_shared_server`); the helpers
#' here only guarantee they all target the same DOM id and class.
#'
#' @param region_id Stable string id for the region. Shares the
#'   `state$stage_enabled()` namespace; pick a non-colliding id.
#' @param head_label A label for the head `checkboxInput()` (typically a
#'   `shiny::tags$code()` of the verb, e.g. `head()`).
#' @param body Body content rendered inside the fields wrapper.
#' @param ns_fn Namespace function for both the checkbox `inputId` and the
#'   wrapper DOM id. Pass `identity` for shared-panel ids that are not
#'   ns'd. Defaults to `identity`.
#' @param default_on Initial value of the head checkbox.
#'
#' @return A `shiny.tag` div ready to be inserted into a UI tree.
#' @keywords internal
#' @noRd
controllable_region <- function(region_id, head_label, body,
                                ns_fn = identity, default_on = TRUE) {
  shiny::div(
    class = "ptr-stage",
    id = ns_fn(paste0(region_id, "_stage_block")),
    shiny::div(
      class = "ptr-stage-head",
      shiny::checkboxInput(
        inputId = ns_fn(region_id),
        label = head_label, value = default_on, width = "auto"
      )
    ),
    shiny::div(class = "ptr-stage-fields", body)
  )
}

#' @keywords internal
#' @noRd
controllable_region_continuation <- function(body) {
  shiny::div(class = "ptr-stage-fields", body)
}
