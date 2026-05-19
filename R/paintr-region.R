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

#' Shared-panel header (title + help-icon tooltip) — internal
#'
#' Single source of truth for the `.ptr-shared-panel` header DOM. Both the
#' inline shared section (`shared_section_tags()` in paintr-app.R) and the
#' grid shared-controls panel (`ptr_shared_panel()` in
#' paintr-shared-coordinator.R) emit this header; routing both through one
#' helper keeps the markup/a11y contract identical.
#'
#' The explanatory hint is no longer an always-visible paragraph. It lives
#' behind a focusable `?` trigger and is revealed on hover/focus by CSS
#' alone (no JS, no bslib — `ptr_app()` is plain shiny). The hint text stays
#' in the DOM twice — as the trigger's `aria-label` (announced to assistive
#' tech on focus) and as the visually-revealed `.ptr-shared-panel__tip`
#' body (`aria-hidden`, so it is not double-announced).
#'
#' Contract (must not drift):
#' - `<p class="ptr-shared-panel__title">` carries the title text, then a
#'   `<span class="ptr-shared-panel__help" tabindex="0" aria-label="<hint>">`.
#' - That span holds the literal `?` glyph followed by
#'   `<span class="ptr-shared-panel__tip" aria-hidden="true"><hint></span>`.
#' - No standalone `.ptr-shared-panel__hint` element is emitted.
#'
#' @param title Section/panel title string.
#' @param hint Explanatory hint string (ui_text-overridable upstream).
#'
#' @return A `shiny.tag` `<p>` ready to insert into the panel.
#' @keywords internal
#' @noRd
shared_panel_header <- function(title, hint) {
  shiny::tags$p(
    class = "ptr-shared-panel__title",
    title,
    shiny::tags$span(
      class = "ptr-shared-panel__help",
      tabindex = "0",
      `aria-label` = hint,
      "?",
      shiny::tags$span(
        class = "ptr-shared-panel__tip",
        `aria-hidden` = "true",
        hint
      )
    )
  )
}
