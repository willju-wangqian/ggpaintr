# Public entry points for the typed-AST core. Each entry point composes the
# UI builder (per-layer panels via `build_ui_for.ptr_layer`) and the server
# wiring (`ptr_server`).

#' Build a Shiny App from a `ggpaintr` Formula
#'
#' Translates `formula` into the typed AST, builds the per-layer panel UI,
#' and wires the server end-to-end. Returns a `shiny.appobj` ready to be run.
#' This page is the canonical reference for the formula grammar and the
#' empty-call cleanup rule used by every public entry point.
#'
#' @param formula A single formula string with `ggpaintr` placeholders. See
#'   **Formula grammar** below.
#' @param envir Environment used to resolve local data objects.
#' @param ui_text Optional named list of copy overrides; see [ptr_ui_text()]
#'   for the full schema and current defaults.
#' @param checkbox_defaults Optional named list of initial checked states for
#'   layer checkboxes.
#' @param expr_check Controls `expr` placeholder validation. Three modes:
#'   `TRUE` (default) applies the built-in denylist + AST walker;
#'   `FALSE` disables all validation (for local prototyping with trusted
#'   input only); a `list` with `deny_list` and/or `allow_list` entries
#'   (character vectors) customises the policy without disabling it. See
#'   `vignette("ggpaintr-safety")` for the walker model.
#' @param safe_to_remove Character vector of additional function names whose
#'   zero-argument calls should be dropped after placeholder substitution
#'   leaves them empty. Defaults to `character()`. See **Empty-call cleanup**
#'   below. A user-typed `expr` always wins — whatever the user enters into
#'   an `expr` box is honoured verbatim, even if its top-level name is in
#'   `safe_to_remove`.
#' @param css Optional character vector of paths to additional CSS files. Each
#'   is served as a static resource and linked after `ggpaintr`'s bundled
#'   stylesheet, so its rules override the default `.ptr-*` styling. Relative
#'   `url(...)` references inside a file resolve against that file's own
#'   directory. Defaults to `NULL` (no extra stylesheet).
#'
#' @return A `shiny.appobj`.
#'
#' @section Formula grammar:
#' A `ggpaintr` formula is a single `ggplot()` call written as a string. Drop
#' one of five placeholder keywords anywhere a value would normally go, and
#' the runtime substitutes the user's input back into the expression at
#' render time.
#'
#' \describe{
#'   \item{`var`}{Column picker, data-aware. Renders as a `selectInput`
#'   populated with the upstream data's column-name vector. Example:
#'   `aes(x = var)`.}
#'   \item{`text`}{Free-text input. Renders as a `textInput`. Example:
#'   `labs(title = text)`.}
#'   \item{`num`}{Numeric input. Renders as a `numericInput`. Example:
#'   `geom_point(size = num)`.}
#'   \item{`expr`}{Code editor, validated by `expr_check`. The only keyword
#'   that accepts arbitrary R code; see `vignette("ggpaintr-safety")` for
#'   the model. Example: `facet_wrap(expr)`.}
#'   \item{`upload`}{File picker, returns a data frame. Renders as a
#'   `fileInput` plus an optional dataset-name textbox. Accepted formats:
#'   `.csv`, `.tsv`, `.rds`, `.xlsx`, `.xls`, `.json`. Uploaded data is
#'   normalized via [ptr_normalize_column_names()] automatically. Example:
#'   `ggplot(upload, ...)`.}
#' }
#'
#' Any keyword occurrence may carry `shared = "<id>"` to lift the widget out
#' of its per-layer panel into a top-level shared section. Used by
#' [ptr_app_grid()] to drive multiple plots from one control. See
#' `vignette("ggpaintr-use-cases")` for worked examples of each keyword.
#'
#' @section Empty-call cleanup:
#' When a placeholder resolves to "missing" (an empty `var` pick, a blank
#' `text`, a cleared `num`, an unchecked layer checkbox), its argument is
#' dropped from the generated code. If the surrounding call is left empty
#' and its bare name is in the curated cleanup list, the whole call
#' disappears too. This rule applies to both placeholder-driven empties and
#' user-authored literal empty calls like `+ labs()`.
#'
#' Curated `ggplot2` names that are dropped when empty:
#'
#' \preformatted{theme, labs, xlab, ylab, ggtitle,
#' facet_wrap, facet_grid, facet_null,
#' xlim, ylim, lims, expand_limits,
#' guides, annotate, annotation_custom,
#' annotation_map, annotation_raster,
#' aes, aes_, aes_q, aes_string, vars,
#' element_text, element_line, element_rect,
#' element_point, element_polygon, element_geom}
#'
#' Empty calls to similar no-op helpers from `dplyr`, `tidyr`, `tibble`,
#' `pillar`, `purrr`, `stringr`, `forcats`, `lubridate`, and `hms` are
#' covered by the same rule.
#'
#' `geom_*()` and `stat_*()` layers are **never** dropped, regardless of
#' whether they end up empty — they inherit their aesthetics from
#' `ggplot()` and remain meaningful with no arguments.
#'
#' `element_blank()` is intentionally **not** in the cleanup list: its
#' empty form is a meaningful "suppress" directive, not a no-op.
#'
#' Third-party helpers (e.g. `pcp_theme()` from `ggpcp`) are not in the
#' cleanup list — being absent is the "removal safety unknown" signal.
#' Use `safe_to_remove = c("pcp_theme")` to opt a specific name in.
#'
#' @seealso [ptr_app_bslib()] for the same contract with a `bslib` theme;
#'   [ptr_app_grid()] for multi-plot apps with shared widgets;
#'   [ptr_define_placeholder_value()] et al. for registering custom
#'   keywords; [ptr_ui_text()] for copy overrides; [ptr_css()] for the
#'   `css =` argument and themable CSS custom properties;
#'   `vignette("ggpaintr-use-cases")` for tutorial examples.
#' @export
ptr_app <- function(formula,
                       envir = parent.frame(),
                       ui_text = NULL,
                       checkbox_defaults = NULL,
                       expr_check = TRUE,
                       safe_to_remove = character(),
                       css = NULL) {
  parts <- ptr_app_components(
    formula,
    envir = envir,
    ui_text = ui_text,
    checkbox_defaults = checkbox_defaults,
    expr_check = expr_check,
    safe_to_remove = safe_to_remove,
    css = css
  )
  shiny::shinyApp(ui = parts$ui, server = parts$server)
}

ptr_app_components <- function(formula,
                                  envir = parent.frame(),
                                  ui_text = NULL,
                                  checkbox_defaults = NULL,
                                  expr_check = TRUE,
                                  safe_to_remove = character(),
                                  ns = shiny::NS(NULL),
                                  css = NULL) {
  tree <- ptr_translate(formula, expr_check = expr_check)

  ui <- ptr_build_app_ui(
    tree,
    ui_text = ui_text,
    checkbox_defaults = checkbox_defaults,
    ns = ns,
    render_shared_section = TRUE,
    app_chrome = TRUE,
    css = css
  )
  server <- ptr_make_app_server(
    formula, tree,
    envir = envir, ui_text = ui_text,
    checkbox_defaults = checkbox_defaults, expr_check = expr_check,
    safe_to_remove = safe_to_remove, ns = ns
  )
  list(ui = ui, server = server)
}

# The server closure shared by `ptr_app()` / `ptr_app_components()` and
# `ptr_app_bslib()`: wires `ptr_server()` plus the host-side shared-widget
# binding (`ptr_bind_shared_consumer_uis()`). `tree` is the translated AST,
# already in hand at the call site (it also drives the UI there).
ptr_make_app_server <- function(formula, tree, envir, ui_text,
                                checkbox_defaults, expr_check,
                                safe_to_remove, ns) {
  shared_entries <- collect_shared_placeholders(tree)
  shared_keys <- vapply(shared_entries, `[[`, character(1), "key")
  shared_resolutions <- ptr_resolve_shared_consumers(tree)
  consumer_keys <- names(shared_resolutions)
  representative_nodes <- lapply(consumer_keys, function(k) {
    nodes <- collect_shared_consumer_occurrences(tree)[[k]]
    n <- nodes[[1L]]
    n$id <- canonical_shared_id(k)
    n$shared_label <- shared_widget_label(nodes)
    # Multi-param shared widget: clear the first-occurrence param so
    # copy resolution falls through to `defaults$<keyword>` instead of
    # dragging the leading occurrence's `params$<param>$<keyword>` over
    # the user's override. Mirrors the same logic in
    # `collect_shared_placeholders()` for value-shared widgets.
    params <- vapply(nodes, function(x) x$param %||% NA_character_,
                     character(1))
    distinct_params <- unique(params[!is.na(params) & nzchar(params) &
                                       params != "__unnamed__"])
    if (length(distinct_params) > 1L) {
      n$param <- NA_character_
    }
    n
  })
  names(representative_nodes) <- consumer_keys

  function(input, output, session) {
    shared_reactives <- if (length(shared_keys) > 0L) {
      stats::setNames(
        lapply(shared_keys, function(k) {
          canonical <- canonical_shared_id(k)
          shiny::reactive(input[[ns(canonical)]])
        }),
        shared_keys
      )
    } else {
      list()
    }
    state <- ptr_server(
      input, output, session, formula,
      envir = envir,
      ui_text = ui_text,
      checkbox_defaults = checkbox_defaults,
      expr_check = expr_check,
      safe_to_remove = safe_to_remove,
      shared = shared_reactives,
      ns = ns,
      auto_bind_shared = TRUE,
      shared_resolutions = shared_resolutions
    )
    if (length(consumer_keys) > 0L) {
      ptr_bind_shared_consumer_uis(
        output = output, input = input, ns = ns,
        resolutions = shared_resolutions,
        representative_nodes = representative_nodes,
        ui_text = ui_text,
        eval_env = envir,
        expr_check = expr_check,
        errors_rv = state$shared_resolution_errors,
        state = state
      )
    }
  }
}

# Sidebar contents for an embedded formula: optional shared section + layer
# picker + hidden tabset + the "Update plot" button. Returns a list of tags so
# callers can do.call(shiny::sidebarPanel, .) or wrap in shiny::tagList().
ptr_controls_panel <- function(tree, ui_text = NULL,
                               checkbox_defaults = NULL,
                               ns = shiny::NS(NULL),
                               render_shared_section = FALSE) {
  shell_copy <- layer_panel_default_shell_copy(ui_text)
  layer_names <- vapply(tree$layers, function(l) l$name, character(1))

  panels <- lapply(tree$layers, function(layer) {
    build_ui_for(layer,
                 ui_text = ui_text,
                 ns_fn = ns,
                 checkbox_defaults = checkbox_defaults,
                 shell_copy = shell_copy)
  })

  picker <- shinyWidgets::pickerInput(
    inputId = ns("ptr_layer_select"),
    label = shell_copy$layer_picker_label %||% "Layer",
    choices = layer_names,
    selected = if (length(layer_names)) layer_names[1L] else NULL
  )
  hidden_tabset <- do.call(
    shiny::tabsetPanel,
    c(list(id = ns("ptr_layer_tabset"), type = "hidden"), panels)
  )

  shared_section <- if (isTRUE(render_shared_section)) {
    shared_entries <- collect_shared_placeholders(tree)
    if (length(shared_entries) > 0L) {
      widgets <- lapply(shared_entries, function(e) {
        build_ui_for(e$node, ui_text = ui_text, ns_fn = ns,
                     label_override = e$label_override)
      })
      keep <- !vapply(widgets, is.null, logical(1))
      widgets <- widgets[keep]
      kept_entries <- shared_entries[keep]
      orphan_stages <- collect_orphan_shared_stages(tree)
      widgets <- wrap_shared_widgets_with_stage_blocks(
        kept_entries, widgets, orphan_stages, ns
      )
      if (length(widgets) > 0L) {
        shiny::wellPanel(
          shiny::div(
            class = "ptr-shared-panel",
            shiny::tags$p(class = "ptr-shared-panel__title", "Shared controls"),
            shiny::tags$p(class = "ptr-shared-panel__hint",
                          "One value here is reused everywhere it is referenced."),
            do.call(shiny::tagList, widgets)
          )
        )
      } else NULL
    } else NULL
  } else NULL

  drop_null(list(
    shared_section,
    picker,
    hidden_tabset,
    shiny::actionButton(
      ns("ptr_update_plot"),
      label = shell_copy$update_plot_label %||% "Update plot"
    )
  ))
}

# ---- Output panes: shared builders + public pieces ----
#
# `ptr_outputs_panel()` (the bundled `.ptr-output` block used by
# `ptr_app()` / `ptr_module_ui()`) and the exported single-piece
# functions below are both assembled from the same `*_tag()` builders, so
# the bundled DOM, input ids and output ids are byte-identical to the
# pre-split implementation. The ids `ptr_plot` / `ptr_error` / `ptr_code`
# are the contract `ptr_register_plot()` / `_error()` / `_code()` write to.

# SVG used for the show-code toggle button in the plot card head.
code_toggle_icon <- function() {
  shiny::HTML(
    '<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.2" stroke-linecap="round" stroke-linejoin="round"><path d="m18 16 4-4-4-4"></path><path d="m6 8-4 4 4 4"></path><path d="m14.5 4-5 16"></path></svg>'
  )
}

error_slot_tag <- function(ns) {
  shiny::uiOutput(ns("ptr_error"))
}

plot_card_tag <- function(ns, error = TRUE, code_toggle = FALSE) {
  head_kids <- list(shiny::tags$h3(class = "ptr-card__title", "Plot"))
  if (isTRUE(code_toggle)) {
    head_kids <- c(head_kids, list(
      shiny::tags$button(
        type = "button",
        class = "ptr-icon-btn ptr-code-toggle",
        title = "Show generated code",
        `aria-label` = "Show generated code",
        code_toggle_icon()
      )
    ))
  }
  body_kids <- list(shiny::plotOutput(ns("ptr_plot")))
  if (isTRUE(error)) {
    body_kids <- c(body_kids, list(error_slot_tag(ns)))
  }
  shiny::tags$div(
    class = "ptr-card ptr-card--plot",
    do.call(shiny::tags$div, c(list(class = "ptr-card__head"), head_kids)),
    do.call(shiny::tags$div, c(list(class = "ptr-card__body"), body_kids))
  )
}

code_block_tag <- function(ns, style = c("panel", "window")) {
  style <- match.arg(style)
  if (identical(style, "window")) {
    return(shiny::tags$div(
      class = "ptr-code-window",
      shiny::tags$div(
        class = "ptr-code-window__head",
        shiny::tags$span(class = "ptr-code-window__title", shiny::HTML("&lt;/&gt; Generated code")),
        shiny::tags$span(
          class = "ptr-code-window__actions",
          shiny::tags$button(type = "button", class = "ptr-copy-btn", "Copy"),
          shiny::tags$button(
            type = "button", class = "ptr-code-window__close",
            title = "Close", `aria-label` = "Close",
            shiny::HTML("&times;")
          )
        )
      ),
      shiny::tags$div(
        class = "ptr-code-window__body",
        shiny::verbatimTextOutput(ns("ptr_code"))
      )
    ))
  }
  shiny::tags$div(
    class = "ptr-card ptr-card--code",
    shiny::tags$div(
      class = "ptr-card__head",
      shiny::tags$h3(class = "ptr-card__title", "Generated code")
    ),
    shiny::tags$div(
      class = "ptr-card__body",
      shiny::verbatimTextOutput(ns("ptr_code"))
    )
  )
}

# Bundled `.ptr-output` block: plot (+ inline error) + slide-out code
# window, wired to the show-code toggle. Byte-identical to the
# pre-split implementation; consumed by ptr_app() / ptr_module_ui().
ptr_outputs_panel <- function(ns = shiny::NS(NULL)) {
  shiny::tags$div(
    class = "ptr-output",
    plot_card_tag(ns, error = TRUE, code_toggle = TRUE),
    code_block_tag(ns, style = "window")
  )
}

#' Plot Pane Piece for a `ggpaintr` Formula
#'
#' The plot card on its own: a `shiny::plotOutput()` bound to the
#' `ptr_plot` id the server writes to (see [ptr_server()]). One of the
#' single-piece UI builders for the L3 "own every UI piece" workflow;
#' place it anywhere in your own layout and wire the server with
#' [ptr_server()] / [ptr_module_server()].
#'
#' @param id Optional module id; the namespace prefix for the output.
#'   Defaults to `NULL` (identity namespace) for the single-embedding
#'   case. When set, must match the `id` passed to the other piece
#'   functions and to the `shiny::moduleServer()` wrapping [ptr_server()]
#'   (or to [ptr_module_server()]).
#' @param error If `TRUE` (default) the inline error slot
#'   ([ptr_ui_error()]) is nested in the card body, reproducing the
#'   bundled layout. Set `FALSE` to place [ptr_ui_error()] elsewhere.
#' @param code_toggle If `TRUE`, render the show-code button in the card
#'   header. The button only does anything when the page also contains a
#'   slide-out [ptr_ui_code()] wired via [ptr_ui_code_toggle()]; the
#'   bundled apps set this for you. Defaults to `FALSE`.
#'
#' @return A [shiny::tag].
#' @seealso [ptr_ui_error()], [ptr_ui_code()], [ptr_ui_code_toggle()],
#'   [ptr_ui_controls()], [ptr_server()]
#' @export
ptr_ui_plot <- function(id = NULL, error = TRUE, code_toggle = FALSE) {
  plot_card_tag(shiny::NS(id), error = error, code_toggle = code_toggle)
}

#' Inline Error Pane Piece for a `ggpaintr` Formula
#'
#' The inline error slot on its own: a `shiny::uiOutput()` bound to the
#' `ptr_error` id the server writes parse/runtime error alerts to (see
#' [ptr_server()]). One of the single-piece UI builders for the L3 "own
#' every UI piece" workflow.
#'
#' @param id Optional module id; the namespace prefix for the output.
#'   Defaults to `NULL` (identity namespace). When set, must match the
#'   `id` passed to the other piece functions and the server wiring.
#'
#' @return A [shiny::tag].
#' @seealso [ptr_ui_plot()], [ptr_ui_code()], [ptr_ui_controls()], [ptr_server()]
#' @export
ptr_ui_error <- function(id = NULL) {
  error_slot_tag(shiny::NS(id))
}

#' Generated-Code Pane Piece for a `ggpaintr` Formula
#'
#' The generated-code output on its own: a `shiny::verbatimTextOutput()`
#' bound to the `ptr_code` id the server writes to (see [ptr_server()]).
#' One of the single-piece UI builders for the L3 "own every UI piece"
#' workflow.
#'
#' @param id Optional module id; the namespace prefix for the output.
#'   Defaults to `NULL` (identity namespace). When set, must match the
#'   `id` passed to the other piece functions and the server wiring.
#' @param style `"panel"` (default) renders a plain, always-visible code
#'   card suitable for free placement. `"window"` renders the draggable
#'   slide-out window (with Copy / Close) used by the bundled apps; it is
#'   hidden until toggled and only works when wired via
#'   [ptr_ui_code_toggle()].
#'
#' @return A [shiny::tag].
#' @seealso [ptr_ui_code_toggle()], [ptr_ui_plot()], [ptr_server()]
#' @export
ptr_ui_code <- function(id = NULL, style = c("panel", "window")) {
  code_block_tag(shiny::NS(id), style = match.arg(style))
}

#' Toggle-Wired Plot + Slide-Out Code Block
#'
#' Convenience helper that returns the plot card and the slide-out
#' generated-code window wrapped in the `.ptr-output` scope the bundled
#' JavaScript needs, with the show-code button wired to open/close and
#' drag the code window. This is exactly the block [ptr_app()] and
#' [ptr_module_ui()] render internally.
#'
#' Use this when you want the familiar toggle behaviour while still
#' owning the surrounding layout. For fully independent placement of the
#' plot and code panes (no toggle), use bare [ptr_ui_plot()] and
#' [ptr_ui_code()] instead — a standalone [ptr_ui_code()] is always
#' visible and needs no wiring.
#'
#' @param id Optional module id; the namespace prefix for the outputs.
#'   Defaults to `NULL` (identity namespace). When set, must match the
#'   `id` passed to [ptr_ui_controls()] and the server wiring.
#' @param plot_error If `TRUE` (default) the inline error slot is nested
#'   in the plot card body, matching the bundled layout. Set `FALSE` to
#'   place [ptr_ui_error()] yourself.
#'
#' @return A [shiny::tag].
#' @seealso [ptr_ui_plot()], [ptr_ui_code()], [ptr_outputs_ui()], [ptr_server()]
#' @export
ptr_ui_code_toggle <- function(id = NULL, plot_error = TRUE) {
  ns <- shiny::NS(id)
  shiny::tags$div(
    class = "ptr-output",
    plot_card_tag(ns, error = plot_error, code_toggle = TRUE),
    code_block_tag(ns, style = "window")
  )
}

ptr_build_app_ui <- function(tree, ui_text = NULL,
                                checkbox_defaults = NULL,
                                ns = shiny::NS(NULL),
                                render_shared_section = FALSE,
                                app_chrome = FALSE,
                                css = NULL) {
  # NOTE: ptr_layer_assets() (the layer-disabled CSS) must stay as the first
  # fluidPage child -- it is the regression guard for the layer cue.
  title <- ptr_resolve_ui_text("title", ui_text = ui_text)$label %||% ""
  header <- if (isTRUE(app_chrome)) {
    ptr_ui_header(if (nzchar(title)) title else "ggpaintr")
  } else if (nzchar(title)) {
    shiny::titlePanel(title)
  } else {
    NULL
  }
  shiny::fluidPage(
    ptr_assets(css = css),
    shiny::tags$div(
      class = "ptr-app",
      header,
      shiny::sidebarLayout(
        do.call(
          shiny::sidebarPanel,
          ptr_controls_panel(tree, ui_text = ui_text,
                             checkbox_defaults = checkbox_defaults,
                             ns = ns,
                             render_shared_section = render_shared_section)
        ),
        shiny::mainPanel(ptr_outputs_panel(ns))
      )
    )
  )
}

#' App Header Piece for `ggpaintr`
#'
#' The slim branded header bar (logo + title) the polished default shell
#' uses in place of `shiny::titlePanel()`. One of the single-piece UI
#' builders for the L3 "own every UI piece" workflow.
#'
#' @param title Heading text. Defaults to `"ggpaintr"`.
#'
#' @return A [shiny::tag].
#' @seealso [ptr_ui_controls()], [ptr_ui_plot()], [ptr_app()]
#' @export
ptr_ui_header <- function(title = "ggpaintr") {
  shiny::tags$header(
    class = "ptr-app__header",
    shiny::tags$img(
      class = "ptr-app__mark",
      src = "ggpaintr/ggpaintr-logo.png",
      alt = "ggpaintr"
    ),
    shiny::tags$h1(class = "ptr-app__title", title)
  )
}

# ---- Module variants ----

#' Module UI for a `ggpaintr` Formula
#'
#' Namespaced UI side of a Shiny module wrapping a `ggpaintr` formula. Pair
#' with [ptr_module_server()].
#'
#' @param id Module id; the namespace prefix for inputs and outputs.
#' @param formula A single formula string with `ggpaintr` placeholders.
#' @param ui_text Optional named list of copy overrides; see [ptr_ui_text()]
#'   for the full schema and current defaults.
#' @param checkbox_defaults Optional named list of initial checked states.
#' @param expr_check Controls `expr` placeholder validation. Defaults to `TRUE`.
#' @param css Optional character vector of paths to additional CSS files;
#'   linked after `ggpaintr`'s bundled stylesheet so its rules win. See
#'   [ptr_app()] for the full semantics. Defaults to `NULL`.
#'
#' @return A Shiny tag list.
#' @seealso [ptr_css()] for the `css =` argument and themable CSS custom properties.
#' @export
ptr_module_ui <- function(id, formula, ui_text = NULL,
                             checkbox_defaults = NULL, expr_check = TRUE,
                             css = NULL) {
  # Composition of the split-mode pair wrapped in the same chrome
  # `ptr_build_app_ui()` emits (fluidPage > div.ptr-app > sidebarLayout)
  # so the DOM, inputIds and outputIds the previous monolithic
  # implementation produced are preserved.
  #
  # `ptr_controls_ui()` already calls
  # `ptr_controls_panel(..., render_shared_section = FALSE)`, the key
  # property the shared-multi-instance design relies on: each module
  # suppresses its own inline shared section so `ptr_shared_ui()` owns
  # the page-level panel. The cosmetic cost of composing
  # `ptr_controls_ui()` + `ptr_outputs_ui()` here is one duplicate
  # `<link>`/`<script>` per module instance (both assets are idempotent
  # against the same href, so the browser dedupes them).
  title <- ptr_resolve_ui_text("title", ui_text = ui_text)$label %||% ""
  header <- if (nzchar(title)) shiny::titlePanel(title) else NULL
  shiny::fluidPage(
    shiny::tags$div(
      class = "ptr-app",
      header,
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          ptr_controls_ui(
            id = id, formula = formula,
            ui_text = ui_text,
            checkbox_defaults = checkbox_defaults,
            expr_check = expr_check,
            css = css
          )
        ),
        shiny::mainPanel(
          ptr_outputs_ui(id = id, css = css)
        )
      )
    )
  )
}

#' Controls Piece for a `ggpaintr` Formula
#'
#' The generated control widgets (layer picker, per-layer parameter
#' panels, the "Update plot" button) as a bare [shiny::tagList()] with
#' **no** `.ptr-app` wrapper and **no** bundled assets. One of the
#' single-piece UI builders for the L3 "own every UI piece" workflow:
#' compose it with [ptr_ui_assets()] and the output pieces, place each
#' wherever you like, and wire the server with [ptr_server()] /
#' [ptr_module_server()].
#'
#' Because the panel includes a `shinyWidgets::pickerInput()` (the layer
#' selector) and the Bootstrap grid, it must be rendered inside a
#' Bootstrap page that also carries the `.ptr-app` theme scope and the
#' asset bundle. Don't assemble that scaffolding by hand: wrap your
#' composed pieces in [ptr_ui_page()], which *is* the Bootstrap page and
#' owns the single `.ptr-app` scope + the (deduped) assets. For a
#' `navbarPage` or bslib root (which `ptr_ui_page()` does not cover) see
#' the decomposition recipe in `vignette("ggpaintr-use-cases")`.
#'
#' For finer control still — placing individual placeholder widgets
#' independently rather than the whole panel — use the exported
#' [build_ui_for()] generic on the nodes of `ptr_translate(formula)`.
#'
#' @param id Optional module id; the namespace prefix for inputs.
#'   Defaults to `NULL` (identity namespace). When set, must match the
#'   `id` passed to the other piece functions and the server wiring.
#' @param formula A single formula string with `ggpaintr` placeholders.
#' @param ui_text Optional named list of copy overrides; see
#'   [ptr_ui_text()] for the full schema and current defaults.
#' @param checkbox_defaults Optional named list of initial checked states.
#' @param expr_check Controls `expr` placeholder validation. Defaults to
#'   `TRUE`.
#' @param render_shared_section If `TRUE`, include the inline "Shared
#'   controls" section for any `shared = "..."` placeholders. Defaults to
#'   `FALSE` (the embedded default — use [ptr_shared_ui()] for the
#'   page-level shared panel).
#'
#' @return A [shiny::tagList()].
#' @seealso [ptr_ui_page()], [ptr_ui_assets()], [ptr_ui_plot()],
#'   [ptr_ui_code()], [build_ui_for()], [ptr_server()]
#' @export
ptr_ui_controls <- function(id = NULL, formula, ui_text = NULL,
                            checkbox_defaults = NULL, expr_check = TRUE,
                            render_shared_section = FALSE) {
  tree <- ptr_translate(formula, expr_check = expr_check)
  do.call(
    shiny::tagList,
    ptr_controls_panel(tree, ui_text = ui_text,
                       checkbox_defaults = checkbox_defaults,
                       ns = shiny::NS(id),
                       render_shared_section = render_shared_section)
  )
}

#' Control Widgets for an Embedded `ggpaintr` Formula
#'
#' UI fragment containing only the generated controls (layer picker, per-layer
#' parameter panels, the "Update plot" button) for a `ggpaintr` formula. Place
#' it anywhere in your app's layout; pair with [ptr_outputs_ui()] for the
#' plot/error/code panes and [ptr_server()] (or [ptr_module_server()]) for the
#' server logic. The fragment self-wraps in `<div class="ptr-app">` so the
#' bundled stylesheet attaches without any host-side scaffolding. This is a
#' thin convenience composite of [ptr_ui_assets()] + [ptr_ui_controls()].
#'
#' @param id Optional module id; the namespace prefix for inputs. Defaults to
#'   `NULL` (identity namespace) for the single-embedding case. When set,
#'   must match the `id` passed to [ptr_outputs_ui()] and the
#'   `shiny::moduleServer()` wrapping [ptr_server()] (or to
#'   [ptr_module_server()]).
#' @param formula A single formula string with `ggpaintr` placeholders.
#' @param ui_text Optional named list of copy overrides; see [ptr_ui_text()]
#'   for the full schema and current defaults.
#' @param checkbox_defaults Optional named list of initial checked states.
#' @param expr_check Controls `expr` placeholder validation. Defaults to `TRUE`.
#' @param css Optional character vector of paths to additional CSS files;
#'   linked after `ggpaintr`'s bundled stylesheet so its rules win. See
#'   [ptr_app()] for the full semantics. Defaults to `NULL`.
#'
#' @return A [shiny::tagList()].
#' @seealso [ptr_outputs_ui()], [ptr_module_ui()], [ptr_module_server()], [ptr_css()]
#' @export
ptr_controls_ui <- function(id = NULL, formula, ui_text = NULL,
                            checkbox_defaults = NULL, expr_check = TRUE,
                            css = NULL) {
  # The `.ptr-app` wrapper attaches the bundled stylesheet (scoped under
  # that class) when this composite is used standalone (L2). There is NO
  # `.ptr-app .ptr-app` neutralisation rule in ggpaintr.css; when this is
  # nested inside an outer `.ptr-app` (e.g. `ptr_module_ui()`'s wrap, or a
  # grid cell) the inner `.ptr-app` keeps `min-height:100vh`/background --
  # acceptable because it is a *descendant* inside a flowed layout cell,
  # not a vertical sibling. Only sibling stacking (N pieces each
  # self-wrapping) was the bug; the L3 single-shell model (`ptr_ui_page()`)
  # is what prevents that, not a CSS rule.
  shiny::tags$div(
    class = "ptr-app",
    ptr_ui_assets(css = css),
    ptr_ui_controls(
      id = id, formula = formula, ui_text = ui_text,
      checkbox_defaults = checkbox_defaults, expr_check = expr_check,
      render_shared_section = FALSE
    )
  )
}

#' Plot / Error / Code Panes for an Embedded `ggpaintr` Formula
#'
#' UI fragment containing only the plot, inline error, and generated-code
#' outputs for a `ggpaintr` formula. Pair with [ptr_controls_ui()] and
#' [ptr_server()] (or [ptr_module_server()]). The fragment self-wraps in
#' `<div class="ptr-app">` so the bundled stylesheet attaches without any
#' host-side scaffolding. This is a thin convenience composite of
#' [ptr_ui_assets()] + [ptr_ui_code_toggle()]; for fully independent
#' placement use the single pieces [ptr_ui_plot()], [ptr_ui_error()],
#' [ptr_ui_code()] directly.
#'
#' @param id Optional module id; the namespace prefix for outputs. Defaults
#'   to `NULL` (identity namespace) for the single-embedding case. When set,
#'   must match the `id` passed to [ptr_controls_ui()] and the
#'   `shiny::moduleServer()` wrapping [ptr_server()] (or to
#'   [ptr_module_server()]).
#' @param css Optional character vector of paths to additional CSS files;
#'   linked after `ggpaintr`'s bundled stylesheet so its rules win. See
#'   [ptr_app()] for the full semantics. Defaults to `NULL`.
#'
#' @return A [shiny::tagList()].
#' @seealso [ptr_ui_plot()], [ptr_ui_error()], [ptr_ui_code()],
#'   [ptr_ui_code_toggle()], [ptr_controls_ui()], [ptr_module_ui()], [ptr_css()]
#' @export
ptr_outputs_ui <- function(id = NULL, css = NULL) {
  shiny::tags$div(
    class = "ptr-app",
    ptr_ui_assets(css = css),
    ptr_ui_code_toggle(id = id)
  )
}

#' Page shell for hand-composed ggpaintr UIs
#'
#' Wraps composed L3 pieces in a Bootstrap page + the single `.ptr-app`
#' theme scope + the (deduped) asset bundle. The only thing an L3 user
#' must remember.
#'
#' @param ... UI children (pieces, layout, your own widgets).
#' @param page A Bootstrap-3 page builder whose `...` are tag children:
#'   `shiny::fluidPage` (default), `fixedPage`, `fillPage`,
#'   `bootstrapPage`, `basicPage`. NOT `navbarPage` (needs a positional
#'   `title` + `tabPanel` children) and NOT bslib/BS5 pages (the bundled
#'   CSS is Bootstrap-3-scoped -- see [ptr_app_bslib()]). For those roots,
#'   compose by hand: see the decomposition example in
#'   `vignette("ggpaintr-use-cases")`.
#' @param css Optional character vector of extra stylesheet paths,
#'   linked after `ggpaintr.css`. See [ptr_css()].
#' @seealso [ptr_ui_plot()], [ptr_ui_controls()], [ptr_server()], [ptr_css()]
#' @export
ptr_ui_page <- function(..., page = shiny::fluidPage, css = NULL) {
  assertthat::assert_that(is.function(page))
  page(
    ptr_assets(css = css),                       # internal bundle (htmlDependency)
    shiny::tags$div(class = "ptr-app", ...)
  )
}

#' Module Server for a `ggpaintr` Formula
#'
#' Namespaced server side of a Shiny module wrapping a `ggpaintr` formula.
#' Pair with [ptr_module_ui()]. Additional arguments are forwarded to
#' [ptr_init_state()] (e.g. `shared`, `draw_trigger`, `expr_check`,
#' `safe_to_remove`, `ui_text`, `checkbox_defaults`).
#'
#' When the formula declares any `shared = "..."` placeholder, pass the
#' `ptr_shared_state` returned by [ptr_shared_server()] as `shared_state =`.
#' The state's `shared` / `draw_trigger` / `shared_resolutions` slots are
#' unpacked and forwarded to [ptr_init_state()]; if an explicit `shared = ...`
#' / `draw_trigger = ...` / `shared_resolutions = ...` is also passed via
#' `...`, that explicit value wins.
#'
#' @param id Module id; must match the id passed to [ptr_module_ui()].
#' @param formula A single formula string with `ggpaintr` placeholders.
#' @param envir Environment used to resolve local data objects.
#' @param ... Forwarded to [ptr_init_state()].
#' @param shared_state Optional `ptr_shared_state` returned by
#'   [ptr_shared_server()]. When supplied, populates `shared`,
#'   `draw_trigger`, and `shared_resolutions` defaults. Required when the
#'   formula declares a `shared = "..."` placeholder and the equivalent
#'   `...` arguments are not supplied directly.
#'
#' @return The `ptr_state` list from [ptr_init_state()] (returned by the
#'   inner module session for advanced wiring; usually consumed for its
#'   side effects only).
#' @seealso [ptr_module_ui()], [ptr_shared_ui()], [ptr_shared_server()].
#' @export
ptr_module_server <- function(id, formula, envir = parent.frame(), ...,
                                 shared_state = NULL) {
  dots <- list(...)

  # `shared_state` is the convenience path: a one-shot bundle produced by
  # `ptr_shared_server()` carrying `shared`, `draw_trigger`,
  # `shared_resolutions`. Equivalent (and still public) escape hatch is to
  # pass each of those directly via `...`; explicit `...` entries win when
  # both are supplied.
  if (!is.null(shared_state)) {
    validate_ptr_shared_state(shared_state)
    if (is.null(dots$shared))             dots$shared <- shared_state$shared
    if (is.null(dots$draw_trigger))       dots$draw_trigger <- shared_state$draw_trigger
    if (is.null(dots$shared_resolutions)) dots$shared_resolutions <- shared_state$shared_resolutions
    if (is.null(dots$shared_stage_enabled)) dots$shared_stage_enabled <- shared_state$shared_stage_enabled
  }

  # Pre-flight contract checks: surface a clear, module-scoped message
  # *before* delegating to `ptr_server()`, whose generic validator can't
  # know that the embedder forgot a `shared_state` argument.
  pre_tree <- ptr_translate(formula, expr_check = FALSE)
  declared_value_keys <- vapply(
    collect_shared_placeholders(pre_tree), `[[`, character(1), "key"
  )
  declared_consumer_keys <- names(collect_shared_consumer_occurrences(pre_tree))
  declared_keys <- unique(c(declared_value_keys, declared_consumer_keys))

  if (length(declared_keys) > 0L &&
      is.null(shared_state) &&
      length(dots$shared %||% list()) == 0L &&
      length(dots$shared_resolutions %||% list()) == 0L) {
    rlang::abort(paste0(
      "`ptr_module_server()` was given a formula with shared placeholder",
      if (length(declared_keys) == 1L) "" else "s",
      " (",
      paste0("\"", declared_keys, "\"", collapse = ", "),
      "), but `shared_state = NULL` and no `shared = ...` was supplied. Build a `ptr_shared_state` with `ptr_shared_server()` and pass it as `shared_state =`."
    ))
  }

  # Soft warning: state is supplied but a formula key has no entry in it.
  # We fall through to `ptr_server()` with `auto_bind_shared = TRUE` so it
  # treats the missing entry as "host did not wire it" rather than aborting
  # with the strict-validator error.
  missing_keys <- setdiff(declared_keys, names(dots$shared %||% list()))
  if (length(missing_keys) > 0L &&
      length(dots$shared %||% list()) > 0L) {
    cli::cli_warn(c(
      "!" = "Formula for module {.val {id}} has shared key(s) not covered by {.code shared_state$shared}: {.val {missing_keys}}.",
      "i" = "Those pickers will read unbound inputs."
    ))
    if (is.null(dots$auto_bind_shared)) dots$auto_bind_shared <- TRUE
  }

  shiny::moduleServer(id, function(input, output, session) {
    # Two namespaces are needed under moduleServer:
    #   ns         = session$ns  -> wraps tag inputIds rendered server-side
    #                              via renderUI (Shiny does NOT auto-namespace
    #                              tag attributes emitted from dynamic output).
    #   server_ns  = NS(NULL)    -> identity for input[[]] / output[[]] /
    #                              updateXxx(session, id, ...) lookups, since
    #                              moduleServer's session already
    #                              auto-namespaces those keys.
    do.call(
      ptr_server,
      c(
        list(input = input, output = output, session = session,
             formula = formula, envir = envir,
             ns = session$ns, server_ns = shiny::NS(NULL)),
        dots
      )
    )
  })
}

# ---- Grid app: N plots + top-level shared widgets + draw-all ----

#' Grid App: Multiple `ggpaintr` Plots With Shared Controls
#'
#' Builds a fluid layout of N plot modules with a top-level `wellPanel` for
#' shared input widgets and a "Draw all" button that triggers a redraw across
#' every plot. Each plot's `shared = "..."` placeholders read from the
#' corresponding entry in `shared_ui` instead of rendering local widgets.
#'
#' For the formula grammar (placeholder keywords, `shared = "<id>"`
#' annotation, empty-call cleanup), see [ptr_app()].
#'
#' @param plots A list of formula strings, one per plot.
#' @param shared_ui Named list mapping shared key → `function(id) -> shiny.tag`
#'   builder. Names must match the `shared = "..."` annotations used in
#'   `plots`. Pass `list()` if there are no shared placeholders.
#' @param envir Environment used to resolve local data objects.
#' @param ui_text Optional named list of copy overrides. The page header
#'   reads `ui_text$shell$title$label`; defaults to `"ggpaintr grid"`. See
#'   [ptr_ui_text()] for the full schema.
#' @param draw_all_label Label for the draw-all action button.
#' @param expr_check Controls `expr` placeholder validation. Defaults to `TRUE`.
#' @param css Optional character vector of paths to additional CSS files,
#'   linked once at the page level after `ggpaintr`'s bundled stylesheet so
#'   its rules win. See [ptr_app()] for the full semantics. Defaults to `NULL`.
#'
#' @return A `shiny.appobj`.
#' @seealso [ptr_css()] for the `css =` argument and themable CSS custom properties.
#' @export
ptr_app_grid <- function(plots,
                            shared_ui = list(),
                            envir = parent.frame(),
                            ui_text = NULL,
                            draw_all_label = "Draw all",
                            expr_check = TRUE,
                            css = NULL) {
  parts <- ptr_app_grid_components(
    plots = plots,
    shared_ui = shared_ui,
    envir = envir,
    ui_text = ui_text,
    draw_all_label = draw_all_label,
    expr_check = expr_check,
    css = css
  )
  shiny::shinyApp(ui = parts$ui, server = parts$server)
}

ptr_app_grid_components <- function(plots,
                                       shared_ui = list(),
                                       envir = parent.frame(),
                                       ui_text = NULL,
                                       draw_all_label = "Draw all",
                                       expr_check = TRUE,
                                       css = NULL) {
  title <- ptr_resolve_ui_text("title", ui_text = ui_text)$label %||%
    "ggpaintr grid"
  if (is.character(plots)) plots <- as.list(plots)
  assertthat::assert_that(
    is.list(plots),
    length(plots) >= 1L,
    all(vapply(plots, rlang::is_string, logical(1)))
  )
  assertthat::assert_that(is.list(shared_ui))

  # The grid delegates its shared-widget panel + server-side reactives to
  # `ptr_shared_ui()` / `ptr_shared_server()` (PR-B of the
  # shared-multi-instance plan). This consolidates the inline shared
  # panel and the inline `ptr_bind_shared_consumer_uis()` wiring that
  # used to live here. The widget inputIds therefore migrate from the
  # bare-key form (e.g. `"col"`) to `canonical_shared_id("col")` = `"shared_col"`,
  # matching the single-instance `ptr_app()` convention.
  trees <- lapply(plots, ptr_translate, expr_check = expr_check)
  any_shared <- any(vapply(trees, function(tr) {
    length(collect_shared_placeholders(tr)) > 0L ||
      length(collect_shared_consumer_occurrences(tr)) > 0L
  }, logical(1)))

  if (!any_shared && length(shared_ui) > 0L) {
    rlang::abort(
      "`shared_ui` was supplied but no plot formula declares a `shared = \"...\"` annotation."
    )
  }

  shared_panel <- if (any_shared) {
    # Leave `css = NULL` here: the grid page already injects the user
    # stylesheet at fluidPage level via ptr_assets(css = css). Threading
    # `css = css` through would emit the same <link> twice on this page.
    ptr_shared_ui(
      formulas = plots,
      shared_ui = shared_ui,
      expr_check = expr_check,
      draw_all_label = draw_all_label
    )
  } else NULL

  plot_module_ids <- paste0("plot_", seq_along(plots))
  n_plots <- length(plots)
  col_width <- max(1L, 12L %/% n_plots)

  plot_columns <- do.call(
    shiny::fluidRow,
    lapply(seq_along(plots), function(i) {
      shiny::column(
        width = col_width,
        ptr_module_ui(plot_module_ids[[i]], plots[[i]], expr_check = expr_check)
      )
    })
  )

  ui <- shiny::fluidPage(
    ptr_assets(css = css),
    shiny::tags$div(
      class = "ptr-app",
      ptr_ui_header(title),
      shared_panel,
      plot_columns
    )
  )

  force(plots)
  force(envir)
  force(expr_check)

  server <- function(input, output, session) {
    state <- if (any_shared) {
      ptr_shared_server(
        formulas = plots, envir = envir, expr_check = expr_check
      )
    } else NULL

    for (i in seq_along(plots)) {
      local({
        idx <- i
        args <- list(
          plot_module_ids[[idx]],
          formula = plots[[idx]],
          envir = envir,
          expr_check = expr_check,
          plots = plots
        )
        if (!is.null(state)) args$shared_state <- state
        do.call(ptr_module_server, args)
      })
    }
  }

  list(ui = ui, server = server)
}
