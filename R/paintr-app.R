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
#'   keywords; [ptr_ui_text()] for copy overrides;
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

# Output contents for an embedded formula: plot + error + code, in that DOM
# order. The ids are the contract ptr_register_plot()/_error()/_code() write to.
ptr_outputs_panel <- function(ns = shiny::NS(NULL)) {
  code_icon <- shiny::HTML(
    '<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.2" stroke-linecap="round" stroke-linejoin="round"><path d="m18 16 4-4-4-4"></path><path d="m6 8-4 4 4 4"></path><path d="m14.5 4-5 16"></path></svg>'
  )
  shiny::tags$div(
    class = "ptr-output",
    shiny::tags$div(
      class = "ptr-card ptr-card--plot",
      shiny::tags$div(
        class = "ptr-card__head",
        shiny::tags$h3(class = "ptr-card__title", "Plot"),
        shiny::tags$button(
          type = "button",
          class = "ptr-icon-btn ptr-code-toggle",
          title = "Show generated code",
          `aria-label` = "Show generated code",
          code_icon
        )
      ),
      shiny::tags$div(
        class = "ptr-card__body",
        shiny::plotOutput(ns("ptr_plot")),
        shiny::uiOutput(ns("ptr_error"))
      )
    ),
    shiny::tags$div(
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
    )
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
    ptr_app_header(if (nzchar(title)) title else "ggpaintr")
  } else if (nzchar(title)) {
    shiny::titlePanel(title)
  } else {
    NULL
  }
  shiny::fluidPage(
    ptr_layer_assets(),
    ptr_ui_assets(),
    ptr_user_css_assets(css),
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

# Slim header bar that replaces titlePanel() on the polished default shell.
ptr_app_header <- function(title) {
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

#' Control Widgets for an Embedded `ggpaintr` Formula
#'
#' UI fragment containing only the generated controls (layer picker, per-layer
#' parameter panels, the "Update plot" button) for a `ggpaintr` formula. Place
#' it anywhere in your app's layout; pair with [ptr_outputs_ui()] for the
#' plot/error/code panes and [ptr_module_server()] for the server logic. `id`
#' must match the `id` passed to [ptr_module_server()] / [ptr_outputs_ui()].
#'
#' @param id Module id; the namespace prefix for inputs. Must match the `id`
#'   passed to [ptr_module_server()] and [ptr_outputs_ui()].
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
#' @seealso [ptr_outputs_ui()], [ptr_module_ui()], [ptr_module_server()]
#' @export
ptr_controls_ui <- function(id, formula, ui_text = NULL,
                            checkbox_defaults = NULL, expr_check = TRUE,
                            css = NULL) {
  tree <- ptr_translate(formula, expr_check = expr_check)
  # ptr_layer_assets() + ptr_ui_assets() must ride along: ptr_build_app_ui()
  # injects them at fluidPage level, but in the split layout there is no
  # combiner -- without them the layer-disabled CSS cue and the bundled
  # ggpaintr.css/code-window JS silently go missing. The controls side owns
  # them since the layer panels live here. Harmless if included twice on a
  # page (both are idempotent / dedupe to the same href).
  do.call(
    shiny::tagList,
    c(
      list(ptr_layer_assets(), ptr_ui_assets(), ptr_user_css_assets(css)),
      ptr_controls_panel(tree, ui_text = ui_text,
                         checkbox_defaults = checkbox_defaults,
                         ns = shiny::NS(id), render_shared_section = FALSE)
    )
  )
}

#' Plot / Error / Code Panes for an Embedded `ggpaintr` Formula
#'
#' UI fragment containing only the plot, inline error, and generated-code
#' outputs for a `ggpaintr` formula. Pair with [ptr_controls_ui()] and
#' [ptr_module_server()]. `id` must match the `id` passed to those.
#'
#' @param id Module id; the namespace prefix for outputs. Must match the `id`
#'   passed to [ptr_controls_ui()] and [ptr_module_server()].
#' @param css Optional character vector of paths to additional CSS files;
#'   linked after `ggpaintr`'s bundled stylesheet so its rules win. See
#'   [ptr_app()] for the full semantics. Defaults to `NULL`.
#'
#' @return A [shiny::tagList()].
#' @seealso [ptr_controls_ui()], [ptr_module_ui()], [ptr_module_server()]
#' @export
ptr_outputs_ui <- function(id, css = NULL) {
  shiny::tagList(
    ptr_ui_assets(),
    ptr_user_css_assets(css),
    ptr_outputs_panel(shiny::NS(id))
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
#' @param title App title shown in the page header.
#' @param draw_all_label Label for the draw-all action button.
#' @param expr_check Controls `expr` placeholder validation. Defaults to `TRUE`.
#' @param css Optional character vector of paths to additional CSS files,
#'   linked once at the page level after `ggpaintr`'s bundled stylesheet so
#'   its rules win. See [ptr_app()] for the full semantics. Defaults to `NULL`.
#'
#' @return A `shiny.appobj`.
#' @export
ptr_app_grid <- function(plots,
                            shared_ui = list(),
                            envir = parent.frame(),
                            title = "ggpaintr grid",
                            draw_all_label = "Draw all",
                            expr_check = TRUE,
                            css = NULL) {
  parts <- ptr_app_grid_components(
    plots = plots,
    shared_ui = shared_ui,
    envir = envir,
    title = title,
    draw_all_label = draw_all_label,
    expr_check = expr_check,
    css = css
  )
  shiny::shinyApp(ui = parts$ui, server = parts$server)
}

ptr_app_grid_components <- function(plots,
                                       shared_ui = list(),
                                       envir = parent.frame(),
                                       title = "ggpaintr grid",
                                       draw_all_label = "Draw all",
                                       expr_check = TRUE,
                                       css = NULL) {
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
    ptr_layer_assets(),
    ptr_ui_assets(),
    ptr_user_css_assets(css),
    shiny::tags$div(
      class = "ptr-app",
      ptr_app_header(if (nzchar(title)) title else "ggpaintr grid"),
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
