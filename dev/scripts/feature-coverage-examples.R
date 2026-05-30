# ============================================================================
# feature-coverage-examples.R
#
# Two deliberately complex ggpaintr examples that, between them, exercise every
# documented feature of the package. Companion to dev/scripts/feature-sweep.R.
#
# Usage:
#   devtools::load_all(".")
#   source("dev/scripts/feature-coverage-examples.R", echo = TRUE)  # static + headless
#   # then highlight a single `ptr_app*()` / `shinyApp(...)` line below the
#   # `if (interactive())` guard and send it to your R session to launch one
#   # app at a time.
#
# Feature map
# -----------
# Example 1 (`ex1_*`) -- maximal turn-key formula:
#   * built-in keywords var / text / num / expr, plus a custom `pick_ds` *source*
#     in the data-source slot (the `upload` keyword itself is exercised by
#     Example 3 below)
#   * all 3 custom-placeholder constructors:
#       ptr_define_placeholder_value()    -> `range` (non-data widget)
#       ptr_define_placeholder_consumer() -> `colvars` (+ optional validate_input hook)
#       ptr_define_placeholder_source()   -> `pick_ds` (produces a data frame;
#                                            both `resolve_data` AND `resolve_expr`
#                                            hooks overridden)
#   * `resolve_expr()` / `resolve_data()` returning NULL == "missing" -> arg dropped
#   * `copy_defaults` with `{param}` interpolation
#   * process-global registry (re-sourcing warns + overwrites the keyword)
#   * `shared = "<id>"` annotation on a built-in keyword (`num(shared = 'lvl')`)
#   * pipeline-into-ggplot with placeholder-bearing stages -> per-stage Data
#     sub-tab + per-stage enable/disable
#   * per-geom-layer enable/disable checkbox
#   * formula-level transforms wrapping `var` (`var + log(var)`, `var > num`)
#   * layer-level `data = <local frame>` literal (resolved against `envir`)
#   * `ptr_normalize_column_names()` for non-syntactic local data
#   * empty-placeholder cleanup of curated removable calls (`facet_wrap`, `labs`)
#   * `safe_to_remove =` (opt a 3rd-party function into the cleanup pass)
#   * every `ptr_app()` argument: envir, ui_text (via ptr_ui_text(), all 6
#     sections + every `shell` sub-key + both `upload` sub-keys + every leaf
#     field including `empty_text` + the `__unnamed__` positional key),
#     expr_check, safe_to_remove, css. (ADR 0020 removed `checkbox_defaults`
#     and the `checkbox_default_all_other_layer` option; use `ppLayerOff()`
#     in the formula instead.)
#   * every `ptr_app_bslib()` argument including the bslib-only `theme` / `title`
#
# Example 2 (`ex2_*`) -- everything that is not a single `ptr_app()` call:
#   * `ptr_app_grid()` + `shared_ui` + draw-all + `title` / `draw_all_label` / `css`
#   * split-UI embed: `ptr_controls_ui()` + `ptr_outputs_ui(css=)` + `ptr_module_server()`
#   * Shiny-module embed: `ptr_module_ui()` with every arg (id, formula, ui_text,
#     expr_check, css) + `ptr_module_server()` returning state,
#     a user-owned output pane fed by the reactive `state$runtime()`, and `ptr_gg_extra()`
#   * granular wiring: `ptr_init_state()` + `ptr_setup_*()` (internal) +
#     `ptr_register_plot()` / `ptr_register_error()` / `ptr_register_code()`
#   * headless extraction via `shiny::testServer()` + `ptr_server()` +
#     `ptr_extract_plot()` / `ptr_extract_code()` / `ptr_extract_error()`
#   * pure-headless helpers (no Shiny session): `ptr_run_formula()` one-shot
#     + `ptr_translate()` + `ptr_default_snapshot()` + `ptr_exec_headless()`
#   * `ptr_runtime_input_spec()` on a parsed tree
#   * `ptr_resolve_ui_text()` lookup across every documented `component` value
#     (title / draw_button / draw_all_button / layer_picker / data_subtab /
#      controls_subtab / upload_file / upload_name / layer_checkbox / control)
#   * `ptr_options()` read / set / round-trip
#   * `ptr_llm_primer()` / `ptr_llm_topics()` / `ptr_llm_topic()` /
#     `ptr_llm_register(chat, tool_name = ...)`
#   * `expr_check` in every form: TRUE / FALSE / list(deny_list=) /
#     list(allow_list=) (translate-time block)
#   * `shared = "<id>"` on every value-placeholder kind (num, text, expr) plus
#     consumer kinds (var, custom `colvars`)
#   * `ptr_clear_placeholder()` tear-down (final commented block)
#
# Example 3 (`ex3_*`) -- the original probe formula, verbatim:
#   * the `upload` keyword end-to-end (used as pipeline head AND `data = upload`
#     on a layer), `var(shared = ...)`, `var`-as-call, bare dplyr verbs in the
#     pipeline. Launch is plain `ptr_app(ex3_formula)`; upload a data file at runtime.
# ============================================================================

devtools::load_all(".", export_all = FALSE)
library(shiny)
library(ggplot2)
library(dplyr)

# Bind every ggpaintr name (exported + internal) into the global env so S3
# dispatch through internal generics (prune_walk / render_walk / classify_walk
# / build_ui_for / ...) resolves to the namespace copies, and the internal
# `ptr_setup_*()` / `ptr_translate()` / `ptr_render()` helpers used below are
# visible. Same trick as feature-sweep.R and the gallery vignette.
local({
  ns <- asNamespace("ggpaintr")
  for (nm in ls(ns, all.names = FALSE)) {
    assign(nm, get(nm, envir = ns), envir = globalenv())
  }
})

# ----------------------------------------------------------------------------
# Custom placeholders -- one per constructor. Registration is process-global;
# re-sourcing this file emits a "duplicate keyword" warning and overwrites the
# previous entry (a documented behaviour, demonstrated here by re-source).
# ----------------------------------------------------------------------------

# (a) ptr_define_placeholder_value -- non-data-aware widget.
ptr_define_placeholder_value(
  keyword       = "range",
  build_ui      = function(node, label = NULL, ...) {
    sliderInput(node$id, label = label %||% "Range",
                min = 0, max = 100, value = c(0, 100), step = 0.1)
  },
  resolve_expr  = function(value, node, ...) {
    if (is.null(value) || length(value) != 2L) return(NULL)   # NULL == "missing"
    rlang::expr(c(!!value[1], !!value[2]))
  },
  ui_text_defaults = list(label = "Range for {param}")           # {param} interpolation
)

# (b) ptr_define_placeholder_consumer -- data-aware; receives the upstream
#     column-name vector as `cols`. Also wires the optional `validate_input`
#     hook (rejects unknown column names against `upstream_cols`).
ptr_define_placeholder_consumer(
  keyword        = "colvars",
  build_ui       = function(node, cols = character(), label = NULL,
                            selected = character(0), ...) {
    selectInput(node$id, label = label %||% "Columns",
                choices = cols, selected = intersect(selected, cols),
                multiple = TRUE)
  },
  resolve_expr   = function(value, node, ...) {
    if (length(value) == 0L) return(NULL)
    rlang::call2("c", !!!as.list(value))
  },
  validate_input = function(value, upstream_cols, ...) {
    bad <- setdiff(value, upstream_cols)
    if (length(bad)) sprintf("unknown column(s): %s", paste(bad, collapse = ", "))
    else NULL
  },
  ui_text_defaults  = list(label = "Columns for {param}")
)

# (c) ptr_define_placeholder_source -- data-aware; PRODUCES a data frame.
#     `resolve_data` returns the frame; `resolve_expr` is explicitly
#     overridden (rather than left at the constructor's default
#     `rlang::sym(value)`) to demonstrate the hook + gate the NULL
#     "missing" return when no dataset is picked yet.
ptr_define_placeholder_source(
  keyword       = "pick_ds",
  build_ui      = function(node, label = NULL, ...) {
    selectInput(node$id, label = label %||% "Dataset",
                choices = c("mpg", "diamonds", "midwest", "economics"))
  },
  resolve_data  = function(value, node, ...) {
    if (is.null(value) || !nzchar(value)) return(NULL)
    get(value, envir = asNamespace("ggplot2"))
  },
  resolve_expr  = function(value, node, ...) {
    if (is.null(value) || !nzchar(value)) return(NULL)  # NULL == "missing"
    rlang::sym(value)
  },
  ui_text_defaults = list(label = "Dataset for {param}")
)

# A small user stylesheet on disk -- `css =` takes file path(s) to `.css` files
# (not inline CSS), and is wired on every UI entry point.
ex_css <- tempfile("ggpaintr-demo", fileext = ".css")
writeLines(
  c("body { font-family: ui-monospace, monospace; }",
    ".well { background: #fafbfc; }",
    ".container-fluid { padding: 1rem; }"),
  ex_css
)

# ============================================================================
# Example 1 -- maximal turn-key formula
# ============================================================================

# A local data frame with non-syntactic column names; normalize once before a
# `data = <this frame>` reference inside the formula can see syntactic columns.
ex1_local <- ptr_normalize_column_names(
  data.frame(`Eng L`   = mpg$displ,
             `Hwy MPG`  = mpg$hwy,
             drv        = mpg$drv,
             check.names = FALSE)
)
# columns after normalization: Eng.L, Hwy.MPG, drv

ex1_formula <- "
  pick_ds |>
    head(num) |>
    dplyr::select(colvars) |>
    dplyr::mutate(metric = var + log(var)) |>
    dplyr::filter(var > num) |>
    ggplot(aes(x = var, y = var, color = var)) +
    geom_point(alpha = num(shared = 'lvl'), size = num(shared = 'lvl')) +
    geom_smooth(method = expr, se = FALSE) +
    geom_line(data = pick_ds, aes(x = var, y = var)) +
    facet_wrap(expr) +
    coord_cartesian(xlim = range, ylim = range) +
    labs(title = text, x = text)
"

# Copy overrides touching all six `ui_text` sections, every `shell` sub-key,
# both `upload` sub-keys, every leaf field (`label` / `help` / `placeholder` /
# `empty_text`), and the `__unnamed__` positional-argument key (facet_wrap
# registers copy under it).
ex1_ui_text <- ptr_ui_text(list(
  shell          = list(title           = list(label = "Feature-coverage demo"),
                        draw_button     = list(label = "Render plot"),
                        draw_all_button = list(label = "Render all"),
                        layer_picker    = list(label = "Pick a layer"),
                        data_subtab     = list(label = "Data inputs"),
                        controls_subtab = list(label = "Plot controls")),
  upload         = list(file = list(label = "Upload data file"),
                        name = list(label = "Name this dataset")),
  layer_checkbox = list(label = "Include this layer in the plot"),
  defaults       = list(num   = list(help       = "Any number works here.",
                                     empty_text = "No number yet"),
                        range = list(label = "A 0-100 range for {param}")),
  params         = list(title = list(text = list(label = "Plot title"))),
  layers         = list(facet_wrap = list(expr = list(`__unnamed__` =
                          list(label = "Facet by", placeholder = "~ drv"))))
))

# --- static checks: parsed tree + widget spec --------------------------------
ex1_tree <- ptr_translate(ex1_formula, expr_check = TRUE)
cat("EX1 layers:",
    paste(vapply(ex1_tree$layers, `[[`, character(1), "name"), collapse = " | "),
    "\n")
print(ptr_runtime_input_spec(ex1_tree))

# --- the live apps (run one at a time) ---------------------------------------
if (interactive()) {

  # 1a. ptr_app() with every argument it accepts. `safe_to_remove = "pcp_theme"`
  #     opts a (hypothetical) third-party theme helper into the empty-call
  #     cleanup; harmless if the formula never uses it. ADR 0020 removed the
  #     per-call `checkbox_defaults =` argument; to start a layer off, wrap it
  #     in `ppLayerOff(layer(), TRUE)` inside the formula instead.
  ptr_app(
    ex1_formula,
    envir             = environment(),
    ui_text           = ex1_ui_text,
    expr_check        = TRUE,                       # denylist active on `expr` inputs
    safe_to_remove    = "pcp_theme",
    css               = ex_css
  )

  # 1b. ptr_app_bslib() -- same formula, bslib shell, bslib-only `theme`/`title`.
  ptr_app_bslib(
    ex1_formula,
    envir             = environment(),
    ui_text           = ex1_ui_text,
    expr_check        = TRUE,
    safe_to_remove    = "pcp_theme",
    theme             = bslib::bs_theme(version = 5, bootswatch = "minty"),
    title             = "Feature-coverage demo (bslib)"
  )

}

# ============================================================================
# Example 3 -- the original "upload" probe formula
# ============================================================================
# Self-contained copy of the probe formula this script was written to extend:
# it is the one example here that exercises the `upload` keyword end-to-end
# (the `upload` data source appears twice -- once at the head of the pipeline,
# once as `data = upload` on a layer; a single uploaded file feeds both). Also
# covers `var(shared = ...)` (shared annotation on `var`, not just `num`),
# bare `select`/`mutate`/`filter` (no `dplyr::` prefix -- relies on `library(dplyr)`),
# and `var`-as-call with build-ui args (`var(shared = 'v')`).

ex3_formula <- "upload |>
  head(num) |>
  select(colvars) |>
  mutate(new_var = var + var) |>
  filter(var > num) |>
  ggplot(aes(x = var, y = var, color = var)) +
  geom_point(alpha = var(shared = 'v'), size = var(shared = 'v')) +
  geom_line(data = upload, aes(x = var, y = var)) +
  facet_wrap(expr) +
  labs(title = text)"

# --- static checks -----------------------------------------------------------
ex3_tree <- ptr_translate(ex3_formula, expr_check = TRUE)
cat("EX3 layers:",
    paste(vapply(ex3_tree$layers, `[[`, character(1), "name"), collapse = " | "),
    "\n")
print(ptr_runtime_input_spec(ex3_tree))

# --- the live app (upload a .csv/.tsv/.rds/.xlsx/.xls/.json at runtime) -------
if (interactive()) {
  ptr_app(ex3_formula)   # default envir/ui_text/expr_check; pure turn-key
}

# ============================================================================
# Example 2 -- embed / extract / grid / options / llm
# ============================================================================

ex2_simple  <- "ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point(size = num) + labs(title = text)"
ex2_grid_a  <- "ggplot(iris |> dplyr::select(Species, colvars(shared = 'cv')),
                       aes(x = Species, y = Sepal.Length, fill = Species)) + geom_boxplot()"
ex2_grid_b  <- "ggplot(iris |> dplyr::select(Species, colvars(shared = 'cv')),
                       aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
                  geom_point(alpha = num(shared = 'pt'))"

# --- ptr_runtime_input_spec() on a parsed tree (no Shiny session needed) -----
print(ptr_runtime_input_spec(ptr_translate(ex2_simple)))

# --- ptr_resolve_ui_text() lookup -------------------------------------------
# `control` (placeholder leaf) takes the full coordinates (keyword/param/layer).
print(ptr_resolve_ui_text(
  "control", keyword = "text", param = "title", layer_name = "labs",
  ui_text = list(params = list(title = list(text = list(label = "My title"))))
))
# Every other documented `component` value resolves shell-level labels that
# do not need a placeholder coordinate. Iterate them all so the resolver's
# shell/upload/layer-checkbox branches are exercised.
for (comp in c("title", "draw_button", "draw_all_button",
               "layer_picker", "data_subtab", "controls_subtab",
               "upload_file", "upload_name", "layer_checkbox")) {
  res <- ptr_resolve_ui_text(comp)
  cat(sprintf("EX2 resolve_ui_text[%s]$label = %s\n",
              comp, res$label %||% "<unset>"))
}

# --- ptr_options(): read / set / round-trip ---------------------------------
# ADR 0020 removed `checkbox_default_all_other_layer`; `verbose` is the only
# remaining setting.
ex2_old_opts <- ptr_options(verbose = TRUE)
print(ptr_options())                 # current values
do.call(ptr_options, ex2_old_opts)   # restore previous values

# --- expr denylist + expr_check toggle (translate-time) ----------------------
tryCatch(
  ptr_translate("ggplot(mtcars) + geom_point() + labs(title = system('id'))"),
  error = function(e) message("denylist blocked at translate: ", conditionMessage(e))
)
# expr_check = FALSE bypasses the guard entirely (the runtime widget then also
# skips the check). Only do this with fully trusted formula strings.
invisible(ptr_translate("ggplot(mtcars) + geom_point() + labs(title = system('id'))",
                        expr_check = FALSE))
# expr_check also accepts a list: `deny_list =` swaps in a custom denylist,
# `allow_list =` switches to strict whitelist mode (only those names allowed).
invisible(ptr_translate("ggplot(mtcars) + geom_point()",
                        expr_check = list(deny_list = c("system", "eval", "parse"))))
tryCatch(
  ptr_translate("ggplot(mtcars) + geom_point() + labs(title = eval(1))",
                expr_check = list(deny_list = "eval")),
  error = function(e) message("custom deny_list blocked: ", conditionMessage(e))
)
# `allow_list =` switches the safety walker to strict whitelist mode -- ONLY
# names in the list are permitted; combine with `deny_list =` to whitelist
# minus a few names.
invisible(ptr_translate(
  "ggplot(mtcars) + geom_point() + labs(title = log(1))",
  expr_check = list(allow_list = c("ggplot", "geom_point", "labs",
                                   "aes", "log", "+"))
))
tryCatch(
  ptr_translate("ggplot(mtcars) + geom_point() + labs(title = system('id'))",
                expr_check = list(allow_list = c("ggplot", "geom_point", "labs"))),
  error = function(e) message("allow_list blocked: ", conditionMessage(e))
)

# --- shared = on every value-placeholder kind (`text`, `expr`, in addition to
# `num` / `var` / `colvars` already exercised above). Translate-time check
# (the live grid app in Example 2a wires `num`/`colvars` shared widgets).
print(ptr_runtime_input_spec(ptr_translate(
  "ggplot(mtcars) + geom_point() + labs(title = text(shared = 'st'), subtitle = text(shared = 'st')) + facet_wrap(expr(shared = 'fc'))",
  expr_check = FALSE
)))

# --- granular server wiring: ptr_init_state() + ptr_setup_*() + ptr_register_*()
#     This is exactly what ptr_server() does internally; spelled out here to
#     exercise the individually-exported register helpers.
shiny::testServer(function(input, output, session) {
  st <- ptr_init_state(ex2_simple, envir = environment())
  ptr_setup_producer_inputs(st, input, output, session)
  ptr_setup_pipelines(st, input, output, session)
  ptr_setup_stage_enabled(st, input, output, session)
  ptr_setup_runtime(st, input, output, session)
  ptr_setup_consumer_uis(st, input, output, session)
  ptr_setup_layer_picker(st, input, output, session)
  ptr_setup_layer_panel_classes(st, input, output, session)
  ptr_register_plot(output, st)
  ptr_register_error(output, st)
  ptr_register_code(output, st)
  session$setInputs(ptr_update_plot = 1, .dummy = 1)
  session$userData$st <- st
}, {
  cat("EX2 manual-wired code:\n", ptr_extract_code(session$userData$st), "\n")
})

# --- headless extraction via ptr_server() + ptr_extract_*() ------------------
shiny::testServer(function(input, output, session) {
  st <- ptr_server(input, output, session, ex2_simple)
  session$setInputs(ptr_update_plot = 1, .dummy = 1)
  session$userData$st <- st
}, {
  cat("EX2 ptr_server code:\n", ptr_extract_code(session$userData$st), "\n")
  cat("EX2 plot class:", class(ptr_extract_plot(session$userData$st)), "\n")
  cat("EX2 error:", ptr_extract_error(session$userData$st) %||% "<none>", "\n")
})

# --- pure headless: no Shiny session at all ----------------------------------
# `ptr_run_formula()` is the one-shot helper; `ptr_exec_headless()` is the
# inner step that takes an already-parsed tree + a value snapshot, and
# `ptr_default_snapshot()` produces the snapshot from the tree's input spec.
ex2_run <- ptr_run_formula(ex2_simple, inputs = list(), envir = environment())
cat("EX2 ptr_run_formula ok:", ex2_run$ok, "\n")
cat("EX2 ptr_run_formula code:\n", ex2_run$code_text, "\n")
ex2_tree <- ptr_translate(ex2_simple)
ex2_snap <- ptr_default_snapshot(ptr_runtime_input_spec(ex2_tree), ex2_tree)
ex2_exec <- ptr_exec_headless(ex2_tree, ex2_snap, eval_env = environment())
cat("EX2 ptr_exec_headless ok:", ex2_exec$ok, "\n")

# --- LLM helpers -------------------------------------------------------------
cat("EX2 llm topics:", paste(ptr_llm_topics(), collapse = ", "), "\n")
invisible(ptr_llm_primer())
invisible(ptr_llm_topic(ptr_llm_topics()[[1L]]))
if (requireNamespace("ellmer", quietly = TRUE)) {
  tryCatch(
    # `tool_name =` renames the registered tool; the default is "ggpaintr_help".
    ptr_llm_register(ellmer::chat_openai(), tool_name = "ggpaintr_help"),
    error = function(e) message("ptr_llm_register needs a configured ellmer Chat: ",
                                conditionMessage(e))
  )
}

# --- the live embed apps (run one at a time) ---------------------------------
if (interactive()) {

  # 2a. ptr_app_grid(): two plots, one shared multi-select at the top, draw-all.
  ptr_app_grid(
    plots          = list(ex2_grid_a, ex2_grid_b),
    shared_ui      = list(
      cv = function(id) selectInput(id, "Columns to plot",
                                    choices  = setdiff(names(iris), "Species"),
                                    selected = c("Sepal.Length", "Sepal.Width"),
                                    multiple = TRUE),
      pt = function(id) sliderInput(id, "Point alpha", 0, 1, value = 0.6, step = 0.05)
    ),
    envir          = environment(),
    title          = "Two iris views, shared column pick",
    draw_all_label = "Render both",
    expr_check     = TRUE,
    css            = ex_css
  )

  # 2b. Split-UI embed: ptr_controls_ui() + ptr_outputs_ui() share one id with
  #     ptr_module_server(), which binds purely by id.
  ui_split <- fluidPage(
    fluidRow(
      column(4, ptr_controls_ui("embed", ex2_simple,
                                ui_text = list(shell = list(title = list(label = "Embedded"))),
                                css = ex_css)),
      column(8, ptr_outputs_ui("embed", css = ex_css))
    )
  )
  server_split <- function(input, output, session) {
    ptr_module_server(ex2_simple, "embed")
  }
  shinyApp(ui_split, server_split)

  # 2c. Module embed with a user-owned output pane + ptr_gg_extra().
  ui_mod <- fluidPage(
    actionButton("toggle_log", "Toggle log-x"),
    fluidRow(
      column(5, ptr_module_ui(ex2_simple, "m",
                              ui_text = list(shell = list(title = list(label = "Module"))),
                              expr_check = TRUE,
                              css = ex_css)),
      column(7, plotOutput("my_plot", height = "420px"))
    )
  )
  server_mod <- function(input, output, session) {
    state <- ptr_module_server(ex2_simple, "m")
    output$my_plot <- renderPlot({
      # Read `state$runtime()` directly (NOT `ptr_extract_plot()`, which is
      # `isolate()`-wrapped for non-reactive callers) so this pane re-renders
      # on every "Update plot" click. Same pattern as the plotly/ggiraph
      # recipes in vignette("ggpaintr-gallery").
      res <- state$runtime()
      req(isTRUE(res$ok), res$plot)
      res$plot + ggplot2::theme_minimal()
    })
    observeEvent(input$toggle_log, {
      ptr_gg_extra(state, ggplot2::scale_x_log10())  # replaces extras each call
    })
  }
  shinyApp(ui_mod, server_mod)

}

# ----------------------------------------------------------------------------
# Tear-down: ptr_clear_placeholder() rolls back a process-global keyword.
# Uncomment to roll all three custom keywords back to "unregistered" so the
# next `source()` does not emit "duplicate keyword" warnings; commented out
# by default because the live apps above re-use them.
# ----------------------------------------------------------------------------
# ptr_clear_placeholder("range")
# ptr_clear_placeholder("colvars")
# ptr_clear_placeholder("pick_ds")

# ----------------------------------------------------------------------------
# Coverage notes
# ----------------------------------------------------------------------------
# Not mechanically reachable from a script:
#   * the *runtime* denylist rejection -- a user pasting e.g. `system(...)` into
#     an `expr` widget. The translate-time block above is the static analogue
#     for the `expr_check` denylist + allowlist forms.
#   * `ptr_define_placeholder_source(companion_id_fn = ...)` -- the companion-id
#     pattern for a "dataset name" sidecar input; see `ptr_builtin_upload_build_ui`.
#   * real file uploads of every accepted format (.csv/.tsv/.rds/.xlsx/.xls/.json)
#     -- exercised via the `upload` keyword in the basic probe formula + manual
#     tests under tests/manual/.
#   * `ptr_register_builtins()` -- auto-called from `.onLoad`; not meaningful
#     to invoke from a user script.
#   * `ptr_init_state(producer_debounce_ms / shared_resolutions / auto_bind_shared
#     / plots / server_ns)` -- advanced args exercised transitively by
#     `ptr_app_grid()` and `ptr_app()`; a direct script invocation would be
#     artificial and not user-facing.
# ----------------------------------------------------------------------------
