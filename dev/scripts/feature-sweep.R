# Feature sweep for the typed-AST core (post-4c cutover).
#
# Usage:
#   devtools::load_all(".")
#   source("dev/scripts/feature-sweep.R", echo = TRUE)  # static checks only
#
# The interactive app sections at the bottom run only under `interactive()`.
# Highlight + send a single `ptr_app(...)` line to your R session to launch
# one app at a time.
devtools::load_all(".", export_all = FALSE)
library(shiny)
library(ggplot2)

# Bind every ggpaintr name (exported + internal) into the script's env from
# the namespace. Under `devtools::load_all()` + `library(ggpaintr)`, the
# export env holds copies of exported functions whose closure is `package:`
# rather than the namespace, which breaks S3 dispatch through internal
# generics like prune_walk / render_walk / classify_walk. Local bindings
# defined here are searched before `package:`, so all calls below — bare
# `ptr_app(...)` pastes included — resolve to the namespace version where
# the S3 method table actually lives.
local({
  ns <- asNamespace("ggpaintr")
  for (nm in ls(ns, all.names = FALSE)) {
    assign(nm, get(nm, envir = ns), envir = globalenv())
  }
})

# ---------------------------------------------------------------------------
# Static checks (no Shiny launch needed)
# ---------------------------------------------------------------------------

# 1. Parse a formula -> typed AST. Inspect the tree shape.
tree <- ptr_translate(
  "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point(size = num) + labs(title = text)"
)
str(tree, max.level = 3)
sapply(tree$layers, `[[`, "name")            # "ggplot" "geom_point" "labs"

# 2. Runtime input spec -> which widgets the UI will emit, in what order.
print(ptr_runtime_input_spec(tree))

# 3. Pipe surface preservation. Both pipe operators survive.
cat(ptr_render(ptr_translate(
  "mtcars |> head(num) |> ggplot(aes(x = var, y = var)) + geom_point()"
)), "\n")
cat(ptr_render(ptr_translate(
  "mtcars %>% head(num) |> ggplot(aes(x = var, y = var)) + geom_point()"
)), "\n")

# 4. Safety walker: dangerous calls are rejected at translate time.
tryCatch(
  ptr_translate("ggplot(mtcars) + geom_point() + system('id')"),
  error = function(e) message("blocked: ", conditionMessage(e))
)

# 5. Shared key validation (P3) -- call-form fields are checked at parse time.
tryCatch(
  ptr_translate('ggplot(mtcars) + geom_point(size = num(shared = ""))'),
  error = function(e) message("rejected empty shared: ", conditionMessage(e))
)

# ---------------------------------------------------------------------------
# Interactive apps -- launch one at a time
# ---------------------------------------------------------------------------

if (interactive()) {

# 6. Basic scatter -- exercises var (data consumer) + layer-checkbox toggle.
ptr_app(
  "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
)

# 7. Multi-layer with every value-placeholder type (text/num/expr) and a
#    layer-checkbox default that starts geom_smooth OFF.
ptr_app(
  "ggplot(data = mtcars, aes(x = var, y = var)) +
   geom_point(size = num) +
   geom_smooth(method = expr) +
   labs(title = text, x = text, y = text)",
  checkbox_defaults = list(geom_smooth = FALSE)
)

# 8. Pipeline data placeholder -- num threads through head() before ggplot
#    sees the data, drives the per-layer Data sub-tab and the G11
#    stage-enabled toggles (one checkbox per pipeline stage). Everything
#    is committed via the single "Update plot" button.
ptr_app(
  "mtcars |> head(num) |> ggplot(aes(x = var, y = var)) + geom_point()"
)

# 9. expr placeholder -- provenance subtree is preserved across prune (G5).
ptr_app(
  "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point() +
   facet_wrap(expr)"
)

# 10. upload source -- paired widget (file + name); reserved-word
#     column-name normalization kicks in if the upload has names like
#     'if', 'NULL', etc.
ptr_app(
  "ggplot(data = upload, aes(x = var, y = var)) + geom_point()"
)

# 11. Custom placeholder via the three-constructor API.
#
# Hook contract (rewrite, post-cutover):
#   value role:    build_ui(node, label = NULL, ...)            -> id is node$id
#                  resolve_expr(value, node, ...)               -> scalar/expr/NULL
#   consumer role: build_ui(node, cols, label = NULL,
#                           selected = character(0), ...)        -> id is node$id;
#                                                                   cols is the
#                                                                   upstream column
#                                                                   name vector
#                  resolve_expr(value, node, ...)               -> single-valued
#                                                                   (length(value)>1
#                                                                   is rejected by
#                                                                   substitute_walk)
#                  validate_input(value, upstream_cols)          -> TRUE or msg
#   source role:   build_ui(node, label = NULL, ...)            -> id is node$id;
#                                                                   companion via
#                                                                   node$companion_id
#                  resolve_data(value, node, ...)               -> data.frame
#
# Returning NULL from resolve_expr is how a placeholder declares "missing"
# at runtime so the argument drops out of the generated code (matches the
# legacy ptr_missing_expr() role).

# 11a. Non-data-aware value placeholder -- pct slider mapped to alpha.
ptr_define_placeholder_value(
  keyword     = "pct",
  build_ui    = function(node, label = NULL, ...) {
    sliderInput(node$id, label = label %||% "Percent",
                min = 0, max = 100, value = 50)
  },
  resolve_expr = function(value, node, ...) value / 100,
  copy_defaults = list(label = "Pick a percentage for {param}")
)
ptr_app(
  "ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point(alpha = pct)"
)

# 11b. Data-aware consumer placeholder -- a vanilla selectInput drop-in
#      replacement for `var`. `cols` is the resolved upstream column-name
#      vector (passed automatically by ptr_setup_consumer_uis); `selected`
#      survives renderUI re-fires so the user's pick is preserved across
#      stage toggles and Update plot clicks.
ptr_define_placeholder_consumer(
  keyword = "dropvar",
  build_ui = function(node, cols = character(), label = NULL,
                      selected = character(0), ...) {
    selectInput(
      node$id, label = label %||% "Pick a column",
      choices = c("", cols),
      selected = if (length(selected)) selected[1] else ""
    )
  },
  resolve_expr = function(value, node, ...) {
    if (!is.character(value) || length(value) != 1L || !nzchar(value)) {
      return(NULL)
    }
    rlang::sym(value)
  },
  copy_defaults = list(label = "Column for {param}")
)
ptr_app(
  "ggplot(data = mtcars, aes(x = dropvar, y = dropvar)) + geom_point()"
)

# 11c. Custom data-aware placeholder + multi-stage pipeline + multi-layer.
#      Pipeline: subset() -> head(num); aes uses the custom dropvar;
#      geom_point/geom_smooth/labs each consume different built-ins.
#      Exercises every join-point: stage_enabled checkbox, the single
#      Update plot button, layer checkbox, custom consumer picker, and the
#      P5 re-walk on resolve_expr returning a symbol.
ptr_app(
  "iris |> subset(Species == text) |> head(num) |>
     ggplot(aes(x = dropvar, y = dropvar, color = dropvar)) +
     geom_point(size = num) +
     geom_smooth(method = expr) +
     labs(title = text)"
)

# 11d. Multi-column data-aware consumer (`colvars`) -- selectInput with
#      multiple = TRUE that resolves to a `c("col_a", "col_b", ...)`
#      character vector. Drops directly into any data-shaping verb that
#      takes a column-name vector: `subset(data, select = ...)`,
#      `dplyr::select(data, all_of(...))`, `tidyr::pivot_longer(cols =
#      all_of(...))`, etc. Demonstrates that the rewrite no longer
#      enforces single-value semantics on consumers — arity is the
#      placeholder's own responsibility (`var` still requires a single
#      column via its own `validate_input` / `resolve_expr`).
ptr_define_placeholder_consumer(
  keyword = "colvars",
  build_ui = function(node, cols = character(), label = NULL,
                      selected = character(0), ...) {
    selectInput(
      node$id, label = label %||% "Columns",
      choices = cols,
      selected = intersect(selected, cols),
      multiple = TRUE
    )
  },
  resolve_expr = function(value, node, ...) {
    if (length(value) == 0L) return(NULL)
    # `c("Sepal.Length", "Petal.Length")` as a call object that drops
    # straight into the substituted tree.
    rlang::call2("c", !!!as.list(value))
  },
  copy_defaults = list(label = "Columns for {param}")
)
ptr_app(
  "iris |> subset(select = colvars) |>
     ggplot() + geom_histogram(aes(x = var), bins = num)"
)

# 12. ui_text overrides -- every label and the title are user-customizable.
ptr_app(
  "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()",
  ui_text = list(
    shell  = list(title = list(label = "Exploratory plot builder")),
    params = list(x = list(var = list(label = "X axis (numeric)")),
                  y = list(var = list(label = "Y axis (numeric)")))
  )
)

# 13. bslib variant -- themed shell wrapping the same server.
ptr_app_bslib(
  "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point() +
   labs(title = text)",
  theme = bslib::bs_theme(version = 5, bootswatch = "minty"),
  # ptr_app_bslib() has no `title` arg; the shell title is set through
  # the `ui_text` shell slot (same contract as ptr_app()).
  ui_text = list(shell = list(title = list(label = "ggpaintr x bslib")))
)

# 14. Module wrappers -- two namespaced instances side by side, no collisions.
{
  ui <- fluidPage(
    fluidRow(
      column(6, ptr_module_ui(
        "ggplot(mtcars, aes(x = var, y = var)) + geom_point()", "a")),
      column(6, ptr_module_ui(
        "ggplot(iris, aes(x = var, y = var)) + geom_point()", "b"))
    )
  )
  server <- function(input, output, session) {
    ptr_module_server("ggplot(mtcars, aes(x = var, y = var)) + geom_point()", "a")
    ptr_module_server("ggplot(iris, aes(x = var, y = var)) + geom_point()", "b")
  }
  shinyApp(ui, server)
}

# 15. Grid app + shared placeholder. ONE slider drives `size` across both
#     plots; the "Draw all" button at the top forces a redraw of every panel.
ptr_app_grid(
  plots = list(
    'ggplot(data = mtcars, aes(x = var, y = var)) + geom_point(size = num(shared = "sz"))',
    'ggplot(data = mtcars, aes(x = wt,  y = var)) + geom_point(alpha = num(shared = "sz")/10)'
  ),
  shared_ui = list(
    sz = function(id) sliderInput(id, "Point size", min = 1, max = 10, value = 3)
  )
)

ptr_app_grid(
  plots = list(
    'ggplot(data = mtcars, aes(x = var, y = var)) + geom_point(size = num(shared = "sz"))',
    'ggplot(data = mtcars, aes(x = wt,  y = var)) + geom_point(alpha = num(shared = "sz")/10)'
  )
)

# 15b. Single-plot shared section (plan 06). `ptr_app()` auto-renders one
#      widget per unique `shared = "<key>"` key in a collapsible
#      `<details>` section above the layer picker; the same key referenced
#      from multiple layers shows up as ONE widget that drives both layers
#      in lockstep. Watch:
#        - "Shared inputs" section sits above the Layer picker.
#        - `sz` slider drives both geom_point size and geom_smooth size.
#        - `ttl` text box flows into labs(title = ...).
#        - The geom_point Controls tab is empty (no per-layer size widget),
#          since the shared placeholder filter (C1) keeps `num(shared=...)`
#          out of layer panels.
#      Validation: typo a key the formula doesn't use (e.g. swap "sz" to
#      "szz" in one layer only) and confirm grid-style cross-checks fire
#      via `ptr_app_grid()` instead — single-plot auto-binds and never
#      complains about extra/missing keys.
ptr_app(
  "ggplot(data = mtcars, aes(x = mpg, y = hp)) +
     geom_point(size = num(shared = \"sz\")) +
     geom_smooth(size = num(shared = \"sz\"), method = \"lm\") +
     labs(title = text(shared = \"ttl\"))"
)

# 15c. Same shared key referenced from BOTH a pipeline stage and an aes
#      mapping — still one widget, drives both. Move the `head(num(...))`
#      slider; the plotted point count changes AND the title updates in
#      lockstep because they read the same shared key.
ptr_app(
  "mtcars |> head(num(shared = \"n\")) |>
     ggplot(aes(x = mpg, y = hp)) +
     geom_point() +
     labs(subtitle = paste(\"showing\", num(shared = \"n\"), \"rows\"))"
)

# 15d. Shared `var` (data-consumer) host-resolved end-to-end.
#      Both `ptr_app()` and `ptr_app_grid()` auto-bind shared `var`
#      widgets at host scope. The host picks ONE upstream per shared
#      key from these rules:
#        - same source dataset across all occurrences, identical
#          upstream chains  -> use the upstream (cols reflect transforms).
#        - same source, divergent upstreams                -> fall back to
#          source cols (so a column the user picks may not exist for
#          some layers — that's surfaced as a runtime error, by design).
#        - different source datasets                       -> resolver
#          error: in-slot alert in the shared section + a mirrored entry
#          in the error panel (single-plot only; grid has no host-level
#          error panel, so the alert in the shared widget slot is the
#          single source).
#      The shared `var(shared = "v")` picker drives both x in ggplot
#      and y in geom_point in lockstep.
ptr_app(
  "ggplot(data = mtcars, aes(x = var(shared = \"v\"), y = mpg)) +
     geom_point(aes(y = var(shared = \"v\")))"
)

# 15e. Diverging sources — shared `v` references mtcars in one layer
#      and iris in another. Resolver detects the divergence; the shared
#      section shows a red alert ("layers use different source datasets")
#      and the error panel mirrors it. The picker is replaced with the
#      alert, so no choice is presented; downstream substitution sees
#      `ctx$shared[["v"]]` as NULL → the shared placeholder drops out
#      via `ptr_missing()`.
ptr_app(
  "ggplot(data = mtcars, aes(x = var(shared = \"v\"))) +
     geom_point(data = iris, aes(y = var(shared = \"v\")))"
)

# 15f. Same source, divergent upstreams — both layers narrow `mtcars`
#      via `dplyr::select`, but to different column subsets. Resolver
#      falls back to the source (mtcars), so the picker shows EVERY
#      mtcars column. Picking `mpg` works for ggplot's layer (mpg is in
#      the first select) but fails for geom_point's (mpg not in second
#      select) — the failure surfaces in the plot's error panel. By
#      design: user is responsible for picking a column valid in every
#      layer's narrowed scope.
ptr_app(
  "ggplot(data = mtcars |> dplyr::select(mpg, cyl), aes(x = var(shared = \"v\"))) +
     geom_point(data = mtcars |> dplyr::select(hp, wt), aes(y = var(shared = \"v\")))"
)

# 15g. Grid auto-render of shared `var` (no `shared_ui` supplied). One
#      top-level picker drives the shared key across all plot modules.
#      Same resolution contract as single-plot.
ptr_app_grid(
  plots = list(
    'ggplot(data = mtcars, aes(x = var(shared = "v"), y = var)) + geom_point(size = num)',
    'ggplot(data = mtcars, aes(x = wt,  y = var(shared = "v"))) + geom_point(alpha = num)'
  )
)

# 15i. Shared CUSTOM consumer placeholder. The host-resolution machinery
#      dispatches on the S3 class `ptr_ph_data_consumer`, which custom
#      consumers (registered via `ptr_define_placeholder_consumer`)
#      inherit. The host calls the registry hook with `cols`, `data`,
#      `selected`, exactly as the per-layer path would. So
#      `dropvar(shared = "v")` (the vanilla selectInput consumer from
#      block 11b) Just Works — one selectInput in the shared section,
#      driving both layers.
#
#      NOTE: relies on `dropvar` being defined; run block 11b first, or
#      uncomment the registration below.
# ptr_define_placeholder_consumer(
#   keyword = "dropvar",
#   build_ui = function(node, cols = character(), label = NULL,
#                       selected = character(0), ...) {
#     selectInput(node$id, label = label %||% "Pick a column",
#                 choices = c("", cols),
#                 selected = if (length(selected)) selected[1] else "")
#   },
#   resolve_expr = function(value, node, ...) {
#     if (!is.character(value) || length(value) != 1L || !nzchar(value)) return(NULL)
#     rlang::sym(value)
#   },
#   copy_defaults = list(label = "Column for {param}")
# )
ptr_app(
  "ggplot(data = mtcars, aes(x = dropvar(shared = \"v\"), y = mpg)) +
     geom_point(aes(y = dropvar(shared = \"v\")))"
)

# 15j. Shared SOURCE placeholder (`upload(shared = "ds")`). The shared
#      section renders ONE file-input widget plus its dataset-name
#      companion at the canonical id; both layers read the same
#      uploaded frame as their data source. Steps to verify:
#        1. Upload a small CSV (mtcars-like with numeric columns).
#        2. Confirm the `var` pickers in BOTH layers populate with the
#           uploaded columns.
#        3. Pick x / y for each layer; click Update Plot.
#        4. Confirm both geoms render against the same dataset.
#      Note: shared SOURCE wiring predates plan 06 — the upload widget
#      and resolved-data path are exercised here for end-to-end sanity,
#      not because the host-resolution change touches sources.
ptr_app(
  "ggplot(data = upload(shared = \"ds\"), aes(x = var, y = var)) +
     geom_point(data = upload(shared = \"ds\"), aes(x = var, y = var)) +
     geom_smooth(method = \"lm\")"
)

# 15k. Self-reference / cross-key dependency neutralized by truncation.
#      When a shared widget's upstream chain itself contains other
#      placeholders, the host truncates exclusive of the first inner
#      placeholder. So `var(shared = "b")`'s shared upstream below is
#      `mtcars |> filter(...)` (the chain stops at the inner
#      `select(var(shared = "a"))`). Both `a` and `b` end up resolved
#      against the same source-side prefix and share consistently —
#      no circular-dep error, no manual ordering required.
#
#      NEW: the shared panel now also hosts a stage(verb) checkbox for
#      any pipeline stage whose only placeholder is shared. The
#      `dplyr::select(var(shared = "a"), gear)` stage above is exactly
#      that — its widget for "a" is wrapped in a `select()`-labelled
#      `.ptr-stage` head checkbox. Untick it to drop that select() stage
#      at evaluation; the picker for "a" stays usable (other layers /
#      formulas could still reference it). The `var(shared = "b")` aes
#      placeholder is NOT inside a pipeline stage, so it renders bare.
ptr_app(
  "mtcars |>
     dplyr::filter(mpg > 10) |>
     dplyr::select(var(shared = \"a\"), gear) |>
     ggplot(aes(x = var(shared = \"b\"), y = gear)) + geom_point()"
)

# 15l. Shared stage(verb) checkbox in `ptr_app_grid()` — one checkbox in
#      the shared panel prunes the orphan stage in EVERY plot module that
#      hosts an occurrence of the shared key inside a pipeline stage.
#      Watch:
#        - Single head checkbox at the top of the shared panel, labelled
#          `select()/filter()` (union of verbs across both formulas).
#        - Untick it → plot 1's `dplyr::select(var(shared="a"), mpg)` AND
#          plot 2's `dplyr::filter(var(shared="a") > 0)` both drop their
#          stage simultaneously on the next "Draw all".
#        - The shared picker for "a" stays usable.
ptr_app_grid(
  plots = list(
    "mtcars |> dplyr::select(var(shared = \"a\"), mpg) |>
       ggplot(aes(x = mpg, y = mpg)) + geom_point()",
    "mtcars |> dplyr::filter(var(shared = \"a\") > 0) |>
       ggplot(aes(x = mpg, y = mpg)) + geom_point()"
  )
)

# 15m. Same feature exposed through the host-authored `ptr_shared_ui()` /
#      `ptr_shared_server()` pair, for embedders that own their own
#      layout. The synthetic checkbox input id is `shared_<key>_stage_enabled`
#      (here `shared_a_stage_enabled`); `ptr_shared_server()` exposes its
#      value via `state$shared_stage_enabled$a`, and each
#      `ptr_module_server()` mirrors it into its module's stage pruning.
{
  formulas <- c(
    "mtcars |> dplyr::select(var(shared = \"a\"), mpg) |>
       ggplot(aes(x = mpg, y = mpg)) + geom_point()",
    "mtcars |> dplyr::filter(var(shared = \"a\") > 0) |>
       ggplot(aes(x = mpg, y = mpg)) + geom_point()"
  )
  ui <- fluidPage(
    titlePanel("Shared stage(verb) checkbox via ptr_shared_ui()"),
    ptr_shared_ui(formulas),
    fluidRow(
      column(6, ptr_module_ui(formulas[[1]], "plot_1")),
      column(6, ptr_module_ui(formulas[[2]], "plot_2"))
    )
  )
  server <- function(input, output, session) {
    shared_state <- ptr_shared_server(formulas)
    ptr_module_server(formulas[[1]], "plot_1", shared_state = shared_state)
    ptr_module_server(formulas[[2]], "plot_2", shared_state = shared_state)
  }
  shinyApp(ui, server)
}

# 16a. Level-3 render override -- use plotly::ggplotly() instead of the
#      default renderPlot. Demonstrates that `ptr_extract_plot()` (and
#      the underlying `state$runtime()` reactive) exposes the fitted
#      ggplot for any backend that consumes a ggplot — plotly,
#      ggiraph, gganimate, Cairo, etc.
#
#      The default ptr_plot output is still bound by ptr_module_server;
#      placing only `plotly::plotlyOutput()` on screen means the user
#      sees the plotly version. (We don't strip the default binding —
#      it's a harmless no-op when no plotOutput is rendered.)
#      Reading `state$runtime()` outside `isolate` is what wires the
#      plotly re-render to Update Plot clicks.
#      Requires `install.packages("plotly")`.
{
  formula <- "ggplot(data = mpg,
                     aes(x = displ, y = hwy, color = var, text = var)) +
                geom_point(size = num, alpha = num)"
  ui <- fluidPage(
    fluidRow(
      column(5, ptr_module_ui(formula, "plotly_demo")),
      column(7, plotly::plotlyOutput("interactive_plot", height = "500px"))
    )
  )
  server <- function(input, output, session) {
    state <- ptr_module_server(formula, "plotly_demo")
    output$interactive_plot <- plotly::renderPlotly({
      res <- state$runtime()                     # reactive dep
      shiny::req(isTRUE(res$ok), res$plot)
      plotly::ggplotly(res$plot, tooltip = "text")
    })
  }
  shinyApp(ui, server)
}

# 16. Programmatic ggplot extras -- attach scales/themes from the server side.
{
  ui <- fluidPage(
    actionButton("add_log", "Toggle log-scale"),
    ptr_module_ui("ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()", "p")
  )
  server <- function(input, output, session) {
    state <- ptr_module_server(
      "p", "ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()"
    )
    observeEvent(input$add_log, {
      ptr_gg_extra(state, ggplot2::scale_x_log10())
    })
  }
  shinyApp(ui, server)
}

}  # end if (interactive())

# ---------------------------------------------------------------------------
# 17. Programmatic accessors via shiny::testServer -- useful for tests or
#     for composing custom UIs without launching a browser.
# ---------------------------------------------------------------------------

shiny::testServer(function(input, output, session) {
  state <- ptr_server(
    input, output, session,
    "ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()"
  )
  # `state$runtime()` is a reactiveVal(NULL) until the Update plot button
  # fires; trigger it (and flush) so the accessors below see real output.
  session$setInputs(ptr_update_plot = 1)
  session$flushReact()
  session$userData$paintr <- state
}, {
  cat("CODE:\n", ptr_extract_code(session$userData$paintr), "\n")
  cat("PLOT class:", class(ptr_extract_plot(session$userData$paintr)), "\n")
})

# ---------------------------------------------------------------------------
# Things to specifically poke at as you click through:
#
# - (8) Watch the Data sub-tab: per-stage checkbox (G11). Edit a var to
#   a non-existent column, click "Update plot" -- the inline error panel
#   appears.
# - (10) Upload a CSV with reserved-word column names (`if`, `NULL`,
#   `TRUE`) and watch the var dropdowns still work (P11.5 normalization).
# - (11a) The registered `pct` placeholder picks up the `{param}`
#   interpolation in copy automatically.
# - (11b) `dropvar` mirrors the builtin `var` but with a vanilla
#   selectInput. Watch the `selected` argument: the choice is preserved
#   across renderUI re-fires (toggle a layer off/on, re-pick the column,
#   confirm the picker doesn't snap back to the first option).
# - (11c) Toggle the head() stage checkbox -- the layers downstream
#   refresh; click "Update plot" to redraw. dropvar in `color`
#   exercises the consumer picker through aes() inside a non-ggplot layer.
# - (11d) Pick two or more columns in the colvars picker, click
#   "Update plot" -- the code panel shows `subset(select = c("..", ".."))`. The
#   selectable column set comes from upstream `iris` (cols passed by
#   the framework); `var` running in the same app would still reject
#   multi-select via its own validate_input.
# - (15) Moving the shared slider invalidates both plots' state in one
#   user action -- no per-plot button-clicks needed.
# - (16a) Pick a column for `color`, a column for `text`, and any size
#   / alpha numbers; click Update Plot. The plotly panel re-renders
#   with hover tooltips driven by the `text` aesthetic. Same ggpaintr
#   state would also drive ggiraph, gganimate, or any other backend
#   that consumes a `ggplot` — only the renderXxx call changes.
# - (16) Clicking "Toggle log-scale" repeatedly demonstrates that
#   ptr_gg_extra REPLACES the extras list on each call (per P12.10-12).
# - Denylist sweep: try `system2`, `eval`, `parse`, `do.call`, `attr<-`
#   etc. as malicious formulas; every entry of
#   `ggpaintr:::unsafe_expr_denylist` blocks at translate time.
# ---------------------------------------------------------------------------


library(ggpcp)
data(flea, package = "GGally")

ptr_define_placeholder_consumer(
  keyword = "colvars",
  build_ui = function(node, cols = character(), label = NULL,
                      selected = character(0), ...) {
    selectInput(
      node$id, label = label %||% "Columns",
      choices = cols,
      selected = intersect(selected, cols),
      multiple = TRUE
    )
  },
  resolve_expr = function(value, node, ...) {
    if (length(value) == 0L) return(NULL)
    # `c("Sepal.Length", "Petal.Length")` as a call object that drops
    # straight into the substituted tree.
    rlang::call2("c", !!!as.list(value))
  },
  copy_defaults = list(label = "Columns for {param}")
)

ptr_app(
  "ggplot(data = flea |>
                  pcp_select(colvars) |>
                  pcp_scale(method = 'uniminmax') |>
                  pcp_arrange(),
          mapping = aes_pcp()) +
     geom_pcp_axes() +
     geom_pcp(aes(colour = species))"
)
