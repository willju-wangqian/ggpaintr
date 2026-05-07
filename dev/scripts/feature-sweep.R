# Feature sweep for the typed-AST core (post-4c cutover).
#
# Usage:
#   devtools::load_all(".")
#   source("dev/scripts/feature-sweep.R", echo = TRUE)  # static checks only
#
# The interactive app sections at the bottom run only under `interactive()`.
# Highlight + send a single `ptr_app(...)` line to your R session to launch
# one app at a time.

library(shiny)
library(ggplot2)

# ---------------------------------------------------------------------------
# Static checks (no Shiny launch needed)
# ---------------------------------------------------------------------------

# `ptr_translate` and `ptr_render` are unexported internals — reach through
# `:::` so S3 dispatch resolves correctly under `devtools::load_all()`.
# Public users call `ptr_app()`, which doesn't need this.
ptr_translate <- ggpaintr:::ptr_translate
ptr_render    <- ggpaintr:::ptr_render

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
#    sees the data, drives the per-layer Data sub-tab, the Update Data
#    button (G6 atomic snapshot), and the new G11 stage-enabled toggles
#    (one checkbox per pipeline stage).
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

# 11. Custom placeholder via the new three-constructor API.
ptr_define_placeholder_value(
  keyword     = "pct",
  build_ui    = function(node, id, label, ...) {
    sliderInput(id, label = label, min = 0, max = 100, value = 50)
  },
  resolve_expr = function(value, node, ...) value / 100,
  copy_defaults = list(label = "Pick a percentage for {param}")
)
ptr_app(
  "ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point(alpha = pct)"
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
  title = "ggpaintr x bslib"
)

# 14. Module wrappers -- two namespaced instances side by side, no collisions.
{
  ui <- fluidPage(
    fluidRow(
      column(6, ptr_module_ui("a",
        "ggplot(mtcars, aes(x = var, y = var)) + geom_point()")),
      column(6, ptr_module_ui("b",
        "ggplot(iris, aes(x = var, y = var)) + geom_point()"))
    )
  )
  server <- function(input, output, session) {
    ptr_module_server("a", "ggplot(mtcars, aes(x = var, y = var)) + geom_point()")
    ptr_module_server("b", "ggplot(iris, aes(x = var, y = var)) + geom_point()")
  }
  shinyApp(ui, server)
}

# 15. Grid app + shared placeholder. ONE slider drives `size` across both
#     plots; the "Draw all" button at the top forces a redraw of every panel.
ptr_app_grid(
  plots = list(
    'ggplot(data = mtcars, aes(x = mpg, y = hp)) + geom_point(size = num(shared = "sz"))',
    'ggplot(data = mtcars, aes(x = wt,  y = qsec)) + geom_point(size = num(shared = "sz"))'
  ),
  shared_ui = list(
    sz = function(id) sliderInput(id, "Point size", min = 1, max = 10, value = 3)
  )
)

# 16. Programmatic ggplot extras -- attach scales/themes from the server side.
{
  ui <- fluidPage(
    actionButton("add_log", "Toggle log-scale"),
    ptr_module_ui("p", "ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()")
  )
  server <- function(input, output, session) {
    state <- ptr_module_server(
      "p", "ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()"
    )
    observeEvent(input$add_log, {
      ptr_gg_extra(state, list(quote(ggplot2::scale_x_log10())))
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
  session$setInputs(.dummy = 1)
  session$userData$paintr <- state
}, {
  cat("CODE:\n", ptr_extract_code(session$userData$paintr), "\n")
  cat("PLOT class:", class(ptr_extract_plot(session$userData$paintr)), "\n")
})

# ---------------------------------------------------------------------------
# Things to specifically poke at as you click through:
#
# - (8) Watch the Data sub-tab: per-stage checkbox (G11) + Update Data
#   button (G6 atomic snapshot). Edit a var to a non-existent column,
#   click Update Data -- the inline error appears and the cache is left
#   untouched.
# - (10) Upload a CSV with reserved-word column names (`if`, `NULL`,
#   `TRUE`) and watch the var dropdowns still work (P11.5 normalization).
# - (11) The registered `pct` placeholder picks up the `{param}`
#   interpolation in copy automatically.
# - (15) Moving the shared slider invalidates both plots' state in one
#   user action -- no per-plot button-clicks needed.
# - (16) Clicking "Toggle log-scale" repeatedly demonstrates that
#   ptr_gg_extra REPLACES the extras list on each call (per P12.10-12).
# - Denylist sweep: try `system2`, `eval`, `parse`, `do.call`, `attr<-`
#   etc. as malicious formulas; every entry of
#   `ggpaintr:::unsafe_expr_denylist` blocks at translate time.
# ---------------------------------------------------------------------------
