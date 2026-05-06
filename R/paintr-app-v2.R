# Public entry points for the typed-AST core (cohabit names — the legacy
# `ptr_app` etc. live in paintr-app.R until the 4c cutover).
#
# Each entry point composes the UI builder (per-layer panels via
# `build_ui_for.ptr_layer`) and the server wiring (`ptr_server_v2`).

ptr_app_v2 <- function(formula,
                       envir = parent.frame(),
                       ui_text = NULL,
                       checkbox_defaults = NULL,
                       expr_check = TRUE,
                       safe_to_remove = character(),
                       ns = shiny::NS(NULL)) {
  parts <- ptr_app_components_v2(
    formula,
    envir = envir,
    ui_text = ui_text,
    checkbox_defaults = checkbox_defaults,
    expr_check = expr_check,
    safe_to_remove = safe_to_remove,
    ns = ns
  )
  shiny::shinyApp(ui = parts$ui, server = parts$server)
}

ptr_app_components_v2 <- function(formula,
                                  envir = parent.frame(),
                                  ui_text = NULL,
                                  checkbox_defaults = NULL,
                                  expr_check = TRUE,
                                  safe_to_remove = character(),
                                  ns = shiny::NS(NULL)) {
  tree <- ptr_translate(formula, expr_check = expr_check)

  ui <- ptr_build_app_ui_v2(
    tree,
    ui_text = ui_text,
    checkbox_defaults = checkbox_defaults,
    ns = ns
  )

  server <- function(input, output, session) {
    ptr_server_v2(
      input, output, session, formula,
      envir = envir,
      ui_text = ui_text,
      checkbox_defaults = checkbox_defaults,
      expr_check = expr_check,
      safe_to_remove = safe_to_remove,
      ns = ns
    )
  }
  list(ui = ui, server = server)
}

ptr_build_app_ui_v2 <- function(tree, ui_text = NULL,
                                checkbox_defaults = NULL,
                                ns = shiny::NS(NULL)) {
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

  shiny::fluidPage(
    shiny::titlePanel(ptr_resolve_ui_text("title", ui_text = ui_text)$label %||% ""),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        picker,
        hidden_tabset
      ),
      shiny::mainPanel(
        shiny::plotOutput(ns("ptr_plot")),
        shiny::verbatimTextOutput(ns("ptr_code")),
        shiny::textOutput(ns("ptr_error"))
      )
    )
  )
}

# ---- Module variants ----

ptr_module_ui_v2 <- function(id, formula, ui_text = NULL,
                             checkbox_defaults = NULL, expr_check = TRUE) {
  tree <- ptr_translate(formula, expr_check = expr_check)
  ptr_build_app_ui_v2(
    tree,
    ui_text = ui_text,
    checkbox_defaults = checkbox_defaults,
    ns = shiny::NS(id)
  )
}

ptr_module_server_v2 <- function(id, formula, envir = parent.frame(), ...) {
  shiny::moduleServer(id, function(input, output, session) {
    ptr_server_v2(input, output, session, formula,
                  envir = envir, ns = session$ns, ...)
  })
}
