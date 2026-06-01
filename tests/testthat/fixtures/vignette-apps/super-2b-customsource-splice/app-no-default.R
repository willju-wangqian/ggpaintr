pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
# super-2b-customsource-splice (no-default variant): same formula structure
# as app.R but every `pp*(default)` is stripped of its positional default.
# Path-A pressure-test variant per ADR-0016. ppUpload's bareword companion
# name (df_rug) is STRUCTURAL identifier, not a default value — it stays.
library(shiny)

ppCoef <- ptr_define_placeholder_value(
  keyword       = "ppCoef",
  build_ui      = function(node, label = NULL, selected = NULL, ...) {
    sliderInput(node$id, label = label %||% "Coef",
                min = 0, max = 1,
                value = selected %||% node$default %||% 0.5,
                step = 0.01)
  },
  resolve_expr  = function(value, node, ...) {
    if (is.null(value)) return(NULL)
    rlang::expr(!!value)
  },
  ui_text_defaults = list(label = "Coef for {param}"),
  parse_positional_arg   = ptr_arg_numeric()
)

ppFactor <- ptr_define_placeholder_consumer(
  keyword       = "ppFactor",
  build_ui      = function(node, cols = character(), label = NULL,
                           selected = character(0), ...) {
    shinyWidgets::pickerInput(
      node$id, label = label %||% "Factor",
      choices = cols,
      selected = if (length(selected)) selected[[1L]] else character(0)
    )
  },
  resolve_expr  = function(value, node, ...) {
    if (length(value) == 0L || !nzchar(value)) return(NULL)
    rlang::sym(value)
  },
  ui_text_defaults = list(label = "Factor for {param}"),
  parse_positional_arg   = ptr_arg_symbol_or_string()
)

ppSample <- ptr_define_placeholder_source(
  keyword       = "ppSample",
  build_ui      = function(node, label = NULL, selected = NULL, ...) {
    selectInput(
      node$id, label = label %||% "Sample dataset",
      choices = c("iris", "mtcars", "ChickWeight", "ToothGrowth"),
      selected = selected %||% node$default %||% "iris"
    )
  },
  resolve_data  = function(value, ...) {
    if (is.null(value) || !nzchar(value)) return(NULL)
    get(value, envir = asNamespace("datasets"))
  },
  embellish_eval       = function(x, ...) {
    if (missing(x) || is.null(x) || !nzchar(x)) {
      rlang::abort("`ppSample()` needs a dataset name (e.g., \"iris\").")
    }
    get(x, envir = asNamespace("datasets"))
  },
  ui_text_defaults = list(label = "Sample for {param}"),
  parse_positional_arg   = ptr_arg_string()
)

smooth_template <- rlang::expr(
  geom_smooth(
    method    = ppText(),
    linewidth = ppNum(),
    alpha     = ppCoef()
  )
)

# `ppUpload(df_rug)`: the bareword `df_rug` is a STRUCTURAL shortcut id (it is
# NOT a stripped positional default — see header), and at boot the shortcut
# source loads the object named `df_rug` from this script env (falling back to
# a user upload). Bind it so this no-default variant also boots clean instead
# of raising "object 'df_rug' not found". The super-pressure no-default test
# uploads sample_rug.csv; an upload wins over this env binding.
df_rug <- mtcars

ptr_app(
  ggplot(ppSample(),
         aes(x = ppVar(),
             y = ppVar(),
             color = ppFactor(shared = "fac"))) +
    geom_point(size = ppNum()) +
    !!smooth_template +
    geom_rug(data = ppUpload(df_rug),
             aes(x = ppVar(), y = ppVar()),
             inherit.aes = FALSE) +
    facet_wrap(vars(ppFactor(shared = "fac"))) +
    labs(title = ppText()),
  ui_text = list(defaults = list(
    ppCoef = list(label = "Coef: {param}")
  ))
)
