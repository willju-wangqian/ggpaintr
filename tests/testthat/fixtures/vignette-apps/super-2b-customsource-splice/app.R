# Boot scaffolding (NOT vignette code): load the in-development package into
# this child app process so the e2e test exercises dev source, not a stale
# system install. Super-app pressure fixture (ADR 0013 §App-2b): custom D3
# source at formula head + G3 `!!` splice with placeholders inside the
# spliced expression + layer-data ppUpload(df_rug) + shared= on a custom
# consumer placeholder. NOT vignette-paired.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)

# --- Custom placeholders ---------------------------------------------------
# ppCoef: VALUE role. 0-1 slider; resolve_expr returns the numeric literal.
# Bind the returned runtime fn to the keyword name so the formula evaluates as
# plain ggplot under Path B (ADR-0016) — Path A would work without this since
# the parser walks the AST, but discarding the return value defeats Path B.
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
  copy_defaults = list(label = "Coef for {param}"),
  default_arg   = ptr_default_numeric()
)

# ppFactor: CONSUMER role. Single-select picker -> bare symbol via rlang::sym.
# Supports `shared=` to collapse across aes(color=) + facet_wrap(vars(...)).
# `ppFactor <-` binding required for Path-B evaluability (ADR-0016).
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
  copy_defaults = list(label = "Factor for {param}"),
  default_arg   = ptr_default_symbol_or_string()
)

# ppSample: SOURCE role. selectInput from a hard whitelist of base-R datasets.
# resolve_data reads the chosen dataset out of `asNamespace("datasets")`.
# default_arg = ptr_default_string() per PLAN-04 SC: the formula's literal
# initial value MUST be a string (`ppSample("iris")`), matching ADR §App-2b.
# `ppSample <-` binding required for Path-B evaluability (ADR-0016).
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
  # Custom `runtime`: invoked when the formula is evaluated as plain R outside
  # `ptr_app()` (ADR-0016 Path B). Defaults to an abort guard at the registry
  # level; we override so `ppSample("iris")` resolves to `datasets::iris`,
  # making the canonical formula Path-B evaluable.
  runtime       = function(x, ...) {
    if (missing(x) || is.null(x) || !nzchar(x)) {
      rlang::abort("`ppSample()` needs a dataset name (e.g., \"iris\").")
    }
    get(x, envir = asNamespace("datasets"))
  },
  copy_defaults = list(label = "Sample for {param}"),
  default_arg   = ptr_default_string()
)

# --- Spliced template (G6 forwarded-symbol; resolved at capture time) -----
# The body of the splice carries THREE placeholders. ADR 0013 §App-2b puts
# them under load: the AST walker must descend INTO the spliced body when
# the formula is captured, otherwise the spliced placeholders' input ids
# never appear in the page.
smooth_template <- rlang::expr(
  geom_smooth(
    method    = ppText("lm"),
    linewidth = ppNum(1),
    alpha     = ppCoef(0.3)
  )
)

# `ppUpload(df_rug)` is a shortcut source: at boot it loads the object named
# `df_rug` from this script env (R/paintr-server.R::try_bind_source_default,
# `get(lookup_name, envir = state$eval_env, inherits = TRUE)`), falling back
# to a user upload. Bind it so the app boots faithfully to reference.R (which
# binds the same `df_rug <- mtcars`) instead of raising "object 'df_rug' not
# found" at first render. mtcars = mpg+wt (no Species) per the inherit.aes
# note below. The super-pressure test still uploads sample_rug.csv, and an
# upload wins over this env binding (resolve_upload_source: file_info present
# => upload data), so post-upload assertions are unaffected.
df_rug <- mtcars

# --- The app --------------------------------------------------------------
# G2 BARE-EXPRESSION formula carrying:
#   - ppSample("iris") at root data        (custom D3 source)
#   - aes(x, y, color = ppFactor(... shared = "fac"))
#   - geom_point(size = ppNum(2))
#   - !!smooth_template                    (G3 splice; ppText/ppNum/ppCoef inside)
#   - geom_rug(data = ppUpload(df_rug), aes(x, y))   -- layer-data ppUpload
#   - facet_wrap(vars(ppFactor(... shared = "fac"))) -- shared fac collapse
#   - labs(title = ppText("..."))
ptr_app(
  ggplot(ppSample("iris"),
         aes(x = ppVar(Sepal.Length),
             y = ppVar(Sepal.Width),
             color = ppFactor(Species, shared = "fac"))) +
    geom_point(size = ppNum(2)) +
    !!smooth_template +
    geom_rug(data = ppUpload(df_rug),
             aes(x = ppVar(mpg), y = ppVar(wt)),
             # `inherit.aes = FALSE` so geom_rug's own aes is authoritative.
             # Without this, the root's `color = ppFactor(Species, ...)` leaks
             # into geom_rug's eval scope and Path-B blows up when df_rug
             # lacks a Species column (the sample CSV is just mpg+wt). Same
             # pattern as super-2a's geom_smooth F4 fix.
             inherit.aes = FALSE) +
    facet_wrap(vars(ppFactor(Species, shared = "fac"))) +
    labs(title = ppText("ppSample + splice + layer-upload")),
  ui_text = list(defaults = list(
    ppCoef = list(label = "Coef: {param}")
  ))
)
