# Tutorial

``` r

library(ggpaintr)
library(ggplot2)
library(rlang)   # for `%||%`, sym(), call2(), abort() used in the examples
```

This tutorial walks through ggpaintr from the outside in. Section 1 gets
you a running app from a one-line formula and introduces the built-in
placeholders. Section 2 shows how to define your own placeholders,
taking each argument of the three `ptr_define_placeholder_*()`
constructors in turn. Section 3 leaves the turn-key entry point behind:
you write your own Shiny app, embed several plots, and wire one control
to all of them.

Every app chunk is marked `eval = interactive()` — paste it at the R
prompt to launch the app. Pure-ggplot chunks that produce a static plot
run inline.

## 1. Basics

A ggpaintr **formula** is an ordinary
[`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html) call
written out as code, with **placeholder keywords** dropped in wherever a
user should get to choose a value.
[`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md)
reads the formula, turns each placeholder into a Shiny widget, and
re-runs the
[`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html) call
with the user’s inputs spliced back in.

The smallest useful app is one line:

``` r

ptr_app(
  ggplot(mtcars, aes(x = ppVar, y = ppVar)) + geom_point()
)
```

This launches a Shiny app with two column dropdowns (one per `ppVar`), a
layer panel, and an **Update plot** button. There is no `data =`
argument: ggpaintr resolves the bare symbol `mtcars` in the calling
environment (`envir`, default
[`parent.frame()`](https://rdrr.io/r/base/sys.parent.html)). The only
required argument to
[`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md)
is the formula itself; `envir`, `ui_text`, `css`, `expr_check`, and
`spec` are all optional.

### The five built-in placeholders

Each keyword maps to a fixed widget and a fixed way of folding the input
back into the formula:

| Keyword | Widget | Role | Folds back as |
|----|----|----|----|
| `ppVar` | column picker (data-aware) | consumer | a column symbol, e.g. `mpg` |
| `ppText` | text input | value | a string |
| `ppNum` | numeric input | value | a number |
| `ppExpr` | code box (validated) | value | live code, parsed to an expression |
| `ppUpload` | file picker (+ dataset-name box) | source | a data frame |

The three **roles** in the right column matter once you start defining
your own placeholders (Section 2):

- A **value** placeholder produces a self-contained value — it needs
  nothing from the rest of the formula.
- A **consumer** placeholder needs the upstream data’s column names (a
  `ppVar` dropdown can only list columns once it knows what data flows
  into it).
- A **source** placeholder *produces* the data frame the rest of the
  formula reads.

A formula can mix all of them, and placeholders work anywhere in a
pipeline, not just inside
[`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html):

``` r

ptr_app(
  mtcars |>
    dplyr::filter(ppExpr(mpg > 15)) |>
    ggplot(aes(x = ppVar, y = ppVar, color = ppVar)) +
    geom_point(size = ppNum) +
    labs(title = ppText)
)
```

### Seeding the widgets

Give a placeholder a single **positional argument** and it becomes the
widget’s starting value:

``` r

ptr_app(
  ggplot(mtcars, aes(x = ppVar("wt"), y = ppVar("mpg"))) +
    geom_point(size = ppNum(3), alpha = ppNum(0.6)) +
    labs(title = ppText("Weight vs. mileage"))
)
```

The app now boots with `wt`/`mpg` already picked, size 3, and the title
pre-filled. The default is read *literally* from the formula text — it
is never evaluated as user code. (`ppNum` does accept simple arithmetic
like `ppNum(2 * pi)`, folded at build time against a small allowlist.)

### Nothing renders until you click Update

ggpaintr re-draws only when the **Update plot** button is clicked.
Changing a widget stages a new value but does not redraw on its own.
This keeps a half-typed expression from strobing the plot — and it is
the one thing to remember when scripting the app in tests: set the
inputs, *then* click the button.

## 2. Defining your own placeholders

The five built-ins are themselves registered through the same public API
you are about to use. You define a placeholder by calling one of three
constructors, keyed by the role from Section 1:

- [`ptr_define_placeholder_value()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_define_placeholder_value.md)
- [`ptr_define_placeholder_consumer()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_define_placeholder_consumer.md)
- [`ptr_define_placeholder_source()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_define_placeholder_source.md)

All three are thin wrappers over a shared core, so they take an
overlapping set of arguments. We cover the **value** constructor in full
first, then describe only what is *different* for consumer and source.

A few facts hold for all three:

- The registry is **process-global**. Calling a constructor *registers*
  the keyword as a side effect (and returns a plain-R function, see
  `embellish_eval` below); there is no `placeholders =` argument to
  thread anywhere. Register once near the top of your script, then use
  the keyword in any formula.
- The keyword you register is the keyword you write in the formula. By
  convention built-ins use the `pp` prefix, but yours need not.

### 2.1 A value placeholder — every argument

Here is a custom `ppPercent`: a 0–100 slider whose value is divided by
100 before it reaches the plot. It exercises **every** argument of
[`ptr_define_placeholder_value()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_define_placeholder_value.md).

``` r

ptr_define_placeholder_value(
  keyword = "ppPercent",

  build_ui = function(node, label = NULL, selected = NULL,
                      named_args = list(), ...) {
    step <- named_args$step %||% 1
    shiny::sliderInput(
      node$id, label = label %||% "Percent",
      min = 0, max = 100,
      value = selected %||% node$default %||% 50,
      step = step
    )
  },

  resolve_expr = function(value, node, ...) {
    if (is.null(value)) return(NULL)
    as.numeric(value) / 100
  },

  validate_session_input = function(value, ctx) {
    v <- suppressWarnings(as.numeric(value))
    if (length(v) != 1L || is.na(v) || v < 0 || v > 100) {
      rlang::abort("Percent must be a single number between 0 and 100.")
    }
    value
  },

  parse_positional_arg = ptr_arg_numeric(),
  parse_named_args     = list(step = ptr_arg_numeric()),
  embellish_eval       = function(x, ...) as.numeric(x) / 100,
  ui_text_defaults = list(label = "Percent for {param}")
)
```

Used in a formula — the positional `40` seeds the slider, the named
`step = 5` is forwarded to `build_ui`:

``` r

ptr_app(
  ggplot(mtcars, aes(x = ppVar("wt"), y = ppVar("mpg"))) +
    geom_point(alpha = ppPercent(40, step = 5))
)
```

#### `keyword`

The name to register and to write in formulas. Required.

#### `build_ui` — and why its signature looks the way it does

`build_ui` is a function that returns the Shiny control. Its **first
argument is `node`** (required) — an object carrying, among other
things, `node$id` (the input id you must give your widget) and
`node$default` (the literal positional default from the formula, or
`NULL`).

The framework does not call `build_ui(node)` bare. It calls it through
an internal injector that *also* passes:

- `label` — the resolved label text (from `ui_text_defaults`, possibly
  overridden by the app’s `ui_text`);
- `selected` — `node$default`, injected **only when present and only if
  your function can receive it**;
- `named_args` — the validated named arguments from the formula call.

This is why the signature is
`function(node, label = NULL, selected = NULL, named_args = list(), ...)`:

- You **opt in** to an injected argument by naming it (`label`,
  `selected`, `named_args`) — or by having `...`. The injector checks
  your formals: if you neither name `selected` nor accept `...`, the
  default is never injected. Naming the ones you use and keeping `...`
  to swallow the rest is the safe, forward-compatible shape.
- Seed your widget’s starting value from
  `selected %||% node$default %||% <fallback>`. `selected` carries the
  persisted input across re-renders; `node$default` is the boot seed;
  the fallback keeps the widget usable when the formula gave no default.
- Use `node$id` — and *only* `node$id` — as the widget id. Do not
  namespace it yourself; the framework already did.

#### `resolve_expr`

`function(value, node, ...)` → the value (or expression) spliced back
into the formula. `value` is the current input. Return `NULL` to
contribute nothing (e.g. an empty field), which drops the argument
cleanly. Here we divide by 100 so the plot sees a 0–1 alpha.

#### `validate_session_input`

Optional. `function(value, ctx)`, run before `resolve_expr`. Return the
value to accept it, or
[`rlang::abort()`](https://rlang.r-lib.org/reference/abort.html) to
surface an inline error. For mid-typing artifacts that should not flash
an error, signal
[`ptr_signal_partial()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_signal_partial.md)
instead of aborting — it is caught on the live keystroke path but not on
the draw path. (`ctx` carries context; for value placeholders it is
mostly empty — it earns its keep for consumers, Section 2.2.)

#### `parse_positional_arg`

Declares whether the placeholder accepts a single positional default in
the formula, and validates it. Pass one of the argument validators —
[`ptr_arg_string()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_arg_validators.md),
[`ptr_arg_numeric()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_arg_validators.md),
[`ptr_arg_symbol()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_arg_validators.md),
[`ptr_arg_symbol_or_string()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_arg_validators.md),
[`ptr_arg_expression()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_arg_validators.md).
Each is a factory returning a checker that inspects the default **as
unevaluated code** (no [`eval()`](https://rdrr.io/r/base/eval.html)), so
`ppPercent(40)` is validated to be a numeric literal at translate time.
The element factories also take `vector = TRUE` to accept a `c(...)` of
elements instead of a scalar —
`ptr_arg_numeric(vector = TRUE, length = 2)`,
`ptr_arg_symbol(vector = TRUE)` (a multi-column default like
`c(mpg, hp)`) — so a multi-column consumer can carry a positional
default. Leaving `parse_positional_arg = NULL` (the default) **rejects**
any positional argument — `ppPercent(40)` would error.

#### `parse_named_args`

A fully-named list mapping extra named-argument names to validators, in
the same family as `parse_positional_arg`. It lets the formula write
`ppPercent(40, step = 5)`. The validated values arrive in `build_ui` as
the `named_args` list. The name `shared` is reserved (it is ggpaintr’s
cross-widget binding key — Section 3) and may not appear here.

#### `embellish_eval`

The plain-R meaning of the keyword *outside*
[`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md).
A placeholder-embellished formula must stay valid plain R that still
renders the original plot with no app running; `embellish_eval` is the
callable that supplies that meaning. Each constructor returns this
function, so you can bind it under the keyword name —

``` r

ppPercent <- ptr_define_placeholder_value("ppPercent", ...)
ppPercent(40)   # => 0.4, as ordinary R
```

— which makes a formula that uses the keyword still evaluate as ordinary
ggplot code. The meaning is **author-controlled, never derived** — only
you know what the keyword should mean as live R.

If you omit `embellish_eval`, value and consumer keywords default to
[`embellish_identity()`](https://willju-wangqian.github.io/ggpaintr/reference/embellish_helpers.md)
(the identity `function(x, ...) x`), so the placeholder call is a
transparent no-op wrapper. Two built-in helpers cover the common cases:

- [`embellish_identity()`](https://willju-wangqian.github.io/ggpaintr/reference/embellish_helpers.md)
  — the default; returns its argument unchanged.
- [`embellish_symbol_to_string()`](https://willju-wangqian.github.io/ggpaintr/reference/embellish_helpers.md)
  — captures its argument *unevaluated* and returns the referenced
  column names as a character vector. This is the pattern a
  column-selecting consumer needs to run as plain R: a tidyselect verb
  evaluates an unknown wrapper call in non-masked scope, where bare
  column symbols throw `object 'mpg' not found`; returning the names as
  strings lets the naked formula still select by name.

#### `ui_text_defaults`

A named list of copy defaults over `label`, `help`, `placeholder`, and
`empty_text`, with `{param}` interpolated to the argument name. These
are the *defaults*; an app can override them per-keyword or
per-parameter through `ptr_app(ui_text = ...)`.

### 2.2 A consumer placeholder — the delta

A consumer is a value placeholder that additionally needs the **upstream
column names**. Everything in 2.1 applies — `resolve_expr`,
`validate_session_input`, `parse_positional_arg`, `parse_named_args`,
`embellish_eval`, `ui_text_defaults` all mean the same thing. Only two
things change.

**`build_ui` gains two required arguments: `cols` and `data`.** The
injector fills `cols` with the column names of the data flowing into
this point of the pipeline, and `data` with that data frame, re-running
`build_ui` whenever the upstream changes. Your picker’s `choices` come
from `cols`:

``` r

ptr_define_placeholder_consumer(
  keyword = "colvars",

  build_ui = function(node, cols = character(), data = NULL,
                      label = NULL, selected = character(0), ...) {
    shiny::selectInput(
      node$id, label = label %||% "Columns",
      choices  = cols,
      selected = intersect(selected, cols),  # keep only still-valid picks
      multiple = TRUE
    )
  },

  resolve_expr = function(value, node, ...) {
    if (length(value) == 0L) return(NULL)
    rlang::call2("c", !!!as.list(value))   # c(col1, col2, ...)
  },

  parse_positional_arg = ptr_arg_symbol_or_string(),
  ui_text_defaults = list(label = "Columns for {param}")
  # validate_session_input / parse_named_args / embellish_eval: same shape as 2.1, omitted here.
)
```

``` r

ptr_app(
  mtcars |>
    dplyr::select(colvars) |>
    ggplot(aes(x = ppVar, y = ppVar)) + geom_point()
)
```

**`validate_session_input`’s `ctx` is now useful.** For a consumer,
`ctx$data` holds the upstream data frame, so a validator can do
data-aware checks — reject a non-numeric column, range-check the chosen
values, and so on. (Same `function(value, ctx)` signature as 2.1; the
difference is that `ctx$data` is populated.)

### 2.3 A source placeholder — the delta

A source *produces* the data the rest of the formula reads, so it sits
at the head of a pipeline. The shared arguments (`parse_positional_arg`,
`parse_named_args`, `embellish_eval`, `ui_text_defaults`) work exactly
as in 2.1. Three things differ.

**`resolve_data` replaces `resolve_expr` as the required producer.**
`function(value, node, ...)` must return a data frame. `resolve_expr`
becomes *optional* and defaults to
`function(value, node, ...) rlang::sym(value)` (the symbol that names
the produced frame in generated code) — override it only if you need
different generated code.

**`embellish_eval` defaults to an abort guard**, not identity — a source
has no sensible plain-R meaning until you give it one. Override
`embellish_eval` if you want the formula to be runnable as ordinary R.

**`shortcut = TRUE` adds a framework-owned companion text box.** When
set, ggpaintr renders a sibling `textInput` (at `node$shortcut_id`) into
which the user can type the *name* of an object to load from the app
environment — the same “or type a dataset name” box you saw on
`ppUpload`. Because the framework owns that box, **your `build_ui` must
render only `node$id`** (or, for an env-name-only source, nothing at
all) — rendering the shortcut yourself would bind the id twice.

This `ppDataset` lets the user type the name of any data frame in scope;
the slider/selector style of widget is unnecessary because the framework
text box is the entry point. Note that `resolve_data` runs later, inside
the framework — so we capture the registration environment now (`.env`)
and load from it, rather than reaching for
[`parent.frame()`](https://rdrr.io/r/base/sys.parent.html) at resolve
time:

``` r

.env <- environment()   # the scope whose data frames should be loadable

ptr_define_placeholder_source(
  keyword  = "ppDataset",
  shortcut = TRUE,

  build_ui = function(node, label = NULL, ...) {
    # env-name-only source: the framework's shortcut text box is the sole
    # entry point, so build_ui contributes no widget of its own.
    NULL
  },

  resolve_data = function(value, node, ...) {
    nm <- if (is.character(value) && length(value) == 1L && nzchar(value)) value else NULL
    if (is.null(nm)) return(NULL)
    tryCatch(get(nm, envir = .env, inherits = TRUE),
             error = function(e) NULL)
  },

  resolve_expr     = function(value, node, ...) rlang::sym(value),
  ui_text_defaults = list(label = "Dataset for {param}")
)
```

``` r

ptr_app(
  ppDataset() |> ggplot(aes(x = ppVar("mpg"))) + geom_histogram()
)
```

A source that owns a real widget (e.g. a `selectInput` of dataset names)
is the same shape with `shortcut = FALSE` and a `build_ui` that renders
the picker at `node$id`.

## 3. Multiple plots, and writing your own Shiny app

[`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md)
is the turn-key entry point: it builds the whole Shiny app for you. When
you want to place a ggpaintr plot inside your *own* app — alongside
other UI, several plots at once, controls of your own — you drop down
one level to
[`ptr_ui()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui.md)
/
[`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md)
and own the [`shinyApp()`](https://rdrr.io/pkg/shiny/man/shinyApp.html)
shell yourself.

### One plot inside your own app

You write the `fluidPage` and the server function. Put
`ptr_ui(formula, id)` where the plot’s controls and output should go,
and `ptr_server(formula, id)` in the server with a **matching `id`**.
[`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md)
namespaces itself — call it **bare**, never wrapped in your own
[`moduleServer()`](https://rdrr.io/pkg/shiny/man/moduleServer.html):

[`ptr_ui()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui.md)
and
[`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md)
take the formula the same way
[`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md)
does: pass the
[`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html) call
directly, or — to write it once and hand the *same* formula to both —
store it with
[`rlang::expr()`](https://rlang.r-lib.org/reference/expr.html) and
splice it in with `!!`. (The string form still works as a fallback.)

``` r

f <- rlang::expr(ggplot(mtcars, aes(x = ppVar("wt"), y = ppVar("mpg"))) + geom_point())

ui <- shiny::fluidPage(
  shiny::h3("My dashboard"),
  ptr_ui(!!f, "plot1")
)
server <- function(input, output, session) {
  ptr_server(!!f, "plot1")
}
shiny::shinyApp(ui, server)
```

The `id` (`"plot1"`) is the namespace shared by the UI and the server;
they must agree on it. Omitting it (`id = NULL`) gives bare,
un-namespaced ids — fine for a single plot.

### Sharing one control across several plots

Often several plots should be driven by the *same* control — one x-axis
picker, one size slider — rather than a copy per plot. You declare this
with the reserved `shared = "<key>"` argument on any placeholder.
Placeholders that carry the same key are backed by a single widget.

How that single widget is *placed* depends on how many formulas
reference the key — this is the **partition**:

- A key used by **two or more formulas** is owned by a standalone
  **shared panel** that sits above the plots and drives all of them.
- A key used by **exactly one formula** renders **inline** in that one
  plot’s own controls.

For several plots you build a small coordinator object with
[`ptr_shared()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared.md)
and hand it to three pieces:
[`ptr_shared_panel()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared_panel.md)
(the standalone panel UI), each `ptr_ui(..., shared = obj)`, and
[`ptr_shared_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared_server.md)
(whose result you thread into each `ptr_server(..., shared_state = )`).

The example below has two scatter plots over `iris`. The **size** slider
is shared by both formulas, so it lands in the standalone panel and
moves both plots at once. Each plot’s **x-axis** picker is shared within
its own formula only, so it renders inline under that plot:

``` r

# A custom value placeholder for the shared size control: a 1-6 slider.
ptr_define_placeholder_value(
  keyword        = "ppSize",
  parse_positional_arg = ptr_arg_numeric(),
  build_ui = function(node, label = NULL, selected = NULL, ...) {
    val <- suppressWarnings(as.numeric(selected %||% node$default %||% 3))
    if (length(val) != 1L || is.na(val)) val <- 3
    shiny::sliderInput(node$id, label %||% "Size", min = 1, max = 6, value = val)
  },
  resolve_expr = function(value, node, ...) {
    out <- suppressWarnings(as.numeric(value))
    if (length(out) != 1L || is.na(out)) NULL else out
  }
)

plots <- list(
  rlang::expr(ggplot(iris, aes(x = ppVar(shared = "ax1"), y = Sepal.Width,
                               color = Species)) + geom_point(size = ppSize(shared = "sz"))),
  rlang::expr(ggplot(iris, aes(x = ppVar(shared = "ax2"), y = Petal.Width,
                               color = Species)) + geom_point(size = ppSize(shared = "sz")))
)

obj <- ptr_shared(formulas = plots)        # a list of formulas, passed as-is
obj$panel_keys           # "sz"  -- used by both formulas, so panel-owned

ui <- shiny::fluidPage(
  ptr_shared_panel(obj),                 # holds the shared size slider
  shiny::fluidRow(
    shiny::column(6, ptr_ui(!!plots[[1]], "plot_1", shared = obj)),  # ax1 inline
    shiny::column(6, ptr_ui(!!plots[[2]], "plot_2", shared = obj))   # ax2 inline
  )
)
server <- function(input, output, session) {
  sh <- ptr_shared_server(obj)
  ptr_server(!!plots[[1]], "plot_1", shared_state = sh)
  ptr_server(!!plots[[2]], "plot_2", shared_state = sh)
}
shiny::shinyApp(ui, server)
```

`obj$panel_keys` reports which keys ended up panel-owned — here, `"sz"`.
Move the size slider in the panel and click the panel’s draw button:
both plots re-render in lockstep. Each x-axis picker, being
formula-local, changes only its own plot.

The same partition rule means that with a **single** ggpaintr instance,
reusing one `shared` key several times in one formula needs no
coordinator at all — ggpaintr renders one inline widget and wires every
occurrence to it automatically.
[`ptr_shared()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared.md)
and the panel are only for the multi-formula case.

This is where the tutorial stops. For tailoring labels and copy,
theming, and the trust model behind `ppExpr` and `ppUpload`, see the
companion vignettes.
