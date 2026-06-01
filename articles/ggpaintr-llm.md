# Using ggpaintr from an LLM with ellmer

``` r

library(ggpaintr)
```

## 1. Motivation

Large language models are fluent at writing raw
[`shiny::fluidPage()`](https://rdrr.io/pkg/shiny/man/fluidPage.html) +
`server <- function(input, output, session) {...}` scaffolding from
memory. They are *not* fluent at writing ggpaintr, because every model
today has zero prior exposure to this package. Without help, a user
asking “build me a Shiny app where I can pick columns from iris and see
a scatter plot” gets hundreds of lines of hand-rolled Shiny code that
the ggpaintr
[`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md)
one-liner would have covered.

This vignette shows how to make any
[ellmer](https://ellmer.tidyverse.org/) chat session reach for ggpaintr
first, using two exported helpers:

- [`ptr_llm_primer()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_llm_primer.md)
  — the system-prompt preamble that teaches the model the three-level
  integration model and the “call the docs tool before writing code”
  rule.
- [`ptr_llm_register()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_llm_register.md)
  — one-line registration of the `ggpaintr_docs` tool that serves
  focused runnable examples on demand.

The tool is backed by
[`ptr_llm_topic()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_llm_topic.md),
which returns the content of one file under `inst/llm/topics/` (12
topics, ~2,600–10,600 characters each). The LLM pulls only the topic it
needs, instead of the whole manual loading the context window on every
turn.

## 2. Quick start — three lines of setup

``` r

library(ellmer)
library(ggpaintr)

chat <- chat_anthropic(system_prompt = ptr_llm_primer())
ptr_llm_register(chat)

chat$chat("Build a Shiny app where the user picks X and Y columns from mtcars.")
```

Under the hood:

- [`chat_anthropic()`](https://ellmer.tidyverse.org/reference/chat_anthropic.html)
  loads the primer (~4,000 tokens) into the system prompt, which stays
  in context every turn.
- [`ptr_llm_register()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_llm_register.md)
  attaches a single tool named `ggpaintr_docs` to the chat session. The
  tool’s one argument — `name` — is constrained by
  [`type_enum()`](https://ellmer.tidyverse.org/reference/type_boolean.html)
  to the exact set of topic names returned by
  [`ptr_llm_topics()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_llm_topics.md),
  so the LLM cannot request a topic that does not exist.

On the first turn the model sees the user’s prompt, recognises it as a
plot-explorer task, calls `ggpaintr_docs(name = "level1_ptr_app")`, and
writes a `ptr_app(...)` snippet rather than raw Shiny.

## 3. What the primer + tool split actually buys you

A common alternative is to stuff the entire ggpaintr manual into the
system prompt. That works but wastes tokens: every chat turn pays for
the full manual whether or not it is relevant.

The primer/tool split is a lightweight form of retrieval:

| Layer | When it loads | What it teaches |
|----|----|----|
| Primer | Every turn (~4,000 tokens) | ggpaintr exists; 3-level model; decision rule. |
| `ggpaintr_docs(topic)` | On demand | One focused runnable example (~650–2,650 tokens). |

The primer has an explicit rule: *“Before writing R code for an
interactive plot task, call `ggpaintr_docs(topic)` to fetch the runnable
example.”* That rule plus the enum-constrained argument is what makes
the LLM reliably reach for the tool instead of hallucinating an API.

## 4. The available topics

``` r

ptr_llm_topics()
#>  [1] "custom_placeholder"   "formula_syntax"       "level1_ptr_app"      
#>  [4] "level1_ptr_options"   "level2_custom_ids"    "level2_module"       
#>  [7] "level2_shared"        "level2_ui_text"       "level3_custom_render"
#> [10] "level3_gg_extra"      "level3_layout"        "overview"
```

A short guide to each:

| Topic | When the LLM should call it |
|----|----|
| `overview` | First orientation — the 3-level model and decision rule. |
| `formula_syntax` | Which placeholder keyword goes where; pipelines; `shared = "..."`. |
| `level1_ptr_app` | Quick turn-key single-plot app ([`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md)). |
| `level1_ptr_options` | Package-global settings: verbosity, layer-checkbox default. |
| `level2_module` | Embed the self-contained default-layout block ([`ptr_ui()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui.md) / [`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md)) in the user’s own Shiny app. |
| `level2_custom_ids` | Id collisions, the `ptr_` reserved prefix, generated-id grammar. |
| `level2_shared` | Multiple ggpaintr instances + the shared-coordinator trio ([`ptr_shared()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared.md) / [`ptr_shared_panel()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared_panel.md) / [`ptr_shared_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared_server.md)) for cross-instance widgets. |
| `level2_ui_text` | Override labels, help, placeholders; multi-section cascade. |
| `level3_custom_render` | Own `renderPlot()` / `renderPlotly()` reading `state$runtime()`. |
| `level3_layout` | Own the layout — bare `ptr_ui_*` pieces, the two combinators, the optional [`ptr_ui_page()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_page.md) shell. |
| `level3_gg_extra` | Round-trip host layers into the generated code pane. |
| `custom_placeholder` | Date pickers, sliders, any widget not in the five built-ins. |

Topics whose example is a standalone runnable app open with a
[`library()`](https://rdrr.io/r/base/library.html) preamble; signature-
or fragment-only topics (`level1_ptr_app`, `level1_ptr_options`,
`overview`) do not, by design.

## 5. Inspecting what the LLM will see

You can read any topic directly, without booting a chat session, which
is useful when writing integration tests or checking content before
release:

``` r

# Head of the primer
cat(substr(ptr_llm_primer(), 1, 400), "\n...")
#> # ggpaintr — R package for generating ggplot Shiny apps from formula strings
#> 
#> **Use ggpaintr — do NOT write raw Shiny — when the user asks in R for:** an interactive ggplot explorer, a dashboard with widgets tied to a ggplot, letting a user pick columns/labels/sizes for a plot, uploading a CSV / TSV / RDS / Excel / JSON file and plotting it. Raw `shiny::fluidPage()` for plot exploration is the wro 
#> ...
```

``` r

# A full topic payload
cat(ptr_llm_topic("level1_ptr_app"))
```

    #> # Level 1 — turn-key app with `ptr_app()`
    #> 
    #> Use when: the user wants an interactive ggplot explorer and does not need to own the Shiny UI. No Shiny code is written.
    #> 
    #> ## Signature
    #> 
    #> ```r
    #> ptr_app(
    #>   formula,
    #>   envir          = parent.frame(),
    #>   ui_text        = NULL,
    #>   expr_check     = TRUE,
    #>   safe_to_remove = character(),
    #>   css            = NULL,
    #>   spec           = NULL
    #> )
    #> ```
    #> 
    #> There is no `placeholders =` argument: custom placeholder keywords are registered against a **process-global** registry via `ptr_define_placeholder_value()` / `_consumer()` / `_source()` before the app launches (see `custom_placeholder`).
    #> 
    #> ## Minimal example
    #> 
    #> ```r
    #> library(ggpaintr)
    #> 
    #> ptr_app(
    #>   "ggplot(iris, aes(ppVar, ppVar, color = ppVar)) + geom_point() + labs(title = ppText)"
    #> )
    #> ```
    #> 
    #> Every `ppVar`, `ppText` becomes a sidebar widget. Clicking **Update plot** re-renders the plot and refreshes the generated code on the side.
    #> 
    #> ## Single-instance shared widgets — no coordinator
    #> 
    #> One ggpaintr instance never sees `ptr_shared()` / `ptr_shared_panel()`. Every shared key is formula-local by definition and auto-renders in the inline shared section. Write the annotation and you are done:
    #> 
    #> ```r
    #> ptr_app(
    #>   "ggplot(iris, aes(x = ppVar(shared = 'col'), y = ppVar(shared = 'col'),
    #>                     color = Species)) + geom_point()"
    #> )
    #> ```
    #> 
    #> The standalone shared panel (and the coordinator that feeds it) only exists for **multiple** instances — see `level2_shared`.
    #> 
    #> ## Data sources — three paths
    #> 
    #> 1. **Named frame in the calling environment.** `data = iris` inside the formula string; `iris` is resolved via `envir`.
    #> 2. **`ppUpload` keyword.** Replace `data = iris` with `data = ppUpload` (or use `ppUpload` anywhere a data frame is needed); the user picks a `.csv`, `.tsv`, `.rds`, `.xlsx`, `.xls`, or `.json` file at runtime.
    #> 3. **Non-syntactic column names.** Wrap the frame with `ptr_normalize_column_names()` before passing it in; uploads get the same normalization automatically.
    #> 
    #> ## When to move up a level
    #> 
    #> - Need a specific Shiny layout but ggpaintr's default block is fine → Level 2 (`ptr_ui()` / `ptr_server()`; see `level2_module`).
    #> - Need multiple linked instances sharing one widget → Level 2 shared trio (see `level2_shared`).
    #> - Need to hand-place every pane, or render the plot yourself (Plotly, ggiraph), or post-process the ggplot object → Level 3 (`ptr_server()` returns the state; see `level3_layout` and `level3_custom_render`).
    #> - Need a widget type not in the five built-ins → custom placeholder (see `custom_placeholder`).

## 6. Provider swap and model choice

The tool definition is provider-neutral — the same registration line
works with every chat constructor ellmer exports.

``` r

# Anthropic
chat <- chat_anthropic(system_prompt = ptr_llm_primer(),
                       model = "claude-sonnet-4-6")
ptr_llm_register(chat)

# OpenAI
chat <- chat_openai(system_prompt = ptr_llm_primer(),
                    model = "gpt-4o-mini")
ptr_llm_register(chat)

# Google
chat <- chat_google_gemini(system_prompt = ptr_llm_primer())
ptr_llm_register(chat)

# Local (ollama)
chat <- chat_ollama(system_prompt = ptr_llm_primer(),
                    model = "llama3.1")
ptr_llm_register(chat)
```

The primer is small; any model that supports tool calling will work. For
fast iteration on the registration itself, a small and cheap model
(Haiku, gpt-4o-mini) is a reasonable default.

## 7. Manual registration — what the helper does

[`ptr_llm_register()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_llm_register.md)
is a thin convenience wrapper. If you need to customise the tool
(different name, tighter description, extra arguments), do it by hand:

``` r

library(ellmer)

ggpaintr_docs <- tool(
  ptr_llm_topic,
  name        = "ggpaintr_docs",
  description = paste(
    "Fetch a runnable ggpaintr example for one integration topic.",
    "Call before writing R code for interactive ggplot tasks."
  ),
  arguments = list(
    name = type_enum(
      ptr_llm_topics(),
      "Which topic to fetch."
    )
  )
)

chat <- chat_anthropic(system_prompt = ptr_llm_primer())
chat$register_tool(ggpaintr_docs)
```

The pieces:

- `tool(ptr_llm_topic, name = ..., description = ..., arguments = ...)`
  — ellmer’s constructor, wrapping the exported R function.
- `type_enum(ptr_llm_topics(), ...)` — constrains the LLM’s choice of
  topic to the exact names the package ships, preventing hallucinated
  topic names.
- `chat$register_tool(...)` — attaches the tool to the chat session.

If you use this form, refresh the enum by re-running
[`ptr_llm_topics()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_llm_topics.md)
whenever the package updates. The helper snapshots the list once at
registration time; any new topics will not be visible to the LLM until
you re-register.

## 8. Testing without spending tokens

The ellmer [`tool()`](https://ellmer.tidyverse.org/reference/tool.html)
object wraps the underlying R function — you can still call the function
directly for tests or sanity checks:

``` r

# Pull a topic without a chat session.
payload <- ptr_llm_topic("level2_module")
nchar(payload)
#> [1] 5268
```

[`ptr_llm_topics()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_llm_topics.md)
returns the canonical list; if you are adding a topic in a downstream
repo, assert the name is present before sending the registration out to
users.

## 9. When to re-register

- **After upgrading ggpaintr** — new topics will not be reachable until
  [`ptr_llm_register()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_llm_register.md)
  runs again (the enum list is snapshotted).
- **Switching chat providers** — registration is per-`Chat` object, so
  every new session needs its own registration.
- **Changing the tool name** — rebuild the chat from scratch; `Chat`
  objects do not expose a public unregister API in current ellmer.

## 10. Current behavior boundary

- The primer and topic files are bundled under `inst/llm/`. They are
  hand-curated from `README.Rmd` and
  [`vignette("ggpaintr-tutorial")`](https://willju-wangqian.github.io/ggpaintr/articles/ggpaintr-tutorial.md)
  — but regenerating them from the source vignettes is not yet
  automated. If you edit a vignette chunk, update the corresponding
  topic file manually.
- [`ptr_llm_register()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_llm_register.md)
  requires [ellmer](https://ellmer.tidyverse.org/) in `Suggests:` — it
  is not loaded until you call the helper. If ellmer is not installed,
  [`ptr_llm_register()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_llm_register.md)
  errors with a clear message.
- The helper sets only the tool. The system prompt is your
  responsibility — pass
  [`ptr_llm_primer()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_llm_primer.md)
  to the `chat_*()` constructor explicitly.
