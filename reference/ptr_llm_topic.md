# Fetch a ggpaintr LLM topic

Returns the runnable example + commentary for one topic as a single
character string. Designed to back an LLM tool such as
`ggpaintr_docs(topic)`: the model calls it when the user asks for help
with an interactive ggplot task, and receives exactly one focused
example instead of the entire manual.

## Usage

``` r
ptr_llm_topic(topic)
```

## Arguments

- topic:

  Topic name. Must be one of
  [`ptr_llm_topics()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_llm_topics.md).

## Value

A single character string.

## Details

Each topic is derived from (and kept in sync with) either `README.Rmd`
or the tutorial vignette (`ggpaintr-tutorial`).

## See also

[`ptr_llm_topics()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_llm_topics.md),
[`ptr_llm_primer()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_llm_primer.md)

## Examples

``` r
cat(ptr_llm_topic("level1_ptr_app"))
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
```
