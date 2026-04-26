# Fetch a ggpaintr LLM topic by name

Returns the runnable example + commentary for one topic as a single
character string. Designed to back an LLM tool such as
`ggpaintr_docs(topic)`: the model calls it when the user asks for help
with an interactive ggplot task, and receives exactly one focused
example instead of the entire manual.

## Usage

``` r
ptr_llm_topic(name)
```

## Arguments

- name:

  Topic name. Must be one of
  [`ptr_llm_topics()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_llm_topics.md).

## Value

A single character string.

## Details

Each topic is derived from (and kept in sync with) either `README.Rmd`
or one of the two vignettes (`ggpaintr-extensibility`,
`ggpaintr-placeholder-registry`).

## See also

[`ptr_llm_topics()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_llm_topics.md),
[`ptr_llm_primer()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_llm_primer.md)

## Examples

``` r
cat(ptr_llm_topic("level1_ptr_app"))
#> # Level 1 — turn-key app with `ptr_app()`
#> 
#> Use when: the user wants an interactive ggplot explorer and does not
#> need to own the Shiny UI. No Shiny code is written.
#> 
#> ## Signature
#> 
#> ```r
#> ptr_app(
#>   formula,
#>   envir             = parent.frame(),
#>   ui_text           = NULL,
#>   placeholders      = NULL,
#>   checkbox_defaults = NULL,
#>   expr_check        = TRUE
#> )
#> ```
#> 
#> `ptr_app_bslib()` has the same API plus `theme` and `title` for a bslib
#> shell.
#> 
#> ## Minimal example
#> 
#> ```r
#> library(ggpaintr)
#> 
#> ptr_app("
#> ggplot(data = iris, aes(x = var, y = var)) +
#>   geom_point(aes(color = var), size = num) +
#>   labs(title = text)
#> ")
#> ```
#> 
#> Every `var`, `num`, `text` becomes a sidebar widget. Clicking
#> **Update plot** re-renders the plot and refreshes the generated code
#> on the side.
#> 
#> ## Data sources — three paths
#> 
#> 1. **Named frame in the calling environment.** `data = iris` inside the
#>    formula string; `iris` is resolved via `envir`.
#> 2. **`upload` keyword.** Replace `data = iris` with `data = upload` (or
#>    use `upload` anywhere a data frame is needed); the user picks a
#>    `.csv` or `.rds` file at runtime.
#> 3. **Non-syntactic column names.** Wrap the frame with
#>    `ptr_normalize_column_names()` before passing it in; uploads get the
#>    same normalization automatically.
#> 
#> ## When to move up a level
#> 
#> - Need a specific Shiny layout (tabs, custom sidebar) → Level 2.
#> - Need to post-process the `ggplot` object, or render without Shiny
#>   → Level 3.
#> - Need a widget type not in the five built-ins → custom placeholder.
```
