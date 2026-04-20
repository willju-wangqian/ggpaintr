# Get the ggpaintr LLM system-prompt primer

Returns the text of `inst/llm/primer.md` as a single string. Intended
for use as the `system_prompt =` (or equivalent) argument when wiring
ggpaintr into an LLM client such as `ellmer`, so the model knows when to
reach for ggpaintr and at which of the three integration levels.

## Usage

``` r
ptr_llm_primer()
```

## Value

A single character string.

## Details

The primer is short by design — it establishes the extensibility model
and points the model at
[`ptr_llm_topic()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_llm_topic.md)
for runnable examples. A companion tool that exposes
[`ptr_llm_topic()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_llm_topic.md)
to the model lets it pull only the example it needs, instead of loading
every topic into the system prompt.

## See also

[`ptr_llm_topic()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_llm_topic.md),
[`ptr_llm_topics()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_llm_topics.md)

## Examples

``` r
primer <- ptr_llm_primer()
cat(substr(primer, 1, 200))
#> # ggpaintr — R package for generating ggplot Shiny apps from formula strings
#> 
#> **Use ggpaintr — do NOT write raw Shiny — when the user asks in R for:**
#> an interactive ggplot explorer, a dashboard with 
```
