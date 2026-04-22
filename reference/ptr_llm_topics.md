# List available ggpaintr LLM topic names

Returns the character vector of topic names accepted by
[`ptr_llm_topic()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_llm_topic.md).
Matches the files in `inst/llm/topics/` (stripped of the `.md`
extension), sorted alphabetically.

## Usage

``` r
ptr_llm_topics()
```

## Value

A character vector.

## See also

[`ptr_llm_topic()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_llm_topic.md),
[`ptr_llm_primer()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_llm_primer.md)

## Examples

``` r
ptr_llm_topics()
#>  [1] "custom_placeholder"   "formula_syntax"       "level1_ptr_app"      
#>  [4] "level2_custom_ids"    "level2_embed"         "level2_namespacing"  
#>  [7] "level2_ui_text"       "level3_custom_render" "level3_gg_extra"     
#> [10] "level3_headless"      "overview"            
```
