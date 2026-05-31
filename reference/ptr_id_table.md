# Enumerate every Shiny id a ggpaintr formula produces

Walks the typed AST of a single formula and returns one row per Shiny id
ever rendered by
[`ptr_ui()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui.md)
/
[`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md)
for that formula: placeholder sleeves, inner widgets, source companions,
layer-level controls (checkbox, subtab, content container), pipeline
stage toggles, plus the static infrastructure ids (`ptr_plot`,
`ptr_error`, `ptr_code`, `ptr_code_mode`, `ptr_update_plot`).

## Usage

``` r
ptr_id_table(formula, id = NULL)
```

## Arguments

- formula:

  A single ggpaintr formula string (the same input you would pass to
  [`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md)
  /
  [`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md)).

- id:

  Optional outer namespace — the same string you pass to
  `ptr_server(formula, id = ...)`. When supplied, rows whose `scope` is
  `"instance"` come back with that namespace prefixed (`<id>-<raw>`);
  `"global"` rows stay bare. When `NULL`, every row shows its bare id.

## Value

A `data.frame` with one row per Shiny id and ten columns:

- `id` — the Shiny id (prefixed when `id=` is given and
  `scope == "instance"`).

- `kind` — `"input_widget"` or `"output_slot"`.

- `role` — semantic role: `"placeholder"`, `"source_companion"`,
  `"layer_checkbox"`, `"layer_subtab"`, `"layer_content"`,
  `"stage_enabled"`, `"ptr_plot"`, `"ptr_error"`, `"ptr_code"`,
  `"ptr_code_mode"`, `"ptr_update_plot"`.

- `scope` — `"instance"` (namespaced via `shiny::NS(id)`) or `"global"`
  (un-namespaced; only used by cross-formula shared-panel keys in a
  [`ptr_shared()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared.md)
  setup — see Single-formula section).

- `include_in_ui` — `TRUE` when the row is something the user *places*
  in their custom UI; `FALSE` when the server populates it inside a
  sleeve (`<id>_ui`) and the user must not place it manually. The two
  `FALSE` cases are the inner placeholder widget and `ppUpload`'s
  file-name companion.

- `layer` — layer name (`"ggplot"`, `"geom_point"`, …) or `NA`.

- `keyword` — placeholder keyword (`"ppVar"`/`"ppText"`/…) or `NA`.

- `param` — argument or aesthetic name (`"x"`, `"color"`, `"data"`, …)
  or `NA`.

- `parent_call` — immediate enclosing call (`"aes"`, `"head"`, or the
  layer itself when the placeholder is a direct layer arg) or `NA`.

- `shared` — shared key (`"xcol"`, …) or `NA`.

## Details

Advanced (L3) users call this to build their own UI by hand and still
get the server's bindings to match. The default-layout L2 path does not
need it;
[`ptr_ui()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui.md)
emits these ids internally.

## Stability under formula edits

Adding a layer at the end keeps existing ids stable. Reordering
arguments inside
[`aes()`](https://ggplot2.tidyverse.org/reference/aes.html) or a
pipeline shifts positional paths and therefore changes ids; renaming a
placeholder keyword or its `shared=` annotation also changes ids.

## Single-formula

`ptr_id_table()` accepts a single formula. In multi-instance
(`ptr_shared(formulas = list(…))`) layouts the partition rule decides
whether `shared_<key>` lives in an instance's inline section or the
cross-formula panel. The `scope` column reflects the single-formula
interpretation (`"instance"`); when embedding into a shared-panel
context those rows are bare (`"global"`) and you should override
accordingly.

## Examples

``` r
ptr_id_table("ggplot(ppUpload, aes(x = ppVar)) + geom_point(color = ppText)")
#>                               id         kind             role    scope
#> 1                ptr_update_plot input_widget  ptr_update_plot instance
#> 2                       ptr_plot  output_slot         ptr_plot instance
#> 3                      ptr_error  output_slot        ptr_error instance
#> 4                       ptr_code  output_slot         ptr_code instance
#> 5                  ptr_code_mode input_widget    ptr_code_mode instance
#> 6       ptr_layer_content_ggplot  output_slot    layer_content instance
#> 7                  ggplot_subtab input_widget     layer_subtab instance
#> 8        ggplot_0_ppUpload_NA_ui  output_slot      placeholder instance
#> 9           ggplot_0_ppUpload_NA input_widget      placeholder instance
#> 10 ggplot_0_ppUpload_NA_shortcut input_widget source_companion instance
#> 11        ggplot_1_1_ppVar_NA_ui  output_slot      placeholder instance
#> 12           ggplot_1_1_ppVar_NA input_widget      placeholder instance
#> 13           geom_point_checkbox input_widget   layer_checkbox instance
#> 14  ptr_layer_content_geom_point  output_slot    layer_content instance
#> 15     geom_point_1_ppText_NA_ui  output_slot      placeholder instance
#> 16        geom_point_1_ppText_NA input_widget      placeholder instance
#>    include_in_ui      layer  keyword param parent_call shared
#> 1           TRUE       <NA>     <NA>  <NA>        <NA>   <NA>
#> 2           TRUE       <NA>     <NA>  <NA>        <NA>   <NA>
#> 3           TRUE       <NA>     <NA>  <NA>        <NA>   <NA>
#> 4           TRUE       <NA>     <NA>  <NA>        <NA>   <NA>
#> 5           TRUE       <NA>     <NA>  <NA>        <NA>   <NA>
#> 6           TRUE     ggplot     <NA>  <NA>        <NA>   <NA>
#> 7           TRUE     ggplot     <NA>  <NA>        <NA>   <NA>
#> 8           TRUE     ggplot ppUpload  data      ggplot   <NA>
#> 9          FALSE     ggplot ppUpload  data      ggplot   <NA>
#> 10         FALSE     ggplot ppUpload  data      ggplot   <NA>
#> 11          TRUE     ggplot    ppVar     x         aes   <NA>
#> 12         FALSE     ggplot    ppVar     x         aes   <NA>
#> 13          TRUE geom_point     <NA>  <NA>        <NA>   <NA>
#> 14          TRUE geom_point     <NA>  <NA>        <NA>   <NA>
#> 15          TRUE geom_point   ppText color  geom_point   <NA>
#> 16         FALSE geom_point   ppText color  geom_point   <NA>
ptr_id_table("ggplot(ppUpload, aes(x = ppVar))", id = "myplot")
#>                                      id         kind             role    scope
#> 1                myplot-ptr_update_plot input_widget  ptr_update_plot instance
#> 2                       myplot-ptr_plot  output_slot         ptr_plot instance
#> 3                      myplot-ptr_error  output_slot        ptr_error instance
#> 4                       myplot-ptr_code  output_slot         ptr_code instance
#> 5                  myplot-ptr_code_mode input_widget    ptr_code_mode instance
#> 6       myplot-ptr_layer_content_ggplot  output_slot    layer_content instance
#> 7                  myplot-ggplot_subtab input_widget     layer_subtab instance
#> 8        myplot-ggplot_0_ppUpload_NA_ui  output_slot      placeholder instance
#> 9           myplot-ggplot_0_ppUpload_NA input_widget      placeholder instance
#> 10 myplot-ggplot_0_ppUpload_NA_shortcut input_widget source_companion instance
#> 11        myplot-ggplot_1_1_ppVar_NA_ui  output_slot      placeholder instance
#> 12           myplot-ggplot_1_1_ppVar_NA input_widget      placeholder instance
#>    include_in_ui  layer  keyword param parent_call shared
#> 1           TRUE   <NA>     <NA>  <NA>        <NA>   <NA>
#> 2           TRUE   <NA>     <NA>  <NA>        <NA>   <NA>
#> 3           TRUE   <NA>     <NA>  <NA>        <NA>   <NA>
#> 4           TRUE   <NA>     <NA>  <NA>        <NA>   <NA>
#> 5           TRUE   <NA>     <NA>  <NA>        <NA>   <NA>
#> 6           TRUE ggplot     <NA>  <NA>        <NA>   <NA>
#> 7           TRUE ggplot     <NA>  <NA>        <NA>   <NA>
#> 8           TRUE ggplot ppUpload  data      ggplot   <NA>
#> 9          FALSE ggplot ppUpload  data      ggplot   <NA>
#> 10         FALSE ggplot ppUpload  data      ggplot   <NA>
#> 11          TRUE ggplot    ppVar     x         aes   <NA>
#> 12         FALSE ggplot    ppVar     x         aes   <NA>
```
