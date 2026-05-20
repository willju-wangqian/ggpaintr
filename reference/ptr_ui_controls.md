# Controls Piece for a `ggpaintr` Formula

The generated control widgets (layer picker, per-layer parameter panels,
the "Update plot" button) as a bare
[`shiny::tagList()`](https://rdrr.io/pkg/shiny/man/reexports.html) with
**no** `.ptr-app` wrapper and **no** bundled assets. One of the
single-piece UI builders for the L3 "own every UI piece" workflow:
compose it with
[`ptr_ui_assets()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_assets.md)
and the output pieces, place each wherever you like, and wire the server
with
[`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md)
/
[`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md).

## Usage

``` r
ptr_ui_controls(
  formula,
  id = NULL,
  ui_text = NULL,
  checkbox_defaults = NULL,
  expr_check = TRUE,
  shared = NULL
)
```

## Arguments

- formula:

  A single formula string with `ggpaintr` placeholders.

- id:

  Optional module id; the namespace prefix for inputs. Defaults to
  `NULL` (identity namespace). When set, must match the `id` passed to
  the other piece functions and the server wiring.

- ui_text:

  Optional named list of copy overrides; see
  [`ptr_ui_text()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_text.md)
  for the full schema and current defaults.

- checkbox_defaults:

  Optional named list of initial checked states.

- expr_check:

  Controls `expr` placeholder validation: `TRUE` (default) applies the
  built-in denylist + AST walker; `FALSE` disables all validation; a
  `list` with `deny_list`/`allow_list` entries customises the policy.
  See
  [`vignette("ggpaintr-safety")`](https://willju-wangqian.github.io/ggpaintr/articles/ggpaintr-safety.md).

- shared:

  Optional coordinator object from
  [`ptr_shared()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared.md)
  for the multi-instance embedding. When `NULL` (the single-instance
  default) the inline "Shared controls" section renders **every**
  `shared = "..."` placeholder in `formula`. When a `ptr_shared_spec` is
  supplied, its cross-formula keys (`shared$panel_keys`) are excluded
  here because they belong to the one standalone
  [`ptr_shared_panel()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared_panel.md);
  only this formula's formula-local shared keys render inline.

## Value

A [`shiny::tagList()`](https://rdrr.io/pkg/shiny/man/reexports.html).

## Details

Because the panel includes a
[`shinyWidgets::pickerInput()`](https://dreamrs.github.io/shinyWidgets/reference/pickerInput.html)
(the layer selector) and the Bootstrap grid, it must be rendered inside
a Bootstrap page that also carries the `.ptr-app` theme scope and the
asset bundle. Don't assemble that scaffolding by hand: wrap your
composed pieces in
[`ptr_ui_page()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_page.md),
which *is* the Bootstrap page and owns the single `.ptr-app` scope + the
(deduped) assets. For a `navbarPage` or bslib root (which
[`ptr_ui_page()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_page.md)
does not cover) see the decomposition recipe in
[`vignette("ggpaintr-use-cases")`](https://willju-wangqian.github.io/ggpaintr/articles/ggpaintr-use-cases.md).

For finer control still — placing individual placeholder widgets
independently rather than the whole panel — register a custom
placeholder type; see
[`ptr_define_placeholder_value()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_define_placeholder_value.md).

## See also

[`ptr_ui_page()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_page.md),
[`ptr_ui_assets()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_assets.md),
[`ptr_ui_plot()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_plot.md),
[`ptr_ui_code()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_code.md),
[`ptr_shared()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared.md),
[`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md)

## Examples

``` r
ptr_ui_controls(
  "ggplot(mtcars, aes(x = var, y = var)) + geom_point()",
  id = "p"
)
#> <div class="form-group shiny-input-container">
#>   <label class="control-label" id="p-ptr_layer_select-label" for="p-ptr_layer_select">Layer</label>
#>   <select data-state-input="true" id="p-ptr_layer_select" class="selectpicker form-control" autocomplete="off"><option value="ggplot" selected>ggplot</option>
#> <option value="geom_point">geom_point</option></select>
#> </div>
#> <div class="tabbable">
#>   <ul class="nav nav-hidden shiny-tab-input" id="p-ptr_layer_tabset" data-tabsetid="6312">
#>     <li class="active">
#>       <a href="#tab-6312-1" data-toggle="tab" data-bs-toggle="tab" data-value="ggplot">ggplot</a>
#>     </li>
#>     <li>
#>       <a href="#tab-6312-2" data-toggle="tab" data-bs-toggle="tab" data-value="geom_point">geom_point</a>
#>     </li>
#>   </ul>
#>   <div class="tab-content" data-tabsetid="6312">
#>     <div class="tab-pane active" data-value="ggplot" id="tab-6312-1">
#>       <div id="p-ptr_layer_content_ggplot" class="ptr-layer-content">
#>         <div id="p-ggplot_1_1_var_NA_ui" class="shiny-html-output"></div>
#>         <div id="p-ggplot_1_2_var_NA_ui" class="shiny-html-output"></div>
#>       </div>
#>     </div>
#>     <div class="tab-pane" data-value="geom_point" id="tab-6312-2">
#>       <div class="form-group shiny-input-container">
#>         <div class="checkbox">
#>           <label>
#>             <input id="p-geom_point_checkbox" type="checkbox" class="shiny-input-checkbox" checked="checked"/>
#>             <span>Include this layer in the plot</span>
#>           </label>
#>         </div>
#>       </div>
#>       <div id="p-ptr_layer_content_geom_point" class="ptr-layer-content"></div>
#>     </div>
#>   </div>
#> </div>
#> <button id="p-ptr_update_plot" type="button" class="btn btn-default action-button"><span class="action-label">Update plot</span></button>
```
