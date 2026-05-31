# Render the Standalone Shared Panel (L3, Bare)

The bare counterpart to
[`ptr_shared_panel()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared_panel.md):
identical inner markup (the wellPanel holding `obj$panel_keys`) with
**no** `.ptr-app` shell and **no** asset bundle. The L3 user supplies
their own shell / assets (e.g. via
[`ptr_ui_page()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_page.md)).

## Usage

``` r
ptr_ui_shared_panel(obj)
```

## Arguments

- obj:

  A `ptr_shared_spec` from
  [`ptr_shared()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared.md).

## Value

A `shiny.tag` wellPanel with no wrapper and no injected assets, or
`NULL` when no panel keys exist.

## Details

Namespacing is inherited from `obj$id`; supply it to
[`ptr_shared()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared.md).

## See also

[`ptr_shared()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared.md),
[`ptr_shared_panel()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared_panel.md),
[`ptr_shared_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared_server.md).

## Examples

``` r
obj <- ptr_shared(c(
  "ggplot(mtcars, aes(x = ppVar(shared='x'), y = ppVar)) + geom_point()",
  "ggplot(mtcars, aes(x = ppVar(shared='x'), y = ppVar)) + geom_bar()"
))
ptr_ui_shared_panel(obj)
#> <div class="well">
#>   <div class="ptr-shared-panel">
#>     <p class="ptr-shared-panel__title">
#>       Shared controls
#>       <span class="ptr-shared-panel__help" tabindex="0" aria-label="Drives every plot that uses it.">
#>         ?
#>         <span class="ptr-shared-panel__tip" aria-hidden="true">Drives every plot that uses it.</span>
#>       </span>
#>     </p>
#>     <div id="shared_x_ui" class="shiny-html-output"></div>
#>     <button id="ptr_shared_draw_all" type="button" class="btn btn-default action-button"><span class="action-label">Draw all</span></button>
#>     <div id="ptr_shared_errors" class="shiny-html-output"></div>
#>   </div>
#> </div>
```
