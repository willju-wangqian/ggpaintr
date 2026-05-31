# Render the Standalone Shared Panel (L2, Self-Contained)

Renders the single page-level
[`shiny::wellPanel()`](https://rdrr.io/pkg/shiny/man/wellPanel.html)
holding exactly the coordinator's cross-formula keys (`obj$panel_keys`,
referenced by two or more formulas). The panel is self-contained: it
owns its `.ptr-app` theming scope and the bundled ggpaintr asset
dependency, so it can be dropped straight into a host layout. Its server
counterpart
[`ptr_shared_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared_server.md)
must run at the top level.

## Usage

``` r
ptr_shared_panel(obj, css = NULL)
```

## Arguments

- obj:

  A `ptr_shared_spec` from
  [`ptr_shared()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared.md).

- css:

  Optional character vector of paths to additional CSS files; linked
  after `ggpaintr`'s bundled stylesheet so its rules win. See
  [`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md)
  for the full semantics. Defaults to `NULL`.

## Value

A `shiny.tag` `div.ptr-app` wrapping the wellPanel and the asset bundle,
suitable for direct placement in the embedder's UI, or `NULL` when no
panel keys exist.

## Details

Namespacing is inherited from `obj$id`; supply it to
[`ptr_shared()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared.md).

## See also

[`ptr_shared()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared.md),
[`ptr_ui_shared_panel()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_shared_panel.md),
[`ptr_shared_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared_server.md).

## Examples

``` r
obj <- ptr_shared(c(
  "ggplot(mtcars, aes(x = ppVar(shared='x'), y = ppVar)) + geom_point()",
  "ggplot(mtcars, aes(x = ppVar(shared='x'), y = ppVar)) + geom_bar()"
))
ptr_shared_panel(obj)
#> <div class="ptr-app">
#>   <div class="well">
#>     <div class="ptr-shared-panel">
#>       <p class="ptr-shared-panel__title">
#>         Shared controls
#>         <span class="ptr-shared-panel__help" tabindex="0" aria-label="Drives every plot that uses it.">
#>           ?
#>           <span class="ptr-shared-panel__tip" aria-hidden="true">Drives every plot that uses it.</span>
#>         </span>
#>       </p>
#>       <div id="shared_x_ui" class="shiny-html-output"></div>
#>       <button id="ptr_shared_draw_all" type="button" class="btn btn-default action-button"><span class="action-label">Draw all</span></button>
#>       <div id="ptr_shared_errors" class="shiny-html-output"></div>
#>     </div>
#>   </div>
#> </div>
```
