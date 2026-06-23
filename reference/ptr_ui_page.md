# Page shell for hand-composed ggpaintr UIs

Wraps composed L3 pieces in a Bootstrap page + the single `.ptr-app`
theme scope + the (deduped) asset bundle. The only thing an L3 user must
remember.

## Usage

``` r
ptr_ui_page(..., page = shiny::fluidPage, css = NULL)
```

## Arguments

- ...:

  UI children (pieces, layout, your own widgets).

- page:

  A Bootstrap-3 page builder whose `...` are tag children:
  [`shiny::fluidPage`](https://rdrr.io/pkg/shiny/man/fluidPage.html)
  (default), `fixedPage`, `fillPage`, `bootstrapPage`, `basicPage`. NOT
  `navbarPage` (needs a positional `title` + `tabPanel` children) and
  NOT bslib/BS5 pages (the bundled CSS is Bootstrap-3-scoped – see
  [`ptr_app_bslib()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app_bslib.md)).
  For those roots, compose by hand: see
  [`vignette("ggpaintr-tutorial")`](https://willju-wangqian.github.io/ggpaintr/articles/ggpaintr-tutorial.md).

- css:

  Optional character vector of extra stylesheet paths, linked after
  `ggpaintr.css`. See
  [`ptr_css()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_css.md).

## Value

A `shiny.tag` — the Bootstrap page node ready to pass to
[`shiny::shinyApp()`](https://rdrr.io/pkg/shiny/man/shinyApp.html) as
`ui`.

## See also

[`ptr_ui_plot()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_plot.md),
[`ptr_ui_controls()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_controls.md),
[`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md),
[`ptr_css()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_css.md)

## Examples

``` r
f <- rlang::expr(ggplot(mtcars, aes(x = ppVar, y = ppVar)) + geom_point())
ptr_ui_page(
  shiny::sidebarLayout(
    shiny::sidebarPanel(ptr_ui_controls(id = "p", formula = !!f)),
    shiny::mainPanel(ptr_ui_plot("p"))
  )
)
#> <div class="container-fluid">
#>   <div class="ptr-app">
#>     <div class="row">
#>       <div class="col-sm-4">
#>         <form class="well" role="complementary">
#>           <div class="form-group shiny-input-container">
#>             <label class="control-label" id="p-ptr_layer_select-label" for="p-ptr_layer_select">Layer</label>
#>             <select data-state-input="true" id="p-ptr_layer_select" class="selectpicker form-control" autocomplete="off"><option value="ggplot" selected>ggplot</option>
#> <option value="geom_point">geom_point</option></select>
#>           </div>
#>           <div class="tabbable">
#>             <ul class="nav nav-hidden shiny-tab-input" id="p-ptr_layer_tabset" data-tabsetid="9891">
#>               <li class="active">
#>                 <a href="#tab-9891-1" data-toggle="tab" data-bs-toggle="tab" data-value="ggplot">ggplot</a>
#>               </li>
#>               <li>
#>                 <a href="#tab-9891-2" data-toggle="tab" data-bs-toggle="tab" data-value="geom_point">geom_point</a>
#>               </li>
#>             </ul>
#>             <div class="tab-content" data-tabsetid="9891">
#>               <div class="tab-pane active" data-value="ggplot" id="tab-9891-1">
#>                 <div id="p-ptr_layer_content_ggplot" class="ptr-layer-content">
#>                   <div id="p-ggplot_1_1_ppVar_NA_ui" class="shiny-html-output"></div>
#>                   <div id="p-ggplot_1_2_ppVar_NA_ui" class="shiny-html-output"></div>
#>                 </div>
#>               </div>
#>               <div class="tab-pane" data-value="geom_point" id="tab-9891-2">
#>                 <div class="form-group shiny-input-container">
#>                   <div class="checkbox">
#>                     <label>
#>                       <input id="p-geom_point_checkbox" type="checkbox" class="shiny-input-checkbox" checked="checked"/>
#>                       <span>Include this layer in the plot</span>
#>                     </label>
#>                   </div>
#>                 </div>
#>                 <div id="p-ptr_layer_content_geom_point" class="ptr-layer-content"></div>
#>               </div>
#>             </div>
#>           </div>
#>           <button id="p-ptr_update_plot" type="button" class="btn btn-default action-button"><span class="action-label">Update plot</span></button>
#>         </form>
#>       </div>
#>       <div class="col-sm-8" role="main">
#>         <div class="ptr-card ptr-card--plot">
#>           <div class="ptr-card__head">
#>             <h3 class="ptr-card__title">Plot</h3>
#>           </div>
#>           <div class="ptr-card__body">
#>             <div class="shiny-plot-output html-fill-item" id="p-ptr_plot" style="width:100%;height:400px;"></div>
#>           </div>
#>         </div>
#>       </div>
#>     </div>
#>   </div>
#> </div>
```
