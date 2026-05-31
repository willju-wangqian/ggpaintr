# Wire a Plot-ish Piece to a Slide-Out Code Window via the `</>` Toggle

Output combinator: wraps a plot-ish tag (a bare
[`ptr_ui_plot()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_plot.md)
or the output of
[`ptr_ui_inline_error()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_inline_error.md))
and a bare code piece
([`ptr_ui_code()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_code.md))
in the single `.ptr-output` scope the bundled JavaScript needs,
injecting the `</>` show-code button into the plot card head and
presenting the code inside the draggable slide-out `.ptr-code-window`
(with Copy / Close). The button hides/shows that window purely
DOM-locally — no Shiny input/output is involved. This is the toggle
layout
[`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md)
/
[`ptr_ui()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui.md)
render internally.

## Usage

``` r
ptr_ui_toggle_code(plotish, code)
```

## Arguments

- plotish:

  A plot-ish tag. The designed input is a bare `ptr_ui_plot(id)` or
  `ptr_ui_inline_error(ptr_ui_plot(id), ptr_ui_error(id))` — a single
  `.ptr-card--plot` tag, where the toggle button is injected into the
  card head (DOM byte-identical to the bundled output block). An
  arbitrary custom output also works: a `shiny.tag.list` (e.g.
  [`plotly::plotlyOutput()`](https://rdrr.io/pkg/plotly/man/plotly-shiny.html),
  `ggiraph::girafeOutput()`) or a childless single tag (e.g.
  [`shiny::plotOutput()`](https://rdrr.io/pkg/shiny/man/plotOutput.html))
  gets the toggle button as an explicit sibling inside the `.ptr-output`
  wrapper instead.

- code:

  A code piece, typically `ptr_ui_code(id)` built with the same `id`.
  Its style is irrelevant — this combinator supplies the slide-out
  window chrome around it.

## Value

A [shiny::tag](https://rdrr.io/pkg/shiny/man/reexports.html) — one
`.ptr-output` containing `plotish` (with the toggle button) and the
`.ptr-code-window`-wrapped `code`.

## Details

Use this when you want the familiar toggle behaviour while still owning
the surrounding layout. For fully independent placement of the plot and
code panes (no toggle), keep the bare pieces uncombined — a standalone
[`ptr_ui_code()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_code.md)
is always visible and needs no wiring.

## See also

[`ptr_ui_plot()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_plot.md),
[`ptr_ui_code()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_code.md),
[`ptr_ui_inline_error()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_inline_error.md),
[`ptr_ui_controls()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_controls.md),
[`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md)

## Examples

``` r
ptr_ui_toggle_code(
  ptr_ui_inline_error(ptr_ui_plot("p"), ptr_ui_error("p")),
  ptr_ui_code("p", style = "window")
)
#> <div class="ptr-output">
#>   <div class="ptr-card ptr-card--plot">
#>     <div class="ptr-card__head">
#>       <h3 class="ptr-card__title">Plot</h3>
#>       <button type="button" class="ptr-icon-btn ptr-code-toggle" title="Show generated code" aria-label="Show generated code"><svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.2" stroke-linecap="round" stroke-linejoin="round"><path d="m18 16 4-4-4-4"></path><path d="m6 8-4 4 4 4"></path><path d="m14.5 4-5 16"></path></svg></button>
#>     </div>
#>     <div class="ptr-card__body">
#>       <div class="shiny-plot-output html-fill-item" id="p-ptr_plot" style="width:100%;height:400px;"></div>
#>       <div id="p-ptr_error" class="shiny-html-output"></div>
#>     </div>
#>   </div>
#>   <div class="ptr-code-window">
#>     <div class="ptr-code-window__head">
#>       <span class="ptr-code-window__title">&lt;/&gt; Generated code</span>
#>       <span class="ptr-code-window__actions">
#>         <button type="button" class="ptr-copy-btn">Copy</button>
#>         <button type="button" class="ptr-code-window__close" title="Close" aria-label="Close">&times;</button>
#>       </span>
#>     </div>
#>     <div class="ptr-code-window__body">
#>       <div class="ptr-code-window">
#>         <div class="ptr-code-window__head">
#>           <span class="ptr-code-window__title">&lt;/&gt; Generated code</span>
#>           <span class="ptr-code-window__actions">
#>             <span class="ptr-code-mode">
#>               <div class="form-group shiny-input-container shiny-input-radiogroup shiny-input-container-inline">
#>                 <label class="control-label shiny-label-null" for="p-ptr_code_mode" id="p-ptr_code_mode-label"></label>
#>                 <div id="p-ptr_code_mode" class="radio-group-buttons">
#>                   <div aria-labelledby="p-ptr_code_mode-label" class=" btn-group-xs btn-group-container-sw" data-toggle="buttons" role="group">
#>                     <div class="btn-group btn-group-toggle btn-group-xs" role="group">
#>                       <button class="btn radiobtn btn-default active">
#>                         <input type="radio" autocomplete="off" name="p-ptr_code_mode" value="final" checked="checked"/>
#>                         Final code
#>                       </button>
#>                     </div>
#>                     <div class="btn-group btn-group-toggle btn-group-xs" role="group">
#>                       <button class="btn radiobtn btn-default">
#>                         <input type="radio" autocomplete="off" name="p-ptr_code_mode" value="spec"/>
#>                         Spec
#>                       </button>
#>                     </div>
#>                   </div>
#>                 </div>
#>               </div>
#>             </span>
#>             <button type="button" class="ptr-copy-btn">Copy</button>
#>             <button type="button" class="ptr-code-window__close" title="Close" aria-label="Close">&times;</button>
#>           </span>
#>         </div>
#>         <div class="ptr-code-window__body">
#>           <pre class="shiny-text-output noplaceholder" id="p-ptr_code"></pre>
#>         </div>
#>       </div>
#>     </div>
#>   </div>
#> </div>
```
