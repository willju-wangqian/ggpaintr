# Generated-Code Pane Piece for a `ggpaintr` Formula

The generated-code output on its own: a
[`shiny::verbatimTextOutput()`](https://rdrr.io/pkg/shiny/man/textOutput.html)
bound to the `ptr_code` id the server writes to (see
[`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md)).
One of the single-piece UI builders for the L3 "own every UI piece"
workflow.

## Usage

``` r
ptr_ui_code(id = NULL, style = c("panel", "window"))
```

## Arguments

- id:

  Optional module id; the namespace prefix for the output. Defaults to
  `NULL` (identity namespace). When set, must match the `id` passed to
  the other piece functions and the server wiring.

- style:

  `"panel"` (default) renders a plain, always-visible code card suitable
  for free placement. `"window"` renders the draggable slide-out window
  (with Copy / Close) used by the bundled apps; it is hidden until
  toggled and only works when wired via
  [`ptr_ui_toggle_code()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_toggle_code.md).

## Value

A [shiny::tag](https://rdrr.io/pkg/shiny/man/reexports.html).

## See also

[`ptr_ui_toggle_code()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_toggle_code.md),
[`ptr_ui_plot()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_plot.md),
[`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md)

## Examples

``` r
ptr_ui_code("myplot")
#> <div class="ptr-card ptr-card--code">
#>   <div class="ptr-card__head">
#>     <h3 class="ptr-card__title">Generated code</h3>
#>     <span class="ptr-code-mode">
#>       <div class="form-group shiny-input-container shiny-input-radiogroup shiny-input-container-inline">
#>         <label class="control-label shiny-label-null" for="myplot-ptr_code_mode" id="myplot-ptr_code_mode-label"></label>
#>         <div id="myplot-ptr_code_mode" class="radio-group-buttons">
#>           <div aria-labelledby="myplot-ptr_code_mode-label" class=" btn-group-xs btn-group-container-sw" data-toggle="buttons" role="group">
#>             <div class="btn-group btn-group-toggle btn-group-xs" role="group">
#>               <button class="btn radiobtn btn-default active">
#>                 <input type="radio" autocomplete="off" name="myplot-ptr_code_mode" value="final" checked="checked"/>
#>                 Final code
#>               </button>
#>             </div>
#>             <div class="btn-group btn-group-toggle btn-group-xs" role="group">
#>               <button class="btn radiobtn btn-default">
#>                 <input type="radio" autocomplete="off" name="myplot-ptr_code_mode" value="spec"/>
#>                 Spec
#>               </button>
#>             </div>
#>           </div>
#>         </div>
#>       </div>
#>     </span>
#>   </div>
#>   <div class="ptr-card__body">
#>     <pre class="shiny-text-output noplaceholder" id="myplot-ptr_code"></pre>
#>   </div>
#> </div>
ptr_ui_code("myplot", style = "window")
#> <div class="ptr-code-window">
#>   <div class="ptr-code-window__head">
#>     <span class="ptr-code-window__title">&lt;/&gt; Generated code</span>
#>     <span class="ptr-code-window__actions">
#>       <span class="ptr-code-mode">
#>         <div class="form-group shiny-input-container shiny-input-radiogroup shiny-input-container-inline">
#>           <label class="control-label shiny-label-null" for="myplot-ptr_code_mode" id="myplot-ptr_code_mode-label"></label>
#>           <div id="myplot-ptr_code_mode" class="radio-group-buttons">
#>             <div aria-labelledby="myplot-ptr_code_mode-label" class=" btn-group-xs btn-group-container-sw" data-toggle="buttons" role="group">
#>               <div class="btn-group btn-group-toggle btn-group-xs" role="group">
#>                 <button class="btn radiobtn btn-default active">
#>                   <input type="radio" autocomplete="off" name="myplot-ptr_code_mode" value="final" checked="checked"/>
#>                   Final code
#>                 </button>
#>               </div>
#>               <div class="btn-group btn-group-toggle btn-group-xs" role="group">
#>                 <button class="btn radiobtn btn-default">
#>                   <input type="radio" autocomplete="off" name="myplot-ptr_code_mode" value="spec"/>
#>                   Spec
#>                 </button>
#>               </div>
#>             </div>
#>           </div>
#>         </div>
#>       </span>
#>       <button type="button" class="ptr-copy-btn">Copy</button>
#>       <button type="button" class="ptr-code-window__close" title="Close" aria-label="Close">&times;</button>
#>     </span>
#>   </div>
#>   <div class="ptr-code-window__body">
#>     <pre class="shiny-text-output noplaceholder" id="myplot-ptr_code"></pre>
#>   </div>
#> </div>
```
