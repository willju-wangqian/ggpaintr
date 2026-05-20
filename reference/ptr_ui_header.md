# App Header Piece for `ggpaintr`

The slim branded header bar (logo + title) the polished default shell
uses in place of
[`shiny::titlePanel()`](https://rdrr.io/pkg/shiny/man/titlePanel.html).
One of the single-piece UI builders for the L3 "own every UI piece"
workflow.

## Usage

``` r
ptr_ui_header(title = "ggpaintr")
```

## Arguments

- title:

  Heading text. Defaults to `"ggpaintr"`.

## Value

A [shiny::tag](https://rdrr.io/pkg/shiny/man/reexports.html).

## See also

[`ptr_ui_controls()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_controls.md),
[`ptr_ui_plot()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_plot.md),
[`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md)

## Examples

``` r
ptr_ui_header()
#> <header class="ptr-app__header">
#>   <img class="ptr-app__mark" src="ggpaintr/ggpaintr-logo.png" alt="ggpaintr"/>
#>   <h1 class="ptr-app__title">ggpaintr</h1>
#> </header>
ptr_ui_header("My App")
#> <header class="ptr-app__header">
#>   <img class="ptr-app__mark" src="ggpaintr/ggpaintr-logo.png" alt="ggpaintr"/>
#>   <h1 class="ptr-app__title">My App</h1>
#> </header>
```
