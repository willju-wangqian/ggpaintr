# Bundled CSS / JS Assets Piece for `ggpaintr`

The full `ggpaintr` asset bundle as a
[`shiny::tagList()`](https://rdrr.io/pkg/shiny/man/reexports.html): the
structural-layer dependency (`ptr_set_class` handler + stage CSS), the
cosmetic `ggpaintr.css` theme dependency + code-window JavaScript, and
any user override stylesheets. The CSS/JS ship as
[`htmltools::htmlDependency()`](https://rstudio.github.io/htmltools/reference/htmlDependency.html)
objects, so emitting this anywhere on a page — even several times —
yields exactly one `<head>` injection of each. The single-piece UI
builders
([`ptr_ui_plot()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_plot.md),
[`ptr_ui_controls()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_controls.md),
...) emit **no** assets; an L3 page that composes pieces by hand
normally gets them from the page shell, or includes this directly for a
non-`fluidPage` root. The bundled apps and the `ptr_*_ui()` composites
inject it for you.

## Usage

``` r
ptr_ui_assets(css = NULL)
```

## Arguments

- css:

  Optional character vector of paths to additional CSS files; linked
  after `ggpaintr`'s bundled stylesheet so its rules win. See
  [`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md)
  for the full semantics. Defaults to `NULL`.

## Value

A [`shiny::tagList()`](https://rdrr.io/pkg/shiny/man/reexports.html).

## See also

[`ptr_ui_plot()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_plot.md),
[`ptr_ui_controls()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_controls.md),
[`ptr_ui_toggle_code()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_toggle_code.md),
[`ptr_css()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_css.md)

## Examples

``` r
ptr_ui_assets()
#> 
```
