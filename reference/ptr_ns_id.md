# Apply a Namespace Function to a Placeholder Id

Resolve the namespaced Shiny input/output id for a placeholder. Custom
`bind_ui` callbacks should call this with the namespace function
supplied on the placeholder context, never assume `meta$id` is already
namespaced.

## Usage

``` r
ptr_ns_id(ns_fn, id)
```

## Arguments

- ns_fn:

  A namespace function with signature `character -> character`,
  typically `shiny::NS(id)` or `shiny::NS(NULL)`.

- id:

  A single id string (e.g., `meta$id` from a placeholder context).

## Value

The namespaced id string.

## Details

Inside a custom `bind_ui(input, output, metas, context)` use:

    input_id  <- ptr_ns_id(context$ns_fn    %||% shiny::NS(NULL), meta$id)
    output_id <- ptr_ns_id(context$ui_ns_fn %||% shiny::NS(NULL), meta$id)

Under
[`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md)
both `ns_fn` and `ui_ns_fn` default to `shiny::NS(NULL)` so the
namespaced id equals `meta$id`. When ggpaintr is embedded inside a Shiny
module (any wrapper that supplies a real namespace), they wrap a module
namespace and the two ids diverge. Always go through `ptr_ns_id()`
rather than building the id string yourself.

## See also

[`ptr_define_placeholder()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_define_placeholder.md),
[`ptr_resolve_layer_data()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_resolve_layer_data.md).

## Examples

``` r
ns <- shiny::NS("mod1")
ptr_ns_id(ns, "ggplot_3_2")
#> [1] "mod1-ggplot_3_2"
ptr_ns_id(shiny::NS(NULL), "ggplot_3_2")
#> [1] "ggplot_3_2"
```
