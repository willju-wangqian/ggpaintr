# Extract Runtime Outputs From a `ptr_state`

Read the latest plot object, error message, or generated code text from
the runtime result stored on a `ptr_state`. Use these to compose custom
UIs or to test the runtime in
[`shiny::testServer`](https://rdrr.io/pkg/shiny/man/testServer.html).

## Usage

``` r
ptr_extract_plot(state)

ptr_extract_error(state)

ptr_extract_code(state)
```

## Arguments

- state:

  A `ptr_state` from
  [`ptr_init_state()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_init_state.md).

## Value

`ptr_extract_plot` returns a `ggplot` object (or `NULL` on failure);
`ptr_extract_error` returns a string or `NULL`; `ptr_extract_code`
returns a single string.

## Reactive contexts

Each function wraps its read in
[`shiny::isolate()`](https://rdrr.io/pkg/shiny/man/isolate.html), so it
works in both reactive and non-reactive contexts and returns the current
value without establishing a reactive dependency.

**Do not call these inside a `render*{}` block** if you want the output
to update when the plot rerenders. Because `isolate()` suppresses the
dependency on `state$runtime()`, the render block fires once on mount
and never again. Inside a reactive context, read `state$runtime()`
directly — that takes the dependency and re-fires on every *Update plot*
click. Reserve `ptr_extract_*` for non-reactive contexts: download
handlers,
[`shiny::testServer()`](https://rdrr.io/pkg/shiny/man/testServer.html)
assertions, and one-shot reads outside any session.

## Examples

``` r
shiny::isolate({
  state <- ptr_init_state(
    "ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()"
  )
  ptr_extract_code(state)
})
#> [1] ""
```
