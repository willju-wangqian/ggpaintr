# Placeholder Identity Helpers

These are the plain-R callables returned by registering the five
built-in ggpaintr placeholders (`ppVar`, `ppNum`, `ppText`, `ppExpr`,
`ppUpload`). Inside a formula passed to
[`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md)
/
[`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md)
the parser recognises calls to these names as placeholder invocations
and binds them to Shiny widgets (see
[`vignette("ggpaintr-tutorial")`](https://willju-wangqian.github.io/ggpaintr/articles/ggpaintr-tutorial.md)).
Outside
[`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md)
they behave as plain R functions: the first four return their argument
unchanged (identity), so a formula such as `aes(x = ppVar(mpg))`
evaluates identically to `aes(x = mpg)` under ggplot2's tidy-eval.
`ppUpload` is identical when called with an argument
(`ppUpload(penguins)` returns `penguins`), so a formula such as
`ppUpload(penguins) |> filter(...) |> ggplot(...)` evaluates as plain R
when `penguins` is in scope. The no-arg form `ppUpload()` aborts outside
[`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md)
— it is meaningful only as a placeholder slot.

## Usage

``` r
ppVar(x = NULL, ...)

ppNum(x = NULL, ...)

ppText(x = NULL, ...)

ppExpr(x = NULL, ...)

ppUpload(x, ...)
```

## Arguments

- x:

  A column name (`ppVar`), numeric (`ppNum`), string (`ppText`),
  expression (`ppExpr`), or dataset name/value (`ppUpload`). Passed
  through unchanged.

- ...:

  Additional arguments (e.g. named arguments consumed by a custom
  placeholder's `named_args` schema). Ignored by the built-in identity
  implementation.

## Value

The input value unchanged. The no-arg form `ppUpload()` does not return;
it aborts with a guard message.

## Examples

``` r
# Identity inside ggplot2's tidy-eval:
library(ggplot2)
p1 <- ggplot(mtcars, aes(x = mpg)) + geom_histogram(bins = 10)
p2 <- ggplot(mtcars, aes(x = ppVar(mpg))) + geom_histogram(bins = 10)
# p1 and p2 produce the same plot.

# Inside ptr_app() / ptr_server(), the same call binds to a column picker:
if (interactive()) {
  ptr_app(ggplot(mtcars, aes(x = ppVar(mpg), y = ppVar(wt))) + geom_point())
}
```
