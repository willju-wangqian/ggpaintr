# RStudio addin: wrap a selection in a ggpaintr placeholder

Interactive RStudio addin. Highlight a token in your ggplot expression
(e.g. `mpg` in `aes(x = mpg)`), run the addin, and pick a placeholder
from a command-palette gadget; the selection is rewritten to
`ppVar(mpg)`. With nothing highlighted the same palette opens and
inserts
[`ppVar()`](https://willju-wangqian.github.io/ggpaintr/reference/ppVar.md)
with the caret between the parens. The placeholder list is read live
from the registry, so custom placeholders registered this session (via
[`ptr_define_placeholder_value()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_define_placeholder_value.md)
and friends) appear automatically.

## Usage

``` r
ptr_wrap_placeholder_addin()
```

## Value

Invisibly `NULL`. Called for its side effect of editing the active
RStudio document.

## Details

The gadget's *Wrap in app* button (left of *Insert*) takes a different
action: instead of inserting a placeholder it wraps the whole selection
in a braced block piped into
[`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md)
– `{` / ` <selection>` / `} |> ` /
` `[`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md)
– turning a ggplot expression into a runnable ggpaintr app skeleton.

## Theme

By default the palette follows your RStudio editor theme (dark theme -\>
dark palette, light -\> light), via
[`rstudioapi::getThemeInfo()`](https://rstudio.github.io/rstudioapi/reference/getThemeInfo.html).
Force one with `options(ggpaintr.addin_theme = "dark")`, `"light"`, or
`"auto"` (the default).

## Keyboard shortcut

For a highlight-then-keystroke flow, bind the addin once (RStudio reads
shortcuts only from your own keybindings, so packages cannot ship one).
The addin must be **installed** (not merely `load_all()`-ed) to appear
in the shortcut dialog:

1.  *Tools \> Addins \> Browse Addins...*, then the *Keyboard
    Shortcuts...* button. (Or *Tools \> Modify Keyboard Shortcuts...*
    and type "ggpaintr" in the search box.)

2.  Find the *ggpaintr placeholder* row and click its *Shortcut* cell.

3.  Press the recommended combination: **Cmd+Shift+G** on macOS,
    **Ctrl+Shift+G** on Windows/Linux. (Any free combination works; pick
    another if that one is already bound.)

4.  *Apply*.

Requires rstudioapi and miniUI; it errors with a clear message when run
outside RStudio.
