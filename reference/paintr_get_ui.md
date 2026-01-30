# Get a ui element or its id of a paintr keyword from a `paintr_obj`

Get a ui element or its id of a paintr keyword from a `paintr_obj`

## Usage

``` r
paintr_get_ui(
  paintr_obj,
  selected_ui_name,
  type = "ui",
  scope = NULL,
  verbose = FALSE
)
```

## Arguments

- paintr_obj:

  a `paintr_obj`

- selected_ui_name:

  the keyword of the desired ui element

- type:

  optional. `type` can be `ui` or `id`

- scope:

  one value of `mapping`, `geom_args`, or `plot_settings`. Used to
  distinguish keywords with the same name but in different scope. For
  example `size` can be either a `mapping` keyword or a `geom_args`
  keyword.

- verbose:

  Whether or not to send warning messages when ui element is not found

## Value

the ui or id of `selected_ui_name`

## Examples

``` r
ptr_obj <- paintr("boxplot_id", mtcars, geom_boxplot(aes(x, y)))
paintr_get_ui(ptr_obj, "x")
#> <div class="form-group shiny-input-container">
#>   <label class="control-label" id="boxplot_id-mapX-label" for="boxplot_id-mapX">x:</label>
#>   <select data-max-options="1" data-state-input="true" id="boxplot_id-mapX" class="selectpicker form-control" autocomplete="off" multiple="multiple"><option value="mpg">mpg</option>
#> <option value="cyl">cyl</option>
#> <option value="disp">disp</option>
#> <option value="hp">hp</option>
#> <option value="drat">drat</option>
#> <option value="wt">wt</option>
#> <option value="qsec">qsec</option>
#> <option value="vs">vs</option>
#> <option value="am">am</option>
#> <option value="gear">gear</option>
#> <option value="carb">carb</option></select>
#> </div>
```
