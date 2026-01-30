# Construct a `paintr_obj` based on an expression in `ggplot2` alike layout

Construct a `paintr_obj` based on an expression in `ggplot2` alike
layout

## Usage

``` r
paintr(
  id,
  data,
  expr,
  extra_ui = NULL,
  extra_ui_args = NULL,
  data_path = "data"
)
```

## Arguments

- id:

  An ID string that corresponds with the ID used for all component of
  this `paintr_obj`

- data:

  the dataset for plotting

- expr:

  a `ggplot2` alike expression which is referred to as "paintr
  expression" in `ggpaintr`

- extra_ui:

  a named list: of extra functions that generate paintr_ui. The names of
  this list should be key words of a paintr expression For example
  `extra_ui = list(param1 = my_ui_func1, param2 = my_ui_func2)`

- extra_ui_args:

  a named list: of lists of arguments. The names of this list should be
  key words of a paintr expression. And the element of a key word should
  be a list of arguments that goes into the ui function of this key word
  For example
  `list(param1 = list(my_ui_func1_arg1, my_ui_func1_arg2), param2 = list(my_ui_func2_arg1, my_ui_func2_arg2))`

- data_path:

  string. path to the dataset; used for code of obtaining the data

## Value

a `paintr_obj` that contains all pieces of the paintr expression and and
their corresponding ui elements used to build a shiny app. Additionally,
it includes:

- `id`

- `data`

- `data_path`

## Details

the `expr` should have a `ggplot2` alike layout and be wrapped by
[`rlang::expr`](https://rlang.r-lib.org/reference/expr.html). And the
functionality of `paintr` is based on this `expr` that follows certain
rules. In general, a ggpaintr expression should be:

    rlang::expr(
      geom_<chart>(aes(<mapping_1>, <mapping_2>), <geom_args_1>, <geom_args2>) +
        <plot_settings_1> +
        <plot_settings_2> +
        <plot_settings_3>
    )

And note that:

- `<mapping_*>` is a `mapping` keyword that represents a aesthetic
  mapping in `ggplot2`, like `x`, `y`, or `color`

- `<geom_args_*>` is a `geom_args` keyword that represents an argument
  passed into `geom_<chart>`, like `position`

- `<plot_settings_*>` is a `plot_settings` keyword that represents a
  `ggplot2` function like `coord_flip`, which can modify the plot

- `geom_<chart>` specifies the geom function used for the plot.
  `'geom_'`is used to identify `geom_<chart>`

- `aes` is used to distinguish `mapping` keywords and `geom_args`
  keywords in `geom_<chart>`

## Note

`extraFunc` and `extraFuncArgs` allow users to override the ui functions
provided by `ggpaintr` package

## Examples

``` r
paintr("boxplot_id", mtcars, geom_boxplot(aes(x, y)))
#> $gg_components
#> $gg_components$geom_FUN
#> [1] "geom_boxplot"
#> 
#> $gg_components$mapping
#> [1] "x" "y"
#> 
#> $gg_components$plot_settings
#> list()
#> 
#> 
#> $shiny_components
#> $shiny_components$ui
#> $shiny_components$ui$mapping
#> $shiny_components$ui$mapping$x
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
#> 
#> $shiny_components$ui$mapping$y
#> <div class="form-group shiny-input-container">
#>   <label class="control-label" id="boxplot_id-mapY-label" for="boxplot_id-mapY">y:</label>
#>   <select data-max-options="1" data-state-input="true" id="boxplot_id-mapY" class="selectpicker form-control" autocomplete="off" multiple="multiple"><option value="mpg">mpg</option>
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
#> 
#> 
#> $shiny_components$ui$geom_args
#> NULL
#> 
#> $shiny_components$ui$plot_settings
#> NULL
#> 
#> 
#> $shiny_components$id
#> $shiny_components$id$mapping
#> $shiny_components$id$mapping$x
#> [1] "mapX"
#> 
#> $shiny_components$id$mapping$y
#> [1] "mapY"
#> 
#> 
#> $shiny_components$id$geom_args
#> NULL
#> 
#> $shiny_components$id$plot_settings
#> NULL
#> 
#> 
#> 
#> $id
#> [1] "boxplot_id"
#> 
#> $data
#>                      mpg cyl  disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4           21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag       21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 710          22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
#> Hornet 4 Drive      21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
#> Hornet Sportabout   18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2
#> Valiant             18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1
#> Duster 360          14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4
#> Merc 240D           24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
#> Merc 230            22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
#> Merc 280            19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4
#> Merc 280C           17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4
#> Merc 450SE          16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3
#> Merc 450SL          17.3   8 275.8 180 3.07 3.730 17.60  0  0    3    3
#> Merc 450SLC         15.2   8 275.8 180 3.07 3.780 18.00  0  0    3    3
#> Cadillac Fleetwood  10.4   8 472.0 205 2.93 5.250 17.98  0  0    3    4
#> Lincoln Continental 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3    4
#> Chrysler Imperial   14.7   8 440.0 230 3.23 5.345 17.42  0  0    3    4
#> Fiat 128            32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1
#> Honda Civic         30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2
#> Toyota Corolla      33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1
#> Toyota Corona       21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1
#> Dodge Challenger    15.5   8 318.0 150 2.76 3.520 16.87  0  0    3    2
#> AMC Javelin         15.2   8 304.0 150 3.15 3.435 17.30  0  0    3    2
#> Camaro Z28          13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4
#> Pontiac Firebird    19.2   8 400.0 175 3.08 3.845 17.05  0  0    3    2
#> Fiat X1-9           27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1
#> Porsche 914-2       26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2
#> Lotus Europa        30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2
#> Ford Pantera L      15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4
#> Ferrari Dino        19.7   6 145.0 175 3.62 2.770 15.50  0  1    5    6
#> Maserati Bora       15.0   8 301.0 335 3.54 3.570 14.60  0  1    5    8
#> Volvo 142E          21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2
#> 
#> $data_path
#> [1] "data"
#> 
#> attr(,"class")
#> [1] "paintr_obj"

# alternatively, one can define the expression first
library(rlang)
my_expr <- rlang::expr(geom_boxplot(aes(x, y)))
paintr("boxplot_id", mtcars, !!my_expr)
#> $gg_components
#> $gg_components$geom_FUN
#> [1] "geom_boxplot"
#> 
#> $gg_components$mapping
#> [1] "x" "y"
#> 
#> $gg_components$plot_settings
#> list()
#> 
#> 
#> $shiny_components
#> $shiny_components$ui
#> $shiny_components$ui$mapping
#> $shiny_components$ui$mapping$x
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
#> 
#> $shiny_components$ui$mapping$y
#> <div class="form-group shiny-input-container">
#>   <label class="control-label" id="boxplot_id-mapY-label" for="boxplot_id-mapY">y:</label>
#>   <select data-max-options="1" data-state-input="true" id="boxplot_id-mapY" class="selectpicker form-control" autocomplete="off" multiple="multiple"><option value="mpg">mpg</option>
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
#> 
#> 
#> $shiny_components$ui$geom_args
#> NULL
#> 
#> $shiny_components$ui$plot_settings
#> NULL
#> 
#> 
#> $shiny_components$id
#> $shiny_components$id$mapping
#> $shiny_components$id$mapping$x
#> [1] "mapX"
#> 
#> $shiny_components$id$mapping$y
#> [1] "mapY"
#> 
#> 
#> $shiny_components$id$geom_args
#> NULL
#> 
#> $shiny_components$id$plot_settings
#> NULL
#> 
#> 
#> 
#> $id
#> [1] "boxplot_id"
#> 
#> $data
#>                      mpg cyl  disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4           21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag       21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 710          22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
#> Hornet 4 Drive      21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
#> Hornet Sportabout   18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2
#> Valiant             18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1
#> Duster 360          14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4
#> Merc 240D           24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
#> Merc 230            22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
#> Merc 280            19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4
#> Merc 280C           17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4
#> Merc 450SE          16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3
#> Merc 450SL          17.3   8 275.8 180 3.07 3.730 17.60  0  0    3    3
#> Merc 450SLC         15.2   8 275.8 180 3.07 3.780 18.00  0  0    3    3
#> Cadillac Fleetwood  10.4   8 472.0 205 2.93 5.250 17.98  0  0    3    4
#> Lincoln Continental 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3    4
#> Chrysler Imperial   14.7   8 440.0 230 3.23 5.345 17.42  0  0    3    4
#> Fiat 128            32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1
#> Honda Civic         30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2
#> Toyota Corolla      33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1
#> Toyota Corona       21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1
#> Dodge Challenger    15.5   8 318.0 150 2.76 3.520 16.87  0  0    3    2
#> AMC Javelin         15.2   8 304.0 150 3.15 3.435 17.30  0  0    3    2
#> Camaro Z28          13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4
#> Pontiac Firebird    19.2   8 400.0 175 3.08 3.845 17.05  0  0    3    2
#> Fiat X1-9           27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1
#> Porsche 914-2       26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2
#> Lotus Europa        30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2
#> Ford Pantera L      15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4
#> Ferrari Dino        19.7   6 145.0 175 3.62 2.770 15.50  0  1    5    6
#> Maserati Bora       15.0   8 301.0 335 3.54 3.570 14.60  0  1    5    8
#> Volvo 142E          21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2
#> 
#> $data_path
#> [1] "data"
#> 
#> attr(,"class")
#> [1] "paintr_obj"
```
