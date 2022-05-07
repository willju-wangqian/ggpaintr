
## Installation

You can install the development version from Github with:

``` r
# install.packages("devtools")
devtools::install_github("willju-wangqian/ggpaintr")
```

## Run the shiny app

``` r
library(ggpaintr)

run_ggpaintr_app()
```

## Development

-   location for external data: `inst/extdata`
-   location for example shiny app: `inst/shiny`
-   location for files that need to be uploaded but are not part of the
    package: `preconsideration/<folder_name>`
-   coding style consistency: please try to use
    `underscores_in_variable_names` and (maybe) `camelCase` for
    web-related names…

## Generate plot icons

-   the code that generates the plot icons:
    `preconsideration/generate_plot_icon`
-   we can save the generated icons into
    `preconsideration/saved_plot_icon` or copy them to
    `inst/shiny/ggpaintr_app/www/img_button`
-   different functions for `actionButton`
    -   `shinyWidgets::actionBttn`, [link](https://bttn.surge.sh/)
    -   `actionButton`: this is the default and we can specify the
        `style` attribute
    -   `bsButton`: this includes both the `actionButton` and
        `checkboxInput`, but we don’t have much room for `style`

## Future work

-   more extensible: have a sophisticated way of adding ui and server
    functions from the users
-   allow users to download plots in ggpaintr_app
-   submit to CRAN

## Change log

#### version number: 0.0.9.7

-   fixed code for numeric input

-   allow categorical variables to have less than 3 levels

-   Todo - `ggpaintr_app`:

    -   filters from `DT` will invalidate the code

#### version number: 0.0.9.6

-   alpha as numeric

-   n \< 3 colors

-   added four chart types, next: update functions in the package

-   change expression by clicking the plot icon

-   test if ui functions can be added

-   how to add customized handler functions?

#### version number: 0.0.9.5

-   Updates:
    -   introduced `paintr_obj` and `paintr()`, which use a grammar
        similar to `ggplot2` to specify and control all elements or
        pieces built in the shiny app
    -   use one function to build reactivity of `color` or `fill` so
        that they can work with `paintr_plot_code`
-   TODO:
    -   documentations and dependencies
    -   write-ups
    -   download plot
    -   test and finalize code then push to main

#### version number: 0.0.9.4

-   heuristics:
    -   parameters of handlers
    -   parameters of ui modules
    -   returns of handlers
-   TODO:
    -   documentations
    -   [x] icon position or size
    -   writeup
        -   the concept of grammar of graphics
        -   why this package can be useful
        -   how to use the package
    -   [x] how to wrap up the part of scale_color
    -   template?
    -   download plot
    -   [x] aes then comma

#### version number: 0.0.9.3

-   updates:
    -   the app is now able to show code that generates the plot
    -   dynamic color picker
-   TODO:
    -   update ui
    -   implement other plot types
    -   extend the current functionality: added more handlers (for
        example one for `labs()`)
    -   documentations

#### version number: 0.0.9.2

-   updates:
    -   `*Handler`s are introduced for plot generation
    -   added bar chart

#### version number: 0.0.9.1

-   updates:
    -   new ui
    -   example for box plot finished
-   TODO:
    -   update the geomGenerators
        -   each line of Grammar of Graphics has its onw handler
        -   a function that collects all handlers
        -   find a way to connect handlers to its ui components
    -   allow color pickers to respond to the selected variable
    -   update ui
    -   implement other plot types
    -   display code
