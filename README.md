
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

## Current Version

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
