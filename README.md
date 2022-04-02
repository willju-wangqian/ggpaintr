
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
