# raindancer

This R package imports data from Onset temperature, relative humidity, and precipitation (event) data loggers into R. 
Data collected in the field using Onset loggers are exported to comma delimited files (*.csv) using the Onset HOBOware application.
This package imports the *.csv files into R and summarize the data.

Version: 0.3.0

Depends: R (>= 4.0)

Imports: dplyr, glue, lubridate, RODBC, stringr, tibble, tidyr, utils

Suggests: janitor, knitr, rmarkdown, readr

Author/Maintainer: Matthew Van Scoyoc

Issues: [https://github.com/scoyoc/raindancer/issues](https://github.com/scoyoc/raindancer/issues)

License: MIT + file [LICENSE](https://github.com/scoyoc/raindancer/blob/master/LICENSE.md)

URL: [https://github.com/scoyoc/raindancer](https://github.com/scoyoc/raindancer)

Documentation: See the [raindancer vignette](https://github.com/scoyoc/raindancer/blob/master/doc/raindancer_pdf.pdf) and function help pages.

## Installation

``` r
devtools::install_github("scoyoc/raindancer", build_vignettes = TRUE)
```

## Examples
``` r
library("raindancer")

# Generate list of files
file_list <- list.files(path = "C:/path/to/data", pattern = ".csv", 
                        full.names = TRUE, recursive = FALSE)

# Import file and summarize data
my_file <- file_list[x]        # Select file
import_hobo(my_file) |>        # Import data from logger used from 2020 to present
  process_hobo()               # Summarize data
```
