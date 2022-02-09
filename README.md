# rainDanceR

This package imports data from Onset temperature, relative humidity, and precipitation data loggers into R. 
Data collected in the field using Onset loggers are exported to comma delimited files (*.csv) using the HOBOware application from Onset.
This package imports the *.csv files into R and summarize the data.

Version: 0.0.1

Depends: R (>= 4.0)

Imports: dplyr, lubridate, stringr, tibble, tidyr

Suggests: stringi, readr, janitor

Author: Matthew Van Scoyoc

Maintainer: Matthew Van Scoyoc

Issues: [https://github.com/scoyoc/rainDanceR/issues](https://github.com/scoyoc/rainDanceR/issues)

License: MIT + file [LICENSE](https://github.com/scoyoc/rainDanceR/blob/master/LICENSE.md)

URL: [https://github.com/scoyoc/rainDanceR](https://github.com/scoyoc/rainDanceR)

Documentation: Help pages for now. A Vignette is planned for future releases.

## Installation

``` r
devtools::install_github("scoyoc/rainDanceR")
```

## Examples
``` r
library("rainDanceR")

# Generate list of files
file_list <- list.files(path = system.file("extdata", package = "rainDanceR"),
                        pattern = ".csv", full.names = TRUE, recursive = FALSE)

# Import file and summarize data
my_file <- file_list[1]           # Select file
dat <- import_wxdat(my_file) |>   # Import file into R
  process_hobo()                  # summarize data
dat
```
