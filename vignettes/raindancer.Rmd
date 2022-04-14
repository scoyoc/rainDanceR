---
title: "Import Onset HOBO Logger Data into R"
author: "Matthew Van Scoyoc, National Park Service"
date: "5 April, 2022"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{raindancer}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---
# Introduction
This R package imports data from Onset temperature, relative humidity, and precipitation (event) data loggers into R. 
Data collected in the field using Onset loggers are exported to comma delimited (csv) files using the HOBOware application from Onset.
This package imports the csv files into R and summarizes the data.

The structure of the csv files generated from HOBOware vary wildly. 
They can have anywhere from 4 to 10 columns and the logger details are usually in two "hidden" columns following the data. 
This makes data from the temperature, relative humidity, and precipitation loggers difficult to read into R. 
This package imports data collected by Onset loggers used in the Southeast Utah Group (SEUG) national parks long-term vegetation monitoring program (LTVMP) and then summarize these data.
This package was written to be used with the dataprocessR package, [https://github.com/scoyoc/dataprocessR](https://github.com/scoyoc/dataprocessR), that exports the raw and summarized data to the SEUG LTVMP database (a Microsoft Access database).

```{r, echo = FALSE}
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library("raindancer")
```

# Installation
This package is available on [GitHub](https://github.com/) at [https://github.com/scoyoc/raindancer](https://github.com/scoyoc/raindancer). 
Dependent packages include dplyr, glue, lubridate, RODBC, stringr, tibble, tidyr, and utils. 
Suggested pacakges include janitor, knitr, rmarkdown, and readr. 
```{r Setup, eval=FALSE, echo = TRUE}
if (!"devtools" %in% installed.packages()[, "Package"]) {
  install.packages("devtools")
}
devtools::install_github("scoyoc/raindancer")
library("raindancer")
```

# Import Data into R
There are two function available to import the csv files generated from HOBOware into R, *import_hobo_2008()* and *import_hobo()*. Below is a table of Onset loggers that these functions can import data from.
```{r logger_table, eval=TRUE, echo=FALSE}
onset_loggers <- data.frame(
  'Product' = c("H07 Logger", "HOBO UA-003-64 Pendant Temp/Event",
                "H08 Logger", "HOBO UA-001-64 Pendant Temp", 
                "HOBO U23-001 Temp/RH"),
  'Element' = c("PRCP", "PRCP & TEMP", "TEMP", "TEMP", 
                "TEMP & RH"),
  'Years' = c("2008-2019", "2019-present", "2008-2019", 
                   "2019-present", "2019-present")
)

knitr::kable(onset_loggers, 
             col.names = c("Onset Product", "Element Recorded", 
                           "Years Used"), 
             caption = "Table 1. Onset loggers used in the SEUG LTVMP.",
             format = 'html')
```

The functions *import_hobo_2008()* and *import_hobo()* return a list with three components:  
1. **file_info** is a one row data frame that contains information about the file, the logger, and data.  
2. **details** is a data frame of logger and sampling event information.  
3. **data_raw** is a data frame of raw data.

Lets start by usgin *list.files()* to bring a list of csv files into R. 
```{r import_example, eval=FALSE, echo=TRUE}
file_list <- list.files(path = "C:/path/to/data", pattern = ".csv", 
                        full.names = TRUE, recursive = FALSE)
```

There are some files included in this package for examples, so we'll use those for the vignette. 
```{r import, eval=TRUE, echo=TRUE, results='markup'}
file_list <- list.files(path = system.file("extdata", package = "raindancer"),
                        pattern = ".csv", full.names = TRUE, recursive = FALSE)
print(file_list)
```

The first four csv files are from 2010 and 2012, so lets start by using *import_hobo_2008()*.
```{r import_hobo_2008.R, eval=TRUE, echo=TRUE, results='markup'}
dat.1 <- import_hobo_2008(file_list[1])
```

Let's examine the components of the list returned by the *import_hobo_2008()* function.
The first component returns information about the file, the logger, and the data.
```{r file_info, eval=TRUE, echo=TRUE, results='markup'}
dplyr::glimpse(dat.1$file_info)
```

The second component returns a three (3) column data frame with information about the logger and sampling event.
```{r details, eval=TRUE, echo=TRUE, results='markup'}
dat.1$details
```

And the third component returns a data frame of raw data.
```{r data_raw, eval=TRUE, echo=TRUE, results='markup'}
dplyr::glimpse(dat.1$data_raw)
```

In 2019 SEUG resource staff upgraded the loggers in hopes of preventing logger failure and mainaining the weather data set for the LTVMP.
The new loggers had a different file structure that the older loggers, requiring a new function to import data into R. 
Files five (5) though eight (8) are examples from the new loggers. 
Let's use *import_hobo()* to bring data from one of these files into R and examine the information and data.
```{r import_hobo.R, eval=TRUE, echo=TRUE, results='markup'}
dat.6 <- import_hobo(file_list[6])
str(dat.6)
```

# Summarize Data
The that we have data read into R are either event data for precipitation or recorded at set intervals through out the day for temperature and relative humidity data. Let's summarize them up to daily values.

## Precipitation (Event) Data with *raindance()*
The precipitation loggers record an event every time the bucket inside the rain gauge fills with 0.254 mm of water and tips to trigger an event. 
The data recorded is simply an event (e.g., event 1, event 2, event 3, and so on). 
The *raindance()* function calculates hourly precipitation, provides the number of tips per hour, and provides an estimate of intensity with maximum tips per minute in a given hour.
This function requires the *data_raw* component from *import_hobo()* or *import_hobo_2008()* functions and returns a data frame of hourly data.
```{r raindance, eval=TRUE, echo=TRUE, results='markup'}
raindance(dat.1$data_raw)
```

## Temperature and Relative Humidity Data with *sundance()*
Temperature and relative humidity loggers record data at set intervals through out the day.
The *sundance()* function summarizes these data to the daily data, providing mean, minimum, maximum, and the number of measurements (n) for a given day. 
This function also returns the time that the minimum and maximum were recorded.
This function requires the *data_raw* component from *import_hobo()* or *import_hobo_2008()* functions and returns a data frame of daily data.
```{r sundance, eval=TRUE, echo=TRUE, results='markup'}
sundance(dat.6$data_raw)
```

# Processing HOBO files
The *process_hobo()* function was developed to summarize all the csv files in a data directory.
This function requires an object returned form the *import_hobo()* or *import_hobo_2008()*. 
It evaluates the elements of the data from the *file_info* component and uses *raindance()* or *sundance()* to summarize the data.
It add the summarized data to the original list and returns a four (4) component list.
An effective way to use this function is with *lapply()*.
```{r process_hobo, eval=TRUE, echo=TRUE, results='markup'}
lapply(file_list[2:4], function(this_file){
  dat <- import_hobo_2008(this_file) |> process_hobo()
  print(basename(this_file))
  print(data.class(dat)); print(names(dat))
  dat$data
})
```
# Conclusion
The *raindancer* package eficiently processes and summarized data from the Onset HOBO loggers listed in Table 1 and used in the SEUG LTVMP from 2008-2021. 
The data from 2021 were mostly formatted the same as data from 2020, with a few exceptions. 
There is a likely that some future csv file will have anomolies that will require improvements to the *import_hobo()* function.
Please submit the problem on the Issues page of the GitHub repository, [https://github.com/scoyoc/raindancer/issues](https://github.com/scoyoc/raindancer/issues), or contact the author of the package if this happens.

This package was designed to work with the [dataprocessR package](https://github.com/scoyoc/dataprocessR). 
See the dataprocessR vignette for how to export these data to the SEUG LTVMP database.
