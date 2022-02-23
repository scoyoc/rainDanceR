#' Extract data from csv file produced by Onset HOBOware
#'
#' This function extracts data from a comma delimited (csv) file produced by
#'     HOBOware. This function uses the list, or object, produced from
#'     \code{\link{import_file}}.
#'
#' @param my_file An \code{import_file} object.
#'
#' @details
#' This function extracts the data from the list produced by import_file() then
#'     standardizes the data structure into six columns.
#'
#' @return
#' This function returns a \code{\link[tibble:tibble]{tibble}} of standardized
#'     data. The variables produced by this function include:
#'
#' \itemize{
#'     \item{\strong{FileName}}{ The name of the file the data came from.}
#'     \item{\strong{PlotID}}{ The unique plot identification number (e.g., A03
#'         or I06).}
#'     \item{\strong{DateTime}}{ The date-time of the measurement.}
#'     \item{\strong{Element}}{ The element the data represent. TEMP is
#'         temperature, RH is relative humidity, and PRCP is precipitation.}
#'     \item{\strong{Value}}{ The data value of the measurement recorded by the
#'         data logger.}
#' }
#'
#' @seealso \code{\link{import_file}}, \code{\link{get_details}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library("raindancer")
#'
#' # Generate list of files
#' file_list <- list.files(path = system.file("extdata", package = "raindancer"),
#'                         pattern = ".csv", full.names = TRUE, recursive = FALSE)
#'
#' # Read file into R
#' my_file <- import_file(file_list[2])
#'
#' # Extract data from file
#' get_data(my_file)
#' }
#'
get_data <- function(my_file){
  # my_file = import_file(file_list[12])

  #-- Pull logger type, element, and units from Details
  my_logger = get_product(my_file)

  if(my_file$file_info$col_n != 10){
    dat = my_file$raw_file |>
      dplyr::select('DateTime', 'Value') |>
      dplyr::mutate('DateTime' = lubridate::mdy_hms(DateTime),
                    'FileName' = my_file$file_info$filename,
                    'PlotID' = my_file$file_info$plotid,
                    'Element' = my_logger$Element,
                    'Value' = ifelse(Element == "TEMP" &
                                       stringr::str_detect(my_logger$Units,
                                                           "F"),
                                     Value - 32 * 5/9,
                                     Value)) |>
      dplyr::select('FileName', 'PlotID', 'DateTime', 'Element', 'Value')

  } else if(my_file$file_info$col_n == 10){
    dat = my_file$raw_file |>
      dplyr::select('DateTime', 'Temp', 'RH') |>
      dplyr::rename('TEMP' = Temp) |>
      tidyr::gather(key = 'Element', value = 'Value', TEMP:RH) |>
      dplyr::mutate('DateTime' = lubridate::mdy_hms(DateTime),
                    'FileName' = my_file$file_info$filename,
                    'PlotID' = my_file$file_info$plotid) |>
      dplyr::select('FileName', 'PlotID', 'DateTime', 'Element', 'Value')
  } else(message(paste0("Something is not right here. Check file: ",
                        basename(my_file$file_info$filename),
                        "; ncol = ", my_file$file_info$col_n)))
  return(dat)
}

# Extract HOBO logger type and associated data types it collects.
# Returns a dataframe.
get_product <- function(my_file){
  # DESCRIPTION
  # This function extracts the Onset product name from the Details column of the
  # raw file. It uses the list produced from import_file().
  my_logger = my_file$raw_file |>
    dplyr::select('Details') |>
    tidyr::separate('Details', into = c("Var", "Product"), sep = ":",
                    remove = T, extra = "merge", fill = "right") |>
    dplyr::filter(Var == "Product") |>
    dplyr::distinct() |>
    dplyr::mutate('Product' = trimws(Product, 'left'),
                  'Units' = suppressWarnings(get_units(my_file))) |>
    dplyr::left_join(onset_loggers)
  return(my_logger)
}

# Extracts the units from the raw file.
get_units <- function(my_file){
  # DESCRIPTION
  # This function extracts the units of measurement out of the Details or Units
  # column of the raw file. It uses the list produced form import_file().

  # Strip units from raw_file
  units = if(my_file$file_info$col_n == 4){
    dplyr::select(my_file$raw_file, 'Details') |>
      tidyr::separate('Details', into = c("Var", "Val"), sep = ":") |>
      dplyr::filter(Var == "Series") |>
      tibble::deframe()
  } else(
    dplyr::select(my_file$raw_file, 'Units') |>
      dplyr::filter(Units != "") |>
      tibble::deframe() |>
      dplyr::first()
  )

  units = if(stringr::str_detect(units, "Event")) {"Event"
  } else if(stringr::str_detect(units, "F")) {"F"
  } else if(stringr::str_detect(units, "C")) {"C"
  } else("Unknown")
  return(units)
}

# Data frame of Onset data loggers and what they measure
onset_loggers <- data.frame(
  'Product' = c("H07 Logger", "HOBO UA-003-64 Pendant Temp/Event", "H08 Logger",
                "HOBO UA-001-64 Pendant Temp", "HOBO U23-001 Temp/RH", ""),
  'Element' = c("PRCP", "PRCP", "TEMP", "TEMP", "TEMP-RH", "Unknown")
)
