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
#'     \item{\strong{Units}}{ The units of the measurement.}
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
  # my_file = import_file(file_list[10])

  #-- Pull logger type, element, and units from Details
  my_logger = get_product(my_file)

  if(my_file$file_info$col_n != 10){
    dat = my_file$raw_file |>
      dplyr::select(DateTime, Value) |>
      dplyr::mutate('DateTime' = lubridate::mdy_hms(DateTime),
                    'FileName' = my_file$file_info$FileName,
                    'PlotID' = my_file$file_info$PlotID,
                    'Element' = my_logger$Element,
                    'Units' = my_logger$Units) |>
      dplyr::select(FileName, PlotID, DateTime, Element, Value, Units)

  } else if(my_file$file_info$col_n == 10){
    dat = my_file$raw_file |>
      dplyr::select(DateTime, Temp, RH) |>
      dplyr::rename('TEMP' = Temp) |>
      tidyr::gather(key = 'Element', value = 'Value', TEMP:RH) |>
      dplyr::mutate('DateTime' = lubridate::mdy_hms(DateTime),
                    'FileName' = my_file$file_info$FileName,
                    'PlotID' = my_file$file_info$PlotID,
                    'Units' = ifelse(Element == "RH", "%RH", my_logger$Units)) |>
      dplyr::select(FileName, PlotID, DateTime, Element, Value, Units)
  } else(message(paste0("Something is not right here. Check file: ",
                        basename(my_file$file_info$FileName))))
  return(dat)
}

# Extract HOBO logger type and associated data types it collects.
# Returns a dataframe.
get_product <- function(my_file){
  # DESCRIPTION
  # This function extracts the Onset product name from the Details column of the
  # raw file. It uses the list produced from import_file().
  my_logger = my_file$raw_file |>
    dplyr::select(Details) |>
    tidyr::separate('Details', into = c("Var", "Product"), sep = ":",
                    remove = T, extra = "merge", fill = "right") |>
    dplyr::filter(Var == "Product") |>
    dplyr::distinct() |>
    dplyr::mutate('Product' = trimws(Product, 'left'),
                  'Units' = suppressWarnings(get_units(my_file))) |>
    dplyr::left_join(onset_loggers, by = "Product")
  return(my_logger)
}

# Extracts the units from the raw file.
get_units <- function(my_file){
  # DESCRIPTION
  # This function extracts the units of measurement out of the Details or Units
  # column of the raw file. It uses the list produced form import_file().

  # Strip units from raw_file
  units = if(my_file$file_info$col_n == 4){
    dplyr::select(my_file$raw_file, Details) |>
      tidyr::separate('Details', into = c("Details", "Units"), sep = ":") |>
      dplyr::filter(Details == "Series") |>
      tibble::deframe()
    } else(
      dplyr::select(my_file$raw_file, Units) |>
        dplyr::filter(Units != "") |>
        tibble::deframe() |>
        dplyr::first()
      )

  if(stringr::str_detect(units, "Â°")) units = gsub("Â°", "", units)
  units = trimws(units)
  return(units)
}

# Data frame of Onset data loggers and what they measure
onset_loggers <- data.frame(
  'Product' = c("H07 Logger", "HOBO UA-003-64 Pendant Temp/Event", "H08 Logger",
                "HOBO UA-001-64 Pendant Temp", "HOBO U23-001 Temp/RH", ""),
  'Element' = c("PRCP", "PRCP", "TEMP", "TEMP", "TEMP-RH", "Unknown")
)

# # Returns the units measured
# my_units <- function(raw_file){
#   my_units <- my_file$raw_file |>
#     dplyr::select(Units) |>
#     dplyr::mutate(Units = trimws(my_file$raw_file$Units)) |>
#     dplyr::filter(!Units == "") |>
#     dplyr::slice(1)
#   if(stringr::str_detect(my_units, "Â°")) my_units = gsub("Â°", "", my_units)
#   return(my_units)
# }
