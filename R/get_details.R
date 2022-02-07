#' Extract metadata from csv file produced by Onset HOBOware
#'
#' This function extracts metadata from the 'Details' column and raw data
#'     of a comma delimited (csv) file produced by HOBOware. This function uses
#'     the list, or object, produced from \code{\link{import_file}}.
#'
#' @param my_file An \code{import_file} object.
#' @param my_data The data frame produced from \code{get_data}.
#'
#' @details
#' This function produces a data frame of metadata about the sampling event.
#' Some of the values details are extracted from the "Details" column of the csv
#' file, some are scraped from the file name, and others are calculated form the
#' data.
#'
#' @return
#' This function returns a \code{\link[tibble:tibble]{tibble}} of standardized
#'     data. The variables produced by this function include:
#'
#' \describe{
#'     \item{\strong{FileName}}{The name of the file the data came from.}
#'     \item{\strong{Details}}{This variable will varry with the data contained
#'         in the "Details" colum of the original file. Calculated variables
#'         listed below.
#'         \itemize{
#'             \item Import Date: A character string of the date the data were
#'                 processed useing lubridate::today().
#'             \item Plot ID: The unique plot identification number (e.g., A03
#'                 or I06).
#'             \item Element: The element the data represent. TEMP is
#'                 temperature, RH is relative humidity, and PRCP is
#'                 precipitation.
#'             \item Units: The unit of measurement. Temperature is measured in
#'                 F or C, relative humidity is measured in percent, and
#'                 precipitation is measured in tips of the tipping bucket, or
#'                 events.
#'             \item DateTime (min): The minimum date-time stamp in the data.
#'             \item DateTime (max): The maximum date-time stamp in the data.
#'             \item Records (n): The number of records of data.
#'             \item ConvertFtoC: Logical. Were the temperature data converted
#'                 from F to C? Yes or No.
#' }}
#'     \item{\strong{Value}}{The value the variable in Details.}
#' }
#'
#' @section Calculated variables:
#'
#'
#' @seealso \code{\link{import_file}} to import a csv produced by HOBOware and
#'    \\code{\link{get_data}} to extract the data.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library("dataProcessor")
#' file.list <- list.files(path = "./inst/raw_data", pattern = ".csv",
#'                         full.names = TRUE, recursive = FALSE)
#' my_file <- import_file(file.list[1])
#' my_data <- get_data(my_file)
#' get_details(my_file, my_data)
#' }
#'
get_details <- function(my_file, my_data){
  # DESCRIPTION
  # This function pulls details from the raw file. It uses the list produced
  # from import_file().

  # Pull logger type, element, and units from Details
  my_logger = get_product(my_file)

  # Strip Details from raw_file
  details = my_file$raw_file %>%
    dplyr::select('Details') %>%
    # Reduce column and remove white space
    dplyr::distinct() %>%
    tidyr::separate('Details', into = c("Var", "Value"), sep = ":", remove = T,
                    extra = "merge", fill = "right") %>%
    dplyr::filter(Value != "") %>%
    dplyr::filter(!Var %in% c("Version Number", "Manufacturer", "Header Created",
                              "Launch GMT Offset", "Max", "Min", "Avg")) %>%
    dplyr::filter(!stringr::str_detect(Var, "Std Dev")) %>%
    dplyr::group_by(Var) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate('Value' = trimws(Value, "both")) %>%
    tidyr::spread(key = Var, value = Value, fill = NA) %>%
    dplyr::mutate('Import Date' = as.character(lubridate::today()),
                  'Plot ID' = my_file$file_info$plotid,
                  'Element' = my_logger$Element,
                  'Units' = my_logger$Units,
                  'DateTime (min)' = as.character(min(my_data$DateTime,
                                                      na.rm = T)),
                  'DateTime (max)' = as.character(max(my_data$DateTime,
                                                      na.rm = T)),
                  'Records (n)' = nrow(my_data),
                  'ConvertFtoC' = ifelse(Element == "TEMP" &&
                                           stringr::str_detect(my_logger$Units,
                                                               "F"),
                                         "Yes", "No")) %>%
    tidyr::gather(key = 'Details', value = 'Value') %>%
    dplyr::mutate("FileName" =my_file$file_info$filename) %>%
    dplyr::select('FileName', 'Details', 'Value') %>%
    dplyr::arrange('Details')
  details = details %>%
    dplyr::add_row(tibble::tibble_row("FileName" = my_file$file_info$filename,
                                      "Details" = "QFLAG",
                                      "Value" = qflags(my_logger, details, my_data)))
  return(details)
}

qflags <- function(my_logger, my_details, my_data){
  flags = data.frame(
    Logger = ifelse(my_logger$Element == "Unknown", 1, NA),
    Units = ifelse(my_logger$Units == "Unknown", 2, NA),
    DateTimeNA = ifelse(sum(is.na(my_data$DateTime)) > 0, 3, NA),
    DataNA = ifelse(sum(is.na(my_data$Value)) > 0, 4, NA)
  ) %>%
    tidyr::gather("Category", "Flag")

  qflags = ifelse(sum(is.na(flags$Flag)) != nrow(flags),
                  paste(dplyr::filter(flags, Flag != is.na(Flag))$Flag, sep = ","),
                  NA)
  return(qflags)
}
