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
#'     \item RID: The unique record ID. The record ID
#'         \code{\link[base:paste]{paste}}'s DateTime, PlotID, and
#'         Element to create a unique record ID.
#'     \item FileName: The name of the file the data came from.
#'     \item PlotID: The unique plot identification number (e.g., A03 orI06).
#'     \item DateTime: The date and time the data were collected at.
#'     \item Element: The element the data represent. TEMP is temperature, RH is
#'         relative humidity, and PRCP is precipitation.
#'     \item Value: The data value recorded by the data logger.
#' }
#'
#' @seealso \code{\link{import_file}} to import a csv produced by HOBOware and
#'    \code{\link{get_details}} to extract the metadata.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library("dataProcessor")
#' file_list <- list.files(path = system.file("extdata", package = "dataProcessor"),
#'                         pattern = ".csv", full.names = TRUE, recursive = FALSE)
#' my_file <- import_file(file.list[1])
#' get_data(my_file)
#' }
#'
get_data <- function(my_file){
  # DESCRIPTION
  # Extracts data from the raw file. It uses the list produced in
  # import_file().

  #-- Pull logger type, element, and units from Details
  my_logger = get_product(my_file)

  if(my_file$file_info$col_n != 10){
    dat = my_file$raw_file %>%
      dplyr::select('RID', 'DateTime', 'Value') %>%
      dplyr::mutate('DateTime' = lubridate::mdy_hms(DateTime,
                                                    tz = "America/Denver"),
                    'FileName' = basename(my_file$file_info$filename),
                    'PlotID' = my_file$file_info$plotid,
                    'Element' = my_logger$Element,
                    'RID' = paste(as.numeric(DateTime), PlotID, Element,
                                  sep = "."),
                    'Value' = ifelse(Element == "TEMP" &&
                                       stringr::str_detect(my_logger$Units,
                                                           "F"),
                                     Value - 32 * 5/9,
                                     Value)) %>%
      dplyr::select('RID', 'FileName', 'PlotID', 'DateTime', 'Element', 'Value')

  } else if(my_file$file_info$col_n == 10){
    dat = my_file$raw_file %>%
      dplyr::select('RID', 'DateTime', 'Temp', 'RH') %>%
      dplyr::rename('TEMP' = Temp) %>%
      tidyr::gather(key = 'Element', value = 'Value', TEMP:RH) %>%
      dplyr::mutate('DateTime' = lubridate::mdy_hms(DateTime,
                                                    tz = "America/Denver"),
                    'FileName' = basename(my_file$file_info$filename),
                    'PlotID' = my_file$file_info$plotid,
                    'RID' = paste(as.numeric(DateTime), PlotID, Element,
                                  sep = ".")) %>%
      dplyr::select('RID', 'FileName', 'PlotID', 'DateTime', 'Element', 'Value')

  } else(message(paste0("Something is wrong. Check file: ",
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
  my_logger = my_file$raw_file %>%
    dplyr::select('Details') %>%
    tidyr::separate('Details', into = c("Var", "Product"), sep = ":",
                    remove = T, extra = "merge", fill = "right") %>%
    dplyr::filter(Var == "Product") %>%
    dplyr::distinct() %>%
    dplyr::mutate('Product' = trimws(Product, 'left'),
                  'Units' = suppressWarnings(get_units(my_file))) %>%
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
    dplyr::select(my_file$raw_file, 'Details') %>%
      tidyr::separate('Details', into = c("Var", "Val"), sep = ":") %>%
      dplyr::filter(Var == "Series") %>%
      tibble::deframe()
  } else(
    dplyr::select(my_file$raw_file, 'Units') %>%
      dplyr::filter(Units != "") %>%
      tibble::deframe() %>%
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
