#' Process a *.csv file produced by Onset HOBOware
#'
#' This function combines \code{\link{import_file}}, \code{\link{get_data}}, and
#'     \code{\link{get_details}} to process a comma delimited (*.csv) file
#'     produced by HOBOware. It uses the file name or full path name to produce
#'     a list with three components that contain the file information needed to
#'     import the csv, the raw data, and associated metadata.
#'
#' @param my_file A character string of the file name. This can include full
#'     directory path.
#'
#' @details
#' This function imports the data from a csv file into R and retuns a list
#'     containing the data used to import the file, the metadata, and the
#'     raw data. This function uses \code{\link{import_file}},
#'     \code{\link{get_data}}, and \code{\link{get_details}} process the file.
#'
#' @return This function returns a list with four (4) components.
#'
#' \describe{
#'     \item{\strong{file_info}}{This component is a vector that contains the
#'         file name, the date stamp, plot ID, the number of lines to skip to
#'         properly import the data, the number of columns of data in the
#'         raw file, and the Elements measured. This comonent is the product of
#'         \code{\link{import_file}}.}
#'     \item{\strong{details}}{This component is a data frame of metadata from
#'         \code{\link{get_details}}.}
#'     \item{\strong{data_raw}}{This component is a data frame from
#'         \code{\link{get_data}}.}
#'     \item{\strong{raw_file}}{The raw file that was loaded into R using
#'         \code{\link{import_file}}.}
#' }
#'
#' @seealso \code{\link{import_file}}, \code{\link{get_data}},
#'     \code{\link{get_details}}
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
#' # Import data
#' import_wxdat(file_list[10])
#' }
#'
import_wxdat <- function(my_file){
  # my_file = file_list[12]
  #-- Import file
  my_file = import_file(my_file, datestamp_loc = 1, plotid_loc = 2,
                        plotid_s = 1, plotid_e = 3)
  #-- Extract data
  my_data = suppressMessages(suppressWarnings(get_data(my_file)))
  #-- Extract details
  my_details = suppressMessages(get_details(my_file, my_data))
  #-- Add element to file info
  if(length(unique(my_data$Element)) == 1){
    my_file$file_info$Element = unique(my_data$Element)
    } else if(length(unique(my_data$Element)) > 1){
      my_file$file_info$Element = paste(unique(my_data$Element), collapse = ";")
      } else(my_file$file_info$Element == NA)
  # Return list of objects
  return(list('file_info' = my_file$file_info,
              'details' = my_details,
              'data_raw' = my_data,
              'raw_file' = my_file$raw_file))
}

#-- Internal functions --
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

#-- Pull logger details from raw file
get_details <- function(my_file, my_data){
  # DESCRIPTION
  # This function pulls details from the raw file. It uses the list produced
  # from import_file().

  # Pull logger type, element, and units from Details
  my_logger = get_product(my_file)

  # Strip Details from raw_file
  details = my_file$raw_file |>
    dplyr::select('Details') |>
    # Reduce column and remove white space
    dplyr::distinct() |>
    tidyr::separate('Details', into = c("Var", "Value"), sep = ":", remove = T,
                    extra = "merge", fill = "right") |>
    dplyr::filter(Value != "") |>
    dplyr::filter(!Var %in% c("Version Number", "Manufacturer", "Header Created",
                              "Launch GMT Offset", "Max", "Min", "Avg")) |>
    dplyr::filter(!stringr::str_detect(Var, "Std Dev")) |>
    dplyr::group_by(Var) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::mutate('Value' = trimws(Value, "both")) |>
    tidyr::spread(key = Var, value = Value, fill = NA) |>
    dplyr::mutate('Import Date' = as.character(lubridate::today()),
                  'Plot ID' = my_file$file_info$PlotID,
                  'Element' = my_logger$Element,
                  'Units' = paste(my_logger$Units, my_logger$Units,
                                  sep = ";"),
                  'DateTime (min)' = as.character(min(my_data$DateTime,
                                                      na.rm = T)),
                  'DateTime (max)' = as.character(max(my_data$DateTime,
                                                      na.rm = T)),
                  'Records (n)' = nrow(my_data),
                  'ConvertFtoC' = ifelse(Element == "TEMP" &&
                                           stringr::str_detect(my_logger$Units,
                                                               "F"),
                                         "Yes", "No")) |>
    tidyr::gather(key = 'Details', value = 'Value') |>
    dplyr::mutate("FileName" =my_file$file_info$FileName) |>
    dplyr::select('FileName', 'Details', 'Value') |>
    dplyr::arrange('Details')
  details = details |>
    dplyr::add_row(tibble::tibble_row("FileName" = my_file$file_info$FileName,
                                      "Details" = "QFLAG",
                                      "Value" = qflags(my_logger, details, my_data)))
  return(details)
}

# Extract HOBO logger type and associated data types it collects.
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

#-- Import file into R
import_file <- function(my_file, datestamp_loc = 1, plotid_loc = 2,
                        plotid_s = 1, plotid_e = 3){
  #-- Pull elements from file
  file_info = data.frame(
    # The file name
    FileName = basename(my_file),
    # Strip time stamp from file name
    DateStamp = stringr::str_split(basename(my_file), "_")[[1]][datestamp_loc],
    # Strip Plot ID from file name
    PlotID = toupper(stringr::str_sub(strsplit(basename(my_file), "_")[[1]][plotid_loc],
                                      plotid_s, plotid_e)),
    # Determine if the first row is to be skipped
    skip = ifelse(stringr::str_detect(suppressWarnings(
      read.table(my_file, sep = ",", header = FALSE, nrows = 1, fill = TRUE))['V1'],
      "Plot"),
      2, 1)
  ) |>
    # Count the number of columns of data
    dplyr::mutate(col_n = ncol(suppressWarnings(read.table(my_file, sep = ",",
                                                           header = FALSE,
                                                           fill = TRUE,
                                                           skip = skip))))

  #-- Import raw file
  if(file_info$col_n == 4){
    raw_file =  suppressWarnings(
      read.table(my_file, sep = ",", header = FALSE, fill = TRUE,
                 skip = file_info$skip,
                 col.names = c("RID", "DateTime", "Value", "Details"))) |>
      tidyr::drop_na() |>
      tibble::as_tibble()

  } else if(file_info$col_n == 5){
    raw_file =  suppressWarnings(
      read.table(my_file, sep = ",", header = FALSE, fill = TRUE,
                 skip = file_info$skip,
                 col.names = c("RID", "DateTime", "Value", "Details",
                               "Units"))) |>
      tidyr::drop_na() |>
      tibble::as_tibble()

  } else if(file_info$col_n == 6){
    file_colnames <- names(read.table(my_file, sep = ",", header = TRUE,
                                      skip = 1, comment.char = "$"))
    if("Time" %in% file_colnames){
      raw_file =  suppressWarnings(
        read.table(my_file, sep = ",", header = FALSE, fill = TRUE, skip = 2,
                   col.names = c("RID", "Date", "Time", "Value",
                                 "Details", "Units"),
                   comment.char = "$")
      ) |>
        tidyr::drop_na() |>
        dplyr::mutate(DateTime = paste(Date, Time, sep = " ")) |>
        dplyr::select(RID, DateTime, Value, Details, Units) |>
        tibble::as_tibble()
    } else(
      raw_file =  suppressWarnings(
        read.table(my_file, sep = ",", header = FALSE, fill = TRUE,
                   skip = file_info$skip,
                   col.names = c("RID", "DateTime", "Value", "EndOfFile",
                                 "Details", "Units"))) |>
        tidyr::drop_na() |>
        tibble::as_tibble()
    )

  } else if(file_info$col_n == 7){
    raw_file =  suppressWarnings(
      read.table(my_file, sep = ",", header = FALSE, fill = TRUE,
                 skip = file_info$skip,
                 col.names = c("RID", "DateTime", "Value", "BadBattery",
                               "EndOfFile", "Details", "Units"))) |>
      tidyr::drop_na() |>
      tibble::as_tibble()

  } else if(file_info$col_n == 8){
    raw_file =  suppressWarnings(
      read.table(my_file, sep = ",", header = FALSE, fill = TRUE,
                 skip = file_info$skip,
                 col.names = c("RID", "DateTime", "Value","Attached",
                               "Connected","EndFile", "Details","Units"))) |>
      tidyr::drop_na() |>
      tibble::as_tibble()

  }else if(file_info$col_n == 9){
    raw_file =  suppressWarnings(
      read.table(my_file, sep = ",", header = FALSE, fill = TRUE,
                 skip = file_info$skip,
                 col.names = c("RID", "DateTime", "Value", "Detatched",
                               "Attached", "Connected","EndFile", "Details",
                               "Units"))) |>
      tidyr::drop_na() |>
      tibble::as_tibble()

  } else if(file_info$col_n == 10){
    raw_file =  suppressWarnings(
      read.table(my_file, sep = ",", header = FALSE, fill = TRUE,
                 skip = file_info$skip,
                 col.names = c("RID", "DateTime", "Temp", "RH",
                               "Detatched", "Attached", "Connected",
                               "EndFile", "Details","Units"))) |>
      tidyr::drop_na() |>
      tidyr::as_tibble()

  } else(message(paste0("Something is wrong. Check file: ", basename(my_file),
                        "; ncol = ", file_info$col_n)))

  return(list("file_info" = file_info, "raw_file" = raw_file))
}

#-- Internal data frames --
# Data frame of Onset data loggers and what they measure
onset_loggers <- data.frame(
  'Product' = c("H07 Logger", "HOBO UA-003-64 Pendant Temp/Event", "H08 Logger",
                "HOBO UA-001-64 Pendant Temp", "HOBO U23-001 Temp/RH", ""),
  'Element' = c("PRCP", "PRCP", "TEMP", "TEMP", "TEMP-RH", "Unknown")
)

# QAQC flags
qflags <- function(my_logger, my_details, my_data){
  flags = data.frame(
    Logger = ifelse(my_logger$Element == "Unknown", 1, NA),
    Units = ifelse(my_logger$Units == "Unknown", 2, NA),
    DateTimeNA = ifelse(sum(is.na(my_data$DateTime)) > 0, 3, NA),
    DataNA = ifelse(sum(is.na(my_data$Value)) > 0, 4, NA)
  ) |>
    tidyr::gather("Category", "Flag")

  qflags = ifelse(sum(is.na(flags$Flag)) != nrow(flags),
                  paste(dplyr::filter(flags, Flag != is.na(Flag))$Flag, sep = ","),
                  NA)
  return(qflags)
}
