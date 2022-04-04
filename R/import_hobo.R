#' Process a *.csv file produced by Onset HOBOware (2020-present)
#'
#' This function imports a comma delimited (*.csv) file produced by Onset
#'     HOBOware from Onset data loggers used in NPS Southeast Utah Group parks
#'     from 2020 to present. It uses the file name or full path name to produce
#'     a list with four components that contain the file information needed to
#'     import the csv, metadata about the logger and sampling time, and the raw
#'     data.
#'
#' @param my_file A character string of the file name. This should include the
#'     directory path.
#' @param datestamp_loc The index of the location of the timestamp in the file
#'     name. Default is '1'.
#' @param plotid_loc The index of the location of the Plot ID in the file name.
#'     Default is 2.
#' @param plotid_s The index for the character that starts the Plot ID string.
#'     Default is 1.
#' @param plotid_e The index for the character that ends the Plot ID string.
#'     Default is 3.
#'
#' @details
#' This function imports the data from a csv file into R and retuns a list
#'     containing the data used to import the file, the metadata, and the
#'     raw data.
#'
#' @return This function returns a list with three (3) components.
#'
#' \describe{
#'     \item{\strong{file_info}}{This component is a vector that contains the
#'         file name, the date stamp, plot ID, the number of lines to skip to
#'         properly import the data, the number of columns of data in the
#'         raw file, and the Elements measured.}
#'     \item{\strong{details}}{This component is a data frame of metadata.}
#'     \item{\strong{data_raw}}{This component is a data frame.}
#' }
#'
#' @seealso \code{\link{import_hobo_2008}}
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
#' import_hobo_2020(file_list[11])
#' }
#'
import_hobo <- function(my_file, datestamp_loc = 1, plotid_loc = 2,
                             plotid_s = 1, plotid_e = 3){
  # Testing: my_file = file_list[10]
  # Testing: datestamp_loc = 1; plotid_loc = 2; plotid_s = 1; plotid_e = 3

  # Find number of columns to determind how to process file
  cols = colnames(suppressMessages(suppressWarnings(
    readr::read_csv(my_file, skip = 1, show_col_types = FALSE)
  )))

  my_units <- data.frame(PRCP = pull_element(prcp_list, cols),
                         TEMP = pull_element(c("TEMP", "Temp"), cols),
                         RH = pull_element("RH", cols)) |>
    tidyr::gather(key = "Element", value = "Check") |>
    dplyr::filter(Check == TRUE) |>
    dplyr::select(-Check) |>
    dplyr::mutate(Units =  ifelse(Element == "PRCP", "Event",
                                  ifelse(Element == "RH", "%RH",
                                         ifelse(Element == "TEMP" &
                                                  TRUE %in%
                                                  stringr::str_detect(cols, "F"),
                                                "F", "C") ))
                                         )

  # Strip Datestamp and Plot ID from file name
  DateStamp = stringr::str_split(basename(my_file), "_")[[1]][datestamp_loc]
  PlotID = toupper(stringr::str_sub(strsplit(basename(my_file), "_")[[1]][plotid_loc],
                                    plotid_s, plotid_e))

  if(length(cols) == 4){
    # Get raw data
    dat_raw <- suppressMessages(suppressWarnings(
      readr::read_csv(my_file, skip = 2, col_select = 1:3,
                      col_names = c("RID", "DateTime", "Value"),
                      show_col_types = FALSE)
      )) |>
      dplyr::mutate("FileName" = basename(my_file),
                    "PlotID" = PlotID,
                    "DateTime" = lubridate::parse_date_time(DateTime,
                                                            c("%m%d%y %H%M%S",
                                                              "%y%m%d %H%M%S",
                                                              "%m%d%y %H%M",
                                                              "%y%m%d %H%M")),
                    "Element" = my_units$Element,
                    "Units" = my_units$Unit) |>
      dplyr::select(FileName, PlotID, DateTime, Element, Value, Units) |>
      tibble::tibble()

    #-- Recreate file_info
    sn_string <- unlist(stringr::str_split(gsub(",", "", cols[3]), " "))
    launchname <- as.character(read.table(my_file, header = F, nrows = 1)[3]) |>
      stringr::str_replace("\"", "")

    file_info <- data.frame("FileName" = basename(my_file),
                            "PlotID" = PlotID,
                            "Element" = my_units$Element,
                            "Product" = NA,
                            "SerialNumber" = sn_string[5],
                            "LaunchName" = launchname,
                            "DeploymentNumber" = NA,
                            "LaunchTime" = NA,
                            "FirstSampleTime" = as.character(min(dat_raw$DateTime,
                                                                 na.rm = TRUE)),
                            "LastSampleTime" = as.character(max(dat_raw$DateTime,
                                                                na.rm = TRUE)),
                            "ImportDate" = as.character(lubridate::today()))

    #-- Recreate details
    details <- tidyr::gather(file_info, "Details", "Value") |>
      dplyr::mutate(FileName = basename(my_file),
                    Units = my_units$Unit) |>
      dplyr::select(FileName, Details, Value, Units)

    } else({

      # Extract logger details
      if(length(cols) == 10){
        details <- suppressMessages(suppressWarnings(
          readr::read_csv(my_file, skip = 3, col_select = 09:10,
                          show_col_types = FALSE)
        ))
      } else(details <- suppressMessages(suppressWarnings(
        readr::read_csv(my_file, skip = 3,
                        col_select = (length(cols) + 1):(length(cols) + 2),
                        show_col_types = FALSE)
        ))
      )

      if(sum(is.na(details[1])) == nrow(details[1]) - 1){
        details <- suppressMessages(suppressWarnings(
          readr::read_csv(my_file, skip = 3,
                          col_select = (length(cols) + 1):(length(cols) + 2),
                          show_col_types = FALSE)
        ))
      }

      names(details) <- c("Details", "Units")
      details <- details |>
        tidyr::separate('Details', into = c("Details", "Value"), sep = ":",
                        remove = T, extra = "merge", fill = "right") |>
        dplyr::mutate(FileName = basename(my_file)) |>
        dplyr::select(FileName, Details, Value, Units) |>
        dplyr::distinct()


      # Extract file info
      file_info <- data.frame("FileName" = basename(my_file),
                              "PlotID" = PlotID,
                              "Element" = my_units$Element,
                              "Product" = pull_detail("Product", details),
                              "SerialNumber" = pull_detail("Serial Number",
                                                           details),
                              "LaunchName" = pull_detail("Launch Name", details),
                              "DeploymentNumber" = pull_detail("Deployment Number",
                                                               details),
                              "LaunchTime" = pull_detail("Launch Time",
                                                         details),
                              "FirstSampleTime" = pull_detail("First Sample Time",
                                                              details),
                              "LastSampleTime" =  pull_detail("Last Sample Time",
                                                              details))

    # Get raw data
    if(nrow(my_units) == 1){
      dat_raw = suppressMessages(suppressWarnings(
        readr::read_csv(my_file, skip = 2, col_select = 1:3,
                        col_names = c("RID", "DateTime", "Value"),
                        show_col_types = FALSE)
      )) |>
        dplyr::mutate("FileName" = basename(my_file),
                      "PlotID" = PlotID,
                      "DateTime" = lubridate::parse_date_time(DateTime,
                                                              c("%m%d%y %H%M%S",
                                                                "%y%m%d %H%M%S",
                                                                "%m%d%y %H%M",
                                                                "%y%m%d %H%M")),
                      "Element" = my_units$Element,
                      "Units" = my_units$Units) |>
        dplyr::select(FileName, PlotID, DateTime, Element, Value, Units)

    } else if(nrow(my_units) == 2){
      my_col1 <- toupper(unlist(stringr::str_split(cols[3], ","))[1])
      my_col2 <- toupper(unlist(stringr::str_split(cols[4], ","))[1])
      dat_raw = suppressMessages(suppressWarnings(
        readr::read_csv(my_file, skip = 2, col_select = 1:4,
                        col_names = c("RID", "DateTime", my_col1, my_col2),
                        show_col_types = FALSE)
        )) |>
        tidyr::gather(key = "Element", value = "Value", 3:4) |>
        dplyr::mutate("FileName" = basename(my_file),
                      "PlotID" = PlotID,
                      "DateTime" = lubridate::parse_date_time(DateTime,
                                                              c("%m%d%y %H%M%S",
                                                                "%y%m%d %H%M%S",
                                                                "%m%d%y %H%M",
                                                                "%y%m%d %H%M")),
                      "Element" = ifelse(Element %in% prcp_list, "PRCP",
                                         Element)) |>
        dplyr::filter(!is.na(Value)) |>
        dplyr::left_join(my_units, by = "Element") |>
        dplyr::select(FileName, PlotID, DateTime, Element, Value, Units) |>
        dplyr::arrange(PlotID, DateTime, Element)
      } else(
        stop(glue::glue("cols = {cols}"))
        ) # end data else
    }) # end cols else

  # Clean up details data frame
  details <- dplyr::select(details, -Units) |>
    dplyr::filter(!is.na(Value)) |>
    dplyr::mutate("Value" = trimws(Value, which = "both"))

  # Return list
  return(list("file_info" = file_info,
              "details" = details,
              "data_raw" = dat_raw))
}

#-- Internal functions
# Extract value form detail data frame
pull_detail <- function(var, details_df){
  x = (dplyr::select(details_df, Details, Value) |>
         dplyr::filter(Details %in% var) |>
         dplyr::slice(1))$Value
  trimws(x)
}

# Determine element from unit of measurement
pull_element <- function(my_list, cols){
  TRUE %in% sapply(my_list, function(x)stringr::str_detect(cols, x))
}

pull_units <- function(element, cols){
  unit <- if(element == "PRCP"){ "Event"
  } else if(element == "RH"){ "%RH"
    } else if(TRUE %in% stringr::str_detect(cols, "F")){ "F"
      } else if(TRUE %in% stringr::str_detect(cols, "C")){ "C"
        } else("NA")
  return(unit)
}

# List of measurements
prcp_list <- c("Event", "EvenHD", "Event (PRCP)", "Events", "Rain", "Units",
               "units", "units (PRCP)", "EXT. LINE EVENT", "EVENT")
# temp_list <- c("C", "°C", "C (TEMP)", "C (Temp_C)", "°C (TEMP)", "F", "°F")
# rh_list <- "%RH"
