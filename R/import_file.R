#' Import a csv file produced by Onset HOBOware into R
#'
#' This function extracts data from the file name and comma delimited (csv)
#'    file produced by HOBOware so the data can be imported into R. This
#'    function produces a list that will be used in subsequent functions,
#'    including \code{get_data} and \code{get_details}, to import data into R.
#'
#' @param my_file A character string of the file name. This can include full
#'     directory path.
#' @param datestamp_loc A number for the location of the date stamp in the file
#'     name, an index. Default is 1.
#' @param plotid_loc A number for the location of the Plot ID in the file name,
#'     an index. Default is 2.
#' @param plotid_s A number for the beginning of the Plot ID in the file name,
#'     an index. Default is 1.
#' @param plotid_e A number for the end of the Plot ID in the file name, an
#'     index. Default is 3.
#'
#' @details
#' This function extracts variables from the file name and the raw data so the
#'     file can be imported into R. It returns a list with two components. The
#'     first component is a vector containing information about the file. The
#'     second component is a data frame containing the raw data and the
#'     metadata for the sampling event.
#'
#' @return This function returns a list with two (2) components.
#'
#' \describe{
#'    \item{\strong{file_info}}{ This component is a vector that contains the
#'        file name, the date stamp, plot ID, the number of lines to skip to
#'        properly import the data, and the number of columns of data in the raw
#'        file. Varibles listed below:}
#'            \itemize{
#'                \item{filename: The base name of the file being processed.}
#'                \item{datestamp: A date stamp. stripped from the file name.}
#'                \item{plotid: The plot identification number. Stripped from
#'                    the file name.}
#'                \item{skip:The number of lines to skip to read in the file
#'                    correctly.}
#'                \item{col_n: The number of columns in the raw data file.}}
#'    \item{\strong{raw_file}}{ This component is a data frame containing the
#'        raw data and the "Details" column produced by HOBOware.}
#' }
#'
#' @seealso \code{\link{get_data}}, \code{\link{get_details}}
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
#' import_file(file_list[12])
#' }
#'
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

  } else if(file_info$col_n == 9){
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
