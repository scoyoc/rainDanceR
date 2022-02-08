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
#' @param ... Other arguments to pass to \code{\link{import_file}}. See
#'     \code{\link{import_file}} for more information.
#'
#' @details
#' This function imports the data from a csv file into R and retuns a list
#'     containing the data used to import the file, the metadata, and the
#'     raw data. This function uses \code{\link{import_file}},
#'     \code{\link{get_data}}, and \code{\link{get_details}} process the file.
#'
#' @return
#' This function returns a list with four components.
#'
#' \describe{
#'     \item{\strong{file_info}}{This component is a vector that contains the
#'         file name, the date stamp, plot ID, the number of lines to skip to
#'         properly import the data, the number of columns of data in the
#'         raw file, and the Elements measured. This comonent is the product of
#'         \code{\link{import_file}}.}
#'     \item{\strong{details}}{This component is a data frame of metadata from
#'         \code{\link{get_details}}.}
#'     \item{\strong{data}}{This component is a data frame from
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
#' library("rainDanceR")
#'
#' # Generate list of files
#' file_list <- list.files(path = system.file("extdata", package = "rainDanceR"),
#'                         pattern = ".csv", full.names = TRUE, recursive = FALSE)
#'
#' # Import data
#' import_wxdat(file_list[1])
#' }
#'
import_wxdat <- function(my_file, ...){
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
              'data' = my_data,
              'raw_file' = my_file$raw_file))
}
