#' Process a *.csv file produced by Onset HOBOware
#'
#' This function combines \code{\link{import_file}}, \code{\link{get_data}}, and
#'     \code{\link{get_details}} to process a comma delimited (csv) file
#'     produced by HOBOware. It uses the file name or full path name to produce
#'     a list with three components that contain the file information needed to
#'     import the csv, the raw data, and associated metadata.
#'
#' @param this_file A character string of the file name. This can include full
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
#' This function returns a list with three components.
#'
#' \describe{
#'     \item{\strong{file_info}}{This component is a vector that contains the
#'         file name, the date stamp, plot ID, the number of lines to skip to
#'         properly import the data, and the number of columns of data in the
#'         raw file. This comonent is a component of \code{\link{import_file}}.}
#'     \item{\strong{details}}{This component is a data frame of metadata. See
#'         \code{\link{get_details}} for more details.}
#'     \item{\strong{data}}{This component is a data frame containing the
#'         standardized form of the raw data. See \code{\link{get_data}} for
#'         more details.}
#'     \item{\strong{raw_file}}{The raw, unprocessed or standardized file that
#'         was loaded into R using \code{\link{import_file}}. See
#'         \code{\link{import_file}} for more details.}
#' }
#'
#' @seealso \code{\link{import_file}} to import a csv produced by HOBOware,
#'     \code{\link{get_data}} to extract the data, and \code{\link{get_details}}
#'     to extract the metadata.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library("dataProcessor")
#' file.list <- list.files(path = "./inst/raw_data", pattern = ".csv",
#'                         full.names = TRUE, recursive = FALSE)
#' import_wxdat(file.list[1])
#' }
#'
import_wxdat <- function(this_file, ...){
  #-- Import file
  my_file = import_file(this_file, datestamp_loc = 1, plotid_loc = 2,
                        plotid_s = 1, plotid_e = 3)
  #-- Extract data
  my_data = suppressMessages(suppressWarnings(get_data(my_file)))
  #-- Extract details
  my_details = suppressMessages(get_details(my_file, my_data))
  # Return list of objects
  return(list('file_info' = my_file$file_info,
              'details' = my_details,
              'data' = my_data,
              'raw_file' = my_file$raw_file))
}
