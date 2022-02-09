#' Process data from HOBOware
#'
#' This function processes data from Onset HOBOware.
#'
#' @param my_wxdat List from \code{\link{import_wxdat}}.
#'
#' @details
#' This function processes precipitation, temperature, and relative humidity
#'     data exported from Onset HOBOware. It uses the list created by
#'     \code{\link{import_wxdat}} and then processes the data using
#'     \code{\link{raindance}}, \code{\link{rhdance}}, or
#'     \code{\link{sundance}}.
#'
#'     For precipitation data, this function strips the first 5 minutes and last
#'     10 minutes of data to account for field procedures when downloading the
#'     event logger. It is common practice to trigger an event before
#'     downloading and after launching the logger. To remove these false events
#'     there is a routine to strip the first 5 minutes of the data if the
#'     "launch_time Time" and "First Sample Time" are the same. There is no way
#'     to determine if the tipping bucket is triggered before downloading the
#'     data, so the last 10 minutes of every file is stripped before processing.
#'
#' @return This function returns a five (5) object list.
#'
#' \describe{
#'     \item{\strong{file_info}}{This component is a vector that contains the
#'         file name, the date stamp, plot ID, the number of lines to skip to
#'         properly import the data, the number of columns of data in the
#'         raw file, and the Elements measured. This component is the product of
#'         \code{\link{import_file}}.}
#'     \item{\strong{details}}{This component is a data frame of metadata from
#'         \code{\link{get_details}}.}
#'     \item{\strong{data_raw}}{This component is a data frame from
#'         \code{\link{get_data}}.}
#'     \item{\strong{data}}{This component is a data frame of summarized data
#'         from \code{\link{raindance}}, \code{\link{rhdance}}, or
#'         \code{\link{sundance}}.}
#'     \item{\strong{raw_file}}{The raw file that was loaded into R using
#'         \code{\link{import_file}}.}
#' }
#'
#' @seealso \code{\link{import_wxdat}}, \code{\link{raindance}},
#'     \code{\link{rhdance}}, \code{\link{sundance}}
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
#' my_file <- import_wxdat(file_list[1])
#'
#' # Process data
#' process_hobo(my_file)
#' }
#'
process_hobo <- function(my_wxdat){
  # my_wxdat = import_wxdat(file_list[26])

  if(stringr::str_detect(my_wxdat$file_info$Element, "PRCP")){
    # Determine if first 5-min need to be stripped
    launch_time = dplyr::filter(my_wxdat$details, Details == "Launch Time")
    first_sample = dplyr::filter(my_wxdat$details, Details == "First Sample Time")

    dat <- if(launch_time$Value == first_sample$Value){
      dat <- dplyr::filter(my_wxdat$data_raw, Element == "PRCP" &
                             DateTime > min(DateTime, na.rm = T) + (5*60))
      } else {
        dat <- dplyr::filter(my_wxdat$data_raw, Element == "PRCP")
      }
    # Strip last t0 minues
    dat <- dplyr::filter(dat, DateTime < max(DateTime, na.rm = T) - (10*60))
    # Run raindance
    dat <- raindance(dat)
  } else if(stringr::str_detect(my_wxdat$file_info$Element, "RH")){
    # Process RH data
    dat_rh <- dplyr::filter(my_wxdat$data_raw, Element == "RH")
    dat_rh <- rhdance(dat_rh)
    # Process TEMP data
    dat_t <- dplyr::filter(my_wxdat$data_raw, Element == "TEMP")
    dat_t <- sundance(dat_t)
    # Combine
    dat <- rbind(dat_rh, dat_t)
  } else{
    dat <- dplyr::filter(my_wxdat$data_raw, Element == "TEMP")
    dat <- sundance(dat)
  }

  return(list(file_info = my_wxdat$file_info,
              details = my_wxdat$details,
              data_raw = my_wxdat$data_raw,
              data = dat,
              raw_file = my_wxdat$raw_file))
}
