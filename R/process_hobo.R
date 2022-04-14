#' Process data from HOBOware
#'
#' This function processes data from Onset HOBOware.
#'
#' @param my_wxdat List from \code{\link{import_hobo_2008}} or
#'     \code{\link{import_hobo}}.
#'
#' @details
#' This function processes precipitation, temperature, and relative humidity
#'     data exported from Onset HOBOware. It uses the list created by
#'     \code{\link{import_hobo_2008}} or \code{\link{import_hobo}} and then
#'     processes the data using \code{\link{raindance}} or
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
#' @return This function returns a four (4) object list.
#'
#' \describe{
#'     \item{\strong{file_info}}{This component is a vector that contains the
#'         file name, the date stamp, plot ID, the number of lines to skip to
#'         properly import the data, the number of columns of data in the
#'         raw file, and the Elements measured.}
#'     \item{\strong{details}}{This component is a data frame of metadata.}
#'     \item{\strong{data_raw}}{This component is a data frame.}
#'     \item{\strong{data}}{This component is a data frame of summarized data
#'         from \code{\link{raindance}} or \code{\link{sundance}}.}
#' }
#'
#' @seealso \code{\link{import_hobo_2008}}, \code{\link{import_hobo}},
#'     \code{\link{raindance}}, \code{\link{sundance}}
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
#' # Precipitation data
#' import_hobo_2008(file_list[2]) |> process_hobo()
#'
#' # Temperature and relative humidity data
#' import_hobo(file_list[5]) |> process_hobo()
#' }
#'
process_hobo <- function(my_wxdat){
  # my_wxdat = import_hobo(file_list[1])
  if(TRUE %in% stringr::str_detect(my_wxdat$file_info$Element, "PRCP")){
    if(nrow(my_wxdat$file_info) == 1){
      launch_time <- my_wxdat$file_info$LaunchTime
      first_sample <- my_wxdat$file_info$FirstSampleTime
      # Determine if first 5-min need to be stripped
      if(launch_time == first_sample | is.na(launch_time == first_sample)){
        prcp_dat <- my_wxdat$data_raw |>
          dplyr::filter(Element == "PRCP" &
                          DateTime > min(DateTime, na.rm = T) + (5*60))
        } else({
          prcp_dat <- dplyr::filter(my_wxdat$data_raw, Element == "PRCP")
          })
      # Strip last 10 minutes & process data
      dat <- dplyr::filter(prcp_dat,
                           DateTime < max(DateTime, na.rm = T) - (10*60)) |>
        raindance()

    } else({
      # Process PRPC Data
      pcrp_info <- dplyr::filter(my_wxdat$file_info, Element == "PRCP")
      launch_time <- pcrp_info$LaunchTime
      first_sample <- pcrp_info$FirstSampleTime
      # Determine if first 5-min need to be stripped
      if(launch_time == first_sample | is.na(launch_time == first_sample)){
        prcp_dat <- my_wxdat$data_raw |>
          dplyr::filter(Element == "PRCP" &
                          DateTime > min(DateTime, na.rm = T) + (5*60))
        } else{
          prcp_dat <- dplyr::filter(my_wxdat$data_raw, Element == "PRCP")
        }
      # Strip last 10 minutes & process data
      prcp_dat <- dplyr::filter(prcp_dat,
                                DateTime < max(DateTime, na.rm = T) - (10*60)) |>
        raindance()

      # Process TEMP Data
      temp_dat <- my_wxdat$data_raw |>
        dplyr::filter(Element == "TEMP") |>
        sundance()

      # Combine datasets
      dat <- list(prcp_dat = prcp_dat,
                  temp_dat = temp_dat)
      })
    } else(dat <- sundance(my_wxdat$data_raw))

  return(list(file_info = my_wxdat$file_info,
              details = my_wxdat$details,
              data_raw = my_wxdat$data_raw,
              data = dat))
}
