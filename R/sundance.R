#' Summarize temperature data
#'
#' This function summarizes temperature data from Onset loggers.
#'
#' @param my_wxdat An \code{import_wxdat} object.
#'
#' @details
#' This function summarizes temperature data from Onset loggers. It
#'     uses a list produced from \code{\link{import_wxdat}} and returns a data
#'     frame of summarized temperature data.
#'
#' @return
#' This function returns a nine (9) column \code{\link[tibble:tibble]{tibble}}.
#'
#' \describe{
#'     \item{\strong{RID}}{The unique record ID. The record ID
#'         \code{\link[base:paste]{paste}}'s Date and PlotID to  create a unique
#'         record ID.}
#'     \item{\strong{PlotID}}{The unique ID number for the long-term monitoring
#'         plot.}
#'     \item{\strong{Date}}{The date the data were recorded.}
#'     \item{\strong{TEMP_mean}}{The mean daily temperature.}
#'     \item{\strong{TMIN}}{The minimum daily temperature.}
#'     \item{\strong{TMAX}}{The maximum daily temperature.}
#'     \item{\strong{n}}{The number of records for that day.}
#'     \item{\strong{TMIN_time}}{The time of minimum daily temperature.}
#'     \item{\strong{TEMP_mean}}{The time of maximum daily temperature.}
#' }
#'
#' @seealso \code{\link{import_wxdat}}
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
#' # Read file into R
#' my_temp <- import_wxdat(file_list[2])
#'
#' # Process precipitation data
#' sundance(my_temp)
#' }
sundance <- function(my_wxdat){
  # my_wxdat = import_wxdat(file_list[2])

  #-- QA check
  if(!stringr::str_detect(my_wxdat$file_info$Element, "TEMP")){
    stop("Data are not TEMP. Check data.")
  }
  #-- Summarize temperature data
  dat <- dplyr::filter(my_wxdat$data, Element == "TEMP") |>
    dplyr::mutate(Date = lubridate::date(DateTime)) |>
    dplyr::group_by(PlotID, Date) |>
    dplyr::summarize(TEMP_mean = mean(Value, na.rm = T),
                     TMIN = min(Value, na.rm = T),
                     TMAX = max(Value, na.rm = T),
                     n = dplyr::n())
  tmin.time <- dplyr::filter(my_wxdat$data, Element == "TEMP") |>
    dplyr::mutate(Date = lubridate::date(DateTime)) |>
    dplyr::left_join(dat, by = c("PlotID", "Date")) |>
    dplyr::filter(Value == TMIN) |>
    dplyr::group_by(PlotID, Date) |>
    dplyr::summarise(TMIN_time = strftime(min(DateTime), format="%H:%M:%S"))
  tmax.time <- dplyr::filter(my_wxdat$data, Element == "TEMP") |>
    dplyr::mutate(Date = lubridate::date(DateTime)) |>
    dplyr::left_join(dat, by = c("PlotID", "Date")) |>
    dplyr::filter(Value == TMAX) |>
    dplyr::group_by(PlotID, Date) |>
    dplyr::summarise(TMAX_time = strftime(min(DateTime), format="%H:%M:%S"))
  dat <- dplyr::left_join(dat, tmin.time) |>
    dplyr::left_join(tmax.time) |>
    dplyr::arrange(PlotID, Date) |>
    dplyr::mutate(RID = paste0(PlotID, as.numeric(Date)),
                  Date = as.character(Date)) |>
    dplyr::select(RID, PlotID, Date, TEMP_mean, TMIN, TMAX, n, TMIN_time, TMAX_time)
  return(dat)
}
