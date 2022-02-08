#' Summarize temperature data
#'
#' This function summarizes relative humidity (RH) data from Onset Hobo loggers.
#'
#' @param my_wxdat An \code{import_wxdat} object.
#'
#' @details
#' This function summarizes RH data from Onset Hobo loggers. It uses a list
#'     produced from \code{\link{import_wxdat}} and returns a data frame of
#'     summarized RH data.
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
#'     \item{\strong{RH_mean}}{The mean daily RH}
#'     \item{\strong{RHMIN}}{The minimum daily RH}
#'     \item{\strong{RHMAX}}{The maximum daily RH}
#'     \item{\strong{n}}{The number of records for that day.}
#'     \item{\strong{RHMIN_time}}{The time of minimum daily RH}
#'     \item{\strong{RHMAX_time}}{The time of maximum daily RH}
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
#' my_temp <- import_wxdat(file_list[12])
#'
#' # Process precipitation data
#' rhdance(my_temp)
#' }
rhdance <- function(my_wxdat){
  # my_wxdat = import_wxdat(file_list[12])

  #-- QA check
  if(!stringr::str_detect(my_wxdat$file_info$Element, "RH")){
    stop("Data are not RH. Check data.")
  }

  #-- Summarize temperature data
  dat <- dplyr::filter(my_wxdat$data, Element == "RH") |>
    dplyr::mutate(Date = lubridate::date(DateTime)) |>
    dplyr::group_by(PlotID, Date) |>
    dplyr::summarize(RH_mean = mean(Value, na.rm = T),
                     RHMIN = min(Value, na.rm = T),
                     RHMAX = max(Value, na.rm = T),
                     n = dplyr::n())
  rhmin.time <- dplyr::filter(my_wxdat$data, Element == "RH") |>
    dplyr::mutate(Date = lubridate::date(DateTime)) |>
    dplyr::left_join(dat, by = c("PlotID", "Date")) |>
    dplyr::filter(Value == RHMIN) |>
    dplyr::group_by(PlotID, Date) |>
    dplyr::summarise(RHMIN_time = strftime(min(DateTime), format="%H:%M:%S"))
  rhmax.time <- dplyr::filter(my_wxdat$data, Element == "RH") |>
    dplyr::mutate(Date = lubridate::date(DateTime)) |>
    dplyr::left_join(dat, by = c("PlotID", "Date")) |>
    dplyr::filter(Value == RHMAX) |>
    dplyr::group_by(PlotID, Date) |>
    dplyr::summarise(RHMAX_time = strftime(min(DateTime), format="%H:%M:%S"))
  dat <- dplyr::left_join(dat, rhmin.time) |>
    dplyr::left_join(rhmax.time) |>
    dplyr::arrange(PlotID, Date) |>
    dplyr::mutate(RID = paste0(PlotID, as.numeric(Date)),
                  Date = as.character(Date)) |>
    dplyr::select(RID, PlotID, Date, RH_mean, RHMIN, RHMAX, n, RHMIN_time,
                  RHMAX_time)
  return(dat)
}
