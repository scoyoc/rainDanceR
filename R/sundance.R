#' Summarize temperature data
#'
#' This function summarizes temperature and relative humidity data from Onset
#'     data loggers.
#'
#' @param my_data A data frame with from \code{\link{import_hobo_2008}} or
#'     \code{\link{import_hobo}}. If not from these functions, the columns
#'     must include the following:
#'     \describe{
#'         \item{\strong{Element}}{ The element the data represent. TEMP is
#'             temperature or RH is relative humidity.}
#'         \item{\strong{PlotID}}{ The unique plot identification number (e.g.,
#'             A03 or I06).}
#'         \item{\strong{DateTime}}{ The date-time of the measurement.}
#'         \item{\strong{Value}}{ The data value of the measurement recorded by
#'             the data logger.}
#'         \item{\strong{Units}}{ The unit of the measurement.}
#'     }
#'
#' @details
#' This function summarizes temperature and relative humidity data from Onset
#'     loggers. It uses the data_raw data frame produced from
#'     \code{\link{import_hobo_2008}} ro \code{\link{import_hobo}} and
#'     returns a data frame of summarized temperature or relative humidity data.
#'
#' @return
#' This function returns a \code{\link[tibble:tibble]{tibble}}.
#'
#' \describe{
#'     \item{\strong{PlotID}}{ The unique ID number for the long-term monitoring
#'         plot.}
#'     \item{\strong{Date}}{ The date the data were recorded.}
#'     \item{\strong{TEMP_mean}}{ The mean daily temperature.}
#'     \item{\strong{TMIN}}{ The minimum daily temperature.}
#'     \item{\strong{TMAX}}{ The maximum daily temperature.}
#'     \item{\strong{n}}{ The number of records for that day.}
#'     \item{\strong{TMIN_time}}{ The time of minimum daily temperature.}
#'     \item{\strong{TMAX_time}}{ The time of maximum daily temperature.}
#'     \item{\strong{Units}}{ The unit of the measurement.}
#' }
#'
#' @seealso \code{\link{import_hobo_2008}}, \code{\link{import_hobo}}
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
#' # Temperature data
#' my_temp <- import_hobo_2008(file_list[4])$data_raw
#' sundance(my_temp)
#'
#' # Relative humidity data
#' my_rh <- import_hobo(file_list[8])$data_raw
#' sundance(my_rh)
#' }
sundance <- function(my_data){
  # my_data = raindancer::import_wxdat(file_list[10])$data_raw
  # my_data = raindancer::import_wxdat(file_list[4])$data_raw

  #-- QA check
  my_elements <- paste(unique(my_data$Element), collapse = "; ")
  if(!stringr::str_detect(my_elements, "TEMP") &
     !stringr::str_detect(my_elements, "RH")){
    stop("Data are not TEMP or RH. Check data.")
  }

  # Prep data
  dat <- dplyr::filter(my_data) |>
    dplyr::mutate("Date" = lubridate::date(DateTime),
                  "Time" = format(DateTime, format = "%H:%M")) |>
    dplyr::arrange(PlotID, DateTime, Element) |>
    dplyr::ungroup() |>
    dplyr::group_by(PlotID, Date, Element)
  # Subset Plot IDs and units of measurement
  my_units <- dplyr::select(my_data, PlotID, Element, Units) |>
    dplyr::distinct()
  # Sumarize temperature data
  dat_sum <- dat |>
    dplyr::summarize("Mean" = mean(Value, na.rm = TRUE),
                     "n" = dplyr::n(),
                     .groups = "keep")
  min_time <- dat  |>
    dplyr::filter(Value == min(Value)) |>
    dplyr::slice(1) |>
    dplyr::rename("Min" = Value, "MinTime" = Time) |>
    dplyr::select(PlotID, Date, Element, Min, MinTime)
  max_time <- dat  |>
    dplyr::filter(Value == max(Value)) |>
    dplyr::slice(1) |>
    dplyr::rename("Max" = Value, "MaxTime" = Time) |>
    dplyr::select(PlotID, Date, Element, Max, MaxTime)
  temp_dat <- dplyr::left_join(dat_sum, min_time,
                               by = c("PlotID", "Date", "Element")) |>
    dplyr::left_join(max_time, by = c("PlotID", "Date", "Element")) |>
    dplyr::arrange(PlotID, Date, Element) |>
    dplyr::select(PlotID, Date, Element, Mean, Min, Max, n, MinTime, MaxTime) |>
    dplyr::left_join(my_units, by = c("PlotID", "Element"))
  return(temp_dat)
}
