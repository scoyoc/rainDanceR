#' Summarize relative humidity data
#'
#' This function summarizes relative humidity (RH) data from Onset loggers.
#'
#' @param my_data A data frame with four columns. Typically from
#'     \code{\link{get_data}}. Columns must include the following:
#'     \describe{
#'         \item{\strong{Element}}{ The element the data represent. TEMP is
#'             temperature, RH is relative humidity, and PRCP is precipitation.}
#'         \item{\strong{PlotID}}{ The unique plot identification number (e.g.,
#'             A03 or I06).}
#'         \item{\strong{DateTime}}{ The date-time of the measurement.}
#'         \item{\strong{Value}}{ The data value of the measurement recorded by
#'             the data logger.}
#'     }
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
#'     \item{\strong{PlotID}}{ The unique ID number for the long-term monitoring
#'         plot.}
#'     \item{\strong{Date}}{ The date the data were recorded.}
#'     \item{\strong{RH_mean}}{ The mean daily RH}
#'     \item{\strong{RHMIN}}{ The minimum daily RH}
#'     \item{\strong{RHMAX}}{ The maximum daily RH}
#'     \item{\strong{n}}{ The number of records for that day.}
#'     \item{\strong{RHMIN_time}}{ The time of minimum daily RH}
#'     \item{\strong{RHMAX_time}}{ The time of maximum daily RH}
#' }
#'
#' @seealso \code{\link{get_data}}, \code{\link{import_wxdat}}
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
#' my_rh <- import_wxdat(file_list[10])$data_raw
#'
#' # Process precipitation data
#' rhdance(my_rh)
#' }
rhdance <- function(my_data){
  # my_data = import_wxdat(file_list[26])$data_raw

  #-- QA check
  my_elements <- paste(unique(my_data$Element), collapse = ";")
  if(!stringr::str_detect(my_elements, "RH")){
    stop("Data are not RH. Check data.")
  }

  #-- Summarize temperature data
  dat <- dplyr::filter(my_data, Element == "RH")|>
    dplyr::mutate("Date" = lubridate::date(DateTime)) |>
    dplyr::ungroup() |>
    dplyr::group_by(PlotID, Date)
  dat_sum <- dat |>
    dplyr::summarize("Mean" = mean(Value, na.rm = T),
                     "Min" = min(Value, na.rm = T),
                     "Max" = max(Value, na.rm = T),
                     "n" = dplyr::n(),
                     .groups = "keep")
  min_time <- dat  |>
    dplyr::left_join(dat_sum, by = c("PlotID", "Date")) |>
    dplyr::filter(Value == Min) |>
    dplyr::summarise("MinTime" = strftime(min(DateTime), format="%H:%M:%S"),
                     .groups = "keep")
  max_time <- dat |>
    # dplyr::mutate("Date" = lubridate::date(DateTime)) |>
    dplyr::left_join(dat_sum, by = c("PlotID", "Date")) |>
    dplyr::filter(Value == Max) |>
    dplyr::summarise("MaxTime" = strftime(min(DateTime), format="%H:%M:%S"),
                     .groups = "keep")
  rh_dat <- dplyr::left_join(dat_sum, min_time) |>
    dplyr::left_join(max_time) |>
    dplyr::arrange(PlotID, Date) |>
    dplyr::mutate("Element" = "RH") |>
    dplyr::select(PlotID, Date, Element, Mean, Min, Max, n, MinTime, MaxTime)
  suppressMessages(return(rh_dat))
}
