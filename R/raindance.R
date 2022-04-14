#' Summarize precipitation data
#'
#' This function summarizes precipitation data from Onset event loggers
#'     used in Onset tipping bucket precipitation gauges.
#'
#' @param my_data A data frame with from \code{\link{import_hobo_2008}} or
#'     \code{\link{import_hobo}}. If not from these functions, the columns
#'     must include the following:
#'     \describe{
#'         \item{\strong{Element}}{ The element the data represent. Onset
#'             tipping bucket event loggers record an event when 0.254 mm of
#'             precipitation tips the bucket inside the raingauge.}
#'         \item{\strong{PlotID}}{ The unique plot identification number (e.g.,
#'             A03 or I06).}
#'         \item{\strong{DateTime}}{ The date-time of the precipitation event}
#'         \item{\strong{Value}}{ Event data value recorded by the data logger.}
#'     }
#'
#' @details
#' This function summarizes precipitation data from Onset event loggers
#'     used in Onset tipping bucket precipitation gauges. It uses the data_raw
#'     data frame produced from \code{\link{import_hobo_2008}} or
#'     \code{\link{import_hobo}} and returns a data frame of hourly
#'     precipitation totals, number of tips per hour, and maximum tips per
#'     minute.
#'
#' @return
#' This function returns a \code{\link[tibble:tibble]{tibble}}.
#'
#' \describe{
#'     \item{\strong{DateTime}}{ The date and hour of the data. Hours are the from
#'         0 min 00 sec to 59 min 59 sec. For example 2009-04-15 00:00:00 is
#'         from 00:00:00 to 00:59:59 on April 15, 2009.}
#'     \item{\strong{PlotID}}{ The unique ID number for the long-term monitoring
#'         plot.}
#'     \item{\strong{PRCP_mm}}{ The recorded total precipitation for that hour in
#'         millimeters.}
#'     \item{\strong{Tips}}{ The number of tips recorded for that hour.}
#'     \item{\strong{MaxTips_min}}{ The maximum tips per minute recorded. This is
#'         intended to calculate intensity of precipitation event.}
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
#' # Read data into R
#' my_prcp <- import_hobo_2008(file_list[3])$data_raw
#'
#' # Process precipitation data
#' raindance(my_prcp)
#' }
#'
raindance <- function(my_data){
  # my_data <- import_wxdat(file_list[3])$data_raw

  # TODO: write routine to sequence DateTime's across multiple PlotID's

  # QAQC
  my_elements <- paste(unique(my_data$Element), collapse = ";")
  if(!stringr::str_detect(my_elements, "PRCP")){
    stop("Data are not PRCP. Check data.")
  }

  #-- Summarize precipitation data
  plotid = unique(my_data$PlotID)
  dat <- my_data |>
    dplyr::mutate("Date" = lubridate::date(DateTime),
                  "Hour" = paste(lubridate::hour(DateTime), "00", sep = ":"),
                  "Hour" = ifelse(Hour == "24:00", "0:00", Hour),
                  "Min" = lubridate::minute(DateTime),
                  "DateTime_new" = paste(Date, Hour, sep = " "),
                  "DateTime_new" = lubridate::parse_date_time(DateTime_new,
                                                              orders =  "%Y-%m-%d %H:%M")) |>
    dplyr::ungroup()

  if(nrow(dat) == 0){
    stop("Data are not valid.")
  } else{
    # Calculate hourly precipitation totals
    hr_tot <- dat |>
      dplyr::group_by(PlotID, Date, Hour) |>
      dplyr::summarise("mm.hr" = dplyr::n() * 0.254,
                       .groups = "keep") |>
      dplyr::mutate("Hour" = factor(Hour,
                                  levels = c("0:00",
                                             paste0(seq(1:23), ":00")))) |>
      tidyr::spread(Hour, mm.hr, fill = 0) |>
      tidyr::gather(Hour, mm.hr, c(-PlotID, -Date)) |>
      dplyr::mutate("DateTime" = paste(Date, Hour, sep = " ")) |>
      dplyr::ungroup() |>
      dplyr::select(PlotID, DateTime, mm.hr)
    # Calculate number of tips per hour
    tips_hr <- dat |>
      dplyr::group_by(PlotID, Date, Hour, Min) |>
      dplyr::summarise("tips.min" = dplyr::n(),
                       .groups = "keep") |>
      dplyr::group_by(PlotID, Date, Hour) |>
      dplyr::summarise("tips.hr" = dplyr::n(),
                       .groups = "keep") |>
      dplyr::mutate("Hour" = factor(Hour,
                                  levels = c("0:00",
                                             paste0(seq(1:23), ":00")))) |>
      tidyr::spread(Hour, tips.hr, fill = 0) |>
      tidyr::gather(Hour, tips.hr, c(-PlotID, -Date)) |>
      dplyr::mutate("DateTime" = paste(Date, Hour, sep = " ")) |>
      dplyr::ungroup() |>
      dplyr::select(PlotID, DateTime, tips.hr)
    # Calculate max tips per minute
    max_tips <- dat |>
      dplyr::group_by(PlotID, Date, Hour, Min) |>
      dplyr::summarise("tips.min" = dplyr::n(),
                       .groups = "keep") |>
      dplyr::group_by(PlotID, Date, Hour) |>
      dplyr::summarise("max.tips.min" = max(tips.min),
                       .groups = "keep") |>
      dplyr::mutate("Hour" = factor(Hour,
                                  levels = c("0:00",
                                             paste0(seq(1:23), ":00")))) |>
      tidyr::spread(Hour, max.tips.min, fill = 0) |>
      tidyr::gather(Hour, max.tips.min, c(-PlotID, -Date)) |>
      dplyr::mutate("DateTime" = paste(Date, Hour, sep = " ")) |>
      dplyr::ungroup() |>
      dplyr::select(PlotID, DateTime, max.tips.min)
    # Combine dataframes
    rain_dat1 <- dplyr::full_join(hr_tot, tips_hr,
                                 by = c("PlotID", "DateTime")) |>
      dplyr::full_join(max_tips, by = c("PlotID", "DateTime")) |>
      dplyr::mutate("DateTime" = lubridate::ymd_hm(DateTime)) |>
      dplyr::arrange(PlotID, DateTime)
    # Include empty cells for rainless days
    dd <- tibble::tibble(PlotID = plotid,
                         DateTime = lubridate::ymd_hms(seq(min(dat$DateTime_new,
                                                               na.rm = TRUE),
                                                           max(dat$DateTime_new,
                                                               na.rm = TRUE),
                                                           'hour')))
    rain_dat2 <- dplyr::full_join(rain_dat1, dd,
                                     by = c("PlotID", "DateTime"))
    rain_dat2[is.na(rain_dat2)] <- 0
    rain_dat = rain_dat2 |>
      dplyr::group_by(PlotID, DateTime) |>
      dplyr::summarise("PRCP_mm" = sum(mm.hr, na.rm = TRUE),
                       "Tips" = sum(tips.hr, na.rm = TRUE),
                       "MaxTips_min" = max(max.tips.min, na.rm = TRUE),
                       .groups = "keep") |>
      dplyr::mutate("Element" = "PRCP") |>
      dplyr::select(PlotID, DateTime, Element, PRCP_mm, Tips, MaxTips_min) |>
      dplyr::arrange(PlotID, DateTime)
  }

  return(rain_dat)
}
