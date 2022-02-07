#' Summarize precipitation data
#'
#' This function summarizes precipitation data from Onset Hobo event loggers
#'     used in Onset tipping bucket precipitation gauges. It uses a list
#'     produced from \code{\link{import_wxdat}} and returns a data frame of
#'     hourly precipitation totals, number of tips per hour, and maximum tips
#'     per minute.
#'
#' @param my_wxdat An \code{import_wxdat} object.
#'
#' @details
#' This function summarizes precipitation data from Onset Hobo event  loggers
#'     used in Onset tipping bucket precipitation gauges.
#'
#'     This function srtips the first 5 minutes and last 10 minutes of data to
#'     account for field proceedures when downloading the event logger. It is
#'     common practice to trigger an event before downloading and after
#'     launching the logger. To remove these false events there is a routine to
#'     stip the first 5 minutes of the data if the "Launch Time" and "First
#'     Sample Time" are the same. There is no way to determine if the tipping
#'     bucket is triggered before downloading the data, so the last 10 minutes
#'     of every file is stripped before processing.
#'
#' @return
#' This function returns a five column \code{\link[tibble:tibble]{tibble}}.
#'
#' \describe{
#'     \item RID: The unique record ID. The record ID
#'         \code{\link[base:paste]{paste}}'s DateTime and PlotID to create a
#'         unique record ID.
#'     \item{\strong{DateTime}}{The date and hour of the data. Hours are the from
#'         0 min 00 sec to 59 min 59 sec. For example 2009-04-15 00:00:00 is
#'         from 00:00:00 to 00:59:59 on April 15, 2009.}
#'     \item{\strong{PlotID}}{The unique ID number for the long-term monitoring
#'         plot.}
#'     \item{\strong{PRCP_mm}}{The recorded total precipitation for that hour in
#'         millimeters.}
#'     \item{\strong{Tips}}{The number of tips recorded for that hour.}
#'     \item{\strong{MaxTips_min}}{The maximum tips per minute recorded. This is
#'         inteded to calculate intensity of precipitation event.}
#' }
#'
#' @seealso \code{\link{import_wxdat}} to import a csv produced by Onset
#'     HOBOware into R.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library("rainDanceR")
#' file.list <- list.files(path = "./inst/raw_data", pattern = ".csv",
#'                         full.names = TRUE, recursive = FALSE)
#' my_wxdat <- import_wxdat(file.list[1])
#' raindance(my_wxdat)
#' }
#'
raindance <- function(my_wxdat){
  # my_wxdat = import_wxdat(file_list[1])

  if(unique(my_wxdat$data$Element) != "PRCP"){
    stop("Data are not PRCP. Check data.")
  }

  launch = dplyr::filter(my_wxdat$details, Details == "Launch Time")
  first_sample = dplyr::filter(my_wxdat$details, Details == "First Sample Time")

  #-- Summarize precipitation data
  dat <- if(launch$Value == first_sample$Value){
    dplyr::filter(my_wxdat$data, DateTime > min(DateTime, na.rm = T) + (5*60))
    } else {
      my_wxdat$data
      }

  dat <- dat %>%
    dplyr::filter(DateTime < max(DateTime, na.rm = T) - (10*60)) %>%
    dplyr::mutate(Date = lubridate::date(DateTime),
                  Hour = paste(lubridate::hour(DateTime) + 1, "00", sep = ":"),
                  Hour = ifelse(Hour == "24:00", "0:00", Hour),
                  Min = lubridate::minute(DateTime)) %>%
    dplyr::ungroup()

  if(nrow(dat) == 0){
    stop("No valid data. Check file.")
  } else{
    # Calculate hourly precipitation totals
    hr_tot <- dat %>%
      dplyr::group_by(Date, Hour) %>%
      dplyr::summarise(mm.hr = dplyr::n() * 0.254) %>%
      dplyr::mutate(Hour = factor(Hour,
                                  levels = c("0:00",
                                             paste0(seq(1:23), ":00")))) %>%
      tidyr::spread(Hour, mm.hr, fill = 0) %>%
      tidyr::gather(Hour, mm.hr, 2:ncol(.)) %>%
      dplyr::mutate(DateTime = paste(Date, Hour, sep = " ")) %>%
      dplyr::ungroup() %>%
      dplyr::select(DateTime, mm.hr)
    # Calculate number of tips per hour
    tips_hr <- dat %>%
      dplyr::group_by(Date, Hour, Min) %>%
      dplyr::summarise(tips.min = dplyr::n()) %>%
      dplyr::group_by(Date, Hour) %>%
      dplyr::summarise(tips.hr = dplyr::n()) %>%
      dplyr::mutate(Hour = factor(Hour,
                                  levels = c("0:00",
                                             paste0(seq(1:23), ":00")))) %>%
      tidyr::spread(Hour, tips.hr, fill = 0) %>%
      tidyr::gather(Hour, tips.hr, 2:ncol(.)) %>%
      dplyr::mutate(DateTime = paste(Date, Hour, sep = " ")) %>%
      dplyr::ungroup() %>%
      dplyr::select(DateTime, tips.hr)
    # Calculate max tips per minute
    max_tips <- dat %>%
      dplyr::group_by(Date, Hour, Min) %>%
      dplyr::summarise(tips.min = dplyr::n()) %>%
      dplyr::group_by(Date, Hour) %>%
      dplyr::summarise(max.tips.min = max(tips.min)) %>%
      dplyr::mutate(Hour = factor(Hour,
                                  levels = c("0:00",
                                             paste0(seq(1:23), ":00")))) %>%
      tidyr::spread(Hour, max.tips.min, fill = 0) %>%
      tidyr::gather(Hour, max.tips.min, 2:ncol(.)) %>%
      dplyr::mutate(DateTime = paste(Date, Hour, sep = " ")) %>%
      dplyr::ungroup() %>%
      dplyr::select(DateTime, max.tips.min)
    # Combine dataframes
    rain_dat <- suppressMessages(dplyr::full_join(hr_tot, tips_hr)) %>%
      suppressMessages(dplyr::full_join(max_tips)) %>%
      dplyr::mutate(DateTime = lubridate::ymd_hm(DateTime)) %>%
      dplyr::arrange(DateTime)
    # Include empty cells for rainless days
    dd <- tibble::tibble(DateTime = lubridate::ymd_hms(seq(min(dat$DateTime),
                                                           max(dat$DateTime),
                                                           'hour')),
                 mm.hr = 0,
                 tips.hr = 0,
                 max.tips.min = 0)
    rain_dat <- dplyr::bind_rows(rain_dat, dd) %>%
      dplyr::group_by(DateTime) %>%
      dplyr::summarise(PlotID = my_wxdat$file_info$plotid,
                       PRCP_mm = sum(mm.hr),
                       Tips = sum(tips.hr),
                       MaxTips_min = max(max.tips.min)) %>%
      dplyr::mutate(RID = paste0(as.numeric(DateTime), PlotID, sep = ".")) %>%
      dplyr::select(RID, PlotID, DateTime, PRCP_mm, Tips, MaxTips_min) %>%
      dplyr::arrange(DateTime)
  }

  return(rain_dat)
}
