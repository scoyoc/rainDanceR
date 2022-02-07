#' Summarize temperature data
#'
#' This function summarizes temperature data from Onset Hobo event loggers. It
#'     uses a list produced from \code{\link{import_wxdat}} and returns a data
#'     frame of temperature data.
#'
#' @param my_wxdat An \code{import_wxdat} object.
#'
#' @details
#' This function summarizes temperature data from Onset Hobo event  loggers.
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
#' This function returns a nine (9) column \code{\link[tibble:tibble]{tibble}}.
#'
#' \describe{
#'     \item RID: The unique record ID. The record ID
#'         \code{\link[base:paste]{paste}}'s Date and PlotID to  create a unique
#'         record ID.
#'     \item{\strong{PlotID}}{The unique ID number for the long-term monitoring
#'         plot.}
#'     \item{\strong{Date}}{The date the data were recorded.}
#'     \item{\strong{TEMP_mean}}{The mean daily temperature.}
#'     \item{\strong{TMIN}}{The minimum daily temperature.}
#'     \item{\strong{TMAX}}{The maximum daily temperature.}
#'     \item{\strong{n}}{The number of records for that day.}
#'     \item{\strong{TMIN_time}}{The time of daily minimum temperature.}
#'     \item{\strong{TEMP_mean}}{The time of daily maximum temperature.}
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
#' my_wxdat <- import_wxdat(file.list[2])
#' raindance(my_wxdat)
#' }
sundance <- function(my_wxdat){
  #-- QA check
  if(unique(my_wxdat$data$Element) != "TEMP"){
    stop("Data are not PRCP. Check data.")
  }
  #-- Summarize temperature data
  dat <- raw.dat %>%
    mutate(Date = date(DateTime)) %>%
    group_by(PlotID, Date) %>%
    summarize(TEMP_mean = mean(Value, na.rm = T),
              TMIN = min(Value, na.rm = T),
              TMAX = max(Value, na.rm = T),
              n = n())
  tmin.time <- raw.dat %>%
    mutate(Date = date(DateTime)) %>%
    left_join(dat, by = c("PlotID", "Date")) %>%
    filter(Value == TMIN) %>%
    group_by(PlotID, Date) %>%
    summarise(TMIN_time = strftime(min(DateTime), format="%H:%M:%S"))
  tmax.time <- raw.dat %>%
    mutate(Date = date(DateTime)) %>%
    left_join(dat, by = c("PlotID", "Date")) %>%
    filter(Value == TMAX) %>%
    group_by(PlotID, Date) %>%
    summarise(TMAX_time = strftime(min(DateTime), format="%H:%M:%S"))
  dat <- left_join(dat, tmin.time) %>%
    left_join(tmax.time) %>%
    arrange(PlotID, Date) %>%
    mutate(RID = paste0(PlotID, as.numeric(Date)),
           Date = as.character(Date)) %>%
    select(RID, PlotID, Date, TEMP_mean, TMIN, TMAX, n, TMIN_time, TMAX_time)
  return(dat)
}
