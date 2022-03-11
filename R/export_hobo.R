#' Import Onset Hobo data into Access Database
#'
#' This function processes data from Onset HOBOware used from 2020 to present
#'     and exports them to a Microsoft Access Database.
#'
#' @param my_file A character string of the complete file path of your *.csv
#'     file.
#' @param my_db A connected database from \code{\link{RODBC}}.
#' @param import_table A character string of the name of the import log table.
#' @param raw_data_table A character string of the name of the raw data table.
#' @param prcp_data_table A character string of the name of the processed
#'     precipitation data table.
#' @param temp_rh_data_table A character string of the name of the processed
#'     temperature and relative humidity data table.
#' @param details_table A character string of the name of the logger details
#'     table.
#' @param verbose Logical. Show messages showing progress. Default is TRUE. If
#'     FALSE, messages are suppressed.
#' @param view Logical. Prints data to console before writing them to the
#'     database. Default is TRUE. If FALSE, data are not printed and there is no
#'     prompt before writing data to the database.
#'
#' @details This function uses \code{\link{import_hobo}} to read Hobo data in into R and
#'    then uses \code{\link{process_hobo}} to summarise the data. The processed
#'    data are then exported to a connected database.
#'
#' @return Data is written to database tables. Objects are not returned.
#'
#' @export
#'
#' @seealso \code{\link{import_hobo}}, \code{\link{raindance}},
#'     \code{\link{sundance}}, \code{\link{process_hobo}},
#'     \code{\link{export_hobo_2008}}, \code{\link[RODBC]{sqlSave}},
#'     \code{\link[RODBC]{odbcConnectAccess2007}}
#'
#' @examples
#' \dontrun{
#' library("dataprocessR")
#'
#' # Connect to DB
#' my_db <- RODBC::odbcConnectAccess2007("C:/path/to/database.accdb")
#'
#' # List files
#' my_dir <- "C:/path/to/data"
#' file_list <- list.files(my_dir, pattern = ".csv", full.names = TRUE,
#'                         recursive = FALSE)
#' # Select file
#' my_file <- file_list[12]
#'
#' # Process file and save to database
#' export_hobo(my_file = my_file, my_db = my_db,
#'             import_table = "tbl_import_log",
#'             raw_data_table = "tbl_raw_data",
#'             prcp_data_table = "tbl_prcp_data",
#'             temp_rh_data_table = "tbl_temp_rh_data",
#'             details_table = "tbl_logger_details")
#' }
export_hobo <- function(my_file, my_db, import_table, raw_data_table,
                              prcp_data_table, temp_rh_data_table,
                              details_table, verbose = TRUE, view = TRUE){

  # my_file = file_list[2]

  # Check if file has been processed
  if(import_table %in% RODBC::sqlTables(my_db)$TABLE_NAME){
     if(basename(my_file) %in% RODBC::sqlFetch(my_db, import_table)$FileName){
       stop("File has already been processed.")
       }
  }

  if(verbose == TRUE) message(glue::glue("Processing {basename(my_file)}"))
  #-- Process hobo file --
  dat <- import_hobo(my_file) |> process_hobo()
  if(view == TRUE){
    print(dat)
    readline(prompt = "Press [enter] to export data to database.")
  }

  #-- Import Record --
  # Prep data
  file_info <- dat$file_info |>
    dplyr::mutate("ImportDate" = as.character(lubridate::today()))

  #-- Raw Data --
  # Prep data
  data_raw <- dat$data_raw |>
    dplyr::mutate("DateTime" = as.character(DateTime))
  # Export to DB
  if(verbose == TRUE) message("- Writing raw data to database")
  export_table(my_db, my_df = data_raw, my_table = raw_data_table)

  #-- Data --
  # Prep data
  if(verbose == TRUE) message("- Writing processed data to database")
  if(TRUE %in% (file_info$Element == "PRCP")){
    if(nrow(file_info) == 1){
    prcp_dat <- dat$data |>
      dplyr::mutate("DateTime" = as.character(DateTime,
                                              format = "%Y-%m-%d %H:%M:%S"))
    # Export to DB
    export_table(my_db, my_df = prcp_dat, my_table = prcp_data_table)

    } else({
      # PRCP Data
      prcp_dat <- dat$data$prcp_dat |>
        dplyr::mutate("DateTime" = as.character(DateTime,
                                                format = "%Y-%m-%d %H:%M:%S"))
      # Export to DB
      export_table(my_db, my_df = prcp_dat, my_table = prcp_data_table)

      # TEMP Data
        temp_dat <- dat$data$temp_dat |>
          dplyr::mutate("Date" = as.character(Date,
                                              format = "%Y-%m-%d %H:%M:%S"))
        # Export to DB
        export_table(my_db, my_df = temp_dat, my_table = temp_rh_data_table)

        })
    } else({
        tr_dat <- dat$data |>
          dplyr::mutate("Date" = as.character(Date,
                                              format = "%Y-%m-%d %H:%M:%S"))
        # Export to DB
        export_table(my_db, my_df = tr_dat, my_table = temp_rh_data_table)
        })

  #-- Details --
  # Export to DB
  if(verbose == TRUE) message("- Writing logger details to database")
  export_table(my_db, my_df = dat$details, my_table = details_table)

  # Export Import Record to DB
  if(verbose == TRUE) message("- Writing import log to database")
  export_table(my_db, my_df = file_info, my_table = import_table)
}

export_table <- function(my_db, my_df, my_table){
  # my_df = dat$file_info; my_table = "tblWxImportLog2"
  if(my_table %in% RODBC::sqlTables(my_db)$TABLE_NAME){
    RODBC::sqlSave(my_db, my_df, tablename = my_table,
                   append = TRUE, rownames = FALSE, colnames = FALSE,
                   safer = TRUE, addPK = TRUE, fast = TRUE)
  } else({
    RODBC::sqlSave(my_db, my_df, tablename = my_table,
                   append = FALSE, rownames = FALSE, colnames = FALSE,
                   safer = TRUE, addPK = TRUE, fast = TRUE)
  })
}
