#' Read OANDA DuckDB
#'
#' @param fxPair
#'
#' @returns
#' @export
#'
#' @examples
#' db_read("CHFCHF")
#' db_read("CHFUSD")
#' db_read("USDCHF")
#' db_read("CHFEUR")
#' db_read("USDXXX")
db_read <- function(fxPair = "USDCHF",
                    db_table = "oanda",
                    use_parquet = TRUE){

  # Check FX-Pair
  if (!is.character(fxPair) || length(fxPair) != 1 || !grepl("^[A-Z]{6}$", fxPair)) {
    stop("fxPair muss ein Währungspaar wie 'EURUSD' sein.", call. = FALSE)
  }



  # Check Identity where no conversion is needed
  if(substr(fxPair,1,3) == substr(fxPair,4,6)){
    # Datetime
    dts <- seq(from = as.Date("1990-01-01"), to = Sys.Date(), by ="days")
    # DateLong integer
    dtlong <- as.integer(format(dts, "%Y%m%d"))

    # tibble object
    result <- tibble::tibble(FXPAIR = fxPair,
                          DATELONG = dtlong,
                          DATE = dts,
                          VALUE = as.double(1))

    # result <- result %>% select(DATELONG, DATE, VALUE)
    return(result)
  }

  # Database location
  db_file <- Sys.getenv("FOREX_PATH")

  if (db_file == "")
    stop("Environment variable FOREX_PATH is not set.")


  # Connection
  # See https://duckdb.org/docs/1.5/clients/r

  if(use_parquet){

    # Check DuckDB parquet files
    db_path <- dirname(db_file)
    fls <- list.files(db_path, pattern = ".parquet$", recursive = TRUE)
    stopifnot(length(fls) > 0)

    # Open temporary connection
    con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")

    # Destination for Parquet files
    parquet_path <- file.path(
                    db_path,
                    paste0(unique(dirname(fls)), '/*.parquet')
                    )
    # Remove other databases tests
    parquet_path <- parquet_path[grepl(db_table, parquet_path)]

    # Remove tests
    parquet_path <- parquet_path[!grepl("test|db_export", parquet_path,
                                        ignore.case = TRUE)]

    stopifnot(length(parquet_path) == 1)

    # Create View on Parquet
    query <- "CREATE TEMP VIEW oanda AS SELECT * FROM read_parquet('%path%');"
    query <- gsub("%path%", parquet_path, query)

    # Build memory database
    DBI::dbExecute(con,query)

  } else {

    # Check DuckDB file (archive)
    if (!file.exists(db_file))
      stop("Database file not found: ", db_file)

    # Open db connection
    con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = db_file, read_only = TRUE)

  }

  # Close db connection before exit
  # Registered for automatic execution on exit
  on.exit(
    try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE),
    add = TRUE
  )

  # Select currencies
  query_crncy <- gsub("%forex%", db_table,
                      "SELECT DISTINCT FXPAIR FROM %forex% ;")

  currencies <- DBI::dbGetQuery(con, query_crncy)[[1]]

  # Select data from duckdb
  query <- gsub("%forex%", db_table,
               "SELECT
                 FXPAIR,
                 DATELONG,
                 STRPTIME(CAST(DATELONG AS VARCHAR), '%Y%m%d')::DATE AS DATE,
                 VALUE
               FROM %forex% ")


  if(fxPair %in% currencies){
    doInvert <- FALSE
    query <- paste0(query, " WHERE FXPAIR = '", fxPair, "' ;")
  } else {
    # Virtual fxPair for MID quotes!
    fxPair2 <- paste0(substr(fxPair, 4,6),
                      substr(fxPair, 1,3))

    # stopifnot(fxPair2 %in% currencies)
    if (!(fxPair2 %in% currencies))
      stop("Currency pair not found: ", fxPair)

    # Existing fxPair2 needs invertion of values
    doInvert <- TRUE
    query <- paste0(query, " WHERE FXPAIR = '", fxPair2,"' ;")
  }


  # Get Query
  result <- DBI::dbGetQuery(con, query) %>%
            dplyr::arrange(DATELONG) %>%
            tibble::as_tibble()


  # Invert
  if (doInvert) result$VALUE <- 1/result$VALUE

  # Return
  result # %>% select(DATELONG, DATE, VALUE)

}
