#' Export from DB monthly Parquet Files
#'
#' Function used to generate and test quickly the parquet files as database.
#' After successful testing the function write_parquet() was created in
#' order to have a consistent process for updating the forex db.
#'
#'
#' @param month
#'
#' @returns
#' @export
#'
#' @examples
#' db_export(year = 2026, month = 8)
#' sapply(1:9, function(x) db_export(year = 2026, month = x))
#' sapply(1:12, function(x) db_export(year = 2025, month = x))
#' sapply(2020:2024, function(y){sapply(1:12, function(x) db_export(year = y, month = x))} )
#' sapply(1990:2019, function(y){sapply(1:12, function(x) db_export(year = y, month = x))} )
db_export <- function(year, month,
                      db_table = "oanda",
                      dest = "../db/fx_%db_table%_parquet1"){

  .Deprecated("Use write_parquet() to generate Parquet Files directly from json.")

  # Check Destination
  stopfinot(dir.exists(dirname(dest)))

  # Check Month
  stopifnot(month %in% 1:12)
  month <- sprintf("%02d", month)

  # DATE does not exist
  # dbGetQuery(con, "DESCRIBE oanda")

  # Build Query
  query <- "COPY (
    SELECT *
    FROM oanda
    WHERE substr(cast(DATELONG AS VARCHAR), 1, 4)  = '%year%'
      AND substr(cast(DATELONG AS VARCHAR), 5, 2)  = '%month%'
  )
  TO '%dest%/%year%-%month%_%db_table%.parquet' (FORMAT PARQUET)
  "

  query <- gsub("%year%", year, query)
  query <- gsub("%month%", month, query)
  query <- gsub("%dest%", dest, query)
  query <- gsub("%db_table%", db_table, query)

  # Open db connection
  con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = db_file, read_only = TRUE)

  # Close db connection
  # Registered for automatic execution on exit
  on.exit(
    # shutdown db engine and embedded processes as well
    try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE),
    add = TRUE
  )

  # Export Data to file
  dbExecute(con, query  )

}





