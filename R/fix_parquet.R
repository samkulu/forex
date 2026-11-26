#' Manually Fix Parquet file by Renaming Column
#'
#' @param db_file
#' @param read_only
#'
#' @return
#' @export
#'
#' @examples
#' db_file <- "../db/fx_oanda_parquet/1990-01_oanda_copy.parquet"
#' fix_parquet(db_file)
#'
#' fls <- list.files("../db/fx_oanda_parquet", full.names = TRUE)
#' fls <- list.files("../db/fx_oanda_parquet_db_export", full.names = TRUE)
#' sapply(fls, fix_parquet)
fix_parquet <- function(db_file, check_format = TRUE){

  message(basename(db_file))
  stopifnot(file.exists(db_file))

  # Open db connection
  con <- duckdb::dbConnect(duckdb::duckdb(), read_only = FALSE)

  # New destination
  old <- basename(dirname(db_file))
  new <- paste0(old, "_fixed")

  db_file2 <- gsub(old, new, db_file)
  stopifnot(dir.exists(dirname(db_file2)))



  # Query
  cols_checked <- FALSE

  if(check_format){
    q <- "DESCRIBE FROM read_parquet('%db_file%');"
    q <- gsub("%db_file%", db_file, q)
    tmp <- dbGetQuery(con, q) %>%  as_tibble() %>% pull(column_name)
    cols_checked <- identical(tmp, c("FXPAIR", "DATELONG", "VALUE"))
  }

  # IF Format good do nothing
  if(cols_checked){
    return(NULL)

  } else {
    # Check for correct columns FXPAIR, DATELONG, VALUE ...
    query <- "COPY (
                SELECT PAIR AS FXPAIR, DATELONG, QUOTE AS VALUE
                FROM read_parquet('%db_file%')
            )
            TO '%db_file2%'
            (FORMAT PARQUET)
            "

    query <- gsub("%db_file%", db_file, query)
    query <- gsub("%db_file2%", db_file2, query)


    # Write fix
    dbExecute(con, query)
  }


}
