#' Write monthly Parquet Files from json
#'
#' Used for monthly updates of the forex database. Every month a single file
#' is generated that is later used as performant database.
#'
#' @param ptrnDate
#' @param dest Source Destination from where the raw files are read.
#' @param dest2 Destination to where the prepared parquet files are written.
#' @param overwrite Explicitly choose to overwrite existing parquet files.
#'
#' @return
#' @export
#'
#' @examples
#' write_parquet("2026-10")
#' write_parquet("2026-09")
#' write_parquet("2026-08")
#'
#' dt_from <- as.Date("1990-01-01")
#' dt_to   <- as.Date("2026-10-01")
#' dts <- seq(dt_from, dt_to, by = "1 months")
#' ptrn <- format(dts, "%Y-%m") %>% sort(decreasing = TRUE)
#' sapply(ptrn, write_parquet)
write_parquet <- function(ptrnDate,
                          dest = "../forex_oanda",
                          dest2 = "../db/fx_%db_table%_parquet",
                          overwrite = FALSE,
                          silent = TRUE
                          ){

  message(ptrnDate)

  # Check pattern
  month <- substr(ptrnDate, 6, 7) %>% as.integer()
  stopifnot(month %in% 1:12)
  month <- sprintf("%02d", month)

  year <- substr(ptrnDate, 1, 4) %>% as.integer()
  stopifnot(year %in% 1990:as.integer(format(Sys.Date(),"%Y")))

  # Forex Table e.g. "oanda"
  db_table =  gsub("(.*forex_)(.*)", "\\2", dest)

  # Final destination / dest to
  dest2 <- file.path(dest2, '%year%-%month%_%db_table%.parquet')
  dest2 <- gsub("%db_table%", db_table, dest2)

  dest2 <- gsub("%year%", year, dest2)
  dest2 <- gsub("%month%", month, dest2)
  dest2 <- gsub("%dest%", dest, dest2)


  # Check destinations from
  # fos <- list.dirs(dirname(dest), recursive = FALSE)
  # fos[grepl("/forex_", fos)]

  stopifnot(dir.exists(dest))
  stopifnot(dir.exists(dirname(dest2)))


  if(!overwrite && file.exists(dest2)){
    # stop("File already exits: ", basename(dest2))
    return(NULL)
  }

  # Find currencies
  dirs <- list.dirs(dest, recursive = FALSE)

  fxPairs <- gsub("(.*/forex.*/)(.*)", "\\2", dirs)
  fxPairs <- fxPairs[nchar(fxPairs) == 6]


  # Read fxPairs
  list_df <- lapply(fxPairs, read_oanda,
                    ptrnDate = ptrnDate,
                    silent = silent, force = TRUE)

  # Remove NULL
  result <- list_df[!lapply(list_df, is.null) %>% unlist()]  %>%
            # Remove DATE Column
            lapply(., function(x) x %>% select(-DATE)) %>%
            do.call(rbind.data.frame, .)

  # Stop with dodgy data
  # if(nrow(result) == 0) browser()
  stopifnot(nrow(result) > 0)

  # Write Parquet File
  duckdbfs::write_dataset(result, dest2)

  # Testing / Debugging on results with ...
  if(FALSE){
    con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")

    m1 <- DBI::dbGetQuery(con, "SELECT * FROM read_parquet('../db/fx_oanda_parquet1/2026-08_oanda.parquet');")
    m2 <- DBI::dbGetQuery(con, "SELECT * FROM read_parquet('../db/fx_oanda_parquet2/2026-08_oanda.parquet');")

    dplyr::anti_join(m1, m2) # <0 Zeilen> (oder row.names mit Länge 0)
    dplyr::anti_join(m2, m1) %>% select(DATELONG) %>% distinct() # 20261101

  }

}
