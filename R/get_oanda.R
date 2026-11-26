#' Get OANDA Quotes as list
#'
#' @param fxPairs
#'
#' @return
#' @export
#'
#' @examples
#' get_oanda("USDCHF")
#' get_oanda("CHFUSD")
#' get_oanda(c("CHFUSD", "CHFEUR"))
get_oanda <- function(fxPairs, method = "db"){

  # Check
  stopifnot(method %in% c("fs", "db"))

  # Read oanda quotes
  if (method == "fs"){
    # Read from file system
    listFX <- lapply(fxPairs, read_oanda)
  } else {
    # Read from DuckDB database (default db_table oanda)
    listFX <- lapply(fxPairs, db_read)
  }

  names(listFX) <- fxPairs

  # Class
  class(listFX) <- "listFX"

  # Return
  listFX
}
