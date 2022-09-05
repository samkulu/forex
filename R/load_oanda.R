#' Load OANDA Data
#'
#' @param fxPairs
#' @param path
#'
#' @return
#' @export
#'
#' @examples
#' fxPairs <- c("USDCHF","EURCHF", "GBPCHF", "CADCHF")
#' load_oanda(fxPairs)
load_oanda <- function(fxPairs, path = "../forex_oanda"){
  # Read each forex pair
  listFX <- get_oanda(fxPairs)

  # Load into global Environment
  dataOANDA <<- get_table(listFX)
}
