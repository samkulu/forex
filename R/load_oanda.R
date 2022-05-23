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
load_oanda <- function(fxPairs, path = "../forexOanda"){
  # Read each forex pair
  fx <- lapply(fxPairs, read_oanda)
  names(fx) <- fxPairs

  # Load into global Environment
  dataOANDA <<- get_table(fx)
}
