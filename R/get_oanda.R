#' Get OANDA Quotes as list
#'
#' @param fxPairs
#'
#' @return
#' @export
#'
#' @examples
get_oanda <- function(fxPairs){
  # Read oanda quotes
  listFX <- lapply(fxPairs, read_oanda)
  names(listFX) <- fxPairs

  # Class
  class(listFX) <- "listFX"

  # Return
  listFX
}
