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
#' fxPairs <- c("USDCHF","EURCHF")
#' load_oanda(fxPairs)
#' load_oanda(fxPairs, method = "db")
#' load_oanda(fxPairs, method = "json")
load_oanda <- function(fxPairs, method = "db"){
  # Load Cache
  if(exists("dataOANDA")){
    if (all(fxPairs %in% names(dataOANDA)[-1]))  return(dataOANDA)
  }

  # Read each forex pair
  listFX <- get_oanda(fxPairs, method = method)


  # Load into global Environment
  dataOANDA <<- get_table(listFX)
}
