currencies_oanda <- function(cookie = NA){
  require(jsonlite)
  if (is.na(cookie)){
    # Backup list
    txt <- readLines("./data/oanda_currencies.json", warn = FALSE)
    result <- jsonlite::fromJSON(txt)
    result <- result$currency_list
    result <- result[!is.na(result$value),]
    return(result)

  } else {
    # This request requires authorization
    # https://web-services.oanda.com/rates/api/v1/currencies.json
    stop("NotImplementedException")
    require(httr)
  }


}
