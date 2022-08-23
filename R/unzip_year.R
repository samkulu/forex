#' Title
#'
#' @param year
#' @param dest
#' @param src
#'
#' @return
#' @export
#'
#' @examples
#' unzip_year(2020)
#' sapply(1990:2019, unzip_year)
unzip_year <- function(year = format(Sys.Date(),"%Y"),
                       dest = "../forex_oanda",
                       src = "./data/oanda"){
  message(year)

  zipF <- file.path(src, paste0(year,".zip"))
  stopifnot(file.exists(zipF))

  unzip(zipF,exdir=dest)

}
