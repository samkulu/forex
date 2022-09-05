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
#' # OANDA
#' unzip_year(2022)
#' sapply(1995:2021, unzip_year)
#'
#' # XE
#' unzip_year(2022, dest = "../forex_xe", src = "./data/xe")
#' sapply(1995:2021, unzip_year, dest = "../forex_xe", src = "./data/xe")
unzip_year <- function(year = format(Sys.Date(),"%Y"),
                       dest = "../forex_oanda",
                       src = "./data/oanda"){
  message(year)

  zipF <- file.path(src, paste0(year,".zip"))
  stopifnot(file.exists(zipF))

  unzip(zipF,exdir=dest)

}
