#' Is Date vector monthly complete
#'
#' @param dts
#'
#' @return
#' @export
#'
#' @examples
#' dt <- Sys.Date()
#' dt <- dt-10
#' dts <- dt + 0:30
#' dts_complete(dts)
dts_complete <- function(dts){
  r <- range(dts)
  # Start with day 01.
  if (!format(r[1], "%d") == "01")
    return(FALSE)
  # Check current month
  if (difftime(Sys.Date(), r[2], unit = "days") <= 1)
    return(TRUE)
  # Check number of days in month
  x <- difftime(r[2],r[1], units="days")
  m <- format(median(dts), "%m")
  if (m %in% c("04", "06", "09", "11")) {
    return(x > 28)
  } else if (m == "02" ) {
    return(x > 27)
  } else {
    return(x > 29)
  }
}


#' Is Date Vector of Month
#'
#' @param dts
#' @param m
#'
#' @return
#' @export
#'
#' @examples
#' dts_month(dts, "05")
dts_month <- function(dts, m){
  tmp <- c(tail(dts, 1), median(dts))
  all(format(tmp, "%Y-%m") == m)
}


#' Add 1 Month
#'
#' @param dt
#'
#' @return
#' @export
#'
#' @examples
#' add_month(as.Date("2004-01-01"))
#' add_month(as.Date("2004-02-01"))
#' add_month(as.Date("2004-07-01"))
add_month <- function(dt){
  seq(dt, by = "month", length = 2) [2]
}
