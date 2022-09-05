#' Read Data from OANDA JSON Files
#'
#' @param fxPair
#' @param fromDate
#' @param dest
#'
#' @return
#' @export
#'
#' @examples
#' read_oanda("CHFUSD")
#' read_oanda("CHFEUR")
#' read_oanda("EURCHF")
read_oanda <- function(fxPair = "USDCHF", fromDate = NA, dest = "../forex_oanda"){
  message(fxPair)
  stopifnot(nchar(fxPair) == 6)

  # Source Path
  path <- file.path(dest, fxPair)

  # Source Files
  fls <- list.files(path, fxPair, recursive = TRUE, full.names = TRUE)

  # Check Number of files
  if (length(fls) == 0){
    fxPair2 <- paste0(substr(fxPair, 4,6),
                      substr(fxPair, 1,3))
    path2 <- file.path(dest, fxPair2)
    fls <- list.files(path2, fxPair2, recursive = TRUE, full.names = TRUE)
    # Check Number of files
    if(length(fls) > 0){

      # Flag for Inversion (only suitable for mid quotes!)
      doInvert <- TRUE

    } else {

      # Check Identity where no conversion is needed
      if(substr(fxPair,1,3) == substr(fxPair,4,6)){
        r <- list.files(list.files(gsub(fxPair, "", path), full.names = TRUE)[1]) %>%
             substr(., 1,7) %>% paste0(.,"-01")%>% range() %>% as.Date()
        dts <- seq(from=r[1], to=r[2], by="days")
        return(tibble::tibble(DATE = dts ,VALUE = as.double(1)))
      } else {
        stop("Missing Files!")
      }

    }

  } else {
    # No Inversion
    doInvert <- FALSE
  }

  # Filter
  if(!is.na(fromDate)){
    dt <- as.integer(sub("-","",substr(fromDate,1,7)))
    dts <- as.integer(gsub("-","",substr(basename(fls),1,7)))
    fls <- fls[which(dts >= dt)]
  }

  # Read json text files
  json <- lapply(fls, read_oanda_json)

  # Filter Nulls
  isNull <- unlist(lapply(json, function(x) is.null(x)))

  # Return result
  result <- do.call(rbind, json[!isNull]) %>%
            dplyr::arrange(DATE) %>% dplyr::distinct() %>%
            dplyr::select(DATE, VALUE)

  # Invert
  if (doInvert) result$VALUE <- 1/result$VALUE

  # Return
  tibble::tibble(result)

}
