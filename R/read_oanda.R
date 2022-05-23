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
read_oanda <- function(fxPair = "USDCHF", fromDate = NA, dest = "../forexOANDA"){
  message(fxPair)

  # Source Path
  path <- file.path(dest, fxPair)
  fls <- list.files(path, fxPair, recursive = TRUE, full.names = TRUE)

  # Check Number of files
  if (length(fls) == 0){
    fxPair2 <- paste0(substr(fxPair, 4,6),
                      substr(fxPair, 1,3))
    path2 <- file.path(dest, fxPair2)
    fls <- list.files(path2, fxPair2, recursive = TRUE, full.names = TRUE)
    # Check Number of files
    stopifnot(length(fls) > 0)
    # Flag
    doInvert <- TRUE
  } else {
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
            arrange(DATE) %>% distinct()

  # Invert
  if (doInvert) result$VALUE <- 1/result$VALUE

  # Return
  tibble(result)

}
