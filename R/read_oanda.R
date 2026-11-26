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
#'
#' read_oanda("CHFEUR", fromDate = "2025-01-01")
#' read_oanda("CHFEUR", ptrnDate = "2026-01")
#' read_oanda("CHFEUR", ptrnDate = "2026-[0-9]{2}")
#' read_oanda("CHFEUR", ptrnDate = "202.-[0-9]{2}")
#' read_oanda("CHFCHF", ptrnDate = "2026-[0-9]{2}")
#'
#' read_oanda("CHFADF", ptrnDate = "1990-01")  # Empy -> Error
#' read_oanda("CHFADF", ptrnDate = "2021-[0-9]{2}")  # Empy -> Error
read_oanda <- function(fxPair = "USDCHF",
                       dest = "../forex_oanda",
                       fromDate = NA,
                       ptrnDate = NA,
                       silent = FALSE,
                       force = FALSE
                       ){

  # Check Forex Pair
  if(!silent) message(fxPair)
  stopifnot(nchar(fxPair) == 6)

  # Source Path
  path <- file.path(dest, fxPair)

  # Source Files
  fls <- list.files(path, fxPair, recursive = TRUE, full.names = TRUE)


  # Filter from Date
  if(!is.na(fromDate)){
    dt <- as.integer(sub("-","",substr(fromDate,1,7)))
    dts <- as.integer(gsub("-","",substr(basename(fls),1,7)))
    fls <- fls[which(dts >= dt)]
  }

  # Filter for year & month Pattern
  if(!is.na(ptrnDate)){
    fls <- fls[grepl(ptrnDate, fls)]
  }


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
        # Define Range
        r <- list.files(list.files(gsub(fxPair, "", path), full.names = TRUE)[1]) %>%
             substr(., 1,7) %>% paste0(.,"-01")%>% range() %>% as.Date()
        dts <- seq(from=r[1], to=r[2], by="days")
        # Return 1 double
        return(tibble::tibble(
                          FXPAIR = fxPair,
                          DATELONG = format(DATE, "%Y%m%d") %>% as.integer(),
                          DATE = dts,
                          VALUE = as.double(1)
                          ))
      } else {
        if(force)
          return(NULL)              # see write_parquet
        else
          stop("Missing Files!")    # default
      }

    }

  } else {
    # No Inversion
    doInvert <- FALSE
  }

  # Read json text files
  json <- lapply(fls, read_oanda_json)


  # Filter Nulls
  isNull <- unlist(lapply(json, function(x) is.null(x)))

  # Workaround 2026-11-25
  if(all(isNull)) return(NULL)

  # Return result
  result <- do.call(rbind, json[!isNull]) %>%
            dplyr::arrange(DATE) %>% dplyr::distinct() %>%
            dplyr::select(DATE, VALUE) %>%
            dplyr::mutate(
              FXPAIR = fxPair,
              DATELONG = format(DATE, "%Y%m%d") %>% as.integer()
              ) %>%
            relocate(FXPAIR, DATELONG)

  # Invert
  if (doInvert) result$VALUE <- 1/result$VALUE

  # Return
  tibble::tibble(result)

}
