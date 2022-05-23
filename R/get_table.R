#' Get Forex Table
#'
#' @param fx
#'
#' @return
#' @export
#'
#' @examples
#' fxPairs <- c("USDCHF", "EURCHF", "CZKCHF",
#'              "GBPCHF", "CADCHF", "JPYCHF",
#'              "RUBCHF", "XAUCHF", "DKKCHF",
#'              "AUDCHF", "FRFCHF")
#' listFX <- lapply(fxPairs, read_oanda)
#' names(listFX) <- fxPairs
#' dataOANDA <- get_table(fx)
get_table <- function(listFX){
  # Make Data available for export
  # Just for export and transparency
  result <- listFX[[1]] %>% select(DATE, VALUE)
  names(result)[2] <- names(listFX)[1]

  for(i in 2:length(listFX)){
    result <- merge(result,listFX[[i]] %>% select(DATE, VALUE),
                    by.order = "Datum", all=TRUE)
    names(result)[i+1] <- names(listFX)[i]
  }

  # Fill missing dates
  r <- range(result$DATE)
  dts <- seq(r[1], r[2], by = "days")
  missing <- dts[!dts %in% result$DATE]

  if(!all(dts %in% result$DATE)) {
    # Add missing dts
    # Order by Datum
    # Fill with tidyr "down"
    #   same as last observation carried forward (locf)
    result <- result %>%
              add_row(DATE = missing) %>%
              arrange(DATE) %>%
              tidyr::fill(names(result), .direction = "down" )

  }

  # Return
  tibble(result)
}
