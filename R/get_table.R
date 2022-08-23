#' Get Forex Table
#'
#' @param fx
#'
#' @return
#' @export
#'
#' @examples
#' fxPairs <- c("USDCHF", "EURCHF", "CZKCHF",
#'              "GBPCHF", "CADCHF", "JPYCHF")
#'
#' fxPairs <- c("USDCHF", "EURCHF", "CZKCHF",
#'              "GBPCHF", "CADCHF", "JPYCHF",
#'              "RUBCHF", "XAUCHF", "DKKCHF",
#'              "AUDCHF", "FRFCHF")
#'
#' listFX <- lapply(fxPairs, read_oanda)
#' names(listFX) <- fxPairs
#'
#' listFX <- get_list(fxPairs)
#'
#' dataOANDA <- get_table(listFX)
#' write_xl(dataOANDA)
get_table <- function(listFX){
  # Check names
  stopifnot(!is.null(names(listFX)))

  # Data available for 2-dim export
  # For listFX to dataframe
  # See https://daranzolin.github.io/2016-12-10-join-list-dataframes/
  result <- listFX %>%
            # Joining a List of Data Frames
            purrr::reduce(dplyr::full_join, by = "DATE") %>%
            # Rename merged columns
            purrr::set_names(c("DATE", names(listFX))) %>%
            # DATE Order
            dplyr::arrange(DATE) %>%
            # Fill missing dates
            tidyr::complete(DATE = seq(range(DATE)[1],
                                       range(DATE)[2], by = "days")) %>%
            # Fill NA values
            tidyr::fill(names(listFX), .direction = "down")

  # Attributes
  attr(result, "Name") <- "dataOANDA"

  # Return
  tibble::tibble(result)
}
