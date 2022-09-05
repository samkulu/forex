#' Read Oanda Zip Archive
#'
#' Unfortunately not as performant as reading from unzipped files.
#'
#' @param fxPair
#'
#' @return
#' @export
#'
#' @examples
read_oanda_zips <- function(fxPair = "CHFUSD"){
  listZip <- list_oanda_archive()

  fls <-   listZip %>% filter(grepl(fxPair, Name))

  if(nrow(fls) == 0){
    fxPair2 <- paste0(substr(fxPair, 4,6),
                      substr(fxPair, 1,3))
    fls <- listZip %>% filter(grepl(fxPair2, Name))

    if(nrow(fls) > 0){
      # Flag for Inversion (only suitable for mid quotes!)
    doInvert <- TRUE
    } else {

      # Check Identity where no conversion is needed
      if(substr(fxPair,1,3) == substr(fxPair,4,6)){
        r <- listZip %>% pull(Name) %>% substr(., 8,14) %>%
             paste0(.,"-1") %>% as.Date() %>% range()
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

  read_zip <- function(x){
    fi <- as.list(x)
    txt <- readLines(unz(fi$File, fi$Name))

    # # suppressWarnings not working
    # tryCatch({
    #   txt <- readLines(unz(fi$File, fi$Name))
    # }, error = function(x){
    #   print(paste('error:', e))
    # })

    read_oanda_json(txt)
  }

  result <- apply(fls, 1, read_zip) %>%
            do.call(rbind.data.frame, .) %>%
            dplyr::arrange(DATE) %>% dplyr::distinct() %>%
            dplyr::select(DATE, VALUE) %>%
            remove_rownames()

  # Invert
  if (doInvert) result$VALUE <- 1/result$VALUE

  # Return
  tibble::tibble(result)

}
