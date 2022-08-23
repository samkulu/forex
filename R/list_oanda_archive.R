#' List OANDA Archive Info
#'
#' @param fxPair
#' @param dest
#'
#' @return
#' @export
#'
#' @examples
#' listZip <- list_oanda_archive()
#' txt <- readLines(unz(listZip$File[1], listZip$Name[1]))
#' cat(txt)
#' tmp <- readr::read_file(unz(listZip[1]))
#' my_data <- read_delim(file.choose())
#' my_data <- read_lines(file.choose())
list_oanda_archive <- function(dest = "./data/oanda"){

  # Source Path
  path <- file.path(dest)
  fls <- list.files(path, full.names = TRUE)
  # Read zip archive lists
  result <- lapply(fls, function(x) unzip(x, list = TRUE))
  names(result) <- fls

  # Return
  do.call(rbind.data.frame, result) %>%
  # File info
  mutate(File = gsub("(.*).zip.*", "\\1.zip", rownames(.))) %>%
  # Zip File first
  relocate(File)

}
