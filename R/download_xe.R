#' Download Currency Tables from www.XE.com
#'
#' @param dts
#' @param fx
#' @param exDir
#'
#' @return
#' @export
#'
#' @examples
#' dt <- Sys.Date()
#' dts <- seq.Date(dt-30, dt, by = "days")
#' tmp <- download_xe(dts, fx="CHF")
#' tmp <- download_xe(dts, fx="USD")
#' tmp <- download_xe(dts, fx="EUR")
download_xe <- function(dts = NA, fx ="CHF", exDir = "../forexXE"){
  # URL
  url <- "https://www.xe.com/currencytables/?from=%fx%&date=%yyyy-MM-dd%"

  # Date
  if (all(is.na(dts))){
    dts <- Sys.Date()
  } else if(attributes(dts)$class == "Date"){
    if(length(dts) == 1){
      # Process URL ...
    } else if(length(dts) == 2){
      # Range
      dts <- seq.Date(dts[1], dts[2], by = "days")
      # Filter omitting files
      fls <- list.files(exDir, recursive = T)
      fls <- fls[grep(fx,fls)]
      nms <- basename(fls)
      nms <- substr(nms, 1, 8)
      tmp <- as.Date(nms, format ="%Y%m%d")
      idx <- !dts %in% tmp
      dts <- dts[idx]
      dts <- sort(dts, decreasing = T)
      # Process Range
      result <- sapply(dts, download_xe, fx = fx, exDir = exDir)
      return(result)
    } else {
      # Filter date vector (omit already processed)
      fls <- list.files(exDir, recursive = T)
      fls <- fls[grep(fx,fls)]
      nms <- basename(fls)
      nms <- substr(nms, 1, 8)
      tmp <- as.Date(nms, format ="%Y%m%d")
      idx <- !dts %in% tmp
      dts <- dts[idx]
      dts <- sort(dts, decreasing = T)
      # Process date vector
      result <- sapply(dts, download_xe, fx = fx, exDir = exDir)
      return(result)
    }
  }

  # Single date
  dt <- dts[1]
  # Dates available from
  if (dt < as.Date("1995-11-16")) return(NULL)

  # Destination
  name <- format(dt,"%Y%m%d_XE.html")
  dest <- exDir
  dest <- file.path(dest, format(dt,"%Y"))
  dest <- file.path(dest,fx)
  if (!dir.exists(dest)) dir.create(dest, recursive = T)
  filename <- file.path(dest, name)

  if (file.exists(filename)){
    cat(filename, "exists  \n")
  }  else  {
    # Update URL
    u <- gsub("%fx%", fx, url)
    u <- gsub("%yyyy-MM-dd%", dt, u)
    message(u)
    # Download
    page <- readLines(u, warn = FALSE)
    writeLines(page, filename)
  }
}
