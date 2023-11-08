#' Zip Archive of a Year
#'
#' @param year
#' @param src
#' @param dest
#'
#' @return
#' @export
#'
#' @examples
#' zip_year()
#' zip_year(2022)
#' sapply(1979:2020, zip_year)
#' rm(cacheFLS)
#' zip_year(2022, src = "../forex_xe", dest = "./data/xe")
#' zip_year(src = "../forex_xe", dest = "./data/xe")
zip_year <- function(year = format(Sys.Date(),"%Y"),
                     src = "../forex_oanda",
                     dest = "./data/oanda"){
  message(year)

  # Source files
  if(is.na(src) && exists("user")) src <- user$DESTINATION
  if(is.na(src)) stop("Destination missing!")
  stopifnot(dir.exists(src))

  # Make sure that year pattern can be found
  src <- gsub("\\\\", "/", src)

  # Zipping needs to go into source path
  # Otherwise you will have the path information in the zip archive
  oldPath <- getwd()

  # Destination
  if(!dir.exists(dest)) dir.create(dest, recursive = TRUE)
  if(substr(dest,1,2) == "./")
    dest <- file.path(oldPath, substr(dest, 3, nchar(dest)))
  if(substr(dest,1,3) == "../")
    dest <- file.path(dirname(oldPath), substr(dest, 4, nchar(dest)))

  # Define zip
  zipName <- file.path(dest, paste0(year,".zip"))

  # Make shure you get back to original dir
  on.exit(setwd(oldPath))

  # Goto source location
  setwd(src)

  # Check File cache OR Get Files to archive
  if(exists("cacheFLS") && file.exists(file.path(getwd(), cacheFLS[1])))
    fls <- cacheFLS
  else
    cacheFLS <<- fls <- list.files(recursive = TRUE)

  fls <- fls[grep(paste0("\\b",year,"\\b"), fls)]

  # Process files
  if (length(fls) > 0){
    # Archive into zip
    if(file.exists(zipName)){
      # Read archive
      z <- unzip(zipName, list = TRUE)
      # Find missing files
      tmp <- fls[which(!fls %in% z$Name)]
      # Just add the missing files
      if(length(tmp) > 0)
        zip(zipfile = zipName, tmp)
      else
        cat("nothing to add")

    } else {
      # Zip all files
      zip(zipfile = zipName, fls)
    }
  }

}
