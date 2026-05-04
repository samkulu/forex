#' Download BAZG / ESTV Rates
#'
#' @param year
#' @param dest
#'
#' @return
#' @export
#'
#' @examples
#' download_bazg()
#' download_bazg(2025)
#' sapply(2024:1990, download_bazg)
download_bazg <- function(year = NULL, dest = "../forex_bazg"){

  # Year
  if(is.null(year)) year <- format(Sys.Date(), "%Y") %>% as.integer()

  dir_bazg <- file.path(dest, year)
  if(!dir.exists(dir_bazg)) dir.create(dir_bazg, recursive = TRUE)

  # URL
  url <- "https://www.backend-rates.bazg.admin.ch/daily?d=%YYYYMMDD%&locale=de"

  # Dates Vector
  fromDate <- as.Date(paste0(year, "-01-01"))
  toDate <- as.Date(paste0(year, "-12-31"))
  if(toDate > Sys.Date()) toDate <- Sys.Date()

  dts <- seq.Date(from = fromDate, to = toDate, by = "day")

  # Download jsons
  for(dtLong in  format(dts, "%Y%m%d")){

    cat(dtLong, "")
    # Url for date
    u <- gsub("%YYYYMMDD%", dtLong, url)
    filename <- file.path(dir_bazg, paste0(dtLong, "_bazg_admin_rates.json"))

    # GET download w/o cookie (u = url)
    tryCatch({
      # txt <- readLines(u, warn = FALSE)
      txt <- read_get(u)
    },
    error=function(cond) {
      message(cond)
      # txt <- readLines(u, warn = FALSE)
      txt <- read_get(u)
      return(NULL)
    })

    # Check json format
    if(!is_json(txt)){
      warning(" no json")
      return(NULL)
    }

    # Write file
    if(file.exists(filename)){
      message("exsist")
    } else {
      writeLines(txt, filename)
      message("done.")
    }


  }


}
