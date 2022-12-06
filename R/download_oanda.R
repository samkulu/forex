#' Download OANDA
#'
#' @param fxPair
#' @param dest
#' @param price
#' @param startDate
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
#' download_oanda()
download_oanda <- function(fxPair = NA, dest = "../forex_oanda",
                          price = "mid",
                          startDate = NA, monthComplete = TRUE,
                          verbose = FALSE){

  # Time range / horizon
  thisMonth <- format(Sys.Date(),"%Y-%m")
  endDate <- Sys.Date()

  # Download pattern
  pattern <- "https://www.oanda.com/fx-for-business/historical-rates/api/data/update/?&source=OANDA&adjustment=0&base_currency=%baseCurrency%&start_date=%sDate%&end_date=%eDate%&period=daily&price=%price%&view=graph&quote_currency_0=%quoteCurrency%"

  if (is.na(startDate))
    # Please update regularly
    # Do not wait longer than 180 days!
    startDate <- endDate - 180
  else
    # Free horizon is 180 days back!
    stopifnot(startDate >= (endDate - 180))

  if(monthComplete){
    # Date format
    Y <- as.integer(format(startDate,"%Y"))
    m <- as.integer(format(startDate,"%m"))
    dt <- as.Date(format(ISOdate(Y, m, 1),"%Y-%m-%d"))
    if (dt == startDate)
      startDate <- dt
    else
      startDate <- add_month(dt)
  }

  # Price
  pattern <- gsub("%price%", price, pattern)

  # Months to download
  months <- seq(startDate, endDate, "months")
  n <- length(months)
  months <- c(months, endDate)

  # Currencies
  if(is.na(fxPair)){
    # Run all formerly used fxPairs
    fxPairs <- list.files(dest)
  } else {
    # Run only designated fxPair or fxPairs
    fxPairs <- fxPair
  }

  # Filter for correct currency names e.g. "CHFUSD", "CHFEUR"
  fxPairs <- fxPairs[nchar(fxPairs) == 6]

  # Missing Forex info
  if(length(fxPairs) == 0){
    message("Missing fxPairs!")
    cat("Provide fxPairs or Create Folders in destination (dest) \n")
  }

  # Process
  for(fxPair in fxPairs){
    message(fxPair)
    # URL
    url <- pattern
    baseCurrency <- substr(fxPair,1,3)
    quoteCurrency <- substr(fxPair,4,6)
    url <- gsub("%baseCurrency%", baseCurrency, url)
    url <- gsub("%quoteCurrency%", quoteCurrency, url)

    # Process Monthly
    for(i in 1:n){
      sDate <- months[i]
      eDate <- months[i+1]
      runMonth <- format(sDate,"%Y-%m")
      if (verbose) cat(runMonth)

      # Url
      u <- gsub("%sDate%", sDate, url)
      u <- gsub("%eDate%", eDate, u)

      # Naming Convention
      name <- paste(runMonth, "_", fxPair,".json", sep="")
      filename <- file.path(dest, fxPair, name)
      cat(filename, "\n")

      # Process
      check <- file.exists(filename) &&
                dts_complete(read_oanda_json(filename)$DATE)


      if(check){
        # Json File exists
        if (verbose) cat(" exists ")

      } else {

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

        # Check by Looking into data
        tmp <- read_oanda_json(txt)

        if (is.null(tmp)){
          if (verbose) cat(" empty data")
        } else {
          # Check for correct content
          check <- dts_month(tmp$DATE, runMonth)

          # Check for month complete
          if (monthComplete) check <- check && dts_complete(tmp$DATE)

          if(check){
            # Check Destination
            if(!dir.exists(dirname(filename)))
              dir.create(dirname(filename), recursive = TRUE)
            # Export
            writeLines(txt, filename)
          } else {
            if (verbose) cat(" incomplete data")
          }
        }

      }
      if (verbose) cat("\n")
    }

  }



}
