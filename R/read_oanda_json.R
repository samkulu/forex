read_oanda_json <- function(file_or_txt){
  require(jsonlite)

  # Read file content or take input argument as text
  if(file.exists(file_or_txt))
    txt <- readLines(file_or_txt, warn = FALSE)
  else
    txt <- file_or_txt

  # Parse data
  result <- jsonlite::fromJSON(txt)
  if(!is.null(result$data)){
    # Full Pro (Trial) Account View
    data <- result$data$widget$data
  } else if (!is.null(result$widget)) {
    # Free Account
    data <- result$widget$data[[1]]
    # Check emptyness
    if(is.null(dim(data))) return(NULL)
  }

  # VERY IMPORTAN OPTION
  # stringsAsFactors = FALSE
  # Under Windows it is characters per defeault (HERE)
  # Under Unix / Linux it is factors per default
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  nms <- c("MILLISECONDS", "VALUE")
  names(data) <- nms
  data$VALUE <- as.numeric(data$VALUE)
  data$DATE <- ISOdate(1970,1,1) + as.numeric(data$MILLISECONDS)/1000
  data$DATE <- as.Date(data$DATE, format="%Y-%m-%d")
  # Return data
  data
}
