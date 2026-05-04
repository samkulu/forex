is_json <- function(text) {
  tryCatch({
    jsonlite::fromJSON(text)
    TRUE
  }, error = function(e) FALSE)
}
