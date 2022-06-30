#' loadFunctions
#' @description : helper function to load all functions. Useful when making edits as you can run in console

loadFunctions <- function(){
  functions <- list.files("R/", full.names = TRUE, recursive = TRUE)
  for (i in seq_along(functions)) {
    print(i)
    source(file = functions[i], echo = FALSE)
  }
}
