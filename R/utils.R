stableColumnLayout <- function(...) {
  dots <- list(...)
  n <- length(dots)
  width <- 12 / n
  class <- sprintf("col-xs-%s col-md-%s", width, width)
  shiny::fluidRow(
    lapply(dots, function(el) {
      shiny::div(class = class, el)
    })
  )
}


search_df <- function() {

  # Container
  c <- c()

  # Function to tell which place an object has in the workspace
  w <- function(x) {
    ls <- ls(envir = .GlobalEnv)
    return(which(ls == x))
  }

  # Which object is a dataframe?
  for (data in ls(envir = .GlobalEnv)) {
    if (any(class(eval(parse(text = data))) == "data.frame")) {
      c[w(data)] <- data
    }
  }

  # Return all non-NA values
  return(c[!is.na(c)])

  # Delete the rest
  rm(w)
  rm(c)
}
