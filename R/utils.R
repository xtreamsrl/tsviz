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


get_data_frames_in_env <- function(envir = .GlobalEnv) {
  objs <- ls(envir = envir)
  mask <- sapply(objs, function(x) any(is.data.frame(get(x, envir = envir))))
  res <- list()
  if(any(mask)){
    res <- objs[mask]
  }
  res
}
