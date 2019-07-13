get_data_frames_in_env <- function(envir = .GlobalEnv) {
  objs <- ls(envir = envir)
  mask <- sapply(objs, function(x) any(is.data.frame(get(x, envir = envir))))
  res <- list()
  if (any(mask)) {
    res <- objs[mask]
  }
  res
}

get_time_series_data_frames_in_env <- function(envir = .GlobalEnv) {
  dfs <- get_data_frames_in_env(envir = envir)
  mask <- sapply(dfs, function(x) is_time_series_data_frame(get(x, envir = envir)))
  res <- list()
  if (any(mask)) {
    res <- dfs[mask]
  }
  res
}

is_time_series_data_frame <- function(data) {
  length(get_columns_by_type_match(data, lubridate::is.Date)) > 0 &
    length(get_columns_by_type_match(data, is.numeric)) > 0
}

get_columns_by_type_match <- function(data, match_fun) {
  data %>%
    dplyr::select_if(match_fun) %>%
    colnames()
}
