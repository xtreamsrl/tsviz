test_that("finds a dataframe", {
  x <- data.frame(
    l = c("a", "b", "c"),
    n = c(1, 2, 3)
  )

  assign("x", x, envir = .GlobalEnv)

  res <- get_data_frames_in_env()
  testthat::expect_length(res, 1)

  rm(list = c("x"), envir = .GlobalEnv)
})


test_that("does not crash on no dataframes", {
  res <- get_data_frames_in_env()
  testthat::expect_length(res, 0)
})


test_that("finds a tibble", {
  x <- data.frame(
    l = c("a", "b", "c"),
    n = c(1, 2, 3)
  ) %>% dplyr::as_tibble()

  assign("x", x, envir = .GlobalEnv)

  res <- get_data_frames_in_env()
  testthat::expect_length(res, 1)

  rm(list = c("x"), envir = .GlobalEnv)
})

test_that("finds a time series dataframe", {
  x <- data.frame(
    d = c(lubridate::ymd("2018-01-01"), lubridate::ymd("2018-01-02")),
    n = c(1, 2)
  )

  assign("x", x, envir = .GlobalEnv)

  res <- get_time_series_data_frames_in_env()
  testthat::expect_length(res, 1)

  rm(list = c("x"), envir = .GlobalEnv)
})
