test_that("get_data_frames_env finds a dataframe", {

  x <- data.frame(
    l = c("a", "b", "c"),
    n = c(1, 2, 3)
  )

  assign("x", x, envir = .GlobalEnv)

  res <- get_data_frames_in_env()
  testthat::expect_length(res, 1)

  rm(list=c('x'), envir = .GlobalEnv)
})


test_that("get_data_frames_in_env does not crash on no dataframes", {
  res <- get_data_frames_in_env()
  testthat::expect_length(res, 0)
})


test_that("get_data_frames_in_env finds a tibble", {

  x <- data.frame(
    l = c("a", "b", "c"),
    n = c(1, 2, 3)
  ) %>% dplyr::as_tibble()

  assign("x", x, envir = .GlobalEnv)

  res <- get_data_frames_in_env()
  testthat::expect_length(res, 1)

  rm(list=c('x'), envir = .GlobalEnv)
})

