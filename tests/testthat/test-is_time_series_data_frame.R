test_that("can find numeric columns", {
  d <- data.frame(
    date = c(lubridate::ymd("2018-01-01"), lubridate::ymd("2018-01-02")),
    number = c(1, 2)
  )

  expect_true(is_time_series_data_frame(d))
})
