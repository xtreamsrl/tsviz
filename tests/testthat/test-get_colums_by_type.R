test_that("can find date columns", {
  d <- data.frame(
    date = c(lubridate::ymd("2018-01-01"), lubridate::ymd("2018-01-02")),
    number = c(1, 2)
  )

  cols <- get_columns_by_type_match(d, lubridate::is.Date)

  expect_equal(cols, c("date"))
})

test_that("can find numeric columns", {
  d <- data.frame(
    date = c(lubridate::ymd("2018-01-01"), lubridate::ymd("2018-01-02")),
    number = c(1, 2)
  )

  cols <- get_columns_by_type_match(d, is.numeric)

  expect_equal(cols, c("number"))
})
