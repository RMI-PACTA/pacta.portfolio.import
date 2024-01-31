test_that("properly reads first line of standard portfolio CSV", {
  portfolio <-
    data.frame(
      isin = "",
      currency = "USD",
      market_value = 1000.34
    )

  expected_result <-
    tibble::tibble(
      "X1" = "isin",
      "X2" = "currency",
      "X3" = "market_value"
    )

  csv_file <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(portfolio, file = csv_file)
  expect_identical(read_first_line(csv_file), expected_result)
})

test_that("properly reads first line of CSV with only a header", {
  expected_result <-
    tibble::tibble(
      "X1" = "isin",
      "X2" = "currency",
      "X3" = "market_value"
    )

  csv_file <- withr::local_tempfile(fileext = ".csv")
  writeLines('isin,currency,market_value', csv_file)
  expect_identical(read_first_line(csv_file), expected_result)
})

test_that("properly reads first line of CSV with no header", {
  expected_result <-
    tibble::tibble(
      "X1" = "XS1088274672",
      "X2" = 1000.34,
      "X3" = "USD"
    )

  csv_file <- withr::local_tempfile(fileext = ".csv")
  writeLines("XS1088274672,1000.34,USD", csv_file)
  expect_identical(read_first_line(csv_file), expected_result)
})
