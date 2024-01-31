test_that("identifies a portfolio CSV with only a header but no data", {
  csv_file <- withr::local_tempfile(fileext = ".csv")
  writeLines('investor_name,portfolio_name,isin,currency,market_value', csv_file)
  expect_true(has_header(csv_file))
})

test_that("identifies a portfolio CSV with only data and no header", {
  csv_file <- withr::local_tempfile(fileext = ".csv")
  writeLines('Investor Name,Portfolio Name,XS1088274672,1000.34,USD', csv_file)
  expect_false(has_header(csv_file))
})

test_that("identifies a portfolio CSV with a header and data", {
  csv_file <- withr::local_tempfile(fileext = ".csv")
  writeLines('investor_name,portfolio_name,isin,currency,market_value\nInvestor Name,Portfolio Name,XS1088274672,1000.34,USD', csv_file)
  expect_true(has_header(csv_file))
})
