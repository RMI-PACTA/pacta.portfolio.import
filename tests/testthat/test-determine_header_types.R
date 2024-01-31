test_that("properly identifies header types of standard portfolio CSV", {
  csv_file <- withr::local_tempfile(fileext = ".csv")
  writeLines('investor_name,portfolio_name,isin,currency,market_value\nInvestor Name,Portfolio Name,XS1088274672,1000.34,USD', csv_file)
  expect_identical(determine_header_types(csv_file), rep("character", 5))

  csv_file <- withr::local_tempfile(fileext = ".csv")
  writeLines('isin,currency,market_value\nXS1088274672,1000.34,USD', csv_file)
  expect_identical(determine_header_types(csv_file), rep("character", 3))
})

test_that("properly identifies header types with a numeric header", {
  csv_file <- withr::local_tempfile(fileext = ".csv")
  writeLines('isin,123', csv_file)
  expect_identical(determine_header_types(csv_file), c("character", "double"))
})
