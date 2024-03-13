portfolio_min <-
  tibble::tibble(
    investor_name = "ABC Investor",
    portfolio_name = "ABC Portfolio 1",
    isin = "XS1088274672",
    market_value = 1000.34,
    currency = "USD"
  )

test_that("reads a proper portfolio CSV correctly", {
  csv_file <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(portfolio_min, file = csv_file)

  result <- read_portfolio_csv(csv_file)
  expect_equal(unlist(result), unlist(portfolio_min))
})

test_that("reads a portfolio CSV with old column names", {
  csv_file <- withr::local_tempfile(fileext = ".csv")

  portfolio_alt <- portfolio_min
  names(portfolio_alt) <-
    c(
      "Investor.Name",
      "Portfolio.Name",
      "ISIN",
      "Market.Value",
      "Currency"
    )

  readr::write_csv(portfolio_alt, file = csv_file)

  result <- read_portfolio_csv(csv_file)
  expect_equal(unlist(result), unlist(portfolio_min))
})

test_that("reads a portfolio CSV with upper case column names", {
  csv_file <- withr::local_tempfile(fileext = ".csv")

  portfolio_alt <- portfolio_min
  names(portfolio_alt) <- toupper(names(portfolio_alt))

  readr::write_csv(portfolio_alt, file = csv_file)

  result <- read_portfolio_csv(csv_file)
  expect_equal(unlist(result), unlist(portfolio_min))
})

test_that("reads a portfolio CSV with a space as the separator in column names", {
  csv_file <- withr::local_tempfile(fileext = ".csv")

  portfolio_alt <- portfolio_min
  names(portfolio_alt) <- gsub("_", " ", names(portfolio_alt))

  readr::write_csv(portfolio_alt, file = csv_file)

  result <- read_portfolio_csv(csv_file)
  expect_equal(unlist(result), unlist(portfolio_min))
})

test_that("reads a portfolio CSV with a dot as the separator in column names", {
  csv_file <- withr::local_tempfile(fileext = ".csv")

  portfolio_alt <- portfolio_min
  names(portfolio_alt) <- gsub("_", ".", names(portfolio_alt))

  readr::write_csv(portfolio_alt, file = csv_file)

  result <- read_portfolio_csv(csv_file)
  expect_equal(unlist(result), unlist(portfolio_min))
})

test_that("reads a portfolio CSV with no separator in column names", {
  csv_file <- withr::local_tempfile(fileext = ".csv")

  portfolio_alt <- portfolio_min
  names(portfolio_alt) <- gsub("_", "", names(portfolio_alt))

  readr::write_csv(portfolio_alt, file = csv_file)

  result <- read_portfolio_csv(csv_file)
  expect_equal(unlist(result), unlist(portfolio_min))
})

test_that("reads a portfolio CSV with leading and trailing white space in column names", {
  csv_file <- withr::local_tempfile(fileext = ".csv")

  portfolio_alt <- portfolio_min
  names(portfolio_alt) <- paste0(" ", names(portfolio_alt), " ")

  readr::write_csv(portfolio_alt, file = csv_file)

  result <- read_portfolio_csv(csv_file)
  expect_equal(unlist(result), unlist(portfolio_min))
})

test_that("reads a portfolio CSV with extra columns correctly", {
  csv_file <- withr::local_tempfile(fileext = ".csv")

  portfolio_alt <- portfolio_min
  portfolio_alt$X <- "XXX"

  readr::write_csv(portfolio_alt, file = csv_file)

  result <- read_portfolio_csv(csv_file)
  expect_equal(unlist(result), unlist(portfolio_min))
})

test_that("reads a portfolio CSV with a different column order correctly", {
  csv_file <- withr::local_tempfile(fileext = ".csv")

  portfolio_alt <- portfolio_min
  portfolio_alt <- portfolio_alt[, 5:1]

  readr::write_csv(portfolio_alt, file = csv_file)

  result <- read_portfolio_csv(csv_file)
  expect_equal(unlist(result), unlist(portfolio_min))
})

test_that("reads a portfolio CSV in a different file encoding correctly", {
  csv_file <- withr::local_tempfile(fileext = ".csv")

  portfolio_min <-
    tibble::tibble(
      investor_name = "\u00dcber Investor",
      portfolio_name = "\u00dcber Portfolio 1",
      isin = "XS1088274672",
      market_value = 1,
      currency = "USD"
    )

  write.csv(portfolio_min, file = csv_file, fileEncoding = "ISO-8859-1")

  result <- read_portfolio_csv(csv_file)
  expect_equal(guess_file_encoding(csv_file), "ISO-8859-1")
  expect_equal(unlist(result), unlist(portfolio_min))
})

test_that("reads a portfolio CSV with a grouping marker correctly", {
  csv_file <- withr::local_tempfile(fileext = ".csv")

  portfolio_alt <- portfolio_min
  portfolio_alt$market_value <- "1,000.34"

  readr::write_csv(portfolio_alt, file = csv_file)

  result <- read_portfolio_csv(csv_file)
  expect_equal(unlist(result), unlist(portfolio_min))
})

test_that("reads a portfolio CSV with an '.' grouping marker correctly", {
  csv_file <- withr::local_tempfile(fileext = ".csv")

  portfolio_alt <- portfolio_min
  portfolio_alt$market_value <- "1.000,34"

  readr::write_csv(portfolio_alt, file = csv_file)

  result <- read_portfolio_csv(csv_file)
  expect_equal(unlist(result), unlist(portfolio_min))
})

test_that("reads a portfolio CSV with an ' ' grouping marker correctly", {
  csv_file <- withr::local_tempfile(fileext = ".csv")

  portfolio_alt <- portfolio_min
  portfolio_alt$market_value <- "1 000,34"

  readr::write_csv(portfolio_alt, file = csv_file)

  result <- read_portfolio_csv(csv_file)
  expect_equal(unlist(result), unlist(portfolio_min))
})

test_that("reads a portfolio CSV with numeric names as characters", {
  csv_file <- withr::local_tempfile(fileext = ".csv")

  portfolio_alt <- portfolio_min
  portfolio_alt$investor_name <- 1
  portfolio_alt$portfolio_name <- 2

  readr::write_csv(portfolio_alt, file = csv_file)

  result <- read_portfolio_csv(csv_file)
  expect_type(result$investor_name, "character")
  expect_type(result$portfolio_name, "character")
})

test_that("deals with a portfolio CSV with only a header appropriately", {
  csv_file <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(portfolio_min[0, ], file = csv_file)
  result <- read_portfolio_csv(csv_file)
  expect_equal(result, NA)
})

test_that("deals with a portfolio CSV with no header and only one row of data", {
  csv_file <- withr::local_tempfile(fileext = ".csv")
  writeLines(paste(portfolio_min, collapse = ","), csv_file)
  result <- read_portfolio_csv(csv_file)
  expect_equal(unlist(result), unlist(portfolio_min))
})

test_that("reads and combines multiple, proper portfolio CSVs correctly", {
  csv_file <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(portfolio_min, file = csv_file)

  result <- read_portfolio_csv(c(csv_file, csv_file))
  expect_true(names(result)[[1L]] == "filepath")
  expect_equal(basename(result[[1L]]), basename(c(csv_file, csv_file)))
  expect_true(nrow(result) == 2L)
  expect_equal(result[ , -1L], rbind(portfolio_min, portfolio_min), ignore_attr = TRUE)
})

test_that("reads and combines multiple portfolio CSVs correctly when one is not proper", {
  csv_file <- withr::local_tempfile(fileext = ".csv")
  csv_file2 <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(portfolio_min, file = csv_file)
  readr::write_csv(portfolio_min[0, ], file = csv_file2)

  result <- read_portfolio_csv(c(csv_file, csv_file2))
  expect_true(names(result)[[1L]] == "filepath")
  expect_equal(basename(result[[1L]]), basename(csv_file))
  expect_true(nrow(result) == 1L)
  expect_equal(result[ , -1L], portfolio_min, ignore_attr = TRUE)
})

test_that("reads and combines multiple, proper portfolio CSVs correctly into a list", {
  csv_file <- withr::local_tempfile(fileext = ".csv")
  csv_file <- gsub("//", "/", csv_file)
  readr::write_csv(portfolio_min, file = csv_file)

  result <- read_portfolio_csv(c(csv_file, csv_file), combine = FALSE)
  expect_type(result, "list")
  expect_equal(length(result), 2L)
  expect_named(result)
  expect_equal(basename(names(result)), basename(c(csv_file, csv_file)))
  expect_equal(result[[1L]], portfolio_min, ignore_attr = TRUE)
  expect_equal(result[[2L]], portfolio_min, ignore_attr = TRUE)
})

test_that("reads and combines multiple portfolio CSVs correctly into a list when one is not proper", {
  csv_file <- withr::local_tempfile(fileext = ".csv")
  csv_file2 <- withr::local_tempfile(fileext = ".csv")
  csv_file <- gsub("//", "/", csv_file)
  csv_file2 <- gsub("//", "/", csv_file2)
  readr::write_csv(portfolio_min, file = csv_file)
  readr::write_csv(portfolio_min[0, ], file = csv_file2)

  result <- read_portfolio_csv(c(csv_file, csv_file2), combine = FALSE)
  expect_type(result, "list")
  expect_equal(length(result), 2L)
  expect_named(result)
  expect_equal(basename(names(result)), basename(c(csv_file, csv_file2)))
  expect_equal(result[[1L]], portfolio_min, ignore_attr = TRUE)
  expect_equal(result[[2L]], NA)
})

test_that("passes readr problems attribute through correctly", {
  csv_file <- withr::local_tempfile(fileext = ".csv")

  portfolio_alt <- portfolio_min
  portfolio_alt$market_value <- "X"

  readr::write_csv(portfolio_alt, file = csv_file)

  expect_warning({ result <- read_portfolio_csv(csv_file) })
  expect_true(!is.null(attr(result, "problems")))
  expect_gt(nrow(readr::problems(result)), 0L)
})
