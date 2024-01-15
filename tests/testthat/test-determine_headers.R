test_that("proper column names are properly determined", {
  proper_names <- c("investor_name", "portfolio_name", "isin", "market_value", "currency")
  proper_csv <- withr::local_tempfile()
  writeLines("investor_name,portfolio_name,isin,market_value,currency\nx,y,z,1,a", proper_csv)
  expect_setequal(names(determine_headers(proper_csv)), proper_names)
})

test_that("all capitalized column names are properly determined", {
  proper_names <- c("investor_name", "portfolio_name", "isin", "market_value", "currency")
  all_caps <- withr::local_tempfile()
  writeLines("INVESTOR_NAME,PORTFOLIO_NAME,ISIN,MARKET_VALUE,CURRENCY\nx,y,z,1,a", all_caps)
  expect_setequal(names(determine_headers(all_caps)), proper_names)
})

test_that("column names with no underscore are properly determined", {
  proper_names <- c("investor_name", "portfolio_name", "isin", "market_value", "currency")
  no_underscore <- withr::local_tempfile()
  writeLines("investorname,portfolioname,isin,marketvalue,currency\nx,y,z,1,a", no_underscore)
  expect_setequal(names(determine_headers(no_underscore)), proper_names)
})

test_that("column names with leading and lagging whitespace are properly determined", {
  proper_names <- c("investor_name", "portfolio_name", "isin", "market_value", "currency")
  lead_and_lag_whitespace <- withr::local_tempfile()
  writeLines(" investor_name,portfolio_name , isin , market_value,currency \nx,y,z,1,a", lead_and_lag_whitespace)
  expect_setequal(names(determine_headers(lead_and_lag_whitespace)), proper_names)
})

test_that("column names with leading and lagging whitespace are properly determined (double-padded)", {
  proper_names <- c("investor_name", "portfolio_name", "isin", "market_value", "currency")
  lead_and_lag_whitespace <- withr::local_tempfile()
  writeLines("  investor_name,portfolio_name  ,  isin  ,  market_value,currency \nx,y,z,1,a", lead_and_lag_whitespace)
  expect_setequal(names(determine_headers(lead_and_lag_whitespace)), proper_names)
})

test_that("column names with leading and lagging whitespace are properly determined (tab-padded)", {
  proper_names <- c("investor_name", "portfolio_name", "isin", "market_value", "currency")
  lead_and_lag_whitespace <- withr::local_tempfile()
  writeLines("\tinvestor_name,portfolio_name\t,\tisin\t,\tmarket_value,currency \nx,y,z,1,a", lead_and_lag_whitespace)
  expect_setequal(names(determine_headers(lead_and_lag_whitespace)), proper_names)
})

test_that("column names with leading and lagging whitespace are properly determined (double-tab-padded)", {
  proper_names <- c("investor_name", "portfolio_name", "isin", "market_value", "currency")
  lead_and_lag_whitespace <- withr::local_tempfile()
  writeLines("\t\tinvestor_name,portfolio_name\t\t,\t\tisin\t\t,\t\tmarket_value,currency \nx,y,z,1,a", lead_and_lag_whitespace)
  expect_setequal(names(determine_headers(lead_and_lag_whitespace)), proper_names)
})

test_that("column names with leading and lagging whitespace are properly determined (space-and-tab-padded)", {
  proper_names <- c("investor_name", "portfolio_name", "isin", "market_value", "currency")
  lead_and_lag_whitespace <- withr::local_tempfile()
  writeLines(" \tinvestor_name,portfolio_name\t ,\t isin\t\t, \tmarket_value,currency \nx,y,z,1,a", lead_and_lag_whitespace)
  expect_setequal(names(determine_headers(lead_and_lag_whitespace)), proper_names)
})

test_that("column names orderd improperly are properly determined", {
  proper_names <- c("investor_name", "portfolio_name", "isin", "market_value", "currency")
  out_of_order <- withr::local_tempfile()
  writeLines("currency,market_value,isin,portfolio_name,investor_name\nx,y,z,1,a", out_of_order)
  expect_setequal(names(determine_headers(out_of_order)), proper_names)
})

test_that("old style column names are properly determined", {
  proper_names <- c("investor_name", "portfolio_name", "isin", "market_value", "currency")
  old_names <- withr::local_tempfile()
  writeLines("Investor.Name,Portfolio.Name,ISIN,Market.Value,Currency\nx,y,z,1,a", old_names)
  expect_setequal(names(determine_headers(old_names)), proper_names)
})

test_that("old style column names with no dot separator are properly determined", {
  proper_names <- c("investor_name", "portfolio_name", "isin", "market_value", "currency")
  old_names_no_dot <- withr::local_tempfile()
  writeLines("InvestorName,PortfolioName,ISIN,MarketValue,Currency\nx,y,z,1,a", old_names_no_dot)
  expect_setequal(names(determine_headers(old_names_no_dot)), proper_names)
})

test_that("old style column names separated with a space are properly determined", {
  proper_names <- c("investor_name", "portfolio_name", "isin", "market_value", "currency")
  old_names_space <- withr::local_tempfile()
  writeLines("Investor Name,Portfolio Name,ISIN,Market Value,Currency\nx,y,z,1,a", old_names_space)
  expect_setequal(names(determine_headers(old_names_space)), proper_names)
})
