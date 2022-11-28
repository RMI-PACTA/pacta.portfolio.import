test_that("non-standard column names are properly determined", {
  proper_names <- c("investor_name", "portfolio_name", "isin", "market_value", "currency")

  proper_csv <- withr::local_tempfile()
  writeLines("investor_name,portfolio_name,isin,market_value,currency\nx,y,z,1,a", proper_csv)
  expect_setequal(names(determine_headers(proper_csv)), proper_names)

  all_caps <- withr::local_tempfile()
  writeLines("INVESTOR_NAME,PORTFOLIO_NAME,ISIN,MARKET_VALUE,CURRENCY\nx,y,z,1,a", all_caps)
  expect_setequal(names(determine_headers(all_caps)), proper_names)

  no_underscore <- withr::local_tempfile()
  writeLines("investorname,portfolioname,isin,marketvalue,currency\nx,y,z,1,a", no_underscore)
  expect_setequal(names(determine_headers(no_underscore)), proper_names)

  lead_and_lag_whitespace <- withr::local_tempfile()
  writeLines(" investor_name,portfolio_name , isin ,market_value,currency\nx,y,z,1,a", lead_and_lag_whitespace)
  expect_setequal(names(determine_headers(lead_and_lag_whitespace)), proper_names)

  out_of_order <- withr::local_tempfile()
  writeLines("currency,market_value,isin,portfolio_name,investor_name\nx,y,z,1,a", out_of_order)
  expect_setequal(names(determine_headers(out_of_order)), proper_names)

  old_names <- withr::local_tempfile()
  writeLines("Investor.Name,Portfolio.Name,ISIN,Market.Value,Currency\nx,y,z,1,a", old_names)
  expect_setequal(names(determine_headers(old_names)), proper_names)

  old_names_no_dot <- withr::local_tempfile()
  writeLines("InvestorName,PortfolioName,ISIN,MarketValue,Currency\nx,y,z,1,a", old_names_no_dot)
  expect_setequal(names(determine_headers(old_names_no_dot)), proper_names)

  old_names_space <- withr::local_tempfile()
  writeLines("Investor Name,Portfolio Name,ISIN,Market Value,Currency\nx,y,z,1,a", old_names_space)
  expect_setequal(names(determine_headers(old_names_space)), proper_names)
})
