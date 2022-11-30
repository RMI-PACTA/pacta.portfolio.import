test_that("correctly identifies the grouping mark", {
  comma <- withr::local_tempfile()
  writeLines('investor_name,portfolio_name,isin,market_value,currency\nx,y,z,"1,200",USD', comma)
  expect_identical(guess_grouping_mark(comma), ",")

  decimal <- withr::local_tempfile()
  writeLines('investor_name,portfolio_name,isin,market_value,currency\nx,y,z,"1.200,34",USD', decimal)
  expect_identical(guess_grouping_mark(decimal), ".")

  decimal_inferred <- withr::local_tempfile()
  writeLines('investor_name,portfolio_name,isin,market_value,currency\nx,y,z,"1200,34",USD', decimal_inferred)
  expect_identical(guess_grouping_mark(decimal_inferred), ".")

  space <- withr::local_tempfile()
  writeLines('investor_name,portfolio_name,isin,market_value,currency\nx,y,z,"1 200",USD', space)
  expect_identical(guess_grouping_mark(space), " ")

  apostrophe <- withr::local_tempfile()
  writeLines('investor_name,portfolio_name,isin,market_value,currency\nx,y,z," 256’178’465 ",USD', apostrophe)
  expect_identical(guess_grouping_mark(apostrophe), "’")

  apostrophe_grouping_comma_decimal <- withr::local_tempfile()
  writeLines('investor_name,portfolio_name,isin,market_value,currency\nx,y,z," 256’178’465,00 ",USD', apostrophe_grouping_comma_decimal)
  expect_identical(guess_grouping_mark(apostrophe_grouping_comma_decimal), "’")

  comma_grouping_period_decimal <- withr::local_tempfile()
  writeLines('investor_name,portfolio_name,isin,market_value,currency\nx,y,z," 1,200.25 ",USD', comma_grouping_period_decimal)
  expect_identical(guess_grouping_mark(comma_grouping_period_decimal), ",")

  row_with_no_value <- withr::local_tempfile()  # resolves issue #14
  writeLines('investor_name,portfolio_name,isin,market_value,currency\nx,y,z,100,USD\n,,,,', row_with_no_value)
  expect_identical(guess_grouping_mark(row_with_no_value), ",")
  })
