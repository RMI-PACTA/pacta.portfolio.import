#' Determine the headers of a portfolio CSV to import
#'
#' This function will return a named vector giving the names of the headers in
#' the portfolio CSV that match the proper header names expected by
#' pacta.portfolio.analysis. The name of each element will be the proper column
#' name it matches to.
#'
#' @param filepath A character vector containing an absolute or relative path to
#'   a single portfolio CSV
#'
#' @return A named character vector containing the names of the headers in the
#'   portfolio CSV that match the proper header names expected by
#'   pacta.portfolio.analysis. The name of each element will be the proper
#'   column name it matches to.
#'
#' @export

determine_headers <- function(filepath) {
  if (!is_text_file(filepath)) {
    return(NA_character_)
  }

  encoding <- guess_file_encoding(filepath)
  delimiter <- guess_delimiter(filepath)

  if (any(is.na(c(encoding, delimiter)))) {
    return(NA_character_)
  }

  file_has_header <-
    has_header(
      filepath = filepath,
      encoding = encoding,
      delimiter = delimiter
    )

  first_line <-
    read_first_line(
      filepath = filepath,
      encoding = encoding,
      delimiter = delimiter
    )

  header_types <-
    determine_header_types(
      filepath = filepath,
      encoding = encoding,
      delimiter = delimiter
    )

  if (!file_has_header) {
    num_of_cols <- ncol(first_line)
    if (num_of_cols == 3) {
      col_names <- c("isin", "market_value", "currency")
      names(col_names) <- col_names
      not_portfolio_csv <- FALSE
    } else if (num_of_cols == 5) {
      col_names <- c("investor_name", "portfolio_name", "isin", "market_value", "currency")
      names(col_names) <- col_names
      not_portfolio_csv <- FALSE
    } else {
      not_portfolio_csv <- TRUE
    }
  } else {
    headers <- unlist(first_line, use.names = FALSE)
    num_of_cols <- length(headers)

    if (num_of_cols >= 3) {
      isin_col <- grep(pattern = "^[[:space:]]*isin[[:space:]]*$", x = headers, ignore.case = TRUE, value = TRUE)
      market_value_col <- grep(pattern = "^[[:space:]]*market[._ ]{0,1}value[[:space:]]*$", x = headers, ignore.case = TRUE, value = TRUE)
      currency_col <- grep(pattern = "^[[:space:]]*currency[[:space:]]*$", x = headers, ignore.case = TRUE, value = TRUE)

      if (num_of_cols > 3) {
        investor_name_col <- grep(pattern = "investor", x = headers, ignore.case = TRUE, value = TRUE)
        portfolio_name_col <- grep(pattern = "portfolio", x = headers, ignore.case = TRUE, value = TRUE)
      }

      if (length(isin_col) < 1 || length(market_value_col) < 1 || length(currency_col) < 1) {
        not_portfolio_csv <- TRUE
      } else {
        col_names <- c(isin_col[[1]], market_value_col[[1]], currency_col[[1]])

        if (num_of_cols > 3) {
          if (length(portfolio_name_col) > 0) {
            col_names <- c(portfolio_name_col[[1]], col_names)
          }
          if (length(investor_name_col) > 0) {
            col_names <- c(investor_name_col[[1]], col_names)
          }
        }

        not_portfolio_csv <- FALSE
      }
    } else {
      not_portfolio_csv <- TRUE
    }
  }

  if (not_portfolio_csv) {
    return(NA_character_)
  }
  if (!file_has_header) {
    return(col_names)
  }

  headers[is.na(headers)] <- ""

  names(headers) <- NA
  names(headers)[match(isin_col, headers)] <- "isin"
  names(headers)[match(market_value_col, headers)] <- "market_value"
  names(headers)[match(currency_col, headers)] <- "currency"
  if (num_of_cols > 3) {
    names(headers)[match(investor_name_col, headers)] <- "investor_name"
    names(headers)[match(portfolio_name_col, headers)] <- "portfolio_name"
    names(headers) <- vctrs::vec_as_names(names(headers), repair = "unique", quiet = TRUE)
  }
  attr(headers, which = "header_types") <- header_types
  attr(headers, which = "num_of_cols") <- num_of_cols

  headers
}
