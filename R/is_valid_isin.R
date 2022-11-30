#' Validate a vector of ISINs
#'
#' This function validates that a vector of ISINs are valid codes that conform
#' to the ISO 6166 specification with `TRUE` or `FALSE`. It checks the basic
#' structure (2 alpha characters, 9 alpha-numeric characters, 1 check digit)
#' and also validates the check digit using the Luhn algorithm.
#'
#' @param isins A character vector
#'
#' @return A logical vector the same length as `isins`.
#'
#' @export

is_valid_isin <- function(isins) {
  if (is.data.frame(isins) && identical(length(isins), 1L)) {
    isins <- isins[[1L]]
  }

  isins[is.na(isins)] <- "X" # set NAs to something sure to return FALSE
  isins_factored <- as.factor(isins)
  isins_lvls <- levels(isins_factored)

  is_luhn <- function(x) {
    digits <- suppressWarnings(as.numeric(rev(unlist(strsplit(x, "")))))
    odd <- seq_along(digits) %% 2 == 1
    s1 <- sum(digits[odd])
    s2 <- digits[!odd] * 2
    s2 <- sum(s2 %% 10 + s2 %/% 10)
    sum(s1, s2) %% 10 == 0
  }

  isins_lvls <- toupper(isins_lvls)
  isins_lvls <- gsub(pattern = "[[:blank:]]", replacement = "", isins_lvls)
  valid_struct <- grepl("^[[:upper:]]{2}[[:alnum:]]{9}[[:digit:]]$", isins_lvls)

  valid_luhn <-
    vapply(
      X = isins_lvls,
      FUN = function(x) {
        x <-
          stringi::stri_replace_all_fixed(
            str = x,
            pattern = LETTERS,
            replacement = seq_along(LETTERS) + 9L,
            vectorize_all = FALSE
          )
        out <-
          vapply(
            X = x,
            FUN = is_luhn,
            FUN.VALUE = logical(1L),
            USE.NAMES = FALSE
          )
        out[is.na(out)] <- FALSE
        out
      },
      FUN.VALUE = logical(1),
      USE.NAMES = FALSE
    )

  valid_struct[as.numeric(isins_factored)] & valid_luhn[as.numeric(isins_factored)]
}
