#' Guess the numerical marks in the `market_value` column of a portfolio CSV
#'
#' This function will guess the numerical marks in the `market_value` column of
#' a portfolio CSV. It will return a single character string containing the
#' guessed decimal or thousands grouping mark, depending on the value passed to
#' `type`, for each portfolio CSV passed in `filepaths`.
#'
#' @param filepaths A character vector
#' @param type A single character string, either "decimal" or "grouping"
#'
#' @return A character vector the same length as `filepaths` containing a single
#'   character string defining the guessed numerical mark for each portfolio CSV
#'
#' @export

guess_numerical_mark <- function(filepaths, type = "decimal") {
  filepaths <- simplify_if_one_col_df(filepaths)
  stopifnot("`filepaths` must be a character vector" = typeof(filepaths) == "character")
  filepaths <- canonize_path(filepaths)

  vapply(
    X = filepaths,
    FUN = function(filepath) {
      if (!is_file_accessible(filepath) || !is_text_file(filepath)) {
        return(NA_character_)
      }

      # read in appropriate column ---------------------------------------------------

      # determine encoding and delimiter
      encoding <- guess_file_encoding(filepath)
      delimiter <- guess_delimiter(filepath)

      cust_locale <-
        readr::locale(
          encoding = encoding
        )

      # determine appropriate column
      headers <- determine_headers(filepath)

      if ("market_value" %in% names(headers)) {
        num_col_idx <- match("market_value", names(headers))
        has_header_row <- TRUE
      } else {
        # FIXME not sure what to do if columns can't be determined, or doesn't have header
        return(NA_character_)
      }

      # read in numeric column
      char_data <-
        readr::read_delim(
          file = filepath,
          delim = delimiter,
          locale = cust_locale,
          trim_ws = TRUE,
          col_types = readr::cols(.default = "c"),
          col_names = FALSE,
          col_select = dplyr::all_of(num_col_idx),
          skip = dplyr::if_else(has_header_row, 1L, 0L),
          progress = FALSE,
          show_col_types = FALSE
        )

      all_num_chars <- char_data[[1]]


      # determiine decimal and grouping mark -----------------------------------------

      other_chars <- unique(unlist(strsplit(gsub("[0-9]", "", all_num_chars), split = "")))

      other_chars_cnts <-
        lapply(
          X = other_chars,
          FUN = function(num_mrk) {
            num_mrk_regex <- paste0("[", num_mrk, "]")
            has_num_mrk <- grepl(num_mrk_regex, all_num_chars)

            num_mrk_before_3_regex <- paste0("^((?!", num_mrk_regex, ").)*$|", num_mrk_regex, "[[:digit:]]{3}[^[:digit:]]|", num_mrk_regex, "[[:digit:]]{3}$")
            only_before_3 <- all((has_num_mrk & grepl(num_mrk_before_3_regex, all_num_chars, perl = TRUE)) | !has_num_mrk)

            num_mrk_cnt <- stringr::str_count(all_num_chars, num_mrk_regex)
            num_mrk_more_than_1 <- max(num_mrk_cnt) > 1

            list(
              possible_group_mark = only_before_3,
              possible_decimal = !num_mrk_more_than_1
            )
          }
        )

      possible_group_marks <- other_chars[unlist(lapply(other_chars_cnts, function(x) x$possible_group_mark))]
      possible_decimals <- other_chars[unlist(lapply(other_chars_cnts, function(x) x$possible_decimal))]

      common_decimals <- c(".", ",")
      common_grouping_marks <- c(",", ".", " ", "'", "|")

      possible_decimals <- possible_decimals[possible_decimals %in% common_decimals]  # readr::locale() only accepts decimal_mark %in% c(".", ",")

      if (length(possible_decimals) == 0) {
        decimal_mark <- common_decimals[!common_decimals %in% possible_group_marks][[1]]
        possible_group_marks <- c(possible_group_marks, common_grouping_marks)
        grouping_mark <- possible_group_marks[!possible_group_marks %in% decimal_mark][[1]]
      } else if (all(possible_decimals %in% possible_group_marks) && length(possible_group_marks) == 1) {
        grouping_mark <- possible_group_marks[[1]]
        possible_decimals <- c(possible_decimals, common_decimals)
        decimal_mark <- possible_decimals[!possible_decimals %in% grouping_mark][[1]]
      } else if (all(possible_decimals %in% possible_group_marks)) {
        decimal_mark <- possible_decimals[order(match(possible_decimals, common_decimals))][[1]]
        grouping_mark <- possible_group_marks[!possible_group_marks %in% decimal_mark][[1]]
      } else {
        possible_decimals <- possible_decimals[!possible_decimals %in% possible_group_marks]
        decimal_mark <- possible_decimals[order(match(possible_decimals, common_decimals))][[1]]
        possible_group_marks <- c(possible_group_marks, common_grouping_marks)
        grouping_mark <- possible_group_marks[!possible_group_marks %in% decimal_mark][[1]]
      }

      ifelse(type == "decimal", decimal_mark, grouping_mark)
    },
    FUN.VALUE = character(1L),
    USE.NAMES = FALSE
  )
}

guess_decimal_mark <- function(filepaths) {
  guess_numerical_mark(filepaths, type = "decimal")
}

guess_grouping_mark <- function(filepaths) {
  guess_numerical_mark(filepaths, type = "grouping")
}
