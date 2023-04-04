#' Guess the delimiter of a delimited file for a vector of filenames or
#' filepaths
#'
#' This function will guess the delimiter of a delimited file for a vector of
#' filenames or filepaths and return the delimiter as a string. It defaults to
#' the following delimiters in order if the others are not valid: ",", ";",
#' tab, "|", ":". If the file is inaccessible or binary, it will return `NA`
#' for that element.  If you pass anything that is not a character vector or a
#' single column `data.frame` to the `filepaths` argument, this function will
#' give an error.
#'
#' @param filepaths A character vector
#'
#' @return A character vector the same length as `filepaths`.
#'
#' @export

guess_delimiter <- function(filepaths) {
  filepaths <- simplify_if_one_col_df(filepaths)
  stopifnot("`filepaths` must be a character vector" = typeof(filepaths) == "character")
  filepaths <- canonize_path(filepaths)

  vapply(
    X = filepaths,
    FUN = function(filepath) {
      if (!is_file_accessible(filepath) || !is_text_file(filepath)) {
        return(NA_character_)
      }
      lines <-
        readr::read_lines(
          file = filepath,
          locale = readr::locale(encoding = guess_file_encoding(filepath)),
          n_max = -1L,
          progress = FALSE
        )

      lines <- gsub("\"[^\"]*\"", "", lines) # remove quoted strings

      fields_per_line <- function(delim, lines) {
        delim_regex <- paste0("[", delim, "]")
        delims_per_line <- unique(stringr::str_count(lines, delim_regex))
        if (isFALSE(identical(x = length(delims_per_line), 1L))) {
          return(0L)
        }
        delims_per_line + 1 # 0 delimiters = 1 fields, 1 delimiter = 2 fields, etc.
      }

      delims <- c(",", ";", "\t", "|", ":")
      fields_per_line_per_delim <- vapply(X = delims, FUN = fields_per_line, FUN.VALUE = numeric(1), lines)
      names(which.max(fields_per_line_per_delim))
    },
    FUN.VALUE = character(1L),
    USE.NAMES = FALSE
  )
}
