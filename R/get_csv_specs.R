#' Get a data frame of CSV specifications
#'
#' This function will return a data frame with numerous specifications for every
#' CSV file passed in the `files` argument.
#'
#' @param files A character vector containing absolute or relative paths to
#'   portfolio CSVs, or a directory containing portfolio CSVs
#' @param expected_colnames A character vector containing the names of the
#'   columns expected in the portfolio CSVs
#'
#' @return A data frame (invisibly) containing one row per portfolio CSV with
#'   columns for each identified specification
#'
#' @export

get_csv_specs <- function(files, expected_colnames = c("Investor.Name", "Portfolio.Name", "ISIN", "MarketValue", "Currency")) {
  alert_by_type <- function(type, ...) {
    switch(
      type,
      info = cli::cli_alert_info(...),
      warning = cli::cli_alert_warning(...),
      danger = cli::cli_alert_danger(...),
      success = cli::cli_alert_success(...),
    )
  }

  report_alert_files <- function(msg, bullets, type = "info", info = NULL) {
    cli::cli_div(theme = list(`.indented` = list(`margin-left` = 2), `.file` = list(color = "blue")))
    on.exit(cli::cli_end(), add = TRUE)

    cli::cli({
      alert_by_type(type, msg)
      if (!is.null(info)) {
        alert_by_type("info", info, class = "indented")
      }
      if (length(bullets) > 10L) {
        abbreviated <- c(bullets[1:10], paste0("\u2026 and {.strong ", length(bullets) - 10, " more}"))
        cli::cli_bullets(abbreviated, class = "file indented")
      } else {
        cli::cli_bullets(bullets, class = "file indented")
      }
    })
  }

  if (length(files) == 1 && fs::is_dir(files)) {
    files <- file.path(files, list.files(files))
  }
  files_df <- tibble::tibble(input = files, id = seq_along(files))

  files_df$filename <- basename(files_df$input)
  files_df$extension <- tools::file_ext(files_df$input)

  files_df$filepath <- fs::path_abs(fs::path_expand(files_df$input))

  files_df$file_exists <- unname(fs::file_exists(files_df$filepath))

  if (all(files_df$file_exists)) {
    cli::cli_alert_success("all files exist")
  } else if (all(!files_df$file_exists)) {
    cli::cli_alert_danger("none of the files exist")
    cli::cli_abort("the {.fun get_csv_specs} function had to stop because none of the files could be used")
  } else {
    alert_files <- files_df$filename[!files_df$file_exists]
    report_alert_files("the following files do not exist and will not be considered further:", alert_files, type = "danger")
    files_df <- files_df[files_df$file_exists, ]
  }

  files_df$file_size <- unname(fs::file_size(files_df$filepath))

  if (all(files_df$file_size > 0)) {
    cli::cli_alert_success("all files have a size > 0")
  } else if (all(files_df$file_size == 0)) {
    cli::cli({
      cli::cli_alert_danger("all of the files have a size of 0")
      cli::cli_alert_info("this might mean that they are un-downloaded Dropbox files or are empty files", class = "indented")
    })
    cli::cli_abort("the {.fun get_csv_specs} function had to stop because none of the files could be used")
  } else {
    alert_files <- files_df$filename[files_df$file_size == 0]
    report_alert_files("the following files have a size of 0 and will not be considered further:", alert_files, type = "danger", info = "this might mean that they are un-downloaded Dropbox files or are empty files")
    files_df <- files_df[files_df$file_size > 0, ]
  }


  files_df$file_read_access <- unname(fs::file_access(files_df$filepath, mode = "read"))

  if (all(files_df$file_read_access)) {
    cli::cli_alert_success("all files have read access")
  } else if (all(!files_df$file_read_access)) {
    cli::cli_alert_danger("all of the files do not have read access")
    cli::cli_abort("the {.fun get_csv_specs} function had to stop because none of the files could be used")
  } else {
    alert_files <- files_df$filename[!files_df$file_read_access]
    report_alert_files("the following files do not have read access and will not be considered further:", alert_files, type = "danger")
    files_df <- files_df[files_df$file_read_access, ]
  }


  files_df$file_write_access <- unname(fs::file_access(files_df$filepath, mode = "write"))

  files_df$mime_encoding <- vapply(files_df$filepath, function(x) system2("file", args = c("-b", "--mime-encoding", shQuote(x)), stdout = TRUE), character(1))

  if (all(files_df$mime_encoding != "binary")) {
    cli::cli_alert_success("all files are not binary files")
  } else if (all(files_df$mime_encoding == "binary")) {
    cli::cli_alert_danger("all of the files are binary files")
    cli::cli_abort("the {.fun get_csv_specs} function had to stop because none of the files could be used")
  } else {
    alert_files <- files_df$filename[files_df$mime_encoding == "binary"]
    report_alert_files("the following files are binary files and will not be considered further:", alert_files, type = "danger")
    files_df <- files_df[files_df$mime_encoding != "binary", ]
  }


  files_df$content_type <- guess_content_types(files_df$filepath)

  if (all(grepl("/csv$|/comma-separated-values$", files_df$content_type))) {
    cli::cli_alert_success("all files are CSV files")
  } else if (all(!grepl("/csv$|/comma-separated-values$", files_df$content_type))) {
    cli::cli_alert_danger("all of the files are not CSV files")
    cli::cli_abort("the {.fun get_csv_specs} function had to stop because none of the files could be used")
  } else {
    alert_files <- files_df$filename[!grepl("/csv$|/comma-separated-values$", files_df$content_type)]
    report_alert_files("the following files are not CSV files and will not be considered further:", alert_files, type = "danger")
    files_df <- files_df[grepl("/csv$|/comma-separated-values$", files_df$content_type), ]
  }

  files_df$filepath_is_ascii <- stringi::stri_enc_isascii(files_df$filepath)

  detected_filepath_encoding_tbls <- stringi::stri_enc_detect(files_df$filepath)
  files_df$filepath_encoding <- vapply(detected_filepath_encoding_tbls, function(x) { x$Encoding[[1]] }, character(1))

  files_df$filepath_declared_encoding <- Encoding(files_df$filepath)

  files_df$file_encoding <- guess_file_encoding(files_df$filepath)

  if (all(grepl("ascii|utf-8", files_df$file_encoding, ignore.case = TRUE))) {
    cli::cli_alert_success("all files are encoded in ASCII or UTF-8")
  } else {
    alert_files <- files_df$filename[!grepl("ascii|utf-8", files_df$file_encoding, ignore.case = TRUE)]
    report_alert_files("the following files are not encoded in ASCII or UTF-8:", alert_files, type = "warning", info = "this can be adapted to automatically by the {.fun read_portfolio_csv} function")
  }

  files_df$num_of_lines <- guess_num_of_lines(files_df$filepath)

  files_df$last_line_has_newline <- has_newline_at_end(files_df$filepath)

  if (all(files_df$last_line_has_newline)) {
    cli::cli_alert_success("all files have a newline at the end")
  } else {
    alert_files <- files_df$filename[!files_df$last_line_has_newline]
    report_alert_files("the following files do not have a newline at the end:", alert_files, type = "warning", info = "this can be adapted to automatically by the {.fun read_portfolio_csv} function")
  }

  files_df$delimiter <- guess_delimiter(files_df$filepath)

  if (all(files_df$delimiter == ",")) {
    cli::cli_alert_success(paste0("all files use {.strong ", cli::style_inverse(","), "} for a delimiter"))
  } else {
    alert_files <- files_df$filename[files_df$delimiter != ","]
    report_alert_files(paste0("the following files do not use {.strong ", cli::style_inverse(","), "} for a delimiter:"), alert_files, type = "warning", info = "this can be adapted to automatically by the {.fun read_portfolio_csv} function")
  }

  files_df$read_without_error <- validate_read_without_error(files_df$filepath, files_df$file_encoding, files_df$delimiter)

  if (all(files_df$read_without_error == TRUE)) {
    cli::cli_alert_success(paste0("all files can be read without error"))
  } else if (any(files_df$read_without_error == FALSE)) {
    alert_files <- files_df$filename[isFALSE(files_df$read_without_error)]
    report_alert_files("the following files can not be read without error and will not be considered further:", alert_files, type = "danger")
    files_df <- files_df[files_df$read_without_error == TRUE, ]
  }


  files_df$num_of_columns <- guess_num_of_columns(files_df$filepath, files_df$file_encoding, files_df$delimiter)

  if (all(files_df$num_of_columns == 5L)) {
    cli::cli_alert_success(paste0("all files have {.strong 5} columns"))
  } else if (any(files_df$num_of_columns > 5L)) {
    alert_files <- files_df$filename[files_df$num_of_columns > 5L]
    report_alert_files("the following files have more than {.strong 5} columns:", alert_files, type = "warning", info = "this can usually be adapted to automatically by the {.fun read_portfolio_csv} function")
  } else if (any(files_df$num_of_columns < 4L)) {
    alert_files <- files_df$filename[files_df$num_of_columns < 4L]
    report_alert_files("the following files have less than {.strong 4} columns and will not be considered further:", alert_files, type = "danger")
    files_df <- files_df[files_df$num_of_columns >= 4L, ]
  }



  files_df$decimal_mark <- guess_decimal_mark(files_df$filepath)

  if (all(!is.na(files_df$decimal_mark)) && all(files_df$decimal_mark == ".")) {
    cli::cli_alert_success(paste0("all files use {.strong ", cli::style_inverse("."), "} for a decimal mark"))
  } else {
    alert_files <- files_df$filename[is.na(files_df$decimal_mark) || grepl("^[.]$", files_df$decimal_mark)]
    report_alert_files(paste0("the following files do not use {.strong ", cli::style_inverse("."), "} for a decimal mark"), alert_files, type = "warning", info = "this can be adapted to automatically by the {.fun read_portfolio_csv} function")
  }

  files_df$grouping_mark <- guess_grouping_mark(filepaths = files_df$filepath)

  if (all(!is.na(files_df$grouping_mark) && files_df$grouping_mark == ",")) {
    cli::cli_alert_success(paste0("all files use {.strong ", cli::style_inverse(","), "} for a grouping mark"))
  } else {
    alert_files <- files_df$filename[is.na(files_df$grouping_mark) || grepl("^[,]$", files_df$grouping_mark)]
    report_alert_files(paste0("the following files do not use {.strong ", cli::style_inverse(","), "} for a grouping mark"), alert_files, type = "warning", info = "this can be adapted to automatically by the {.fun read_portfolio_csv} function")
  }

  files_df$tokenizer <- get_tokenizers(files_df$filepath, files_df$file_encoding, files_df$delimiter)

  files_df$fields_per_line <- get_fields_per_line(files_df$filepath, files_df$tokenizer)
  files_df$has_consistent_fields_per_line <- vapply(X = files_df$fields_per_line, FUN = function(x) all(x == x[1]), FUN.VALUE = logical(1))

  if (all(files_df$has_consistent_fields_per_line == TRUE)) {
    cli::cli_alert_success(paste0("all files have a consistent number of fields per line"))
  } else if (any(files_df$has_consistent_fields_per_line == FALSE)) {
    alert_files <- files_df$filename[files_df$has_consistent_fields_per_line == FALSE]
    report_alert_files("the following files do not have a consistent number of fields per line and will not be considered further:", alert_files, type = "danger")
    files_df <- files_df[files_df$has_consistent_fields_per_line == TRUE, ]
  }

  files_df$column_names <- get_column_names(files_df$filepath, files_df$file_encoding, files_df$delimiter)

  files_df$has_expected_colnames <- vapply(X = files_df$column_names, FUN = function(x) isTRUE(all.equal(target = expected_colnames, current = x)), FUN.VALUE = logical(1))

  if (all(files_df$has_expected_colnames == TRUE)) {
    cli::cli_alert_success(paste0("all files have the expected column names"))
  } else if (all(files_df$has_expected_colnames == FALSE)) {
    cli::cli_alert_warning("none of the files have the expected column names")
  } else if (any(files_df$has_expected_colnames == FALSE)) {
    alert_files <- files_df$filename[files_df$has_expected_colnames == FALSE]
    report_alert_files("the following files do not have the expected column names:", alert_files, type = "warning")
  }

  files_df$readr_locale <- get_locales(encodings = files_df$file_encoding, decimal_marks = files_df$decimal_mark, grouping_marks = files_df$grouping_mark)

  investor_name_colname <- expected_colnames[[1]]
  portfolio_name_colname <- expected_colnames[[2]]
  isin_colname <- expected_colnames[[3]]
  market_value_colname <- expected_colnames[[4]]
  currency_colname <- expected_colnames[[5]]
  test <- purrr::map(seq_along(files_df$filepath), ~ suppressWarnings(suppressMessages(readr::read_delim(files_df$filepath[.x], delim = files_df$delimiter[.x], locale = files_df$readr_locale[.x][[1]], progress = FALSE, show_col_types = FALSE))))

  files_df$investor_name_is_string <- vapply(test, function(x) is.character(x[[investor_name_colname]]), logical(1))

  files_df$portfolio_name_is_string <- vapply(test, function(x) is.character(x[[portfolio_name_colname]]), logical(1))

  files_df$market_value_is_numeric <- vapply(test, function(x) is.numeric(x[[market_value_colname]]), logical(1))

  files_df$market_value_has_negatives <- vapply(test, function(x) any(x[[market_value_colname]] < 0), logical(1))

  files_df$market_value_has_nas <- vapply(test, function(x) any(is.na(x[[market_value_colname]])), logical(1))

  files_df$valid_iso4217c_codes <- vapply(test, function(x) all(is_valid_currency_code(x[[currency_colname]])), FUN.VALUE = logical(1), USE.NAMES = FALSE)

  if (all(files_df$valid_iso4217c_codes == TRUE)) {
    cli::cli_alert_success(paste0("all files have only valid iso4217c currency codes"))
  } else if (any(files_df$valid_iso4217c_codes == FALSE)) {
    alert_files <- files_df$filename[files_df$valid_iso4217c_codes == FALSE]
    report_alert_files("the following files have some invalid iso4217c currency codes:", alert_files, type = "warning")
  }

  files_df$has_invalid_isins <- vapply(test, function(x) any(is_valid_isin(x[[isin_colname]]) == FALSE), FUN.VALUE = logical(1), USE.NAMES = FALSE)

  if (all(files_df$has_invalid_isins == FALSE)) {
    cli::cli_alert_success(paste0("all files have only valid ISINs"))
  } else if (any(files_df$has_invalid_isins == TRUE)) {
    alert_files <- files_df$filename[files_df$has_invalid_isins == TRUE]
    report_alert_files("the following files have some invalid ISINs:", alert_files, type = "warning")
  }


  invisible(files_df)
}


guess_ <- function(filepaths, encodings) {
  vapply(
    X = seq_along(filepaths),
    FUN = function(i) {

    },
    FUN.VALUE = character(1)
  )
}


check_column_names <- function(filepaths, encodings, delimiters) {
  vapply(
    X = seq_along(filepaths),
    FUN = function(i) {
      readr::read_delim(file = filepaths[[i]], locale = readr::locale(encoding = encodings[[i]]), delim = delimiters[[i]], n_max = 1L, col_names = FALSE, show_col_types = FALSE, progress = FALSE)
    },
    FUN.VALUE = integer(1)
  )
}

get_column_names <- function(filepaths, encodings, delimiters) {
  vapply(
    X = seq_along(filepaths),
    FUN = function(i) {
      list(
        names(
          suppressMessages(
            readr::read_delim(
              file = filepaths[[i]],
              delim = delimiters[[i]],
              locale = readr::locale(encoding = encodings[[i]]),
              n_max = 1L,
              trim_ws = TRUE,
              col_types = readr::cols(.default = "c"),
              show_col_types = FALSE,
              progress = FALSE
            )
          )
        )
      )
    },
    FUN.VALUE = list(1)
  )
}


get_fields_per_line <- function(filepaths, tokenizers) {
  vapply(
    X = seq_along(filepaths),
    FUN = function(i) {
      list(
        readr::count_fields(
          file = filepaths[[i]],
          tokenizer = tokenizers[[i]]
        )
      )
    },
    FUN.VALUE = list(1)
  )
}


get_locales <- function(encodings, decimal_marks, grouping_marks) {
  vapply(
    X = seq_along(encodings),
    FUN = function(i) {
      if (is.na(decimal_marks[[i]])) { decimal_marks[[i]] <- "."}
      if (is.na(grouping_marks[[i]])) { grouping_marks[[i]] <- ","}
      list(
        readr::locale(
          encoding = encodings[[i]],
          decimal_mark = decimal_marks[[i]],
          grouping_mark = grouping_marks[[i]]
        )
      )
    },
    FUN.VALUE = list(1)
  )
}


get_tokenizers <- function(filepaths, encodings, delimiters) {
  vapply(
    X = seq_along(filepaths),
    FUN = function(i) {
      list(
        readr::tokenizer_delim(
          delim = delimiters[[i]],
          quote = "\"",
          na = "NA",
          quoted_na = TRUE,
          comment = "",
          trim_ws = TRUE,
          escape_double = TRUE,
          escape_backslash = FALSE,
          skip_empty_rows = TRUE
        )
      )
    },
    FUN.VALUE = list(1)
  )
}


guess_content_types <- function(filepaths) {
  vapply(
    X = filepaths,
    FUN = function(x) {
      wand::guess_content_type(x)[[1]]
    },
    FUN.VALUE = character(1))
}


guess_num_of_columns <- function(filepaths, encodings, delimiters) {
  vapply(
    X = seq_along(filepaths),
    FUN = function(i) {
      ncol(readr::read_delim(file = filepaths[[i]], locale = readr::locale(encoding = encodings[[i]]), delim = delimiters[[i]], n_max = 1L, col_names = FALSE, show_col_types = FALSE, progress = FALSE))
    },
    FUN.VALUE = integer(1)
  )
}


guess_num_of_lines <- function(filepaths) {
  vapply(
    X = filepaths,
    FUN = function(filepath) {
      length(readr::read_lines(filepath, n_max = -1L, lazy = TRUE, progress = FALSE))
    },
    FUN.VALUE = integer(1)
  )
}


validate_read_without_error <- function(filepaths, encodings, delimiters) {
  vapply(
    X = seq_along(filepaths),
    FUN = function(i) {
      out <- tryCatch(
        suppressWarnings(suppressMessages(
          readr::read_delim(
            file = filepaths[[i]],
            delim = delimiters[[i]],
            locale = readr::locale(encoding = encodings[[i]]),
            progress = FALSE,
            show_col_types = FALSE
          )
        ))
      )
      !any(class(out) == "error")
    },
    FUN.VALUE = logical(1)
  )
}
