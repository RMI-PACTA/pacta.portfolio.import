# These functions return ascii representations of extended or unicode
# characters using hex or unicode R escape strings

char_to_hex <- function(x) {
  cat(paste0("\"", paste0("\\x", charToRaw(x), collapse = ""), "\""))
}

char_to_unicode <- function(x) {
  cat(paste0("\"\\U", format(as.hexmode(utf8ToInt(x)), width = 8, upper = TRUE), "\""))
}
