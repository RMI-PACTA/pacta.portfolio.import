# This file contains the code to be executed
# when this package is loaded or attached

# This check in place following:
# https://github.com/RMI-PACTA/pacta.portfolio.import/issues/54
# see ?Comparison for help on string comparison.
# This should be accurate under all collations
.onAttach <- function(libname, pkgname) {
  r_version <- paste0(R.Version()[["major"]], ".", R.Version()[["minor"]])
  if (r_version < "4.3.2") {
    if (Sys.info()[["sysname"]] == "Darwin") {
      darwin_version <- Sys.info()[["release"]]
      if (darwin_version >= "23") {
        msg <- paste0(
          "Warning: In Darwin release 23.0.0 (MacOS Sonoma 14.0) or higher,\n",
          "an issue with `iconv` causes R to crash when converting ",
          "Non-ASCII characters to ASCII.\n",
          "You are running R version ", r_version,
          " on Darwin release ", darwin_version, ".\n",
          "Please ensure that portfolios imported with ", pkgname,
          " contain only ASCII characters,\n",
          "or update R to version 4.3.2 or higher.\n"
        )
        packageStartupMessage(msg)
      }
    }
  }
}
