% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/is_valid_isin.R
\name{is_valid_isin}
\alias{is_valid_isin}
\title{Validate a vector of ISINs}
\usage{
is_valid_isin(isins)
}
\arguments{
\item{isins}{A character vector}
}
\value{
A logical vector the same length as \code{isins}.
}
\description{
This function validates that a vector of ISINs are valid codes that conform
to the ISO 6166 specification with \code{TRUE} or \code{FALSE}. It checks the basic
structure (2 alpha characters, 9 alpha-numeric characters, 1 check digit)
and also validates the check digit using the Luhn algorithm.
}
