% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/parseText.R
\name{parseText}
\alias{parseText}
\title{Parse text in character vector into individual elements by quotes and whitespace,
keeping quoted text together in same element (with whitespace but without quotation marks).}
\usage{
parseText(strv, qt = "'", keepQuotes = FALSE, stripComments = TRUE,
  comment = "#", verbose = FALSE)
}
\arguments{
\item{strv}{- chracter vector to parse}

\item{qt}{- the character indicating quotation (' or ")}

\item{keepQuotes}{- flag to keep the quotation marks in the output (does nothing)}

\item{comment}{- character used to indicate the start of a comment}

\item{verbose}{- flag (T/F) to print details of parsing operations to screen}

\item{removeComments}{- flag to remove comments prior to parsing}
}
\value{
numbered list, with elements corresponding to the equivalent vector element in strv
AFTER comments have been removed.
}
\description{
Function to parse text in character vector into individual elements by quotes and whitespace,
keeping quoted text together in same element (with whitespace but without quotation marks).
}
\details{


Each element in the returned list is a character vector whose elements consist of
1) quoted text blocks and 2) unquoted text elements split by white space in the order they occur in the
element of the character vector.
}

