% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ss.R
\name{ss}
\alias{ss}
\alias{ssP}
\alias{ssD}
\alias{ssB}
\alias{ssPD}
\alias{ssPB}
\alias{ssDB}
\alias{ssPDB}
\alias{ch}
\title{Split strings and select/check for elements}
\usage{
ss(..., d = "|", trm = T, sqz = T, u = F, n = NULL)

ssP(..., trm = T, sqz = T, n = NULL, u = F)

ssD(..., trm = T, sqz = T, n = NULL, u = F)

ssB(..., trm = T, sqz = T, n = NULL, u = F)

ssPD(..., trm = T, sqz = T, n = NULL, u = F)

ssPB(..., trm = T, sqz = T, n = NULL, u = F)

ssDB(..., trm = T, sqz = T, n = NULL, u = F)

ssPDB(..., trm = T, sqz = T, n = NULL, u = F)

ch(..., trm = T, sqz = T, n = NULL, u = F)
}
\arguments{
\item{...}{An arbitrary number of objects to be split
(\code{\link[=a]{atomized}} before splitting).}

\item{d}{A character vect of delimiters.}

\item{trm}{Logical scalar indicating whether to trim whitespace from each
element of the result.}

\item{sqz}{Logical scalar indicating whether to squeeze the result by
removing either empty strings (for \code{ss(.)} functions) or characters
that are neither letters, digits, nor spaces (for \code{ch(.)}).}

\item{u}{Logical scalar indicating whether to reduce the result to unique
values.}

\item{n}{An optional integer scalar giving the number of the split or
character to extract.}
}
\value{
A character vector (when \code{vals = NULL}) or a logical vector
(when \code{vals} is not \code{NULL}).
}
\description{
Split strings and select/check for elements
}
\details{
\strong{\code{ss}} splits strings using the delimiter(s) in
\code{d} following these sequential steps:
\enumerate{
\item Reduce \code{...} to an atomic vector containing the constituent
atomic elements of each argument in \code{...}.
\item Convert the result to mode character.
\item Replace each element of the result with that element's constituent
parts as delimited by \code{d}, producting a potentially longer
character vector.
\item If \code{n} is not \code{NULL}, extracts the \code{n}-th
elements(s) from the result.
\item If \code{trm} is \code{TRUE}, trims whitespace (i.e., spaces, tabs,
newlines) from both ends of each element of the result.
\item If \code{sqz} is \code{TRUE}, removes blank strings elements
(i.e., \code{""}) from the result.
\item If \code{u} is \code{TRUE}, reduces the result to unique values.
}
\cr\cr
\strong{\code{ch}} splits strings into their constituent characters
following these sequential steps:
\enumerate{
\item Reduce \code{...} to an atomic vector containing the constituent
atomic elements of each argument in \code{...}.
\item Convert the result to mode character.
\item Replace each element of the result with a character vector of that
element's constituent characters, producing a potentially longer
character vector.
\item If \code{trm} is \code{TRUE}, trims whitespace (i.e., spaces, tabs,
newlines) from both ends of each element of the result.
\item If \code{sqz} is \code{TRUE}, removes characters that are neither
letters, digits, nor spaces from the result.
\item If \code{n} is not \code{NULL}, extracts the \code{n}-th
elements(s) from the result.
\item If \code{u} is \code{TRUE}, reduces the result to unique values.
}
\strong{Extensions}
\cr Functions are extended for specific delimiters, signified by the
following extension characters:
\cr\tabular{lll}{
\strong{Character}\tab\strong{Name}\tab\strong{Delimiter Invoked}
\cr\code{'P'}     \tab Pipe        \tab \code{"|"}
\cr\code{'D'}     \tab Dot         \tab \code{"."}
\cr\code{'B'}     \tab Broken pipe \tab \code{"¦"}
  }
\strong{\code{ssP}, \code{ssD}, and \code{ssB}}
\cr Split strings using, respectively, the delimiters \code{'|'},
\code{'.'}, and \code{'¦'}.
\cr\cr
\strong{\code{ssPD}, \code{ssPB}, \code{ssDB}}
\cr split strings using, respectively, the following pairs of delimiters:
\code{c('|', '.')}, \code{c('|', '¦')}, and \code{c('.', '¦')}.
\cr\cr
\strong{\code{ssPDB}}
\cr Splits strings using the delimiters \code{c('|', '.', '¦')}.
}
\seealso{
Other strings: 
\code{\link{Oxford}},
\code{\link{delim}},
\code{\link{ipat}()},
\code{\link{pgrid}()},
\code{\link{spacing}},
\code{\link{tocase}()},
\code{\link{weave}()}
}
\concept{strings}
