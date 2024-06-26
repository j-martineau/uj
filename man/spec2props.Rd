% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ppp.R
\encoding{UTF-8}
\name{spec2props}
\alias{spec2props}
\title{Convert a Properties Specification Character Scalar or Vector to a Character Vector of Single Properties}
\usage{
spec2props(x)
}
\arguments{
\item{x}{A character vector}
}
\value{
Either \code{NULL} or a non-empty character vector of unique, sorted, lowercase, 3-character values.

Returns \code{NULL} if any of the following criteria are met:
\itemize{
\item \code{x} is not of mode \code{'character'}.
\item Any resulting value is \code{NA}.
\item Any resulting value is not a 3-character string.
\item \code{x} resolves to a zero-length character vector.
}
}
\description{
Splits each element of \code{x} along both \code{|} and \verb{_} to create a character vector of single constituent
properties, removes any blank values, reduces the result to unique values, converts all remaining values to
lowercase, and sorts the remaining values.
}
\seealso{
Other utils: 
\code{\link{check_valid}()},
\code{\link{combo2props}()},
\code{\link{meets_errs}()},
\code{\link{spec2combos}()},
\code{\link{stack2funs}()}
}
\concept{utils}
