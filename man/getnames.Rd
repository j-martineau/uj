% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getnames.R
\name{getnames}
\alias{getnames}
\alias{enames}
\alias{rnames}
\alias{cnames}
\alias{rcnames}
\title{Get element, row, and/or column names}
\usage{
getnames(x, d = 0, u = T, err = F)

enames(x, u = T, err = F)

rnames(x, u = T, err = F)

cnames(x, u = T, err = F)

rcnames(x, u = T, err = F)
}
\arguments{
\item{x}{An object to get names from.}
}
\value{
A character vector containing names or a 2-element list of character
vectors.
}
\description{
Get element, row, and/or column names
}
\details{
\strong{\code{enames}}
\cr Gets element names of an atomic or list vector.
\cr\cr
strong{\code{rnames, cnames}}
\cr Gets row and column names, respectively, of a tibble or matrix.
\cr \cr
\strong{\code{rcnames}}
\cr Get row and column names of a tibble or matrix in a 2-element list.
}
\seealso{
Other naming: 
\code{\link{named}()}

Other meta: 
\code{\link{as_xxx}},
\code{\link{av}()},
\code{\link{binfuns}},
\code{\link{callers}()},
\code{\link{case}()},
\code{\link{ex}()},
\code{\link{failsafe}()},
\code{\link{fx}()},
\code{\link{make}},
\code{\link{na_vals}},
\code{\link{named}()},
\code{\link{nx}()},
\code{\link{os}()},
\code{\link{ply}()},
\code{\link{recycling}},
\code{\link{run}()},
\code{\link{swap}()},
\code{\link{values}},
\code{\link{xb}()}
}
\concept{meta}
\concept{naming}
