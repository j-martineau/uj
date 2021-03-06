% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/failsafe.R
\name{failsafe}
\alias{failsafe}
\alias{isERR}
\alias{notERR}
\alias{msgERR}
\title{Evaluate objects, flag any errors}
\usage{
failsafe(x)

isERR(x)

notERR(x)

msgERR(x)
}
\arguments{
\item{x}{An object or a call to evaluate in the environment of a parent
function where the initial call was made.}
}
\value{
\code{failsafe} returns either \code{x} or an object of class
\code{'simpleError'}. \code{isERR, notERR} returns a logical scalar.
\code{msgERR} returns either \code{NULL} or a character scalar.
}
\description{
Evaluate objects, flag any errors
}
\details{
\strong{\code{failsafe(x)}}
\cr Attempts to call \code{identity(x)}. If an error is produced in doing
so, returns an object of class \code{'error'} or of class
\code{'simpleError'}. If an error is not generated, returns the safely
evaluated value of \code{x}.
\cr\cr
\strong{\code{isERR(x)}}
\cr Evaluates whether calling \code{identity(x)} produced an error.
\cr\cr
\strong{\code{notERR(x)}}
Evaluates whether calling \code{identity(x)} did not produce an error.
\cr\cr
\strong{\code{msgERR(x)}}
Gets any error message associated with calling \code{identity(x)}. If there
is none, returns \code{NULL}.
}
\seealso{
Other meta: 
\code{\link{as_xxx}},
\code{\link{av}()},
\code{\link{binfuns}},
\code{\link{callers}()},
\code{\link{case}()},
\code{\link{ex}()},
\code{\link{fx}()},
\code{\link{getnames}()},
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

Other failsafe: 
\code{\link{is_failsafe}}
}
\concept{errs}
\concept{failsafe}
\concept{meta}
