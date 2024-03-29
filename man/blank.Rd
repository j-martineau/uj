% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/blank.R
\encoding{UTF-8}
\name{blank}
\alias{blank}
\title{Blank strings}
\usage{
blank(x)
}
\arguments{
\item{x}{An R object}
}
\value{
A logical scalar
}
\description{
Evaluate whether \code{x} is a blank string scalar (i.e., \code{""}).
}
\examples{
blank(NA)
blank(c("", ""))
blank("")
blank(v(blank))
blank(2)
blank(NULL)
}
\seealso{
Other strings: 
\code{\link{chn}()},
\code{\link{delim}()},
\code{\link{fsub}()},
\code{\link{gr}},
\code{\link{ipat}()},
\code{\link{makestr}()},
\code{\link{markdown}()},
\code{\link{maxnch}()},
\code{\link{ox}()},
\code{\link{pgrid}()},
\code{\link{revstr}()},
\code{\link{spaces}()},
\code{\link{ss}()},
\code{\link{tocase}()},
\code{\link{weave}()}
}
\concept{strings}
