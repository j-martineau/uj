% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sca.R
\encoding{UTF-8}
\name{sca}
\alias{sca}
\title{Thin wrappers of \code{scales} functions}
\usage{
sca(color, alpha = NA)
}
\arguments{
\item{alpha}{new alpha level in [0,1].  If alpha is \code{NA},
existing alpha values are preserved.}
}
\description{
\tabular{ll}{  \code{sca}   \tab Thinly wraps \code{\link[scales]{alpha}}}
\cr\cr See the link above for complete function documentation.
}
\examples{
sca("red", 0.5)
sca("#FF00FF", 0.1)
}
\seealso{
Other colors: 
\code{\link{markdown}()}

Other plots: 
\code{\link{color}},
\code{\link{ggp}()},
\code{\link{gr}},
\code{\link{markdown}()},
\code{\link{pals}()},
\code{\link{u2u}()}

Other wraps: 
\code{\link{basics}()},
\code{\link{dp}()},
\code{\link{dt}()},
\code{\link{ggp}()},
\code{\link{gr}},
\code{\link{rd}()},
\code{\link{tb}()}
}
\concept{colors}
\concept{plots}
\concept{wraps}
