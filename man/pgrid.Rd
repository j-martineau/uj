% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pgrid.R
\encoding{UTF-8}
\name{pgrid}
\alias{pgrid}
\alias{pgrid0}
\alias{pgrid1}
\alias{pgridN}
\alias{pgridX}
\title{\code{expand.grid} for \code{paste} and \code{paste0}}
\usage{
pgrid(.P, ..., .CH = F, .CROSSED = F, .NA = F)

pgrid0(..., .CH = F, .CROSSED = F, .NA = FALSE)

pgrid1(..., .CH = F, .CROSSED = F, .NA = FALSE)

pgridN(.P, ..., .CH = F, .NA = FALSE)

pgridX(.P, ..., .CH = F, .NA = FALSE)
}
\arguments{
\item{.P}{A \link[=cmp_chr_scl]{complete character scalar} to use as the 'paste'.}

\item{...}{Non-empty atomic objects.}

\item{.CH, .NA}{Non-\code{NA} logical scalars indicating, respectively, whether to split each \code{...} arguments into its constituent characters after conversion to mode 'character' and whether \code{NA} values in \code{...} arguments are valid.}

\item{.CROSSED}{\code{TRUE} or \code{FALSE} indicating whether to construct the result to look like a fully-crossed model. See \emph{the} \code{.CROSSED} \emph{argument} section for details.}
}
\value{
A character vector.
}
\description{
Generate all combinations of the values of 2 or more vectors and glue the combinations into a single character vector.
}
\details{
\tabular{ll}{  \code{pgrid}    \tab Converts the \code{N} \code{...} arguments to character (with additional optional pre-processing) and create a character vector with each element
consisting of sub-strings from across \code{...} arguments pasted together using the 'paste' \code{.P}. See the \emph{the} \code{.CROSSED} \emph{argument} section for how the value
of \code{.CROSSED} affects the return value.           \cr   \tab   \cr
  \code{pgridN}   \tab Calls \code{pgrid} with \code{.CROSSED = FALSE}\eqn{^{(1)}} \cr
  \code{pgridX}   \tab Calls \code{pgrid} with \code{.CROSSED = TRUE}\eqn{^{(1)}}  \cr
  \code{pgrid0}   \tab Calls \code{pgrid} with \code{.P = ""} (blank).             \cr
  \code{pgrid1}   \tab Calls \code{pgrid} with \code{.P = " "} (space).            \cr   \tab     }
\tabular{l}{  \eqn{^{(1)}} See \emph{the} \code{.CROSSED} \emph{argument}.                       }
}
\section{The \code{.CROSSED} Argument}{

The following console excerpt shows the effect of the value of \code{.CROSSED}:

\if{html}{\out{<div class="sourceCode">}}\preformatted{> x <- c("A", "B", "C")
> y <- c(1, 2)
> z <- c("a", "b")
> p <- "."
> list(n = sort(pgrid(p, x, y, z, .CROSSED = FALSE)),
       y = sort(pgrid(p, x, y, z, .CROSSED = TRUE )))
$n
[1] "A.1.a" "A.1.b" "A.2.a" "A.2.b" "B.1.a" "B.1.b"
[7] "B.2.a" "B.2.b" "C.1.a" "C.1.b" "C.2.a" "C.2.b"

$y
 [1] "1"     "1.a"   "1.b"   "2"     "2.a"   "2.b"
 [7] "a"     "A"     "A.1"   "A.1.a" "A.1.b" "A.2"
[13] "A.2.a" "A.2.b" "A.a"   "A.b"   "b"     "B"
[19] "B.1"   "B.1.a" "B.1.b" "B.2"   "B.2.a" "B.2.b"
[25] "B.a"   "B.b"   "C"     "C.1"   "C.1.a" "C.1.b"
[31] "C.2"   "C.2.a" "C.2.b" "C.a"   "C.b"
}\if{html}{\out{</div>}}
}

\examples{
x <- c("A", "B", "C")
y <- c(1, 2)
z <- c("a", "b")
pgrid(".", x, y, z, .CROSSED = F)
pgrid(".", x, y, z, .CROSSED = T)
pgridN(".", x, y, y)
pgridX(".", x, y, z)
pgrid0(x, y, z)
pgrid1(x, y, z)
}
\seealso{
Other strings: 
\code{\link{blank}()},
\code{\link{chn}()},
\code{\link{delim}()},
\code{\link{fsub}()},
\code{\link{gr}},
\code{\link{ipat}()},
\code{\link{makestr}()},
\code{\link{markdown}()},
\code{\link{maxnch}()},
\code{\link{ox}()},
\code{\link{revstr}()},
\code{\link{spaces}()},
\code{\link{ss}()},
\code{\link{tocase}()},
\code{\link{weave}()}
}
\concept{strings}
