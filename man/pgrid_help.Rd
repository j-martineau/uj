% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pgrid.R
\encoding{UTF-8}
\name{pgrid_help}
\alias{pgrid_help}
\alias{pgrid}
\alias{pgrid0}
\alias{pgrid1}
\alias{pgridN}
\alias{pgridX}
\title{\code{expand.grid} for \code{paste} and \code{paste0}}
\usage{
pgrid_help()

pgrid(.p, ..., .ch = F, .crossed = F, .na = F)

pgrid0(..., .ch = F, .crossed = F, .na = FALSE)

pgrid1(..., .ch = F, .crossed = F, .na = FALSE)

pgridN(.p, ..., .ch = F, .na = FALSE)

pgridX(.p, ..., .ch = F, .na = FALSE)
}
\arguments{
\item{.p}{A \link[=cmp_chr_scl]{complete character scalar} to use as the 'paste'.}

\item{...}{Non-empty atomic objects.}

\item{.ch, .na}{Non-\code{NA} logical scalars indicating, respectively, whether to split each \code{...} arguments into its constituent characters after conversion to mode 'character' and whether \code{NA} values in \code{...} arguments are valid.}

\item{.crossed}{\code{TRUE} or \code{FALSE} indicating whether to construct the result to look like a fully-crossed model. See \emph{the} \code{.crossed} \emph{argument} section for details.}
}
\value{
A character vector.
}
\description{
Generate all combinations of the values of 2 or more vectors and glue the combinations into a single character vector.
}
\section{Functions}{
\itemize{
\item \code{pgrid()}: Converts the \code{N} \code{...} arguments to character (with additional optional pre-processing) and create a character vector with each element consisting of sub-strings from across \code{...} arguments pasted together using the 'paste' \code{.p}. See the \emph{the} \code{.crossed} \emph{argument} section for how the value of \code{.crossed} affects the return value.

\item \code{pgrid0()}: Calls \code{pgrid} with \code{.p = ""} (blank).

\item \code{pgrid1()}: Calls \code{pgrid} with \code{.p = " "} (space).

\item \code{pgridN()}: Calls \code{pgrid} with \code{.crossed = FALSE} (see \emph{the} \code{.crossed} \emph{argument}).

\item \code{pgridX()}: Calls \code{pgrid} with \code{.crossed = TRUE} (see \emph{the} \code{.crossed} \emph{argument}).

}}
\section{The \code{.crossed} Argument}{

The following console excerpt shows the effect of the value of \code{.crossed}:

\if{html}{\out{<div class="sourceCode">}}\preformatted{> x <- c("A", "B", "C")
> y <- c(1, 2)
> z <- c("a", "b")
> p <- "."
> list(n = sort(pgrid(p, x, y, z, .crossed = FALSE)),
       y = sort(pgrid(p, x, y, z, .crossed = TRUE )))
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
pgrid(".", x, y, z, .crossed = F)
pgrid(".", x, y, z, .crossed = T)
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
\code{\link{markdown_help}()},
\code{\link{maxnch}()},
\code{\link{ox}()},
\code{\link{ox_vals}()},
\code{\link{revstr}()},
\code{\link{spaces}()},
\code{\link{ss_help}()},
\code{\link{tocase}()},
\code{\link{weave}()}
}
\concept{strings}
