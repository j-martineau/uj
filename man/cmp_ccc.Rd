% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cmp_ccc.R
\encoding{UTF-8}
\name{cmp_ccc}
\alias{cmp_ccc}
\alias{cmp_ccc_funs}
\alias{cmp_arr}
\alias{cmp_dtf}
\alias{cmp_gen}
\alias{cmp_mat}
\alias{cmp_mvc}
\alias{cmp_scl}
\alias{cmp_vec}
\alias{cmp_vls}
\title{Completeness + xclass combination properties}
\usage{
cmp_ccc(x, .CCC, ...)

cmp_ccc_funs()

cmp_arr(x, ...)

cmp_dtf(x, ...)

cmp_gen(x, ...)

cmp_mat(x, ...)

cmp_mvc(x, ...)

cmp_scl(x, ...)

cmp_vec(x, ...)

cmp_vls(x, ...)
}
\arguments{
\item{x}{An R object.}

\item{.CCC}{A character scalar single xclass property from \code{\link{ccc_props}()}.}

\item{...}{
  Arguments passed on to \code{\link[=meets]{meets}}
  \describe{
    \item{\code{}}{}
  }}
}
\value{
\strong{A character vector} \cr\cr \code{cmp_ccc_funs}
\cr\cr  \strong{A logical scalar}   \cr\cr \verb{cmp_\{ccc\}, cmp_ccc}
}
\description{
Check for combinations of \link[=CMP]{completeness} and \link[=ccc]{xclass}.
}
\details{
\tabular{ll}{  \code{cmp_ccc_funs}   \tab What complete + xclass combination property functions are there?                                                                       \cr   \tab   \cr
  \verb{cmp_\{ccc\}}      \tab Is \code{x} both complete and a match to the single xclass property \code{'{ccc}'} where \code{{ccc}} is a placeholder for any given xclass property? \cr   \tab   \cr
  \code{cmp_ccc}        \tab Is \code{x} both complete and a match to the single xclass property in \code{.CCC}?                                                                              }
}
\section{Specifying count and value restrictions}{
 Specifying restrictions in \code{...} is optional. The full set of recognized arguments names are defined in the following table along with the properties each specifies:
\tabular{ll}{  \verb{.MAX, .MAXR, .MAXC}   \tab Scalar maximum valid numbers of elements, rows, and columns, respectively.                                                                 \cr   \tab   \cr
  \verb{.MIN, .MINR, .MINC}   \tab Scalar minimum valid numbers of elements, rows, and columns, respectively.                                                                 \cr   \tab   \cr
  \verb{.LT, .LE, .GE, .GT}   \tab \link[=cmp_srt_scl]{Complete sortable scalar} less-than, less-than-or-equal, greater-than-or-equal, and greater-than bounds, respectively. \cr   \tab   \cr
  \verb{.N, .NR, .NC}         \tab A vector of valid numbers of elements, rows, and columns, respectively.                                                                    \cr   \tab   \cr
  \code{.VALS}                \tab A vector of valid values.                                                                                                                                 }
}

\examples{
cmp_ccc_funs()
cmp_ccc(letters, "mvc")
cmp_ccc(1, "scl")
cmp_ccc(NA, "gen")
cmp_mvc(letters)
cmp_scl(1)

}
\seealso{
Other properties: 
\code{\link{as_mmm}()},
\code{\link{bbb_mmm}()},
\code{\link{bbb}()},
\code{\link{cls}()},
\code{\link{cmp_mmm}()},
\code{\link{comparable}()},
\code{\link{compatible}()},
\code{\link{ddd}()},
\code{\link{eee}()},
\code{\link{iii}()},
\code{\link{is_unique}()},
\code{\link{meets}()},
\code{\link{mmm_ccc}()},
\code{\link{mmm}()},
\code{\link{ppp_fast}()},
\code{\link{ppp}()},
\code{\link{sss}()},
\code{\link{stopperr}()},
\code{\link{unq_ccc}()}
}
\concept{properties}
