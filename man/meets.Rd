% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/meets.R
\encoding{UTF-8}
\name{meets}
\alias{meets}
\title{Evaluate whether an object meets count and/or value restrictions}
\usage{
meets(x, ...)
}
\arguments{
\item{x}{An object.}

\item{...}{Optional named arguments count and/or value restrictions for \code{x}. See the \emph{specifying count and value restrictions} section.}
}
\value{
A logical scalar.
}
\description{
Evaluates whether \code{x} meets any count and value restrictions are provided in \code{...}. If none are provided, returns \code{TRUE}.
}
\section{Specifying count and value restrictions}{
 Specifying restrictions in \code{...} is optional. The full set of recognized arguments names are defined in the following table along with the properties each specifies:
\tabular{ll}{  \verb{.max, .maxr, .maxc}   \tab Scalar maximum valid numbers of elements, rows, and columns, respectively.                                                                 \cr   \tab   \cr
  \verb{.min, .minr, .minc}   \tab Scalar minimum valid numbers of elements, rows, and columns, respectively.                                                                 \cr   \tab   \cr
  \verb{.lt, .le, .ge, .gt}   \tab \link[=cmp_srt_scl]{Complete sortable scalar} less-than, less-than-or-equal, greater-than-or-equal, and greater-than bounds, respectively. \cr   \tab   \cr
  \verb{.n, .nr, .nc}         \tab A vector of valid numbers of elements, rows, and columns, respectively.                                                                    \cr   \tab   \cr
  \code{.vals}                \tab A vector of valid values.                                                                                                                                 }
}

\examples{
chrs <- c("a", "b", "c")
nums <- 1:3
sqrd <- matrix(1:16, nrow = 4)
meets(chrs, .n = 1:5)
meets(chrs, .max = 2)
meets(chrs, .min = 3)
meets(sqrd, .nr = 3, .nr = 2:10)
meets(sqrd, .minr = 2, .maxr = 5, .minc = 4, .maxc = 5)
meets(chrs, .vals = letters)
meets(chrs, .gt = "a", .lt = "c")
meets(sqrd, .ge = 1, .le = 16)
}
\seealso{
Other properties: 
\code{\link{as_mmm}()},
\code{\link{bbb_ccc_help}()},
\code{\link{bbb_help}()},
\code{\link{bbb_mmm_prop_funs}()},
\code{\link{ccc_help}()},
\code{\link{cls}()},
\code{\link{cmp_ccc_help}()},
\code{\link{cmp_mmm_ccc_help}()},
\code{\link{cmp_mmm_help}()},
\code{\link{compatible}()},
\code{\link{ddd_prop_funs}()},
\code{\link{eee_help}()},
\code{\link{iii_help}()},
\code{\link{mmm_ccc_help}()},
\code{\link{mmm_help}()},
\code{\link{ppp_fast_help}()},
\code{\link{ppp_help}()},
\code{\link{sss_ccc_help}()},
\code{\link{sss_prop_funs}()},
\code{\link{unq_ccc_prop_funs}()},
\code{\link{unq_mmm_ccc_help}()},
\code{\link{unq_mmm_help}()}
}
\concept{properties}
