% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ply.R
\encoding{UTF-8}
\name{ply}
\alias{ply}
\alias{none_ply}
\alias{any_ply}
\alias{some_ply}
\alias{many_ply}
\alias{all_ply}
\alias{one_ply}
\alias{two_ply}
\alias{atm_ply}
\alias{mvc_ply}
\alias{vec_ply}
\alias{row_ply}
\alias{col_ply}
\alias{dim_ply}
\alias{vls_ply}
\title{Variations on \code{apply} functions}
\usage{
ply(.X, .FUN, ..., .DIM = 0, .PROC = NULL)

none_ply(.X, .FUN, ..., .DIM = 0, .PROC = NULL)

any_ply(.X, .FUN, ..., .DIM = 0, .PROC = NULL)

some_ply(.X, .FUN, ..., .DIM = 0, .PROC = NULL)

many_ply(.X, .FUN, ..., .DIM = 0, .PROC = NULL)

all_ply(.X, .FUN, ..., .DIM = 0, .PROC = NULL)

one_ply(.X, .FUN, ..., .DIM = 0, .PROC = NULL)

two_ply(.X, .FUN, ..., .DIM = 0, .PROC = NULL)

atm_ply(.X, .FUN, ..., .PROC = NULL)

mvc_ply(.X, .FUN, ..., .PROC = NULL)

vec_ply(.X, .FUN, ..., .PROC = NULL)

row_ply(.X, .FUN, ..., .PROC = NULL)

col_ply(.X, .FUN, ..., .PROC = NULL)

dim_ply(.X, .FUN, ..., .PROC = NULL)

vls_ply(.X, .FUN, ..., .PROC = NULL)
}
\arguments{
\item{.X}{An object to apply \code{.FUN} to.}

\item{.FUN}{Function or character scalar name of a function to apply to \code{.X}.}

\item{...}{An arbitrary number of additional arguments to be passed to the function \code{.FUN}.}

\item{.DIM}{A \link[=cmp_nnw_vec]{complete non-negative whole-number vec} giving dimension(s) of \code{.X} to apply the function \code{.FUN} to (\code{0} indicates applying to elements of a vector or \link[=VLS]{vlist} vs. applying to every cell for arrays and data.frames).}

\item{.PROC}{Either \code{NULL} or a list of named elements with processing instructions. See \emph{the} \code{.PROC} \emph{argument}.}
}
\description{
Apply a function over elements, rows, columns, or all dimensions; to an atomic object, a \link[=MVC]{multivec}, \link[=VEC]{vec}, \link[=VLS]{vlist}, matrix, or data.frame; and/or check the number of \code{TRUE} values in the result.
}
\details{
\tabular{ll}{  \code{none_ply}   \tab Check for \code{0} resulting \code{TRUE} values\eqn{^{(1)}}.               \cr
  \code{one_ply}    \tab Check for \code{1} resulting \code{TRUE} values\eqn{^{(1)}}.               \cr
  \code{two_ply}    \tab Check for \verb{2+} resulting \code{TRUE} values\eqn{^{(1)}}.              \cr
  \code{any_ply}    \tab Check for \verb{1+} resulting \code{TRUE} values\eqn{^{(1)}}.              \cr
  \code{some_ply}   \tab Check for \verb{2+} resulting \code{TRUE} values\eqn{^{(1)}}.              \cr
  \code{many_ply}   \tab Check for \verb{3+} resulting \code{TRUE} values\eqn{^{(1)}}.              \cr
  \code{all_ply}    \tab Check for \emph{only} resulting \code{TRUE} values\eqn{^{(1)}}.            \cr   \tab   \cr
  \code{atm_ply}    \tab Apply \code{.FUN} to \link[=av]{atomized} \code{.X}.                         \cr
  \code{mvc_ply}    \tab Apply \code{.FUN} to elements of \link[=atm_mvc]{atomic multivec} \code{.X}. \cr
  \code{vec_ply}    \tab Apply \code{.FUN} to elements of \link[=atm_vec]{atomic vec} \code{.X}.      \cr
  \code{vls_ply}    \tab Apply \code{.FUN} to elements of \link[=atm_vls]{atomic vlist} \code{.X}.    \cr   \tab   \cr
  \code{row_ply}    \tab Apply \code{.FUN} to rows of \code{.X}.                                      \cr
  \code{col_ply}    \tab Apply \code{.FUN} to columns of \code{.X}.                                   \cr
  \code{dim_ply}    \tab Apply \code{.FUN} to cells of \code{.X}.                                     \cr   \tab   \cr
  \code{ply}        \tab Generalized \code{ply} function.                                      \cr   \tab     }
\tabular{l}{  \eqn{^{(1)}} These functions assume that applying \code{.FUN} produces \code{'logical'} results.             }
}
\section{The \code{.PROC} argument}{
 When not \code{NULL}, the \code{.PROC} argument is an optional list with up to seven named elements, which give processing instructions as follows:
\tabular{ll}{  \strong{Name + value}    \tab \strong{Processing instructions}                                  \cr
  \verb{$arg = '\{spec\}'}   \tab Check \code{.X} for match to \code{\link[=ppp]{spec}}\eqn{^{(1)}}. \cr
  \verb{$out = '\{spec\}'}   \tab Check result for match to \code{'{spec}'}.\eqn{^{(1)}}            \cr   \tab   \cr
  \verb{$agg = 'none'}     \tab Inspect result for \code{0} \code{TRUE} values.                        \cr
  \verb{$agg = 'one'}      \tab Inspect result for \code{1} \code{TRUE} values.                        \cr
  \verb{$agg = 'two'}      \tab Inspect result for \verb{2+} \code{TRUE} values.                       \cr
  \verb{$agg = 'any'}      \tab Inspect result for \emph{any} \code{TRUE} values.                      \cr
  \verb{$agg = 'all'}      \tab Inspect result for \emph{only} \code{TRUE} values.                     \cr   \tab   \cr
  \verb{$na = 'err'}       \tab Throw error if result has any \code{NA} values.                   \cr
  \verb{$na = FALSE}       \tab Replace resulting \code{NA}s with \code{FALSE}.                        \cr
  \verb{$na = TRUE}        \tab Replace resulting \code{NA}s with \code{TRUE}.                         \cr   \tab   \cr
  \verb{$a1 = TRUE}        \tab \link[=av]{Atomize} \code{.X}.                                     \cr
  \verb{$a2 = TRUE}        \tab Atomize the result.                                          \cr   \tab   \cr
  \verb{$s = TRUE}         \tab \link[base:simplify2array]{Simplify} the result.             \cr   \tab     }
\tabular{l}{  \eqn{^{(1)}} \code{{spec}} is a placeholder for a \link[=is_prop_spec]{valid property spec}.              }
}

\examples{
egNumVec <- 1:5
egNumMat <- matrix(1:25, nrow = 5)
egChrDtf <- dtf(az = letters[1:10], AZ = LETTERS[1:10], nm = as.character(0:9))
egChrVls <- list(az = letters, AZ = LETTERS, nm = as.character(0:9))
egComp <- function(.X, comp, value) {if (comp == "<") {.X < value} else {.X > value}}

egNumVec
egNumMat
egChrDtf
egChrVls

none_ply(egNumVec, egComp, ">", value = 6)
none_ply(egNumVec, egComp, ">", value = 4)
any_ply(egNumVec, egComp, ">", value = 6)
any_ply(egNumVec, egComp, ">", value = 4)
all_ply(egNumVec, egComp, "<", value = 6)
all_ply(egNumVec, egComp, "<", value = 4)
one_ply(egNumVec, egComp, ">", value = 6)
one_ply(egNumVec, egComp, ">", value = 4)
two_ply(egNumVec, egComp, ">", value = 2)
two_ply(egNumVec, egComp, ">", value = 5)
two_ply(egNumMat, egComp, ">", value = 15, .DIM = 2)
dim_ply(egNumMat, egComp, ">", value = 15)
dim_ply(egChrDtf, toupper)
row_ply(egNumMat, sum)
col_ply(egChrDtf, paste0, collapse = "")
vls_ply(egChrVls, paste0, collapse = "")
}
\seealso{
Other meta: 
\code{\link{callers}()},
\code{\link{dot_args}()},
\code{\link{pause}()},
\code{\link{purge}()},
\code{\link{recyclable_ns}()},
\code{\link{run}()},
\code{\link{say}()}
}
\concept{meta}
