% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/comparable.R
\name{comparable}
\alias{comparable}
\title{Are objects comparable?}
\usage{
comparable(..., recycle = T)
}
\arguments{
\item{...}{An arbitrary number of arguments to be checked for comparability
with each other.}

\item{recycle}{A logical scalar indicating whether arguments in \code{...}
must be recyclable to be comparable.}
}
\value{
A logical scalar.
}
\description{
Are objects comparable?
}
\details{
Determines whether modes of all arguments in \code{...} are
comparable (i.e., sortable and compatible with each other), meaning that
all are character, logical, numeric, or ordered factor with the same set of
levels (in the same order).
}
\seealso{
Other props: 
\code{\link{ccc}()},
\code{\link{cmp_ccc}},
\code{\link{cmp_mmm_agn}},
\code{\link{cmp_mmm_arr}},
\code{\link{cmp_mmm_atb}},
\code{\link{cmp_mmm_avl}},
\code{\link{cmp_mmm_avt}},
\code{\link{cmp_mmm_gen}},
\code{\link{cmp_mmm_mat}},
\code{\link{cmp_mmm_mvc}},
\code{\link{cmp_mmm_scl}},
\code{\link{cmp_mmm_vec}},
\code{\link{cmp_mmm}},
\code{\link{compatible}()},
\code{\link{ddd}()},
\code{\link{eee}()},
\code{\link{fff}()},
\code{\link{meets}()},
\code{\link{mmm_agn}},
\code{\link{mmm_arr}},
\code{\link{mmm_atb}},
\code{\link{mmm_avl}},
\code{\link{mmm_avt}},
\code{\link{mmm_mat}},
\code{\link{mmm_mvc}},
\code{\link{mmm_scl}},
\code{\link{mmm_vec}},
\code{\link{mmm}()},
\code{\link{pop_xxx}},
\code{\link{sss}()},
\code{\link{ttt}()},
\code{\link{xxx}()}
}
\concept{props}
