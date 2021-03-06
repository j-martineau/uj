% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/is_unique.R
\name{is_unique}
\alias{is_unique}
\title{Does an object have only unique atomic values?}
\usage{
is_unique(x, a = T, na = F)
}
\arguments{
\item{x}{An atomic object.}

\item{a}{\code{TRUE} or \code{FALSE} indicating whether to reduce \code{x} to
an atomic vector containing all of its atomic values. When \code{FALSE} and
\code{x} is not atomic, throws an error.}

\item{na}{\code{TRUE} or \code{FALSE} indicating whether \code{NA} values are
allowed.}
}
\value{
\code{TRUE} or \code{FALSE}.
}
\description{
Does an object have only unique atomic values?
}
\seealso{
Other is_functions: 
\code{\link{is_color}()},
\code{\link{is_failsafe}},
\code{\link{is_sortable}()},
\code{\link{is_xxx_array}},
\code{\link{is_xxx_generic}},
\code{\link{is_xxx_matrix}},
\code{\link{is_xxx_mvect}},
\code{\link{is_xxx_scalar}},
\code{\link{is_xxx_tibble}},
\code{\link{is_xxx_vect}},
\code{\link{is_xxx_vlist}},
\code{\link{is_xxx_vtype}},
\code{\link{logicals}}
}
\concept{is_functions}
