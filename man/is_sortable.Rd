% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/is_sortable.R
\name{is_sortable}
\alias{is_sortable}
\alias{is_nonsortable}
\title{Is an object and sortable or non-sortable?}
\usage{
is_sortable(x)

is_nonsortable(x)
}
\arguments{
\item{x}{An object}
}
\value{
\code{TRUE} or \code{FALSE}
}
\description{
Sortable objects are of non-empty atomic objects of mode
'character', 'logical', 'numeric', or 'ordered'. Non-sortable objects are
atomic objects of modes that are not sortable.
}
\seealso{
Other is_functions: 
\code{\link{is_color}()},
\code{\link{is_failsafe}},
\code{\link{is_unique}()},
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
