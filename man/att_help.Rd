% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/att.R
\encoding{UTF-8}
\name{att_help}
\alias{att_help}
\alias{att}
\alias{atts}
\alias{att_names}
\alias{is_att}
\alias{add_att}
\alias{set_att}
\alias{rm_att}
\alias{xatt}
\alias{rm_atts}
\alias{xatts}
\title{Attribute Management Functions}
\usage{
att_help()

att(.x, ..., .a = FALSE)

atts(x)

att_names(x)

is_att(x, name)

add_att(.x, ...)

set_att(.x, ...)

rm_att(x, ...)

xatt(x, ...)

rm_atts(x)

xatts(x)
}
\arguments{
\item{...}{For \code{add_att} and \code{set_att}, named arguments. For \code{rm_att/xatt} and \code{att} optional character objects (also optionally pipe delimited). When none are provided, indicates all attributes of \code{.x}.}

\item{.a}{\code{TRUE} or \code{FALSE} indicating whether to \link[=av]{atomize} the result and convert the resulting vector to character.}

\item{x, .x}{An R object.}

\item{name}{A complete character scalar (?cmp_chr_scl).}
}
\description{
Get attributes of an object, names of attributes of an object, determine whether there is a named attribute of an object, add attributes to an object, modify values of attributes of an object, and remove attributes of an object.
}
\section{Functions}{
\itemize{
\item \code{att()}: Get the attributes of \code{.x} named in \code{...}, optionally \link[=av]{atomizing} the result when \code{.a = TRUE}. When there are no \code{...} args, gets all attributes of \code{.x}. Return value varies depending on \code{x} and \code{...}.

\item \code{atts()}: Gets the attributes of \code{x}. Thinly wraps \code{attributes(x)}. Returns either \code{NULL} or a named list.

\item \code{att_names()}: Gets the names of attributes of \code{x}. Thinly wraps \code{names(attributes(x))}. Returns either \code{NULL} or a character vector.

\item \code{is_att()}: Evaluates whether \code{name} is the name of an attribute of \code{x}. Returns a logical scalar.

\item \code{add_att()}: Adds attributes to \code{.x} as specified in \code{...} where attribute names are the names of \code{...} args and attribute values are the values of \code{...} args. Returns \code{x} with added attributes as specified in \code{...}.

\item \code{set_att()}: Sets attributes of \code{.x} to the values specified in \code{...} where names of modified attributes are the names of \code{...} args and the values of modified attributes are the values of \code{...} args. Returns \code{x} with modified attribute values.

\item \code{rm_att()}: Removes from \code{x} any attributes named in \code{...} (where \code{...} is \link[=av]{atomized} to a character vector and split along pipes (\code{'|'})). When there are no \code{...} args, removes all attributes from \code{x}. Returns \code{x} with the specified attributes removed.

\item \code{xatt()}: An alias for \code{rm_att}.

\item \code{rm_atts()}: Removes all attributes from \code{x}. Returns \code{x} with all attributes removed.

\item \code{xatts()}: An alias for \code{rm_atts}.

}}
\examples{
egAtt <- function() {
  x1 <- matrix(
    NA,
    nrow = 2,
    ncol = 2,
    dimnames = list(rows = c("r1", "r3"), cols = c("c1", "c2"))
  )
  x2 <- add_att(x1, added = "added attribute")
  x3 <- set_att(x2, added = "new attribute value")
  x4 <- rm_att(x2, "added")
  x5 <- xatt(x2)
  x6 <- xatts(x2)
  list(
    x1 = x1, x2 = x2, x3 = x3, x4 = x4, x5 = x5, x6 = x6,
    att.names.x1 = att_names(x1), att.names.x2 = att_names(x2),
    att.names.x3 = att_names(x3), att.names.x4 = att_names(x4),
    att.names.x5 = att_names(x5), att.names.x6 = att_names(x6),
    atts.x1 = atts(x1), atts.x2 = atts(x2), atts.x3 = atts(x3),
    atts.x4 = atts(x4), atts.x5 = atts(x5), atts.x6 = atts(x6),
    att.x2.added = att(x2, "added"), att.x2.a = att(x2, .a = T),
    att.x2 = att(x2), is.att.x1.added = is_att(x1, "added"),
    is.att.x2.added = is_att(x2, "added")
  )
}
egAtt <- egAtt()
egAtt
}
