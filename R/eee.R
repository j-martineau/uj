#' @family props
#' @title Effective Dimensionality (eee) Property Family
#' @description Get a character scalar effective dimensionality property
#'   applicable to \code{x.}.
#' @details Effective dimensionality of a non-empty object is defined as
#'   the number of dimensions with multiple indexed positions. Effective
#'   dimensionality is undefined for empty objects. The following table
#'   describes values of effective dimensionality, property names assigned to
#'   them, and a definition of those values.\tabular{llll}{
#'    NUMBER OF  \tab EFFECTIVE      \tab EFFECTIVE      \tab CHARACTERISTICS\cr
#'    EFFECTIVE  \tab DIMENSIONALITY \tab DIMENSIONALITY \tab OF QUALIFYING  \cr
#'    DIMENSIONS \tab PROPERTY VALUE \tab PROPERTY NAME  \tab OBJECTS        \cr
#'    \code{NaN} \tab \code{'eUD'}   \tab effectively dimensionally undefined
#'                                   \tab \code{NULL} or length 0.           \cr
#'    \code{0}   \tab \code{'e0D'}   \tab effectively \code{0}-dimensional
#'                                   \tab vector of length 1, vlist of length 1,
#'                                        array of length 1, or code{1 × 1}
#'                                        tibble.                            \cr
#'    \code{1}   \tab \code{'e1D'}   \tab effectively \code{1}-dimensional
#'                                   \tab vector or vlist of length 2+ or
#'                                        non-empty array with multiple index
#'                                        positions in \code{1} dimension.   \cr
#'    \code{2}   \tab \code{'e2D'}   \tab effectively \code{2}-dimensional
#'                                   \tab tibble with multiple rows and multiple
#'                                        columns, matrix with multiple rows and
#'                                        multiple columns, non-empty array with
#'                                        multiple index positions in only
#'                                        \code{2} dimensions.               \cr
#'    \code{≥ 3} \tab\code{'eHD'}    \tab effectively hyper-dimensional
#'                                   \tab non-empty array with multiple index
#'                                        positions in at least 3 dimensions.  }
#' @param x. An object.
#' @param xxx. A character scalar containing one or more values from
#'   \code{eee_vals()} separated by pipes and/or underscores. Combinations of
#'   effective dimensionality  properties can be specified by separating them
#'   with underscores. Separating effective dimensionality properties or
#'   combinations of effective dimensionality properties with pipes will result
#'   in a value of \code{TRUE} if any of them applies to \code{x}.
#' @param ... Additional arguments to \code{\link{meets}} containing value and
#'   element/row/column count restrictions.
#' @section Additional Arguments in \code{...}:
#'   Submitting additional arguments to \code{is_eee} via \code{...} allows
#'   for checking not just the effective dimensionality but also whether length,
#'   number of rows, number of columns, and element values meet flexible
#'   criteria.
#' @return \code{eee_vals} and \code{eee} return a character scalar or
#'   vector of effective dimensionality property values. \code{eee(x.)} returns
#'   \code{NaN} or a non-negative whole-number scalar. All others return either
#'   \code{TRUE} or \code{FALSE}.
#' @export
eee <- function(x.) {
  n. <- f0(length(x.) == 0, NaN, f0(NROW(x.) * NCOL(x.) == 1, 0, f0(is.vector(x.), 1, length(which(dim(x.) > 1)))))
  f0(is.nan(n.), 'eUD', f0(0 == n., 'e0D', f0(1 == n., 'e1D', f0(2 == n., 'e2D', f0(3 <= n., 'eHD', NULL)))))
}

#' @describeIn eee Gets the \strong{number} of effective dimensions of \code{x.}
#'   (i.e., not a character scalar value from \code{eee_vals()}.
#' @export
neee <- function(x.) {f0(length(x.) == 0, NaN, f0(NROW(x.) * NCOL(x.) == 1, 0, f0(is.vector(x.), 1, length(which(dim(x.) > 1))))) }

#' @describeIn eee Evaluate whether the effective dimensionality of \code{x.} is
#'   \code{0}.
#' @export
ie0D <- function(x.) {neee(x.) == 0}

#' @describeIn eee Evaluate whether the effective dimensionality of \code{x.} is
#'   \code{1}.
#' @export
ie1D <- function(x.) {neee(x.) == 1}

#' @describeIn eee Evaluate whether the effective dimensionality of \code{x.} is
#'   \code{2}.
#' @export
ie2D <- function(x.) {neee(x.) == 2}

#' @describeIn eee Evaluate whether the effective dimensionality of \code{x.} is
#'   \code{3+}.
#' @export
ieHD <- function(x.) {neee(x.) > 2}

#' @describeIn eee Evaluate whether the effective dimensionality of \code{x.} is
#'   undefined, or \code{NaN}.
#' @export
ieUD <- function(x.) {identical(eee(x.), NaN)}

#' @describeIn eee Get a character vector of all possible essential
#'   dimensionality property values.
#' @export
eee_vals <- function() {x. <- c('e0D', 'e1D', 'e2D', 'eHD', 'eUD'); names(x.) <- rep.int("eee", length(x.)); x.}

#' @describeIn eee Evaluates whether any (combination) property in \code{xxx.} is
#'   an effective dimensionality property applicable to \code{x.}
#' @export
ieee <- function(x., xxx., ...) {
  if (!cmp_chr_scl(x.)) {stop("\n • [xxx.] must be a complete character scalar.")}
  valid. <- eee_vals()
  combos. <- strsplit(xxx., "|", fixed = T)[[1]]
  newxxx. <- strsplit(combos., ".", fixed = T)[[1]]
  valid. <- all(newxxx. %in% valid.)
  if (!valid.) {stop("\n • [xxx.] contains a value not in eee_vals(), after splitting [xxx.] on pipes and underscores.")}
  ixxx(x., xxx., ...)
}
