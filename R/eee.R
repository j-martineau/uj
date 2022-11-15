#' @name eee.
#' @family props
#' @title Effective dimensionality properties
#' @description Effective dimensionality of a non-empty object is defined as
#'   the number of dimensions with multiple indexed positions. Effective
#'   dimensionality is undefined for empty objects. The following table
#'   describes values of effective dimensionality, property names assigned to
#'   them, and a definition of those values.\tabular{llll}{
#'   NUMBER OF    \tab EFFECTIVE        \tab EFFECTIVE
#'                                      \tab CHARACTERISTICS\cr
#'   EFFECTIVE    \tab DIMENSIONALITY   \tab DIMENSIONALITY   
#'                                      \tab OF QUALIFYING\cr
#'   DIMENSIONS   \tab PROPERTY VALUE   \tab PROPERTY NAME   
#'                                      \tab OBJECTS      \cr
#'   \code{NaN}\tab\code{'eUD'}\tab Effectively dimensionally undefined
#'                             \tab \code{NULL} or of length 0.              \cr
#'   \code{0}  \tab\code{'e0D'}\tab Effectively \code{0}-dimensional
#'                             \tab Vector of length 1, vlist of length 1, array
#'                                  of length 1, or code{1 × 1}
#'                                  \code{\link[is_dtf]{dtf}}.               \cr
#'   \code{1}  \tab\code{'e1D'}\tab Effectively \code{1}-dimensional
#'                             \tab Vector or vlist of length 2+ or
#'                                  non-empty array with multiple index
#'                                  positions in \code{1} dimension.         \cr
#'   \code{2}  \tab\code{'e2D'}\tab Effectively \code{2}-dimensional
#'                             \tab A \code{\link[is_dtf]{dtf}} with
#'                                  multiple rows and multiple columns, matrix
#'                                  with multiple rows and multiple columns,
#'                                  non-empty array with multiple index
#'                                  positions in only \code{2} dimensions.   \cr
#'   \code{≥ 3}\tab\code{'eHD'}\tab Effectively hyper-dimensional
#'                             \tab Non-empty array with multiple index
#'                                  positions in at least 3 dimensions.        }
#' Functions related to essential dimensionality are described in the following
#' table:\tabular{ll}{
#' FUNCTION          \tab WHAT THE                                           \cr
#' FORMAT            \tab FUNCTION DOES                                      \cr
#' \code{i***}       \tab Evaluates whether an object is of the essential
#'                        dimensionality represented by \code{***}.          \cr
#' \code{eee}        \tab Gets a character scalar containing the essential
#'                        dimensionality of an object.                       \cr
#' \code{ieee}       \tab Evaluates an object for a specific essential
#'                        dimensionality and any additional properties
#'                        specified in \code{...}.                           \cr
#' \code{eee_vals}   \tab Gets a character vector of all possible essential
#'                        dimensionality property values.                      }
#' @param x An object.
#' @param eee \link[cmp_chr_scl]{Complete character scalar} containing one or
#'   more values from \code{eee_vals()} separated by pipes and/or underscores.
#'   Combinations of effective dimensionality properties can be specified by
#'   underscore-delimiting them. Pipe-delimiting effective dimensionality
#'   properties or combinations of effective dimensionality will result in a
#'   value of \code{TRUE} if any of them applies to \code{x}.
#' @param ... Additional arguments to \code{\link{meets}} containing value and
#'   element/row/column count restrictions.
#' @section Additional Arguments in \code{...}: Submitting additional arguments
#'   to \code{is_eee} via \code{...} allows for checking not just the effective
#'   dimensionality but also whether length, number of rows, number of columns,
#'   and element values meet flexible criteria.
#' @return \code{eee_vals} and \code{eee} return a character scalar or vector of
#'   effective dimensionality property values. \code{eee(x)} returns \code{NaN}
#'   or a non-negative whole-number scalar. All others return either \code{TRUE}
#'   or \code{FALSE}.
#' @export
eee. <- function() {help("eee.", package = "uj")}

#' @describeIn eee. Get the effective dimensionality property of \code{x}
#' @export
eee <- function(x) {
  n <- f0(length(x) == 0, NaN,
          f0(NROW(x) * NCOL(x) == 1, 0,
             f0(is.vector(x), 1, length(which(dim(x) > 1)))))
  f0(is.nan(n), 'eUD',
     f0(0 == n, 'e0D',
        f0(1 == n, 'e1D',
           f0(2 == n, 'e2D',
              f0(3 <= n, 'eHD', NULL)))))
}

#' @describeIn eee. Gets the \strong{number} of effective dimensions of \code{x}
#'   (i.e., not a character scalar value from \code{eee_vals()}.
#' @export
neee <- function(x) {f0(length(x) == 0, NaN, f0(NROW(x) * NCOL(x) == 1, 0, f0(is.vector(x), 1, length(which(dim(x) > 1))))) }

#' @describeIn eee. Evaluate whether the effective dimensionality of \code{x}
#'   is \code{0}.
#' @export
ie0D <- function(x) {neee(x) == 0}

#' @describeIn eee. Evaluate whether the effective dimensionality of \code{x}
#'   is \code{1}.
#' @export
ie1D <- function(x) {neee(x) == 1}

#' @describeIn eee. Evaluate whether the effective dimensionality of \code{x}
#'   is \code{2}.
#' @export
ie2D <- function(x) {neee(x) == 2}

#' @describeIn eee. Evaluate whether the effective dimensionality of \code{x}
#'   is \code{3+}.
#' @export
ieHD <- function(x) {neee(x) > 2}

#' @describeIn eee. Evaluate whether the effective dimensionality of \code{x}
#'   is undefined, or \code{NaN}.
#' @export
ieUD <- function(x) {identical(eee(x), NaN)}

#' @describeIn eee. Get a character vector of all possible essential
#'   dimensionality property values.
#' @export
eee_vals <- function() {c('e0D', 'e1D', 'e2D', 'eHD', 'eUD')}

#' @describeIn eee. Evaluates whether any (combination) property in
#'   \code{eee} is an effective dimensionality property applicable to \code{x}
#' @export
ieee <- function(x, eee, ...) {
  if (!cmp_chr_scl(eee)) {stop("\n \u2022 [eee] must be a non-NA character scalar.")}
  valid.eee <- ttt_vals()
  eee.combos <- strsplit(eee, "|", fixed = T)[[1]]
  new.eee <- unlist(strsplit(eee.combos, "_", fixed = T))
  ok.eee <- all(new.eee %in% valid.eee)
  if (!ok.eee) {stop("\n \u2022 [eee = '", eee, "'] contains a value not in [sss_vals() = c(", paste0(paste0("'", sss_vals(), "'"), collapse = ", "), ")] after splitting along pipes and underscores.")}
  ippp(x, eee, ...)
}
