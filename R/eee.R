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
#'                                  of length 1, or code{1 × 1} data.frame   \cr
#'   \code{1}  \tab\code{'e1D'}\tab Effectively \code{1}-dimensional
#'                             \tab Vector or vlist of length 2+ or
#'                                  non-empty array with multiple index
#'                                  positions in \code{1} dimension.         \cr
#'   \code{2}  \tab\code{'e2D'}\tab Effectively \code{2}-dimensional
#'                             \tab Data frames or matrices with multiple rows
#'                                  and multiple columns and non-empty arrays
#'                                  with multiple index positions in exactly
#'                                  \code{2} dimensions.                     \cr
#'   \code{≥ 3}\tab\code{'eHD'}\tab Effectively hyper-dimensional
#'                             \tab Non-empty array with multiple index
#'                                  positions in at least 3 dimensions.        }
#' Functions related to effective dimensionality are described in the following
#' table:\tabular{ll}{
#' FUNCTION          \tab WHAT THE                                           \cr
#' FORMAT            \tab FUNCTION DOES                                      \cr
#' \code{i[eee]}     \tab Evaluates whether an object is of the effective
#'                        dimensionality represented by \code{[eee]}.        \cr
#' \code{eee}        \tab Gets a character scalar containing the effective
#'                        dimensionality of an object.                       \cr
#' \code{ieee}       \tab Evaluates an object for a specific effective
#'                        dimensionality and any additional properties
#'                        specified in \code{...}.                           \cr
#' \code{neee}       \tab Gets a whole-number scalar giving the number of
#'                        effective dimensions.                              \cr
#' \code{eee_vals}   \tab Gets a character vector of all possible effective
#'                        dimensionality property values.                      }
#' @param x An object.
#' @param eee \link[=cmp_chr_scl]{Complete character scalar} containing one or
#'   more values from \code{eee_vals()} separated by pipes and/or underscores.
#'   Combinations of effective dimensionality properties can be specified by
#'   underscore-delimiting them. Pipe-delimiting effective dimensionality
#'   properties or combinations of effective dimensionality will result in a
#'   value of \code{TRUE} if any of them applies to \code{x}.
#' @inheritDotParams meets.
#' @inheritSection meets. Specifying Additional Property Requirements
#' @return \tabular{lll}{
#'   \code{eee_vals} and \code{eee}\tab   \tab A character scalar or vector. \cr
#'   \code{neee(x)}                \tab   \tab \code{NaN} or a non-negative
#'                                             whole-number scalar.          \cr
#'   \code{ieee} and \code{i[eee]} \tab   \tab A logical scalar.               }
#' @export
eee. <- function() {help("eee.", package = "uj")}

#' @rdname eee.
#' @export
eee <- function(x) {
  n <- f0(length(x) == 0, NaN, f0(NROW(x) * NCOL(x) == 1, 0, f0(is.vector(x), 1, length(which(dim(x) > 1)))))
  f0(is.nan(n), 'eUD', f0(0 == n, 'e0D', f0(1 == n, 'e1D', f0(2 == n, 'e2D', f0(3 <= n, 'eHD', NULL)))))
}

#' @rdname eee.
#' @export
neee <- function(x) {f0(length(x) == 0, NaN, f0(NROW(x) * NCOL(x) == 1, 0, f0(is.vector(x), 1, length(which(dim(x) > 1)))))}

#' @rdname eee.
#' @export
ie0D <- function(x) {neee(x) == 0}

#' @rdname eee.
#' @export
ie1D <- function(x) {neee(x) == 1}

#' @rdname eee.
#' @export
ie2D <- function(x) {neee(x) == 2}

#' @rdname eee.
#' @export
ieHD <- function(x) {neee(x) > 2}

#' @rdname eee.
#' @export
ieUD <- function(x) {identical(eee(x), NaN)}

#' @rdname eee.
#' @export
eee_vals <- function() {c('e0D', 'e1D', 'e2D', 'eHD', 'eUD')}

#' @rdname eee.
#' @export
ieee <- function(x, eee, ...) {
  if (!cmp_chr_scl(eee)) {stop("\n \u2022 [eee] must be a non-NA character scalar.")}
  valid.eee <- ttt_vals()
  eee.combos <- strsplit(eee, "|", fixed = T)[[1]]
  new.eee <- unlist(strsplit(eee.combos, "_", fixed = T))
  if (!all(new.eee %in% valid.eee)) {stop("\n \u2022 [eee = '", eee, "'] specifies an 'effective dimensionality' property not in [uj::eee_vals() = c(", paste0(paste0("'", eee_vals(), "'"), collapse = ", "), ")].")}
  ippp(x, eee, ...)
}
