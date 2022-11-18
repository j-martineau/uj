#' @name ddd.
#' @family props
#' @title Defined dimensionality properties
#' @description An object's defined dimensionality is the number of dimensions
#'   on which its components can be indexed. The following table gives levels of
#'   defined dimensionality, property values assigned to levels of defined
#'   dimensionality, property names, and a definition of each value of defined
#'   dimensionality.\tabular{llll}{
#'           \tab DEFINED          \tab DEFINED          \tab                \cr
#' NUMBER    \tab DIMENSIONALITY   \tab DIMENSIONALITY   \tab CHARACTERISTICS\cr
#' OF DEFINED\tab PROPERTY         \tab PROPERTY         \tab OF QUALIFYING  \cr
#' DIMENSIONS   \tab VALUE         \tab NAME             \tab OBJECTS        \cr
#' \code{0}     \tab\code{'d0D'}   \tab 0-dimensional structure.
#'                                 \tab \code{NULL}.                         \cr
#' \code{1}     \tab\code{'d1D'}   \tab 1-dimensional structure.
#'                                 \tab Vector, \link[=ivls]{vlist},
#'                                      1-dimensional array.                 \cr
#' \code{2}     \tab\code{'d2D'}   \tab 2-dimensional structure.
#'                                 \tab data.frame or matrix.                \cr
#' \code{≥ 3}   \tab\code{'dHD'}   \tab Hyper-dimensional structure.
#'                                 \tab Array with 3+ dimensions.              }
#' Functions related to defined dimensionality are described in the following
#' table:\tabular{ll}{
#' FUNCTION          \tab WHAT THE                                           \cr
#' FORMAT            \tab FUNCTION DOES                                      \cr
#' \code{i[ddd]}     \tab Evaluates whether an object is of the defined
#'                        dimensionality represented by \code{[ddd]}.        \cr
#' \code{ddd}        \tab Gets a character scalar containing the defined
#'                        dimensionality of an object.                       \cr
#' \code{iddd}       \tab Evaluates an object for a specific defined
#'                        dimensionality and any additional properties specified
#'                        in \code{...}.                                     \cr
#' \code{ddd_vals}   \tab Gets a character vector of all possible defined
#'                        dimensionality property values.                      }
#' @param x An R object.
#' @param ddd A \link[=cmp_chr_scl]{complete character scalar} containing one or
#'   more values from \code{ddd_vals()} separated by pipes and/or underscores.
#'   Combinations of defined dimensionality properties can be specified by
#'   separating them with underscores. Separating defined dimensionality
#'   properties or combinations of defined dimensionality properties with pipes
#'   will result in a value of \code{TRUE} if any of them applies to \code{x}.
#' @inheritDotParams meets.
#' @inheritSection meets. Specifying Additional Property Requirements
#' @return \tabular{lll}{
#'   \code{nddd}                   \tab  \tab A non-negative whole-number
#'                                            scalar.                        \cr
#'   \code{ddd_vals} and \code{ddd}\tab  \tab A character scalar or vector.  \cr
#'   \code{iddd} and \code{i[ddd]} \tab  \tab A logical scalar.                }
#' @export
ddd. <- function() {help("ddd.", package = "uj")}

#' @rdname ddd.
#' @export
ddd <- function(x) {
  d <- f0(inll(x), 0, f0(is.vector(x), 1, length(dim(x))))
  f0(d == 0, 'd0D', f0(d == 1, 'd1D', f0(d == 2, 'd2D', f0(d >= 3, 'dHD', NULL))))
}

#' @rdname ddd.
#' @export
nddd <- function(x) {f0(inll(x), 0, f0(is.vector(x), 1, length(dim(x))))}

#' @rdname ddd.
#' @export
id0D <- function(x) {nddd(x) == 0}

#' @rdname ddd.
#' @export
id1D <- function(x) {nddd(x) == 1}

#' @rdname ddd.
#' @export
id2D <- function(x) {nddd(x) == 2}

#' @rdname ddd.
#' @export
idHD <- function(x) {nddd(x) > 2}

#' @rdname ddd.
#' @export
ddd_vals <- function() {c('d0D', 'd1D', 'd2D', 'dHD')}

#' @rdname ddd.
#' @export
iddd <- function(x, ddd, ...) {
  if (!cmp_chr_scl(ddd)) {stop("\n \u2022 [ddd] must be a non-NA character scalar.")}
  valid.ddd <- ttt_vals()
  ddd.combos <- strsplit(ddd, "|", fixed = T)[[1]]
  new.ddd <- unlist(strsplit(ddd.combos, "_", fixed = T))
  if (!all(new.ddd %in% valid.ddd)) {stop("\n \u2022 [ddd = '", ddd, "'] specifies a 'defined dimensionality' property not in [uj:ddd_vals() = c(", paste0(paste0("'", ddd_vals(), "'"), collapse = ", "), ")].")}
  ippp(x, ddd, ...)
}
