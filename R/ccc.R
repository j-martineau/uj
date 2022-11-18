#' @name ccc.
#' @family props
#' @title Extended class properties
#' @description Extended classes are not formally defined, but are dynamically
#' evaluated. Extended classes come in two varieties: universal and atomic. The
#' following two tables define each extended class in universal and atomic form,
#' respectively.:\tabular{lll}{
#' EXTENDED     \tab PROPERTY     \tab QUALIFYING                            \cr
#' CLASS        \tab VALUE        \tab OBJECTS                               \cr
#' vlist        \tab \code{'vls'} \tab Vector-list (i.e., not a data.frame). \cr
#' data.frame   \tab \code{'dtf'} \tab Data.frame.                           \cr
#' generic      \tab \code{'gen'} \tab Vector, array, or vlist.              \cr
#' matrix       \tab \code{'mat'} \tab Matrix.                               \cr
#' array+       \tab \code{'arr'} \tab Array or vector of any length.        \cr
#' scalar       \tab \code{'scl'} \tab Array or vector of any length.        \cr
#' vector+      \tab \code{'vec'} \tab Vector of length 1+ or array of length 1+
#'                                     with multiple index positions in either 0
#'                                     or 1 dimensions.                      \cr
#' multivec     \tab \code{'mvc'} \tab Vector of length 2+ or array of length 2+
#'                                     with multiple index positions in exactly
#'                                     1 dimension.                            }
#' Functions related to extended are described in the following
#' table:\tabular{ll}{
#' FUNCTION          \tab WHAT THE                                           \cr
#' FORMAT            \tab FUNCTION DOES                                      \cr
#' \code{i[ccc]}       \tab Evaluates whether an object is of the extended class
#'                        represented by \code{[ccc]}.                       \cr
#' \code{ccc}        \tab Gets a character vector containing all extended class
#'                        properties of an object.                           \cr
#' \code{iccc}       \tab Evaluates an object for a specific extended class and
#'                        any additional properties specified in \code{...}. \cr
#' \code{ccc_vals}   \tab Gets a character vector of all possible extended class
#'                        type property values.                                }
#' @param x An object.
#' @param ccc \code{NULL} or \link[=cmp_chr_scl]{complete character scalar}
#'   containing one or more values from \code{ccc_vals()} separated by pipes
#'   and/or underscores. Combinations of extended classes can be specified by
#'   separating them with underscores. Separating extended classes or
#'   combinations of extended classes with pipes will result in a value of
#'   \code{TRUE} if any of them applies to \code{x}.
#' @inheritDotParams meets.
#' @inheritSection meets. Specifying Additional Property Requirements
#' @return \tabular{lll}{
#'   \code{ccc_vals}              \tab   \tab A character vector.            \cr
#'   \code{ccc}                   \tab   \tab A character scalar or vector.  \cr
#'   \code{iccc} and \code{i[ccc]}\tab   \tab A logical scalar.                }
#' @export
ccc. <- function() {help("ccc.", package = "uj")}

#' @rdname ccc.
#' @export
iarr <- function(x) {is.array(x) | is.vector(x)}

#' @rdname ccc.
#' @export
idtf <- function(x) {is.data.frame(x)}

#' @rdname ccc.
#' @export
igen <- function(x) {is.vector(x) | is.array(x) | (is.list(x) & !is.data.frame(x))}

#' @rdname ccc.
#' @export
imat <- function(x) {is.matrix(x)}

#' @rdname ccc.
#' @export
imvc <- function(x) {length(x) > 1 & (is.vector(x) | (is.array(x) & length(which(dim(x) > 1)) == 1))}

#' @rdname ccc.
#' @export
iscl <- function(x) {length(x) == 1 & (is.array(x) | is.vector(x))}

#' @rdname ccc.
#' @export
ivec <- function(x) {length(x) > 0 & (is.vector(x) | (is.array(x) & length(which(dim(x) > 1)) <= 1))}

#' @rdname ccc.
#' @export
ivls <- function(x) {is.list(x) & !is.data.frame(x)}

#' @rdname ccc.
#' @export
ccc_vals <- function() {c('arr', 'dtf', 'gen', 'mat', 'mvc', 'scl', 'vec', 'vls')}

#' @rdname ccc.
#' @export
ccc <- function(x) {c(f0(iarr(x), 'arr', NULL), f0(idtf(x), 'dtf', NULL), f0(igen(x), 'gen', NULL), f0(imat(x), 'mat', NULL), f0(imvc(x), 'mvc', NULL), f0(iscl(x), 'scl', NULL), f0(ivec(x), 'vec', NULL), f0(ivls(x), 'vls', NULL))}

#' @rdname ccc.
#' @export
iccc <- function(x, ccc, ...) {
  if (!cmp_chr_scl(ccc)) {stop("\n \u2022 [ccc] must be a non-NA character scalar.")}
  valid.ccc <- ttt_vals()
  ccc.combos <- strsplit(ccc, "|", fixed = T)[[1]]
  new.ccc <- unlist(strsplit(ccc.combos, "_", fixed = T))
  if (!all(new.ccc %in% valid.ccc)) {stop("\n \u2022 [ccc = '", ccc, "'] specifies an 'extended class' property not in [uj::ccc_vals() = c(", paste0(paste0("'", ccc_vals(), "'"), collapse = ", "), ")].")}
  ippp(x, ccc, ...)
}
