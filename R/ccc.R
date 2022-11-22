#' @name ccc
#' @family props
#' @title Extended Class Properties
#' @description NOTE: \code{CCC} is used to represent any given extended class
#'   property.
#'   \cr\cr
#'   Extended class properties are not formally defined, but are dynamically
#'   evaluated, and defined as follows:\tabular{lll}{
#'     PROPERTY     \tab PROPERTY   \tab QUALIFYING                          \cr
#'     NAME         \tab VALUE      \tab OBJECTS                             \cr
#'     array        \tab\code{'arr'}\tab Arrays.                             \cr
#'     data.frame   \tab\code{'dtf'}\tab Data frames.                        \cr
#'     vlist        \tab\code{'vls'}\tab Vector-lists. (A)                   \cr
#'     generic      \tab\code{'gen'}\tab Vectors/vlists/arrays.              \cr
#'     matrix       \tab\code{'mat'}\tab Matrices.                           \cr
#'     scalar       \tab\code{'scl'}\tab Vectors/vlists/arrays of length-1.  \cr
#'     multivec     \tab\code{'mvc'}\tab Vectors/vlists of length 2+ and
#'                                       \link[=eee]{effectively 1D} arrays. \cr
#'     vec          \tab\code{'vec'}\tab Scalars and multivecs.                }
#'   (A) i.e., not a data.frame list.
#' @section Functions in This Family:
#'   \strong{\code{iCCC}}
#'   \cr Evaluates whether \code{x} matches the extended class property
#'   \code{CCC} (subject to any restrictions in \code{...}).
#'   \cr\cr
#'   \strong{\code{ccc}}
#'   \cr Gets a character vector containing all extended class properties
#'   matching \code{x}.
#'   \cr\cr
#'   \strong{\code{iccc}}
#'   \cr Evaluates \code{x} against the extended class property specification in
#'   \code{spec} (subject to any restrictions in \code{...}).
#'   \cr\cr
#'   \strong{\code{ccc_props}}
#'   \cr Gets a character vector of all possible extended class property values.
#'   \cr\cr
#'   \strong{\code{is_ccc_spec}}
#'   \cr Evaluates whether \code{spec} is a valid extended class property
#'   specification.
#' @param x An R object.
#' @param spec \code{NULL} or a \link[=cmp_chr_scl]{complete character vec}
#'   containing one or more extended class properties (i.e., from
#'   \code{ccc_vals()}). \strong{NOTE}: properties may be pipe-separated. If
#'   If there are multiple properties in \code{spec}, \code{x} is inspected for
#'   a match to any of the specified properties.
#' @inheritDotParams meets
#' @inheritSection meets Specifying Count and Value Restrictions
#' @return \strong{\code{ccc_vals}}
#'   \cr A character vector.
#'   \cr\cr
#'   \strong{\code{ccc}}
#'   \cr A character scalar or character vector.
#'   \cr\cr
#'   \strong{\code{iCCC, iccc, is_ccc_spec}}
#'   \cr A logical scalar.
#' @export
ccc <- function(x) {
  props <- .ccc_props()
  out <- NULL
  for (prop in props) {out <- c(out, f0(run('.is_', prop, '(x)'), prop, NULL))}
  out
}

#' @rdname ccc
#' @export
ccc_props <- function() {.ccc_props()}

#' @rdname ccc
#' @export
is_ccc_spec <- function(spec) {spec <- .spec_vals(); f0(length(spec) == 0, F, all(spec %in% .ccc_props()))}

#' @rdname ccc
#' @export
iccc <- function(x, spec, ...) {
  errs <- c(.meets_errs(x, ...), f0(is_ccc_spec(spec), NULL, '\n \u2022 [spec] must be a complete character vec (?cmp_chr_vec) containing one or more (possible pipe-separated) values exclusively from ccc_props().'))
  if (!is.null(errs)) {stop(errs)}
  if (!meets(x, ...)) {return(F)}
  for (prop in .spec_vals(spec)) {if (run('.is_', prop, '(x)')) {return(T)}}
  F
}

#' @rdname ccc
#' @export
iarr <- function(x, ...) {iccc(x, 'arr', ...)}

#' @rdname ccc
#' @export
idtf <- function(x, ...) {iccc(x, 'dtf', ...)}

#' @rdname ccc
#' @export
igen <- function(x, ...) {iccc(x, 'gen', ...)}

#' @rdname ccc
#' @export
imat <- function(x, ...) {iccc(x, 'mat', ...)}

#' @rdname ccc
#' @export
imvc <- function(x, ...) {iccc(x, 'mvc', ...)}

#' @rdname ccc
#' @export
iscl <- function(x, ...) {iccc(x, 'scl', ...)}

#' @rdname ccc
#' @export
ivec <- function(x, ...) {iccc(x, 'vec', ...)}

#' @rdname ccc
#' @export
ivls <- function(x, ...) {iccc(x, 'vls', ...)}
