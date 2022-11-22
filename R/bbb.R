#' @name bbb
#' @family props
#' @title Base Properties
#' @description NOTE: \code{BBB} is used as a wildcard representing any given
#'   based property.
#'   \cr\cr
#'   An object's base properties are defined by its most basic
#'   structural properties as described in the following table: \tabular{lll}{
#'     PROPERTY     \tab PROPERTY   \tab QUALIFYING                          \cr
#'     NAME         \tab VALUE      \tab OBJECTS                             \cr
#'     atomic       \tab\code{'atm'}\tab Atomic objects.                     \cr
#'     defined      \tab\code{'def'}\tab Non-\code{NULL} objects.            \cr
#'     function     \tab\code{'fun'}\tab Function objects or character scalars
#'                                       containing a valid function name
#'                                       accessible to the calling function. \cr
#'     nil          \tab\code{'nil'}\tab Objects of nil (or zero) length.    \cr
#'     null         \tab\code{'nll'}\tab The \code{NULL} object.             \cr
#'     populated    \tab\code{'pop'}\tab Objects of length 1 or greater.     \cr
#'     recursive    \tab\code{'rcr'}\tab Recursive objects (data.frames and
#'                                       lists).                               }
#' @section Functions in This Family:
#'   \strong{\code{iBBB}}
#'   \cr Evaluates whether \code{x} matches the base property \code{BBB}
#'   (subject to any restrictions in \code{...}).
#'   \cr\cr
#'   \strong{\code{bbb}}
#'   \cr Gets a character vector containing all base properties matching
#'   \code{x}.
#'   \cr\cr
#'   \strong{\code{ibbb}}
#'   \cr Evaluates \code{x} against the base property specification in
#'   \code{spec} (subject to any restrictions in \code{...}).
#'   \cr\cr
#'   \strong{\code{bbb_props}}
#'   \cr Gets a character vector of all possible base property values.
#'   \cr\cr
#'   \strong{\code{is_bbb_spec}}
#'   \cr Evaluates whether \code{spec} is a valid base property specification.
#' @param x An R object.
#' @param spec \code{NULL} or a \link[=cmp_chr_scl]{complete character vec}
#'   containing one or more base properties (i.e., from
#'   \code{bbb_vals()}). \strong{NOTE}: properties may be pipe-separated. If
#'   If there are multiple properties in \code{spec}, \code{x} is inspected for
#'   a match to any of the specified properties.
#' @inheritDotParams meets
#' @inheritSection meets Specifying Count and Value Restrictions
#' @return \strong{\code{bbb_vals}}
#'   \cr A character vector.
#'   \cr\cr
#'   \strong{\code{bbb}}
#'   \cr A character scalar or character vector.
#'   \cr\cr
#'   \strong{\code{iBBB, ibbb, is_bbb_spec}}
#'   \cr A logical scalar.
#' @export
bbb <- function(x) {
  props <- .bbb_props()
  out <- NULL
  for (prop in props) {out <- c(out, f0(run('.is_', prop, '(x)'), prop, NULL))}
  out
}

#' @rdname bbb
#' @export
bbb_props <- function() {.bbb_props()}

#' @rdname bbb
#' @export
is_bbb_spec <- function(spec) {spec <- .spec_vals(); f0(length(spec) == 0, F, all(spec %in% .bbb_props()))}

#' @rdname bbb
#' @export
ibbb <- function(x, spec, ...) {
  errs <- c(.meets_errs(x, ...), f0(is_bbb_spec(spec), NULL, '\n \u2022 [spec] must be a complete character vec (?cmp_chr_vec) containing one or more (possible pipe-separated) values exclusively from bbb_props().'))
  if (!is.null(errs)) {stop(errs)}
  if (!meets(x, ...)) {return(F)}
  for (prop in .spec_vals(spec)) {if (run('.is_', prop, '(x)')) {return(T)}}
  F
}

#' @rdname bbb
#' @export
iatm <- function(x, ...) {ibbb(x, 'atm', ...)}

#' @rdname bbb
#' @export
idef <- function(x, ...) {ibbb(x, 'def', ...)}

#' @rdname bbb
#' @export
ifun <- function(x, ...) {ibbb(x, 'fun', ...)}

#' @rdname bbb
#' @export
inil <- function(x, ...) {ibbb(x, 'nill', ...)}

#' @rdname bbb
#' @export
inll <- function(x, ...) {ibbb(x, 'nll', ...)}

#' @rdname bbb
#' @export
ipop <- function(x, ...) {ibbb(x, 'pop', ...)}

#' @rdname bbb
#' @export
ircr <- function(x, ...) {ibbb(x, 'rcr', ...)}
