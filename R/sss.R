#' @name sss
#' @family props
#' @title Shape properties
#' @description NOTE: \code{SSS} is used as a placeholder representing any given
#'   shape property.
#'   \cr\cr
#'   An object's shape properties have to do with the
#'   number of \link[=ddd]{defined dimensions} and/or \link[=eee]{effective
#'   dimensions} with some additional restrictions. All shape properties are
#'   described in the following table:\tabular{lll}{
#'     PROPERTY      \tab PROPERTY   \tab QUALIFYING                         \cr
#'     NAME          \tab VALUE      \tab OBJECTS                            \cr
#'     Empty         \tab\code{'emp'}\tab Non-\code{NULL}, length-1 objects. \cr
#'     Point         \tab\code{'pnt'}\tab Length-1 vectors, length-1
#'                                        \link[=ivls]{vlists}, length-1 arrays,
#'                                        and \code{1 x 1} data.frames.      \cr
#'     Linear        \tab\code{'lin'}\tab Length 2+ vectors, length 2+
#'                                        \link[=ivls]{vlists}, length 2+ arrays
#'                                        with multiple index positions in just
#'                                        1 dimension, and data.frames with 1
#'                                        row and multiple columns or 1 column
#'                                        and multiple rows.                 \cr
#'     Row           \tab\code{'row'}\tab Matrices and data.frames with 1 row
#'                                         and multiple columns.             \cr
#'     Column        \tab\code{'col'}\tab Matrices and data.frames with 1
#'                                         column and multiple rows          \cr
#'     Rectangular   \tab\code{'rct'}\tab Matrices and data.frames with multiple
#'                                        rows and multiple columns.         \cr
#'     Square        \tab\code{'sqr'}\tab Matrices and data.frames with multiple
#'                                        rows, multiple columns, and the same
#'                                        number of rows and columns.        \cr
#'     Solid         \tab\code{'sld'}\tab Arrays with multiple index positions
#'                                        in 3+ dimensions.                    }
#' @section Functions in this Family:
#'   \strong{\code{iSSS}}
#'   \cr Evaluates whether \code{x} matches the shape property \code{SSS}
#'   (subject to any restrictions in \code{...}).
#'   \cr\cr
#'   \strong{\code{sss}}
#'   \cr Gets a character vector containing all shape properties matching
#'   \code{x}.
#'   \cr\cr
#'   \strong{\code{isss}}
#'   \cr Evaluates \code{x} against the shape property specification in
#'   \code{spec} (subject to any restrictions in \code{...}).
#'   \cr\cr
#'   \strong{\code{sss_props}}
#'   \cr Gets a character vector of all possible shape property values.
#'   \cr\cr
#'   \strong{\code{is_sss_spec}}
#'   \cr Evaluates whether \code{spec} is a valid shape property specification.
#' @param x An R object.
#' @param spec \code{NULL} or a \link[=cmp_chr_scl]{complete character vec}
#'   containing one or more shape properties (i.e., from
#'   \code{sss_vals()}). \strong{NOTE}: properties may be pipe-separated. If
#'   If there are multiple properties in \code{spec}, \code{x} is inspected for
#'   a match to any of the specified properties.
#' @inheritDotParams meets
#' @inheritSection meets Specifying Count and Value Restrictions
#' @return \strong{\code{sss_vals}}
#'   \cr A character vector.
#'   \cr\cr
#'   \strong{\code{sss}}
#'   \cr A character scalar or character vector.
#'   \cr\cr
#'   \strong{\code{iSSS, isss, is_sss_spec}}
#'   \cr A logical scalar.
#' @export
sss <- function(x) {
  props <- .sss_props()
  out <- NULL
  for (prop in props) {out <- c(out, f0(run('.is_', prop, '(x)'), prop, NULL))}
  out
}

#' @rdname sss
#' @export
sss_props <- function() {.sss_props()}

#' @rdname sss
#' @export
is_sss_spec <- function(spec) {spec <- .spec_vals(); f0(length(spec) == 0, F, all(spec %in% .sss_props()))}

#' @rdname sss
#' @export
isss <- function(x, spec, ...) {
  errs <- c(.meets_errs(x, ...), f0(is_sss_spec(spec), NULL, '\n \u2022 [spec] must be a complete character vec (?cmp_chr_vec) containing one or more (possible pipe-separated) values exclusively from sss_props().'))
  if (!is.null(errs)) {stop(errs)}
  if (!meets(x, ...)) {return(F)}
  for (prop in .spec_vals(spec)) {if (run('.is_', prop, '(x)')) {return(T)}}
  F
}

#' @rdname sss
#' @export
icol <- function(x, ...) {isss(x, 'col', ...)}

#' @rdname sss
#' @export
iemp <- function(x, ...) {isss(x, 'emp', ...)}

#' @rdname sss
#' @export
ilin <- function(x, ...) {isss(x, 'lin', ...)}

#' @rdname sss
#' @export
ipnt <- function(x, ...) {isss(x, 'pnt', ...)}

#' @rdname sss
#' @export
irct <- function(x, ...) {isss(x, 'rct', ...)}

#' @rdname sss
#' @export
irow <- function(x, ...) {isss(x, 'row', ...)}

#' @rdname sss
#' @export
isld <- function(x, ...) {isss(x, 'sld', ...)}

#' @rdname sss
#' @export
isqr <- function(x, ...) {isss(x, 'sqr', ...)}
