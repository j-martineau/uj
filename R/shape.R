#' @name shape
#' @family props
#' @title Shape Property Family
#' @description An object's shape properties have to do with the number of
#'   \link[=ddim]{defined dimensions} and/or \link[=edim]{effective dimensions}
#'   plus some additional restrictions.
#'   \tabular{lll}{
#'     \emph{Value}\tab\emph{Name}\tab\emph{Requirements}
#'     \cr\code{'emp'}\tab empty      \tab non-\code{NULL}, length = 0.
#'     \cr\code{'pop'}\tab populated  \tab length > 0
#'     \cr\code{'pnt'}\tab point      \tab containing 1 element (for tibbles
#'                                         this means \code{1 × 1} in
#'                                         dimension).
#'     \cr\code{'lin'}\tab linear     \tab effectively 1-dimensional.
#'     \cr\code{'row'}\tab 1-row      \tab row matrix or tibble.
#'     \cr\code{'col'}\tab 1-column   \tab column matrix or tibbles.
#'     \cr\code{'rct'}\tab rectangular\tab matrix or tibble with multiple
#'                                         rows and multiple columns.
#'     \cr\code{'sqr'}\tab square     \tab square atomic matrix.
#'     \cr\code{'sld'}\tab solid      \tab effectively hyper-dimensional.
#'   }
#' @param x An object.
#' @param shapes A character scalar containing one or more values from
#'   \code{shape_vals()} separated by pipes and/or underscores (".").
#'   Combinations of shapes can be specified by separating them with
#'   underscores. Separating shapes or combinations of shapes with pipes will
#'   result in a value of \code{TRUE} if any of them applies to \code{x}.
#' @param ... Additional arguments to \code{\link{meets}} containing value and
#'   element/row/column count restrictions.
#' @details \strong{\code{shape_vals}}
#'   \cr Gets all valid shape properties.
#'   \cr\cr
#'   \strong{\code{shapes}}
#'   \cr Gets a vector of shape properties from \code{shape_vals()} that are
#'   applicable to \code{x}.
#'   \cr\cr
#'   \strong{\code{is_shape}}
#'   \cr Evaluates whether any property in \code{shapes} is a shape property
#'   applicable to \code{x}).
#'   \cr\cr
#'   \strong{\code{xsss}}
#'   \cr Evaluates whether \code{x} has the shape property represented by
#'   \code{sss}.
#'   \cr\cr
#'   \strong{Additional Arguments in \code{...}}
#'   \cr Submitting additional arguments to \code{is_shape} via \code{...}
#'   allows for checking not just the shape but whether length, number of rows,
#'   number of columns, and element values meet flexible criteria.
#' @return \code{shape_vals} and \code{shapes} return a character vector. All
#'   others return either \code{TRUE} or \code{FALSE}.
#' @export
shape_vals <- function() {
  x <- sort(c('emp', 'pnt', 'lin', 'row', 'col', 'rct', 'sqr', 'sld'))
  names(x) <- rep.int("shape", length(x))
  x
}

#' @rdname shape
#' @export
xemp <- function(x) {f0(length(x) != 0, F, !is.null(x))}

#' @rdname shape
#' @export
xpnt <- function(x) {edim(x) == 0}

#' @rdname shape
#' @export
xlin <- function(x) {edim(x) == 1}

#' @rdname shape
#' @export
xrow <- function(x) {f0(!ddim(x) == 2, F, nrow(x) == 1 & ncol(x) > 1)}

#' @rdname shape
#' @export
xcol <- function(x) {f0(!ddim(x) == 2, F, nrow(x) > 1 & ncol(x) == 1)}

#' @rdname shape
#' @export
xrct <- function(x) {f0(!ddim(x) == 2, F, nrow(x) > 1 & ncol(x) > 1)}

#' @rdname shape
#' @export
xsqr <- function(x) {f0(!ddim(x) == 2, F, nrow(x) > 1 & ncol(x) == nrow(x))}

#' @rdname shape
#' @export
xsld <- function(x) {edim(x) > 2}

#' @rdname shape
#' @export
shapes <- function(x) {
  c(if (xcol(x)) {'col'} else {NULL},
    if (xemp(x)) {'emp'} else {NULL},
    if (xlin(x)) {'lin'} else {NULL},
    if (xpnt(x)) {'pnt'} else {NULL},
    if (xrct(x)) {'rct'} else {NULL},
    if (xrow(x)) {'row'} else {NULL},
    if (xsld(x)) {'sld'} else {NULL},
    if (xsqr(x)) {'sqr'} else {NULL})
}

#' @rdname shape
#' @export
is_shape <- function(x, shapes, ...) {
  if (!cmp_chr_scl(x)) {stop("\n  * [shapes] must be a non-NA character scalar.")}
  ValidVec <- shape_vals()
  Combos   <- unlist(strsplit(shapes , "|", fixed = T)[[1]], T, F)
  PropVec  <- unlist(strsplit(Combos, ".", fixed = T)     , T, F)
  Valid    <- all(PropVec %in% ValidVec)
  if (!Valid) {stop("\n  * [shapes] contains a value not in shape_vals() (after splitting [shapes] along pipes and underscores).")}
  is_prop(x, shapes, ...)
}
