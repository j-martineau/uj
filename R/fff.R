#' @name fff
#' @family props
#' @title Form (fff) Property Family
#' @description An object's form (fff) properties have to do with the number of
#'   \link[=ddd]{defined dimensions} and/or \link[=eee]{effective dimensions}
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
#' @param xxx A character scalar containing one or more values from
#'   \code{fff_vals()} separated by pipes and/or underscores (".").
#'   Combinations of forms can be specified by separating them with
#'   underscores. Separating forms or combinations of forms with pipes will
#'   result in a value of \code{TRUE} if any of them applies to \code{x}.
#' @param ... Additional arguments to \code{\link{meets}} containing value and
#'   element/row/column count restrictions.
#' @details \strong{\code{fff_vals}}
#'   \cr Gets all valid form properties.
#'   \cr\cr
#'   \strong{\code{fff}}
#'   \cr Gets a vector of form properties from \code{fff_vals()} that are
#'   applicable to \code{x}.
#'   \cr\cr
#'   \strong{\code{is_fff}}
#'   \cr Evaluates whether any property in \code{xxx} is a form property
#'   applicable to \code{x}).
#'   \cr\cr
#'   \strong{\code{xfff}}
#'   \cr Evaluates whether \code{x} has the form property represented by
#'   \code{fff}.
#'   \cr\cr
#'   \strong{Additional Arguments in \code{...}}
#'   \cr Submitting additional arguments to \code{is_fff} via \code{...}
#'   allows for checking not just the form but whether length, number of rows,
#'   number of columns, and element values meet flexible criteria.
#' @return \code{fff_vals} and \code{fff} return a character vector. All
#'   others return either \code{TRUE} or \code{FALSE}.
#' @export
fff_vals <- function() {
  x <- sort(c('emp', 'pnt', 'lin', 'row', 'col', 'rct', 'sqr', 'sld'))
  names(x) <- rep.int("fff", length(x))
  x
}

#' @rdname fff
#' @export
xemp <- function(x) {f0(length(x) != 0, F, !is.null(x))}

#' @rdname fff
#' @export
xpnt <- function(x) {eee(x) == 0}

#' @rdname fff
#' @export
xlin <- function(x) {eee(x) == 1}

#' @rdname fff
#' @export
xrow <- function(x) {f0(!ddd(x) == 2, F, nrow(x) == 1 & ncol(x) > 1)}

#' @rdname fff
#' @export
xcol <- function(x) {f0(!ddd(x) == 2, F, nrow(x) > 1 & ncol(x) == 1)}

#' @rdname fff
#' @export
xrct <- function(x) {f0(!ddd(x) == 2, F, nrow(x) > 1 & ncol(x) > 1)}

#' @rdname fff
#' @export
xsqr <- function(x) {f0(!ddd(x) == 2, F, nrow(x) > 1 & ncol(x) == nrow(x))}

#' @rdname fff
#' @export
xsld <- function(x) {eee(x) > 2}

#' @rdname fff
#' @export
fff <- function(x) {
  c(if (xcol(x)) {'col'} else {NULL},
    if (xemp(x)) {'emp'} else {NULL},
    if (xlin(x)) {'lin'} else {NULL},
    if (xpnt(x)) {'pnt'} else {NULL},
    if (xrct(x)) {'rct'} else {NULL},
    if (xrow(x)) {'row'} else {NULL},
    if (xsld(x)) {'sld'} else {NULL},
    if (xsqr(x)) {'sqr'} else {NULL})
}

#' @rdname fff
#' @export
is_fff <- function(x, xxx, ...) {
  if (!cmp_chr_scl(x)) {stop("\n  * [xxx] must be a non-NA character scalar.")}
  ValidVec <- fff_vals()
  Combos   <- unlist(strsplit(xxx , "|", fixed = T)[[1]], T, F)
  XXX      <- unlist(strsplit(Combos, ".", fixed = T)     , T, F)
  Valid    <- all(XXX %in% ValidVec)
  if (!Valid) {stop("\n  * [xxx] contains a value not in fff_vals() (after splitting [xxx] along pipes and underscores).")}
  is_xxx(x, xxx, ...)
}
