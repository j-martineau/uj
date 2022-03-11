#' @name as_xxx
#' @family meta
#' @title Extended \code{as} Property (xxx) Functions
#' @details \strong{\code{as_color}}
#'   \cr Coerce a valid ℝ color representation to hexadecimal RGB
#'   character representation.
#'   \cr\cr
#'   \strong{\code{as_fun}}
#'   \cr Return \code{x} if it is a function, otherwise, search for a function
#'   with the name contained in \code{x} and return that function.
#' @param x An object
#' @param na \code{TRUE} or \code{FALSE} indicating whether \code{NA} values
#'   qualify as missing color representations.
#' @export
as_color <- function(x, na = F) {
  E <- NULL
  if (!xchr(x)) {E <- c(E, "\n  * [x] is not of mode character.")}
  if (!isTF(na)) {E <- c(E, "\n  * [na] must be TRUE or FALSE.")}
  if (isF(na) & any(is.na(x))) {E <- c(E, "\n  * [x] contains NA values but [na = FALSE].")}
  if (xdef(E)) {stop(E)}
  if (any(!is.na(x))) {
    y <- tryCatch(col2rgb(x[!is.na(x)], T), error = function(e) e, finally = NULL)
    if (isERR(y)) {E <- c(E, "\n  * [x] does not contain only valid color values.")}
    else {y <- y / 255}
    y <- rgb(y[1, ], y[2, ], y[3, ], y[4, ])
    x[!is.na(x)] <- y
  }
  x
}

#' @rdname as_xxx
#' @export
as_fun <- function(x) {
  if (!xfun(x)) {stop("\n  * [x] is neither a function object nor the name of a function.")}
  f0(is.function(x), x, match.fun(x))
}
