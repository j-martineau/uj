#' @name as_mmm.
#' @family props
#' @title Coerce to base mode or extended mode.
#' @param x An object
#' @param na \code{TRUE} or \code{FALSE} indicating whether \code{NA} values
#'   qualify as missing color representations.
#' @param levs Character vector giving ordered factor levels.
#' @export
as_mmm. <- function() {help("as_mmm.", package = "uj")}

#' @describeIn as_mmm. Coerce a valid ℝ color representation to hexadecimal
#'   RGB character representation.
#' @export
as_clr <- function(x, na = F) {
  err <- NULL
  if (!ichr(x )) {err <- c(err, "\n • [x] is not of mode character.")}
  if (!isTF(na)) {err <- c(err, "\n • [na] must be TRUE or FALSE.")}
  if (isF(na) & any(is.na(x))) {err <- c(err, "\n • [x] contains NA values but [na = FALSE].")}
  if (idef(err)) {stop(err)}
  if (any(!is.na(x))) {
    out <- tryCatch(col2rgb(x[!is.na(x)], T), error = function(e) e, finally = NULL)
    if (isERR(out)) {err <- c(err, "\n • [x] does not contain only valid color values.")}
    else {out <- out / 255}
    out <- rgb(out[1, ], out[2, ], out[3, ], out[4, ])
    x[!is.na(x)] <- out
  }
  x
}

#' @describeIn as_mmm. Return \code{x} if it is a function, otherwise, search
#'   for a function named \code{x} and return that function.
#' @export
as_fun <- function(x) {
  if (!ifun(x)) {stop("\n • [x] is neither a function nor a character scalar name of a function.")}
  f0(is.function(x), x, match.fun(x))
}

#' @describeIn as_mmm. \code{as.character} wrapper.
#' @export
as_chr <- base::as.character

#' @describeIn as_mmm. \code{as.integer} wrapper.
#' @export
as_int <- base::as.integer

#' @describeIn as_mmm. \code{as.numeric} wrapper.
#' @export
as_num <- base::as.numeric

#' @describeIn as_mmm. \code{as.logical} wrapper.
#' @export
as_lgl <- base::as.logical

#' @describeIn as_mmm. Coerce to ordered factor.
#' @export
as_ord <- function(x, levs) {factor(av(x), levels = levs, ordered = T)}

#' @describeIn as_mmm. Coerce to unordered factor.
#' @export
as_uno <- function(x, levs) {factor(av(x), levels = levs, ordered = F)}
