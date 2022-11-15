#' @name as_mmm.
#' @family props
#' @title Coerce to base mode or extended mode.
#' @param x An object
#' @param na \link[cmp_lgl_scl]{Complete logical scalar} indicating whether
#'   \code{NA} values qualify as missing color representations.
#' @param levs \link[cmp_vec]{Complete atomic vec} giving factor levels.
#' @export
as_mmm. <- function() {help("as_mmm.", package = "uj")}

#' @describeIn as_mmm. Coerce a valid R color representation to hexadecimal
#'   RGB character representation.
#' @export
as_clr <- function(x, na = F) {
  errs <- c(f0(ichr(x), NULL, "\n \u2022 [x] is not of mode character."),
            f0(isTF(na), NULL, "\n \u2022 [na] must be TRUE or FALSE."),
            f0(isF(na) & any(is.na(x)), NULL, "\n \u2022 [x] contains NA values but [na = FALSE]."))
  if (idef(errs)) {stop(errs)}
  if (any(!is.na(x))) {
    out <- tryCatch(col2rgb(x[!is.na(x)], T), error = function(e) e, finally = NULL)
    if (isERR(out)) {stop("\n \u2022 [x] does not contain only valid color values.")}
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
  if (!ifun(x)) {stop("\n \u2022 [x] is neither a function nor a character scalar name of a function.")}
  f0(is.function(x), x, match.fun(x))
}

#' @describeIn as_mmm. \code{as.character} wrapper.
#' @export
as_chr <- function(x, ...) {base::as.character(x, ...)}

#' @describeIn as_mmm. \code{as.integer} wrapper.
#' @export
as_int <- function(x, ...) {base::as.integer(x, ...)}

#' @describeIn as_mmm. \code{as.numeric} wrapper.
#' @export
as_num <- function(x, ...) {base::as.numeric(x, ...)}

#' @describeIn as_mmm. \code{as.logical} wrapper.
#' @export
as_lgl <- function(x, ...) {base::as.logical(x, ...)}

#' @describeIn as_mmm. Coerce to ordered factor.
#' @export
as_ord <- function(x, levs) {factor(av(x), levels = levs, ordered = T)}

#' @describeIn as_mmm. Coerce to unordered factor.
#' @export
as_uno <- function(x, levs) {factor(av(x), levels = levs, ordered = F)}
