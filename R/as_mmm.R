#' @name as_mmm
#' @family props
#' @title Coerce to base mode or `xmode`
#' @description Functions to coerce objects to \link[base:mode]{base mode} or \code{\link[=mmm]{xmode}}:
#' \tabular{rl}{
#'       `as_chr`   \tab Thinly wraps \code{\link[base:as.character]{base::as.character}}.
#'   \cr `as_int`   \tab Thinly wraps \code{\link[base:as.integer]{base::as.integer}}.
#'   \cr `as_num`   \tab Thinly wraps \code{\link[base:as.numeric]{base::as.numeric}}.
#'   \cr `as_lgl`   \tab Thinly wraps \code{\link[base:as.logical]{base::as.logical}}.
#'   \cr `as_clr`   \tab Coerces valid R character color representation values to hexadecimal character RGB color values.
#'   \cr `as_fun`   \tab Returns `x` if it is a function, otherwise, search for a function named `x` and return it.
#'   \cr `as_ord`   \tab As `xmode 'ord'` (ordered factor). Wraps `base::factor(x, levels = levs, ordered = TRUE)`.
#'   \cr `as_uno`   \tab As `xmode 'uno'` (unordered factor). Wraps `base::factor(x, levels = levs, ordered = FALSE)`.
#' }
#' @param x For `as_clr`, an object of mode character; for `as_fun`, a character scalar function name or a function object; for `as_ord` and `as_uno`, an atomic object; and for all others, any R object.
#' @param na A non-`NA` logical scalar indicating whether `NA` values are acceptable.
#' @param levs A \link[=cmp_vec]{complete atomic vec} of factor levels (ordered factor levels for `as_ord`).
#' @param ... Further arguments passed to or from other methods.
#' @return \tabular{rl}{
#'       `as_fun`   \tab A function.
#'   \cr `as_chr`   \tab Object of mode `'character'`.
#'   \cr `as_int`   \tab Object of mode `'integer'`.
#'   \cr `as_lgl`   \tab Object of mode `'logical'`.
#'   \cr `as_num`   \tab Object of mode `'numeric'`.
#'   \cr `as_clr`   \tab Object of `xmode 'clr'` (color valued character).
#'   \cr `as_ord`   \tab Object of `xmode 'ord'` (ordered factor).
#'   \cr `as_ord`   \tab Object of `xmode 'uno'` (unordered factor).
#' }
#' @examples
#' bins. <- sample(c(0, 1), 10, replace = T)
#' chrs. <- c("3.14", "2.72", "1.41")
#' clrs. <- c("red", "#AABBCC", "#AABBCCDD", "blue")
#' nums. <- c(pi, exp(1), sqrt(2))
#'
#' as_fun(unique)
#' as_fun("unique")
#'
#' bins.
#' as_lgl(bins.)
#'
#' chrs.
#' as_num(chrs.)
#' as_int(chrs.)
#'
#' clrs.
#' as_clr(clrs.)
#'
#' nums.
#' as_chr(nums.)
#'
#' as_uno(clrs., levs = unique(clrs.))
#' as_ord(clrs., levs = sort(unique(clrs.)))
#' @export
as_clr <- function(x, na = FALSE) {
  errs <- c(f0(ichr(x)                 , NULL, "[x] is not of mode character."),
            f0(isTF(na)                , NULL, "[na] must be TRUE or FALSE."),
            f0(isF(na) | !any(is.na(x)), NULL, "[x] contains NA values but [na = FALSE]."))
  if (!is.null(errs)) {stop(.errs(errs))}
  if (any(!is.na(x))) {
    out <- tryCatch(col2rgb(x[!is.na(x)], T), error = function(e) e, finally = NULL)
    if (isERR(out)) {stop(.errs("[x] does not contain only valid color values."))}
    else {out <- out / 255}
    out <- rgb(out[1, ], out[2, ], out[3, ], out[4, ])
    x[!is.na(x)] <- out
  }
  x
}

#' @rdname as_mmm
#' @export
as_fun <- function(x) {
  if (is.function(x)) {return(x)}
  x <- tryCatch(match.fun(x), error = function(e) e, finally = NULL)
  if (any(class(x) %in% c("error", "simpleError"))) {stop(.errs("[x] is neither a function nor a character scalar name of a function."))}
  x
}

#' @rdname as_mmm
#' @export
as_chr <- function(x, ...) {base::as.character(x, ...)}

#' @rdname as_mmm
#' @export
as_int <- function(x, ...) {base::as.integer(x, ...)}

#' @rdname as_mmm
#' @export
as_num <- function(x, ...) {base::as.numeric(x, ...)}

#' @rdname as_mmm
#' @export
as_lgl <- function(x, ...) {base::as.logical(x, ...)}

#' @rdname as_mmm
#' @export
as_ord <- function(x, levs) {factor(x, levels = levs, ordered = T)}

#' @rdname as_mmm
#' @export
as_uno <- function(x, levs) {factor(x, levels = levs, ordered = F)}
