#' @name as_mmm
#' @family props
#' @title Coerce to base or extended mode.
#' @description Functions to coerce objects to \link[base:mode]{base mode} or \link[=mmm]{extended mode}:
#' \itemize{
#'   \item **`as_clr`**: coerces a valid R color representation of mode character to hexadecimal RGB character representation.
#'   \item **`as_fun`**: returns `x` if it is a function, otherwise, search for a function named `x` and return it.
#'   \item **`as_chr`**: thinly wraps \code{\link[base:as.character]{base::as.character}}.
#'   \item **`as_int`**: thinly wraps \code{\link[base:as.integer]{base::as.integer}}.
#'   \item **`as_num`**: thinly wraps \code{\link[base:as.numeric]{base::as.numeric}}.
#'   \item **`as_lgl`**: thinly wraps \code{\link[base:as.logical]{base::as.logical}}.
#'   \item **`as_ord`**: wraps `base::factor(x, levels = levs, ordered = TRUE)`.
#'   \item **`as_uno`**: wraps `base::factor(x, levels = levs, ordered = FALSE)`.
#' }
#' @param x For `as_clr`, an object of mode character; for `as_fun`, a character scalar function name or a function object; for `as_ord` and `as_uno`, an atomic object; and for all others, any R object.
#' @param na A non-`NA` logical scalar indicating whether `NA` values qualify as missing color representations.
#' @param levs A \link[=cmp_vec]{complete atomic vec} of factor levels (ordered factor levels for `as_ord`).
#' @param ... Further arguments passed to or from other methods.
#' @return \itemize{
#'   \item **`as_fun`**: a function object.
#'   \item **`as_int`**: an object of mode `'integer'`.
#'   \item **`as_num`**: an object of mode `'numeric'`.
#'   \item **`as_lgl`**: an object of mode `'logical'`.
#'   \item **`as_ord`**: an object of mode `'ordered'`` (factor).
#'   \item **`as_uno`**: an object of \link[=mmm]{extended mode} `'uno'` (unordered factor).
#'   \item **`as_clr, as_chr`**: an object of mode `'character'`.
#' }
#' @export
as_clr <- function(x, na = F) {
  errs <- c(f0(ichr(x), NULL, "\n \u2022 [x] is not of mode character."),
            f0(isTF(na), NULL, "\n \u2022 [na] must be TRUE or FALSE."),
            f0(isF(na) & any(is.na(x)), NULL, "\n \u2022 [x] contains NA values but [na = FALSE]."))
  if (!is.null(errs)) {stop(errs)}
  if (any(!is.na(x))) {
    out <- tryCatch(col2rgb(x[!is.na(x)], T), error = function(e) e, finally = NULL)
    if (isERR(out)) {stop("\n \u2022 [x] does not contain only valid color values.")}
    else {out <- out / 255}
    out <- rgb(out[1, ], out[2, ], out[3, ], out[4, ])
    x[!is.na(x)] <- out
  }
  x
}

#' @rdname as_mmm
#' @export
as_fun <- function(x) {
  if (!ifun(x)) {stop("\n \u2022 [x] is neither a function nor a character scalar name of a function.")}
  f0(is.function(x), x, match.fun(x))
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
