#' @name as_mmm
#' @family props
#' @title Coerce to Base Mode or Extended Mode.
#' @description \tabular{ll}{
#'  FUNCTION    \tab WHAT IT DOES                                            \cr
#'   `as_clr`    \tab Coerce a valid R color representation of mode character to
#'                    hexadecimal RGB character representation.              \cr
#'   `as_fun`    \tab Return `x` if it is a function, otherwise, search for a
#'                    function named `x` and return it.                      \cr
#'   `as_chr`    \tab Thin wrapper of `as.character`.                        \cr
#'   `as_int`    \tab Thin wrapper of `as.integer`.                          \cr
#'   `as_num`    \tab Thin wrapper of `as.numeric`.                          \cr
#'   `as_lgl`    \tab Thin wrapper of `as.logical`.                          \cr
#'   `as_ord`    \tab Wrapper for `factor(x, levels = levs, ordered = TRUE)`.\cr
#'   `as_uno`    \tab Wrapper for `factor(x, levels = levs, ordered = FALSE)`. }
#' @param x For `as_clr`, an object of mode character; for `as_fun`, a
#'   character scalar function name or a function object; for `as_ord` and
#'   `as_uno`, an atomic object; and for all others, any R object.
#' @param na A \link[=cmp_lgl_scl]{complete logical scalar} indicating whether
#'   `NA` values qualify as missing color representations.
#' @param levs \link[=cmp_vec]{Complete atomic vec} giving factor levels.
#' @param ... Further arguments passed to or from other methods.
#' @return \tabular{ll}{
#'   FUNCTION             \tab RETURN VALUE                                  \cr
#'   `as_clr`, `as_chr`   \tab An object of mode `'character'`.              \cr
#'   `as_fun`             \tab A function object.                            \cr
#'   `as_int`             \tab An object of mode `'integer'`.                \cr
#'   `as_num`             \tab An object of mode `'numeric'`.                \cr
#'   `as_lgl`             \tab An object of mode `'logical'`.                \cr
#'   `as_ord`             \tab An object of mode `'ordered'`` (factor).      \cr
#'   `as_uno`             \tab An object of \link[=mmm]{extended mode} `'uno'`
#'                             (unordered factor).                             }
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
