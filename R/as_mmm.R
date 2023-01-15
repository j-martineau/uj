#' @name as_mmm
#' @encoding UTF-8
#' @family properties
#' @title Coerce to base mode or `xmode`
#' @description Functions to coerce objects to \link[base:mode]{base mode} or \code{\link[=mmm]{xmode}}:
#' \tabular{rl}{
#'       `as_chr`   \tab Thinly wraps \code{\link[base:as.character]{base::as.character}}.
#'   \cr `as_int`   \tab Thinly wraps \code{\link[base:as.integer]{base::as.integer}}.
#'   \cr `as_num`   \tab Thinly wraps \code{\link[base:as.numeric]{base::as.numeric}}.
#'   \cr `as_lgl`   \tab Thinly wraps \code{\link[base:as.logical]{base::as.logical}}.
#'   \cr `as_clr`   \tab Coerces R character colors to hexadecimal character RGB colors.
#'   \cr `as_fun`   \tab If `x` is a character scalar function return that function, but if `x` is a function object, return it.
#'   \cr `as_ord`   \tab As xmode `'ord'` (ordered factor). Wraps `base::factor(x, levels = levs, ordered = TRUE)`.
#'   \cr `as_uno`   \tab As xmode `'uno'` (unordered factor). Wraps `base::factor(x, levels = levs, ordered = FALSE)`.
#' }
#' @param x For `as_clr`, an object of mode character; for `as_fun`, a character scalar function name or a function object; for `as_ord` and `as_uno`, an atomic object; and for all others, base::any R object.
#' @param na A non-`NA` logical scalar indicating whether `NA` values are acceptable.
#' @param levs A \link[=cmp_vec]{complete atomic vec} of factor levels (ordered factor levels for `as_ord`).
#' @param ... Further arguments passed to or from other methods.
#' @return *An object of base mode* `'character'`
#'  \cr   `as_chr`
#'  \cr\cr *An object of base mode* `'integer'`
#'  \cr   `as_int`
#'  \cr\cr *An object of base mode* `'logical'`
#'  \cr   `as_lgl`
#'  \cr\cr *An object of base mode* `'numeric'`
#'  \cr   `as_num`
#'  \cr\cr *An object of \link[=mmm]{xmode}* `'ord'`\eqn{^1}
#'  \cr   `as_ord`
#'  \cr\cr *An object of xmode* `'uno'`\eqn{^2}
#'  \cr   `as_uno`
#'  \cr\cr *An object of xmode* `'clr'`\eqn{^3}
#'  \cr   `as_clr`
#'  \cr\cr ` `\eqn{^{1.}} Ordered factor.
#'  \cr ` `\eqn{^{2.}} Unordered factor.
#'  \cr ` `\eqn{^{3.}} Character hexadecimal RGB color values in the form `'#RRGGBBAA'`.
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
  errs <- base::c(uj::f0(uj::ichr(x)                             , NULL, "[x] is not of mode character."),
                  uj::f0(uj::isTF(na)                            , NULL, "[na] must be TRUE or FALSE."),
                  uj::f0(uj::isF(na) | !base::any(base::is.na(x)), NULL, "[x] contains NA values but [na = FALSE]."))
  if (!base::is.null(errs)) {stop(uj::format_errs(pkg = "uj", errs))}
  if (base::any(!base::is.na(x))) {
    out <- tryCatch(grDevices::col2rgb(x[!base::is.na(x)], T), error = function(e) e, finally = NULL)
    if (uj::isERR(out)) {stop(uj::format_errs(pkg = "uj", "[x] does not contain only valid color values."))}
    else {out <- out / 255}
    out <- grDevices::rgb(out[1, ], out[2, ], out[3, ], out[4, ])
    x[!base::is.na(x)] <- out
  }
  x
}

#' @rdname as_mmm
#' @export
as_fun <- function(x) {
  if (base::is.function(x)) {return(x)}
  x <- tryCatch(base::match.fun(x), error = function(e) e, finally = NULL)
  if (base::any(base::class(x) %in% base::c("error", "simpleError"))) {stop(uj::.errs("[x] is neither a function nor a character scalar name of a function."))}
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
as_ord <- function(x, levs) {base::factor(x, levels = levs, ordered = T)}

#' @rdname as_mmm
#' @export
as_uno <- function(x, levs) {base::factor(x, levels = levs, ordered = F)}
