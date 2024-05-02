#' @name as_mmm
#' @encoding UTF-8
#' @family properties
#' @title Coerce to base mode or \code{\link[mmm]{extended mode}}
#' @description Functions to coerce objects to \link[base:mode]{base mode} or \code{\link[=mode]{extended mode}}.
#' @details Functions in this family are as follows:
#' \tabular{ll}{  `as_chr`   \tab Thinly wraps \code{\link[base]{as.character}}.                                                          \cr
#'                `as_int`   \tab Thinly wraps \code{\link[base]{as.integer}}.                                                            \cr
#'                `as_num`   \tab Thinly wraps \code{\link[base]{as.numeric}}.                                                            \cr
#'                `as_lgl`   \tab Thinly wraps \code{\link[base]{as.logical}}.                                                            \cr   \tab   \cr
#'                `as_ord`   \tab As extended mode `'ord'` (ordered factor). Wraps `factor(x, levels = levs, ordered = TRUE)`.            \cr   \tab   \cr
#'                `as_uno`   \tab As extended mode `'uno'` (unordered factor). Wraps `factor(x, levels = levs, ordered = FALSE)`.         \cr   \tab   \cr
#'                `as_clr`   \tab Coerces character color values to hexadecimal.                                                          \cr   \tab   \cr
#'                `as_fun`   \tab If `x` is a character scalar function return that function, but if `x` is a function object, return it.                }
#' @param x For `as_clr`, an object of mode character; for `as_fun`, a character scalar function name or a function object; for `as_ord` and `as_uno`, an atomic object; and for all others, any R object.
#' @param mode A \link[mmm]{threechar scalar} containing an \link[mmm]{extended mode} value.
#' @param levs Optional \link[=cmp_vec]{complete atomic vec} of factor levels (ordered factor levels for `as_ord`).
#' @param na `TRUE` or `FALSE` indicating whether `NA` values are acceptable.
#' @return **An object of base mode** `'character'`                                 \cr\cr `as_chr`
#' \cr\cr  **An object of base mode** `'integer'`                                   \cr\cr `as_int`
#' \cr\cr  **An object of base mode** `'logical'`                                   \cr\cr `as_lgl`
#' \cr\cr  **An object of base mode** `'numeric'`                                   \cr\cr `as_num`
#' \cr\cr  **An object of \link[=mode]{extended mode}** `'ord'` (ordered factor)    \cr\cr `as_ord`
#' \cr\cr  **An object of extended mode** `'uno'` (unordered factor)                \cr\cr `as_uno`
#' \cr\cr  **An object of extended mode** `'clr'` (hex color in form `'#RRGGBBAA'`) \cr\cr `as_clr`
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
as_mmm <- function(x, mode, levs = NULL, na = FALSE) {
  valid <- base::c("chr", "clr", "fun", "int", "num", "lgl", "ord", "uno")
  err <- "Unrecognized mode; valid values are c('chr', 'clr', 'fun', 'int', 'num', 'lgl', 'ord', 'uno')."
  if (!ppp::.cmp_chr_scl(mode)) {ppp::stopperr(err, pkg = "uj")}
  else if (uj::not_IN(base::tolower(mode), valid)) {ppp::stopperr(err, pkg = "uj")}
  else {mode <- base::tolower(mode)}
  if (mode == "clr") {
    errs <- NULL
    if (!base::is.character(x)) {errs <- base::c(errs, "[x] is not of mode character.")}
    if (!ppp:::.cmp_lgl_scl(na)) {errs <- base::c(errs, "[.na] must be TRUE or FALSE.")}
    if (base::isTRUE(na) & base::any(base::isna(uj::av(x)))) {errs <- base::c(errs, "[x] contains na values but [.na = FALSE].")}
    if (!base::is.null(errs)) {ppp::stopperr(errs, pkg = "uj")}
    if (!base::all(base::isna(x))) {
      Y <- tryCatch(grDevices::col2rgb(x[!base::is.na(x)], T), error = function(e) e, finally = NULL)
      if (uj::is_err(Y)) {ppp::stopperr("[x] does not contain only valid color values.", pkg = "uj")}
      Y <- Y / 255
      Y <- grDevices::rgb(Y[1, ], Y[2, ], Y[3, ], Y[4, ])
      x[!base::is.na(x)] <- Y
    }
  } else if (mode == "fun") {if (!base::is.function(x)) {
    x <- tryCatch(base::match.fun(x), error = function(e) e, finally = NULL)
    if (uj::is_err(x)) {ppp::stopperr("[x] is neither a function nor a character scalar name of a function.", pkg = "uj")}
  }} else if (mode == "chr") {x <- base::as.character(x)}
  else if (mode == "int") {x <- base::as.integer(x)}
  else if (mode == "lgl") {x <- base::as.logical(x)}
  else if (mode == "num") {x <- base::as.numeric(x)}
  else if (mode == "ord") {x <- base::factor(x, levels = levs, ordered = T)}
  else if (mode == "uno") {x <- base::factor(x, levels = levs, ordered = F)}
  else {ppp::stopperr("Unrecognized mode.", pkg = "uj")}
  x
}

#' @rdname as_mmm
#' @export
as_chr <- function(x, na = FALSE) {uj::as_mmm(x, "chr")}

#' @rdname as_mmm
#' @export
as_clr <- function(x, na = FALSE) {uj::as_mmm(x, "clr", na = na)}

#' @rdname as_mmm
#' @export
as_fun <- function(x) {uj::as_mmm(x, "fun")}

#' @rdname as_mmm
#' @export
as_int <- function(x) {uj::as_mmm(x, "int")}

#' @rdname as_mmm
#' @export
as_num <- function(x) {uj::as_mmm(x, "num")}

#' @rdname as_mmm
#' @export
as_lgl <- function(x) {uj::as_mmm(x, "lgl")}

#' @rdname as_mmm
#' @export
as_ord <- function(x, levs) {uj::as_mmm(x, "ord", levs = levs)}

#' @rdname as_mmm
#' @export
as_uno <- function(x, levs) {uj::as_mmm(x, "uno", levs = levs)}
