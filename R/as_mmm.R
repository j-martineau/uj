#' @name as_mmm
#' @encoding UTF-8
#' @family properties
#' @title Coerce to base mode or `xmode`
#' @description Functions to coerce objects to \link[base:mode]{base mode} or \code{\link[=mmm]{xmode}}.
#' @details Functions in this family are as follows:
#' \tabular{ll}{  `as_chr`   \tab Thinly wraps \code{\link[base]{as.character}}.                                                        \cr
#'                `as_int`   \tab Thinly wraps \code{\link[base]{as.integer}}.                                                          \cr
#'                `as_num`   \tab Thinly wraps \code{\link[base]{as.numeric}}.                                                          \cr
#'                `as_lgl`   \tab Thinly wraps \code{\link[base]{as.logical}}.                                             \cr   \tab   \cr
#'                `as_ord`   \tab As xmode `'ord'` (ordered factor). Wraps `factor(X, levels = Levs, ordered = TRUE)`.     \cr   \tab   \cr
#'                `as_uno`   \tab As xmode `'uno'` (unordered factor). Wraps `factor(X, levels = Levs, ordered = FALSE)`.  \cr   \tab   \cr
#'                `as_clr`   \tab Coerces character color values to hexadecimal.                                           \cr   \tab   \cr
#'                `as_fun`   \tab If `X` is a character scalar function return that function, but if `X` is a function object, return it. }
#' @param X For `as_clr`, an object of mode character; for `as_fun`, a character scalar function name or a function object; for `as_ord` and `as_uno`, an atomic object; and for all others, any R object.
#' @param Na `TRUE` or `FALSE` indicating whether `NA` values are acceptable.
#' @param Levs A \link[=cmp_vec]{complete atomic vec} of factor levels (ordered factor levels for `asORD`).
#' @param ... Further arguments passed to or from other methods.
#' @return **An object of base mode** `'character'`                         \cr\cr `as_chr`
#' \cr\cr  **An object of base mode** `'integer'`                           \cr\cr `as_int`
#' \cr\cr  **An object of base mode** `'logical'`                           \cr\cr `as_lgl`
#' \cr\cr  **An object of base mode** `'numeric'`                           \cr\cr `as_num`
#' \cr\cr  **An object of \link[=mmm]{xmode}** `'ord'` (ordered factor)     \cr\cr `as_ord`
#' \cr\cr  **An object of xmode** `'uno'` (unordered factor)                \cr\cr `as_uno`
#' \cr\cr  **An object of xmode** `'clr'` (hex color in form `'#RRGGBBAA'`) \cr\cr `as_clr`
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
#' as_uno(clrs., Levs = unique(clrs.))
#' as_ord(clrs., Levs = sort(unique(clrs.)))
#' @export
as_mmm <- function(X, Mode, Levs = NULL, Na = FALSE) {
  if (!uj:::.cmp_ch3_scl(Mode)) {uj::stopperr("Unrecognized mode.", PKG = "uj")}
  else {Mode <- base::tolower(Mode)}
  if (Mode == "clr") {
    Errors <- NULL
    if (!base::is.character(X)) {Errors <- base::c(Errors, "[X] is not of mode character.")}
    if (!uj:::.cmp_lgl_scl(Na)) {Errors <- base::c(Errors, "[Na] must be TRUE or FALSE.")}
    if (base::isTRUE(Na) & base::any(base::is.na(uj::av(X)))) {Errors <- base::c(Errors, "[X] contains NA values but [Na = FALSE].")}
    if (!base::is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
    if (!base::all(base::is.na(X))) {
      Y <- tryCatch(grDevices::col2rgb(X[!base::is.na(X)], T), error = function(e) e, finally = NULL)
      if (uj::is_err(Y)) {uj::stopperr("[X] does not contain only valid color values.", PKG = "uj")}
      Y <- Y / 255
      Y <- grDevices::rgb(Y[1, ], Y[2, ], Y[3, ], Y[4, ])
      X[!base::is.na(X)] <- Y
    }
  } else if (Mode == "fun") {if (!base::is.function(X)) {
    X <- tryCatch(base::match.fun(X), error = function(e) e, finally = NULL)
    if (uj::is_err(X)) {uj::stopperr("[X] is neither a function nor a character scalar name of a function.", PKG = "uj")}
  }} else if (Mode == "chr") {X <- base::as.character(X)}
  else if (Mode == "int") {X <- base::as.integer(X)}
  else if (Mode == "lgl") {X <- base::as.logical(X)}
  else if (Mode == "num") {X <- base::as.numeric(X)}
  else if (Mode == "ord") {X <- base::factor(X, levels = Levs, ordered = T)}
  else if (Mode == "uno") {X <- base::factor(X, levels = Levs, ordered = F)}
  else {uj::stopperr("Unrecognized mode.", PKG = "uj")}
  X
}

#' @rdname as_mmm
#' @export
as_chr <- function(X, Na = FALSE) {uj::as_mmm(X, "chr")}

#' @rdname as_mmm
#' @export
as_clr <- function(X, Na = FALSE) {uj::as_mmm(X, "clr", Na = Na)}

#' @rdname as_mmm
#' @export
as_fun <- function(X) {uj::as_mmm(X, "fun")}

#' @rdname as_mmm
#' @export
as_int <- function(X) {uj::as_mmm(X, "int")}

#' @rdname as_mmm
#' @export
as_num <- function(X) {uj::as_mmm(X, "num")}

#' @rdname as_mmm
#' @export
as_lgl <- function(X) {uj::as_mmm(X, "lgl")}

#' @rdname as_mmm
#' @export
as_ord <- function(X, Levs) {uj::as_mmm(X, "ord", Levs = Levs)}

#' @rdname as_mmm
#' @export
as_uno <- function(X, Levs) {uj::as_mmm(X, "uno", Levs = Levs)}
