#' @encoding UTF-8
#' @family missingness
#' @family extensions
#' @family values
#' @title Manage `NA` and non-`NA` values
#' @description All functions in this group take objects of atomic mode, \link[=atm_dtf]{atomic dtfs}, or \link[=atm_vls]{atomic vlists}.
#' @details
#' \tabular{ll}{  `na`       \tab Logically indexes `NA` values of `x`.                                                         \cr   \tab   \cr
#'                `ok`       \tab Logically indexes non-`NA` values of `x`.                                                     \cr   \tab   \cr
#'                `na0`      \tab Evaluates whether `x` is an `NA` scalar.                                                      \cr   \tab   \cr
#'                `ok0`      \tab Evaluates whether `x` is a non-`NA` scalar.                                                   \cr   \tab   \cr
#'                `rm_na`    \tab Removes `NA` values of `x` If there are no `NA` values in `x`, returns `x` unchanged.
#'                                If there are `NA` values in `x`, removing them may change the dimensions and/or class of `x`. \cr   \tab   \cr
#'                `sub_na`   \tab Substitutes `s` for `NA` values of`x`. Modes of `x` and `s` must be \link[=compatible]{compatible}.          }
#' @param x The argument to be inspected/managed.
#' @param s An \link[=atm_scl]{atomic scalar} to replace `NA` values. Mode must be \link[=compatible]{compatible} with`x`
#' @return **A logical object of the same dimension as** `x`                                   \cr\cr `na, ok`
#' \cr\cr  `x` **with `NA` values replaced** \cr (properties may change with `NA` replacement) \cr\cr `sub_na`
#' \cr\cr  `x` **with `NA` values removed**  \cr (properties may change with `NA` removal)     \cr\cr `rm_na`
#' \cr\cr  **A logical scalar**                                                                \cr\cr `na0, ok0`
#' @examples
#' egNA0 <- NA
#' egOK0 <- "a"
#' egVec <- c(egNA0, egOK0)
#' egMat <- sample(vec, 10, replace = T)
#' egMat
#' na(egVec)
#' ok(egVec)
#' na(egMat)
#' ok(egMat)
#' na0(egNAS)
#' na0(egOK0)
#' na0(egVec)
#' ok0(egOK0)
#' ok0(egNAS)
#' ok0(egVec)
#' sub_na(egMat, "x")
#' rm_na(egMat)
#' @export
na <- function(x) {
  .na <- function(y) {base::is.na(y)}
  if (uj:::.pop_atm(x)) {.na(x)}
  else if (uj:::.atm_dtf(x)) {base::apply(x, 2, .na)}
  else if (uj:::.atm_vls(x)) {base::sapply(x, .na)}
  else {uj::stopperr("[x] must be populated and atomic (?pop_atm), a populated atomic vlist (?atm_vls), or a populated atomic data.frame (?atm_dtf).", PKG = "uj")}
}

#' @rdname na
#' @export
ok <- function(x) {
  .ok <- function(y) {!base::is.na(y)}
  if (uj:::.pop_atm(x)) {.ok(x)}
  else if (uj:::.atm_dtf(x)) {base::apply(x, 2, .ok)}
  else if (uj:::.atm_vls(x)) {base::sapply(x, .ok)}
  else {uj::stopperr("[x] must be populated and atomic (?pop_atm), a populated atomic vlist (?atm_vls), or a populated atomic data.frame (?atm_dtf).", PKG = "uj")}
}

#' @rdname na
#' @export
sub_na <- function(x, s) {
  if (base::length(x) == 0) {return(x)}
  if (base::is.atomic(x)) {
    if (uj::compatible_xy(x, s)) {x[base::is.na(x)] <- s; return(x)}
    else {uj::stopperr("[x] and [s] are of incompatible modes (?compatible).", PKG = "uj")}
  } else if (uj:::.atm_dtf(x)) {
    if (base::all(base::apply(x, 2, base::is.atomic))) {
      if (base::all(base::apply(x, 2, uj::compatible_xy, y = s))) {base::apply(x, 2, uj::sub_na, s = s)}
      else {uj::stopperr("[s] is incompatible (?incompatible) with one or more columns of [x].", PKG = "uj")}
    } else {uj::stopperr("When [x] is a data.frame, its columns must be atomic.", PKG = "uj")}
  } else if (uj:::.atm_vls(x)) {
    if (base::all(base::sapply(x, base::is.atomic))) {
      if (base::all(base::sapply(x, uj::compatible_xy, y = s))) {base::lapply(x, uj::sub_na, s = s)}
      else {uj::stopperr("[s] is incompatible (?incompatible) with one or more elements of [x].", PKG = "uj")}
    } else {uj::stopperr("When [x] is a vlist (?VLS), its elements must be atomic.", PKG = "uj")}
  } else {uj::stopperr("[x] must be populated and atomic (?pop_atm), a populated atomic vlist (?atm_vls), or a populated atomic data.frame (?atm_dtf).", PKG = "uj")}
}

#' @rdname na
#' @export
na0 <- function(x) {if (base::length(x) != 1 | !base::is.atomic(x)) {F} else {base::is.na(x)}}

#' @rdname na
#' @export
ok0 <- function(x) {if (base::length(x) != 1 | !base::is.atomic(x)) {F} else {!base::is.na(x)}}

#' @rdname na
#' @export
rm_na <- function(x) {
  if (uj:::.atm.dtf(x)) {
    av.x <- uj::av(x)
    if (!base::any(base::is.na(x))) {x}
    else {av.x[!base::is.na(av.x)]}
  } else if (base::is.atomic(x)) {x[!base::is.na(x)]}
  else if (uj:::.atm.vls(x)) {base::lapply(x, uj::rm_na)}
  else {uj::stopperr("[x] must be populated and atomic (?pop_atm), a populated atomic vlist (?atm_vls), or a populated atomic data.frame (?atm_dtf).", PKG = "uj")}
}
