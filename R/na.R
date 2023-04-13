#' @encoding UTF-8
#' @family missingness
#' @family extensions
#' @family values
#' @title Manage `NA` and non-`NA` values
#' @description All functions in this group take objects of atomic mode, \link[=atm_dtf]{atomic dtfs}, or \link[=atm_vls]{atomic vlists}.
#' @details
#' \tabular{ll}{  `na`       \tab Logically indexes `NA` values of `X`.                                                         \cr   \tab   \cr
#'                `ok`       \tab Logically indexes non-`NA` values of `X`.                                                     \cr   \tab   \cr
#'                `na0`      \tab Evaluates whether `X` is an `NA` scalar.                                                      \cr   \tab   \cr
#'                `ok0`      \tab Evaluates whether `X` is a non-`NA` scalar.                                                   \cr   \tab   \cr
#'                `rm_na`    \tab Removes `NA` values of `X` If there are no `NA` values in `X`, returns `X` unchanged.
#'                                If there are `NA` values in `X`, removing them may change the dimensions and/or class of `X`. \cr   \tab   \cr
#'                `sub_na`   \tab Substitutes `S` for `NA` values of`X`. Modes of `X` and `S` must be \link[=compatible]{compatible}.          }
#' @param X The argument to be inspected/managed.
#' @param S An \link[=atm_scl]{atomic scalar} to replace `NA` values. Mode must be \link[=compatible]{compatible} with`X`
#' @return **A logical object of the same dimension as** `X`                                   \cr\cr `na, ok`
#' \cr\cr  `X` **with `NA` values replaced** \cr (properties may change with `NA` replacement) \cr\cr `sub_na`
#' \cr\cr  `X` **with `NA` values removed**  \cr (properties may change with `NA` removal)     \cr\cr `rm_na`
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
#' sub_na(egMat, "X")
#' rm_na(egMat)
#' @export
na <- function(X) {
  .na <- function(x) {base::is.na(x)}
  if (uj:::.pop_atm(X)) {.na(X)}
  else if (uj:::.atm_dtf(X)) {base::apply(X, 2, .na)}
  else if (uj:::.atm_vls(X)) {base::sapply(X, .na)}
  else {uj::stopperr("[X] must be populated and atomic (?pop_atm), a populated atomic vlist (?atm_vls), or a populated atomic data.frame (?atm_dtf).", PKG = "uj")}
}

#' @rdname na
#' @export
ok <- function(X) {
  .ok <- function(x) {!base::is.na(x)}
  if (uj:::.pop_atm(X)) {.ok(X)}
  else if (uj:::.atm_dtf(X)) {base::apply(X, 2, .ok)}
  else if (uj:::.atm_vls(X)) {base::sapply(X, .ok)}
  else {uj::stopperr("[X] must be populated and atomic (?pop_atm), a populated atomic vlist (?atm_vls), or a populated atomic data.frame (?atm_dtf).", PKG = "uj")}
}

#' @rdname na
#' @export
sub_na <- function(X, S) {
  if (base::length(X) == 0) {return(X)}
  if (base::is.atomic(X)) {
    if (uj::compatible_xy(X, S)) {X[base::is.na(X)] <- S; return(X)}
    else {uj::stopperr("[X] and [S] are of incompatible modes (?compatible).", PKG = "uj")}
  } else if (uj:::.atm_dtf(X)) {
    if (base::all(base::apply(X, 2, base::is.atomic))) {
      if (base::all(base::apply(X, 2, uj::compatible_xy, y = S))) {base::apply(X, 2, uj::sub_na, S = S)}
      else {uj::stopperr("[S] is incompatible (?incompatible) with one or more columns of [X].", PKG = "uj")}
    } else {uj::stopperr("When [X] is a data.frame, its columns must be atomic.", PKG = "uj")}
  } else if (uj:::.atm_vls(X)) {
    if (base::all(base::sapply(X, base::is.atomic))) {
      if (base::all(base::sapply(X, uj::compatible_xy, y = S))) {base::lapply(X, uj::sub_na, S = S)}
      else {uj::stopperr("[S] is incompatible (?incompatible) with one or more elements of [X].", PKG = "uj")}
    } else {uj::stopperr("When [X] is a vlist (?VLS), its elements must be atomic.", PKG = "uj")}
  } else {uj::stopperr("[X] must be populated and atomic (?pop_atm), a populated atomic vlist (?atm_vls), or a populated atomic data.frame (?atm_dtf).", PKG = "uj")}
}

#' @rdname na
#' @export
na0 <- function(X) {if (base::length(X) != 1 | !base::is.atomic(X)) {F} else {base::is.na(X)}}

#' @rdname na
#' @export
ok0 <- function(X) {if (base::length(X) != 1 | !base::is.atomic(X)) {F} else {!base::is.na(X)}}

#' @rdname na
#' @export
rm_na <- function(X) {
  if (uj:::.atm.dtf(X)) {
    Atoms <- uj::av(X)
    if (!base::any(base::is.na(X))) {X}
    else {Atoms[!base::is.na(Atoms)]}
  } else if (base::is.atomic(X)) {X[!base::is.na(X)]}
  else if (uj:::.atm.vls(X)) {base::lapply(X, uj::rm_na)}
  else {uj::stopperr("[X] must be populated and atomic (?pop_atm), a populated atomic vlist (?atm_vls), or a populated atomic data.frame (?atm_dtf).", PKG = "uj")}
}
