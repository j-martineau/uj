#' @encoding UTF-8
#' @family missingness
#' @title Manage `NA` and non-`NA` values
#' @description All functions in this group except `na0` and `ok0` take objects of atomic mode, \link[=atm_dtf]{atomic dtfs}, or \link[=atm_vls]{atomic vlists}.
#' @param x The argument to be inspected/managed.
#' @param sub An \link[=atm_scl]{atomic scalar} to replace `NA` values. Mode must be \link[=compatible]{compatible} with`x`
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
na_help <- function() {utils::help("na_help", package = "uj")}

#' @describeIn na_help Logically indexes `NA` values of `x`. Returns a logical object of the same dimension as `x`.
#' @export
na <- function(x) {
  .na <- function(x) {base::is.na(x)}
  if (uj::.pop_atm(x)) {.na(x)}
  else if (uj::.atm_dtf(x)) {base::apply(x, 2, .na)}
  else if (uj::.atm_vls(x)) {base::sapply(x, .na)}
  else {uj::stopperr("[x] must be populated and atomic (?pop_atm), a populated atomic vlist (?atm_vls), or a populated atomic data.frame (?atm_dtf).")}
}

#' @describeIn na_help Logically indexes non-`NA` values of `x`. Returns a logical object of the same dimension as `x`.
#' @export
ok <- function(x) {
  .ok <- function(x) {!base::is.na(x)}
  if (uj::.pop_atm(x)) {.ok(x)}
  else if (uj::.atm_dtf(x)) {base::apply(x, 2, .ok)}
  else if (uj::.atm_vls(x)) {base::sapply(x, .ok)}
  else {uj::stopperr("[x] must be populated and atomic (?pop_atm), a populated atomic vlist (?atm_vls), or a populated atomic data.frame (?atm_dtf).")}
}

#' @describeIn na_help Substitutes `sub` for `NA` values of`x`. Modes of `x` and `sub` must be \link[=compatible]{compatible}. Returns `x` *with* `NA` *values replaced*.
#' @export
sub_na <- function(x, sub) {
  if (base::length(x) == 0) {return(x)}
  if (base::is.atomic(x)) {
    if (uj::compatible_xy(x, sub)) {x[base::is.na(x)] <- sub; return(x)}
    else {uj::stopperr("[x] and [sub] are of incompatible modes (?compatible).")}
  } else if (uj::.atm_dtf(x)) {
    if (base::all(base::apply(x, 2, base::is.atomic))) {
      if (base::all(base::apply(x, 2, uj::compatible_xy, y = sub))) {base::apply(x, 2, uj::sub_na, sub = sub)}
      else {uj::stopperr("[sub] is incompatible (?incompatible) with one or more columns of [x].")}
    } else {uj::stopperr("When [x] is a data.frame, its columns must be atomic.")}
  } else if (uj::.atm_vls(x)) {
    if (base::all(base::sapply(x, base::is.atomic))) {
      if (base::all(base::sapply(x, uj::compatible_xy, y = sub))) {base::lapply(x, uj::sub_na, sub = sub)}
      else {uj::stopperr("[sub] is incompatible (?incompatible) with one or more elements of [x].")}
    } else {uj::stopperr("When [x] is a vlist (?VLS), its elements must be atomic.")}
  } else {uj::stopperr("[x] must be populated and atomic (?pop_atm), a populated atomic vlist (?atm_vls), or a populated atomic data.frame (?atm_dtf).")}
}

#' @describeIn na_help Evaluates whether `x` is an atomic `NA` scalar. Returns a logical scalar.
#' @export
na0 <- function(x) {if (base::length(x) != 1 | !base::is.atomic(x)) {F} else {base::is.na(x)}}

#' @describeIn na_help Evaluates whether `x` is an atomic, non-`NA` scalar. Returns a logical scalar.
#' @export
ok0 <- function(x) {if (base::length(x) != 1 | !base::is.atomic(x)) {F} else {!base::is.na(x)}}

#' @describeIn na_help Removes `NA` values of `x` If there are no `NA` values in `x`, returns `x` unchanged. If there are `NA` values in `x`, removing them may change the dimensions and/or class of `x`.  Returns `x` *with* `NA` *values removed*.
#' @export
rm_na <- function(x) {
  if (uj::.atm.dtf(x)) {
    atoms <- uj::av(x)
    if (!base::any(base::is.na(x))) {x}
    else {atoms[!base::is.na(atoms)]}
  } else if (base::is.atomic(x)) {x[!base::is.na(x)]}
  else if (uj::.atm.vls(x)) {base::lapply(x, uj::rm_na)}
  else {uj::stopperr("[x] must be populated and atomic (?pop_atm), a populated atomic vlist (?atm_vls), or a populated atomic data.frame (?atm_dtf).")}
}
