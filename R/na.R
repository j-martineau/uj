#' @family missingness
#' @title Manage `NA` and non-`NA` values
#' @description All functions in this group take objects of atomic mode, \link[=atm_dtf]{atomic dtfs}, or \link[=atm_vls]{atomic vlists}:
#' \tabular{rl}{
#'     `sub_na`   \tab Substitutes `s` for `NA` values of`x` Modes of `x` and `s` must be \link[=compatible]{compatible}.
#'  \cr           \tab   
#'  \cr `rm_na`   \tab Removes `NA` values of `x` If there are no `NA` values in `x`, returns `x` unchanged. If there are `NA` values in `x`, removing them may change the dimensions and/or class of `x`.
#'  \cr           \tab   
#'  \cr   `nas`   \tab Evaluates whether `x` is an `NA` scalar.
#'  \cr   `oks`   \tab Evaluates whether `x` is a non-`NA` scalar.
#'  \cr    `na`   \tab Indexes `NA` values of `x`.
#'  \cr    `ok`   \tab Indexes non-`NA` values of `x`.
#' }
#' @param x The argument to be inspected/managed.
#' @param s An \link[=atm_scl]{atomic scalar} to replace `NA` values. Mode must be \link[=compatible]{compatible} with`x`
#' @return \tabular{rl}{
#'     `sub_na` \tab   `x` with `NA` values replaced.
#'  \cr `rm_na` \tab   Either an atomic vector or `x`.
#'  \cr   `nas` \tab   A logical scalar.
#'  \cr   `oks` \tab   A logical scalar.
#'  \cr    `na` \tab   A logical object of the same dimension as `x`.
#'  \cr    `ok` \tab   A logical object of the same dimension as `x`.
#'
#' }
#' @examples
#' na_scl <- NA
#' ok_scl <- "a"
#' vec <- c(na_scl, ok_scl)
#' mat <- sample(vec, 10, replace = T)
#'
#' mat
#'
#' na(vec)
#' ok(vec)
#'
#' na(mat)
#' ok(mat)
#'
#' nas(na_scl)
#' nas(ok_scl)
#' nas(vec)
#'
#' oks(ok_scl)
#' oks(na_scl)
#' oks(vec)
#'
#' sub_na(mat, "x")
#'
#' rm_na(mat)
#' @export
na <- function(x) {
  if (length(x) > 0) {
    if (is.atomic(x)) {return(is.na(x))}
    else if (is.data.frame(x)) {if (all(apply(x, 2, is.atomic))) {return(apply(x, 2, is.na))}}
    if (ivls(x)) {if (all(lengths(x) > 0)) {if (all(sapply(x, is.atomic(x)))) {return(all(lapply(x, is.na)))}}}
  }
  stop(.errs("[x] must be populated (?ipop) and atomic, a populated atomic vlist (?ivls), or a populated atomic tabular (?itab)."))
}

#' @rdname na
#' @export
ok <- function(x) {
  if (length(x) > 0) {
    if (is.atomic(x)) {return(!is.na(x))}
    else if (is.data.frame(x)) {if (all(apply(x, 2, is.atomic))) {return(apply(apply(x, 2, is.na), not))}}
    if (ivls(x)) {if (all(lengths(x) > 0)) {if (all(sapply(x, is.atomic(x)))) {return(all(lapply(lapply(x, is.na), not)))}}}
  }
  stop(.errs("[x] must be populated (?ipop) and atomic, a populated atomic vlist (?ivls), or a populated atomic tabular (?itab)."))
}

#' @rdname na
#' @export
sub_na <- function(x, s) {
  if (length(x) == 0) {return(x)}
  if (is.atomic(x)) {
    if (compatible(x, s)) {x[is.na(x)] <- s; return(x)}
    else {stop(.errs("[x] and [s] are of incompatible modes (?compatible)."))}
  } else if (is.data.frame(x)) {
    if (all(apply(x, 2, is.atomic))) {
      if (all(apply(x, 2, compatible, s))) {x[is.na(x)] <- s; return(x)}
      else {stop(.errs("[s] is incompatible (?incompatible) with one or more columns of [x]."))}
    } else {stop(.errs("When [x] is a dtf (?is_dtf), its columns must be atomic."))}
  } else if (atm_vls(x)) {
    if (all(sapply(x, is.atomic))) {
      if (all(sapply(x, compatible, s))) {return(lapply(x, sub_na, s))}
      else {stop(.errs("[s] is incompatible (?incompatible) with one or more elements of [x]."))}
    } else {stop(.errs("When [x] is a vlist (?is_vls), its elements must be atomic."))}
  } else {stop(.errs("[x] must be an atomic vlist (?atm_vls), atomic dtf (?idtf), or some other atomic object."))}
}

#' @rdname na
#' @export
nas <- function(x) {if (length(x) == 1 & iatm(x)) {is.na(x)} else {F}}

#' @rdname na
#' @export
oks <- function(x) {if (length(x) == 1 & iatm(x)) {!is.na(x)} else {F}}

#' @rdname na
#' @export
rm_na <- function(x) {
  if (is.atomic(x)) {return(x[!is.na(x)])}
  else if (is.data.frame(x)) {if (all(apply(x, 2, is.atomic))) {
    if (!any(is.na(x))) {return(x)}
    x <- av(x)
    return(x[!is.na(x)])
  }}
  else if (atm_vls(x)) {if (all(sapply(x, is.atomic))) {return(lapply(x, rm_na))}}
  stop(.errs("[x] must be populated (?ipop) and either an atomic object, an atomic vlist (?atm_vls), or an atomic dtf (?atm_dtf)."))
}
