#' @encoding UTF-8
#' @family missingness
#' @family extensions
#' @family values
#' @title Manage `NA` and non-`NA` values
#' @description All functions in this group take objects of atomic mode, \link[=atm_dtf]{atomic dtfs}, or \link[=atm_vls]{atomic vlists}:
#' \tabular{rl}{
#'     `sub_na`   \tab Substitutes `s` for `NA` values of`x` Modes of `x` and `s` must be \link[=compatible]{compatible}.
#'  \cr           \tab  
#'  \cr `rm_na`   \tab Removes `NA` values of `x` If there are no `NA` values in `x`, returns `x` unchanged. If there are `NA` values in `x`, removing them may change the dimensions and/or class of `x`.
#'  \cr           \tab  
#'  \cr   `nas`   \tab Evaluates whether `x` is an `NA` scalar.
#'  \cr   `oks`   \tab Evaluates whether `x` is a non-`NA` scalar.
#'  \cr    `na`   \tab Indexes `NA` values of `x`.
#'  \cr    `ok`   \tab Indexes non-`NA` values of `x`.
#' }
#' @param x The argument to be inspected/managed.
#' @param s An \link[=atm_scl]{atomic scalar} to replace `NA` values. Mode must be \link[=compatible]{compatible} with`x`
#' @return *A logical object of the same dimension as `x`*
#'  \cr   `na, ok`
#'  \cr
#'  \cr *`x` with `NA` values replaced*
#'  \cr (properties may change with `NA` replacement)
#'  \cr   `sub_na`
#'  \cr
#'  \cr *`x` with `NA` values removed*
#'  \cr (properties may change with `NA` removal)
#'  \cr   `rm_na`
#'  \cr
#'  \cr *A logical scalaqr*
#'  \cr   `nas, oks`
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
  if (base::length(x) > 0) {
    if (base::is.atomic(x)) {return(base::is.na(x))}
    else if (base::is.data.frame(x)) {if (base::all(base::apply(x, 2, base::is.atomic))) {return(base::apply(x, 2, base::is.na))}}
    if (uj::ivls(x)) {if (base::all(base::lengths(x) > 0)) {if (base::all(base::sapply(x, base::is.atomic(x)))) {return(base::all(base::lapply(x, base::is.na)))}}}
  }
  stop(uj:::.errs("[x] must be populated (?ipop) and atomic, a populated atomic vlist (?ivls), or a populated atomic tabular (?itab)."))
}

#' @rdname na
#' @export
ok <- function(x) {
  if (base::length(x) > 0) {
    if (base::is.atomic(x)) {return(!base::is.na(x))}
    else if (base::is.data.frame(x)) {if (base::all(base::apply(x, 2, base::is.atomic))) {return(base::apply(base::apply(x, 2, base::is.na), uj::not))}}
    if (uj::ivls(x)) {if (base::all(base::lengths(x) > 0)) {if (base::all(base::sapply(x, base::is.atomic(x)))) {return(base::all(base::lapply(base::lapply(x, base::is.na), uj::not)))}}}
  }
  stop(uj:::.errs("[x] must be populated (?ipop) and atomic, a populated atomic vlist (?ivls), or a populated atomic tabular (?itab)."))
}

#' @rdname na
#' @export
sub_na <- function(x, s) {
  if (base::length(x) == 0) {return(x)}
  if (base::is.atomic(x)) {
    if (uj::compatible(x, s)) {x[base::is.na(x)] <- s; return(x)}
    else {stop(uj:::.errs("[x] and [s] are of incompatible modes (?compatible)."))}
  } else if (base::is.data.frame(x)) {
    if (base::all(base::apply(x, 2, base::is.atomic))) {
      if (base::all(base::apply(x, 2, uj::compatible, s))) {x[base::is.na(x)] <- s; return(x)}
      else {stop(uj:::.errs("[s] is incompatible (?incompatible) with one or more columns of [x]."))}
    } else {stop(uj:::.errs("When [x] is a dtf (?is_dtf), its columns must be atomic."))}
  } else if (uj::atm_vls(x)) {
    if (base::all(base::sapply(x, base::is.atomic))) {
      if (base::all(base::sapply(x, uj::compatible, s))) {return(base::lapply(x, uj::sub_na, s))}
      else {stop(uj:::.errs("[s] is incompatible (?incompatible) with one or more elements of [x]."))}
    } else {stop(uj:::.errs("When [x] is a vlist (?is_vls), its elements must be atomic."))}
  } else {stop(uj:::.errs("[x] must be an atomic vlist (?atm_vls), atomic dtf (?idtf), or some other atomic object."))}
}

#' @rdname na
#' @export
nas <- function(x) {uj::f0(base::length(x) == 1 & uj::iatm(x), base::is.na(x), F)}

#' @rdname na
#' @export
oks <- function(x) {uj::f0(base::length(x) == 1 & uj::iatm(x), !base::is.na(x), F)}

#' @rdname na
#' @export
rm_na <- function(x) {
  if (base::is.atomic(x)) {return(x[!base::is.na(x)])}
  else if (base::is.data.frame(x)) {if (base::all(base::apply(x, 2, base::is.atomic))) {
    if (!base::any(base::is.na(x))) {return(x)}
    x <- uj::av(x)
    return(x[!base::is.na(x)])
  }}
  else if (uj::atm_vls(x)) {if (base::all(base::sapply(x, base::is.atomic))) {return(base::lapply(x, uj::rm_na))}}
  stop(uj:::.errs("[x] must be populated (?ipop) and either an atomic object, an atomic vlist (?atm_vls), or an atomic dtf (?atm_dtf)."))
}
