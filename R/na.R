#' @encoding UTF-8
#' @family missingness
#' @family extensions
#' @family values
#' @title Manage `NA` and non-`NA` values
#' @description All functions in this group take objects of atomic mode, \link[=atm_dtf]{atomic dtfs}, or \link[=atm_vls]{atomic vlists}.
#' @details
#' \tabular{ll}{  `na`      \tab Logicall yindexes `NA` values of `x`.                                                               \cr   \tab   \cr
#'                `ok`      \tab Logically indexes non-`NA` values of `x`.                                                           \cr   \tab     }
#' \tabular{ll}{  `nas`     \tab Evaluates whether `x` is an `NA` scalar.                                                            \cr   \tab   \cr
#'                `oks`     \tab Evaluates whether `x` is a non-`NA` scalar.                                                         \cr   \tab     }
#' \tabular{ll}{  `rmNA`    \tab Removes `NA` values of `x` If there are no `NA` values in `x`, returns `x` unchanged.
#'                               If there are `NA` values in `x`, removing them may change the dimensions and/or class of `x`.       \cr   \tab     }
#' \tabular{ll}{  `subNA`   \tab Substitutes `s` for `NA` values of`x`. Modes of `x` and `s` must be \link[=compatible]{compatible}.                }
#' @param x The argument to be inspected/managed.
#' @param s An \link[=atm_scl]{atomic scalar} to replace `NA` values. Mode must be \link[=compatible]{compatible} with`x`
#' @return **A logical object of the same dimension as** `x` \cr `na, ok`
#' \cr\cr  `x` **with `NA` values replaced**                 \cr (properties may change with `NA` replacement) \cr `subNA`
#' \cr\cr  `x` **with `NA` values removed**                  \cr (properties may change with `NA` removal)     \cr `rmNA`
#' \cr\cr  **A logical scalar**                              \cr `nas, oks`
#' @examples
#' egNAS <- NA
#' egOKS <- "a"
#' egVec <- c(egNAS, egOKS)
#' egMat <- sample(vec, 10, replace = T)
#' egMat
#' na(egVec)
#' ok(egVec)
#' na(egMat)
#' ok(egMat)
#' nas(egNAS)
#' nas(egOKS)
#' nas(egVec)
#' oks(egOKS)
#' oks(egNAS)
#' oks(egVec)
#' subNA(egMat, "x")
#' rmNA(egMat)
#' @export
na <- function(x) {
  if (uj::N1P(x)) {
    if (uj::ATM(x)) {return(base::is.na(x))}
    else if (uj::DTF(x)) {if (base::all(base::apply(x, 2, uj::ATM))) {return(base::apply(x, 1:2, uj::na))}}
    if (uj::VLS(x)) {if (base::all(uj::NS1P(x))) {if (base::all(base::sapply(x, base::ATM))) {return(base::all(base::lapply(x, uj::na)))}}}
  } else {uj::stopperr("[x] must be populated and atomic (?pop_atm), a populated atomic vlist (?atm_vls), or a populated atomic data.frame (?atm_dtf).", PKG = "uj")}
}

#' @rdname na
#' @export
ok <- function(x) {
  if (uj::N1P(x)) {
    if (uj::ATM(x)) {return(!base::is.na(x))}
    else if (uj::DTF(x)) {if (base::all(base::apply(x, 2, uj::isATM))) {return(base::apply(base::apply(x, 2, uj::na), uj::not))}}
    if (uj::VLS(x)) {if (base::allNS0(x)) {if (base::all(base::sapply(x, uj::ATM(x)))) {return(base::all(base::lapply(base::lapply(x, uj::na), uj::not)))}}}
  } else {uj::stopperr("[x] must be populated and atomic (?pop_atm), a populated atomic vlist (?atm_vls), or a populated atomic data.frame (?atm_dtf).", PKG = "uj")}
}

#' @rdname na
#' @export
subNA <- function(x, s) {
  if (uj::N0(x)) {return(x)}
  if (uj::ATM(x)) {
    if (uj::compatible(x, s)) {x[uj::na(x)] <- s; return(x)}
    else {uj::stopperr("[x] and [s] are of incompatible modes (?compatible).", PKG = "uj")}
  } else if (uj::atm_dtf(x)) {
    if (base::all(base::apply(x, 2, uj::ATM))) {
      if (base::all(base::apply(x, 2, uj::compatible, s))) {x[uj::na(x)] <- s; return(x)}
      else {uj::stopperr("[s] is incompatible (?incompatible) with one or more columns of [x].", PKG = "uj")}
    } else {uj::stopperr("When [x] is a data.frame, its columns must be atomic.", PKG = "uj")}
  } else if (uj::atm_vls(x)) {
    if (base::all(base::sapply(x, uj::ATM))) {
      if (base::all(base::sapply(x, uj::compatible, s))) {return(base::lapply(x, uj::subNA, s))}
      else {uj::stopperr("[s] is incompatible (?incompatible) with one or more elements of [x].", PKG = "uj")}
    } else {uj::stopperr("When [x] is a vlist (?VLS), its elements must be atomic.", PKG = "uj")}
  } else {uj::stopperr("[x] must be populated and atomic (?pop_atm), a populated atomic vlist (?atm_vls), or a populated atomic data.frame (?atm_dtf).", PKG = "uj")}
}

#' @rdname na
#' @export
nas <- function(x) {uj::f0(uj::N1(x) & uj::ATM(x), uj::na(x), F)}

#' @rdname na
#' @export
oks <- function(x) {uj::f0(uj::N1(x) & uj::ATM(x), uj::ok(x), F)}

#' @rdname na
#' @export
rmNA <- function(x) {
  if (uj::ATM(x)) {return(x[uj::ok(x)])}
  else if (uj::DTF(x)) {
    if (base::all(base::apply(x, 2, uj::ATM))) {
      if (uj::noneNA(x)) {return(x)}
      return(x[uj::ok(uj::av(x))])
  }} else if (uj::atm_vls(x)) {if (base::all(base::sapply(x, uj::ATM))) {return(base::lapply(x, uj::rmNA))}}
  {uj::stopperr("[x] must be populated and atomic (?pop_atm), a populated atomic vlist (?atm_vls), or a populated atomic data.frame (?atm_dtf).", PKG = "uj")}
}
