#' @name na
#' @family extensions
#' @family na_values
#' @title Manage `NA` and non-`NA` values
#' @description All functions in this group take objects of atomic mode,
#'   \link[=atm_dtf]{atomic dtfs}, or \link[=atm_vls]{atomic vlists}.
#'   \tabular{ll}{
#'     FUNCTION   \tab WHAT IT DOES                                          \cr
#'     `na`       \tab Index `NA` values of `x`.                             \cr
#'     `ok`       \tab Index non-`NA` values of `x`.                         \cr
#'     `nas`      \tab Evaluate whether `x` is an `NA` scalar.               \cr
#'     `oks`      \tab Evaluate whether `x` is a non-`NA` scalar.            \cr
#'     `sub_na`   \tab Substitute `s` for `NA` values of`x` Modes of `x` and `s`
#'                     must be \link[=compatible]{compatible}.               \cr
#'     `rm_na`    \tab Remove `NA` values of `x` If there are no `NA` values in
#'                     `x`, this function returns `x` unchanged. If there are
#'                     `NA` values in `x`, by removing `NA` values from `x`,
#'                     this function changes the dimensions of and/or class
#'                     of `x` If `x` is an array or an \link[atm_dtf]{atomic
#'                     data.frame}, its class is reduced to an atomic vector
#'                     sans `NA` values. If `x` is an \link[=atm_vls]{atomic
#'                     vlist}, any elements with `NA` values are reduced to
#'                     atomic vectors sans their `NA` values.                  }
#' @param x The argument to be inspected/managed.
#' @param s An \link[=atm_scl]{atomic scalar} to replace `NA` values. Mode must
#'   be \link[=compatible]{compatible} with`x`
#' @return \tabular{ll}{
#'   FUNCTIONS      \tab RETURN VALUE                                        \cr
#'   `na`, `ok`     \tab A logical object of the same dimension as `x`.      \cr
#'   `nas`, `oks`   \tab A logical scalar.                                   \cr
#'   `rm_na`        \tab Either an atomic vector or `x`.                     \cr
#'   `sub_na`       \tab `x` with `NA` values replaced.                        }
#' @export
na <- function(x) {
  if (length(x) > 0) {
    if (is.atomic(x)) {return(is.na(x))}
    else if (is.data.frame(x)) {if (all(apply(x, 2, is.atomic))) {return(apply(x, 2, is.na))}}
    if (ivls(x)) {if (all(lengths(x) > 0)) {if (all(sapply(x, is.atomic(x)))) {return(all(lapply(x, is.na)))}}}
  }
  stop("\n \u2022 [x] must be populated (?ipop) and atomic, a populated atomic vlist (?ivls), or a populated atomic tabular (?itab).")
}

#' @rdname na
#' @export
ok <- function(x) {
  if (length(x) > 0) {
    if (is.atomic(x)) {return(!is.na(x))}
    else if (is.data.frame(x)) {if (all(apply(x, 2, is.atomic))) {return(apply(apply(x, 2, is.na), not))}}
    if (ivls(x)) {if (all(lengths(x) > 0)) {if (all(sapply(x, is.atomic(x)))) {return(all(lapply(lapply(x, is.na), not)))}}}
  }
  stop("\n \u2022 [x] must be populated (?ipop) and atomic, a populated atomic vlist (?ivls), or a populated atomic tabular (?itab).")
}

#' @rdname na
#' @export
sub_na <- function(x, s) {
  if (length(x) == 0) {return(x)}
  if (is.atomic(x)) {
    if (compatible(x, s)) {x[is.na(x)] <- s; return(x)}
    else {stop("\n \u2022 [x] and [s] are of incompatible modes (?compatible).")}
  } else if (is.data.frame(x)) {
    if (all(apply(x, 2, is.atomic))) {
      if (all(apply(x, 2, compatible, s))) {x[is.na(x)] <- s; return(x)}
      else {stop("\n \u2022 [s] is incompatible (?incompatible) with one or more columns of [x].")}
    } else {stop("\n \u2022 When [x] is a dtf (?is_dtf), its columns must be atomic.")}
  } else if (atm_vls(x)) {
    if (all(sapply(x, is.atomic))) {
      if (all(sapply(x, compatible, s))) {return(lapply(x, sub_na, s))}
      else {stop("\n \u2022 [s] is incompatible (?incompatible) with one or more elements of [x].")}
    } else {stop("\n \u2022 When [x] is a vlist (?is_vls), its elements must be atomic.")}
  } else {stop("\n \u2022 [x] must be an atomic vlist (?atm_vls), atomic dtf (?idtf), or some other atomic object.")}
}

#' @rdname na
#' @export
nas <- function(x) {if (n1(x) & iatm(x)) {is.na(x)} else {F}}

#' @rdname na
#' @export
oks <- function(x) {if (n1(x) & iatm(x)) {!is.na(x)} else {F}}

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
  stop("\n \u2022 [x] must be populated (?ipop) and either an atomic object, an atomic vlist (?atm_vls), or an atomic dtf (?atm_dtf).")
}
