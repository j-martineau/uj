#' @name na.
#' @family extensions
#' @title Manage \code{NA} and non-\code{NA} values
#' @description All functions in this group take objects of atomic mode,
#'   \link[=atm_dtf]{atomic dtfs}, or \link[=atm_vls]{atomic vlists}.
#'   \cr\cr
#'   \code{na}
#'   \cr Indexes \code{NA} values of \code{x}.
#'   \cr\cr
#'   \code{ok}
#'   \cr Indexes non-\code{NA} values of \code{x}.
#'   \cr\cr
#'   \code{nas}
#'   \cr Evaluates whether \code{x} an \code{NA} scalar.
#'   \cr\cr
#'   \code{oks}
#'   \cr Evaluates whether \code{x} a non-\code{NA} scalar.
#'   \cr\cr
#'   \code{rm_na}
#'   \cr Removes \code{NA} values of \code{x}. If there are no \code{NA} values
#'   in \code{x}, this function returns \code{x} unchanged. \strong{However}, if
#'   there are \code{NA} values in \code{x}, by removing \code{NA} values from
#'   \code{x}, this function changes the dimensions of and/or class of \code{x}.
#'   If \code{x} is an array or an \code{\link[atm_dtf]{atomic data.frame}}, its
#'   class is reduced to an atomic vector sans \code{NA} values. If \code{x} is
#'   an \code{\link[=atm_vls]{atomic vlist}}, any elements with \code{NA} values
#'   are reduced to atomic vectors sans their \code{NA} values.
#'   \cr\cr
#'   \code{sub_na}
#'   \cr Substitutes \code{s} for \code{NA} values of \code{x}. Modes of
#'   \code{x} and \code{s} must be compatible (\code{\link{=compatible}}).
#' @param x The argument to be inspected/managed.
#' @param s \link[=atm_scl]{Atomic scalar} to replace \code{NA} values. Mode must
#'   be \code{\link{compatible}} with \code{x}.
#' @return\tabular{lll}{
#'  \code{na} and \code{ok}  \tab   \tab A logical object of the same dimension
#'                                       as \code{x}.                        \cr
#'  \code{nas} and \code{oks}\tab   \tab A logical scalar.                   \cr
#'  \code{rm_na}             \tab   \tab Either an atomic vector or \code{x}.\cr
#'  \code{sub_na}            \tab   \tab \code{x} with \code{NA} values
#'                                        replaced.                            }
#' @export
na. <- function() {help("na.", package = "uj")}

#' @rdname na.
#' @export
na <- function(x) {
  if (length(x) > 0) {
    if (is.atomic(x)) {return(is.na(x))}
    else if (is.data.frame(x)) {if (all(apply(x, 2, is.atomic))) {return(apply(x, 2, is.na))}}
    if (ivls(x)) {if (all(lengths(x) > 0)) {if (all(sapply(x, is.atomic(x)))) {return(all(lapply(x, is.na)))}}}
  }
  stop("\n \u2022 [x] must be populated (?ipop) and atomic, a populated atomic vlist (?ivls), or a populated atomic tabular (?itab).")
}

#' @rdname na.
#' @export
ok <- function(x) {
  if (length(x) > 0) {
    if (is.atomic(x)) {return(!is.na(x))}
    else if (is.data.frame(x)) {if (all(apply(x, 2, is.atomic))) {return(apply(apply(x, 2, is.na), not))}}
    if (ivls(x)) {if (all(lengths(x) > 0)) {if (all(sapply(x, is.atomic(x)))) {return(all(lapply(lapply(x, is.na), not)))}}}
  }
  stop("\n \u2022 [x] must be populated (?ipop) and atomic, a populated atomic vlist (?ivls), or a populated atomic tabular (?itab).")
}

#' @rdname na.
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

#' @rdname na.
#' @export
nas <- function(x) {if (n1(x) & iatm(x)) {is.na(x)} else {F}}

#' @rdname na.
#' @export
oks <- function(x) {if (n1(x) & iatm(x)) {!is.na(x)} else {F}}

#' @rdname na.
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
