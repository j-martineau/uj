#' @name na.
#' @family extensions
#' @title Manage \code{NA} and non-\code{NA} values
#' @description All functions in this group take objects of atomic mode,
#'   \link[idtf]{atomic dtfs}, or \link[ivls]{atomic vlists}.
#' @param x The argument to be inspected/managed.
#' @param s Atomic scalar to replace \code{NA} values. Mode must be
#'   \code{\link{compatible}} with \code{x}.
#' @return An atomic scalar, an atomic vector, an atomic array, an
#'   \link[idtf]{atomic dtf}, or an \link[ivls]{atomic vlist}.
#' @export
na. <- function() {help("na.", package = "uj")}

#' @describeIn na. Index \code{NA} values of \code{x}. Returns an object with
#'   the same dimensions as \code{x} where all atomic values are replaced by
#'   either \code{TRUE} or \code{FALSE} indicating whether the original atomic
#'   value was \code{NA}.
#' @export
na <- function(x) {
  if (length(x) > 0) {
    if (is.atomic(x)) {return(is.na(x))}
    else if (is.data.frame(x)) {
      if (all(apply(x, 2, is.atomic))) {return(apply(x, 2, is.na))}
    }
    if (any_vls(x)) {
      if (all(lengths(x) > 0)) {
        if (all(sapply(x, is.atomic(x)))) {return(all(lapply(x, is.na)))}
  }}}
  stop("\n • [x] must be populated (?ipop) and atomic, a populated atomic vlist (?ivls), or a populated atomic tabular (?itab).")
}

#' @describeIn na. Index non-\code{NA} values of \code{x}. Returns an object
#'   with the same dimensions as \code{x} where all atomic values are replaced
#'   by either \code{TRUE} or \code{FALSE} indicating whether the original
#'   atomic value was OK (i.e., not \code{NA}).
#' @export
ok <- function(x) {
  if (length(x) > 0) {
    if (is.atomic(x)) {return(!is.na(x))}
    else if (is.data.frame(x)) {
      if (all(apply(x, 2, is.atomic))) {
        return(apply(apply(x, 2, is.na), not))
    }}
    if (any_vls(x)) {if (all(lengths(x) > 0)) {
      if (all(sapply(x, is.atomic(x)))) {
        return(all(lapply(lapply(x, is.na), not)))
  }}}}
  stop("\n • [x] must be populated (?ipop) and atomic, a populated atomic vlist (?ivls), or a populated atomic tabular (?itab).")
}

#' @describeIn na. Substitute \code{s} for \code{NA} values of \code{x}.
#'   Returns an object with the same dimensions as \code{x} where all \code{NA}
#'   values are replaced by \code{s}. Note that the modes of \code{x} and
#'   \code{s} must be compatible (\code{\link{compatible}}).
#' @export
sub_na <- function(x, s) {
  if (length(x) == 0) {return(x)}
  if (is.atomic(x)) {
    if (compatible(x, s)) {x[is.na(x)] <- s; return(x)}
    else {stop("\n • [x] and [s] are of incompatible modes (?compatible).")}
  } else if (is.data.frame(x)) {
    if (all(apply(x, 2, is.atomic))) {
      if (all(apply(x, 2, compatible, s))) {x[is.na(x)] <- s; return(x)}
      else {stop("\n • [s] is incompatible (?incompatible) with one or more columns of [x].")}
    } else {stop("\n • When [x] is a dtf (?is_dtf), its columns must be atomic.")}
  } else if (any_vls(x)) {
    if (all(sapply(x, is.atomic))) {
      if (all(sapply(x, compatible, s))) {return(lapply(x, sub_na, s))}
      else {stop("\n • [s] is incompatible (?incompatible) with one or more elements of [x].")}
    } else {stop("\n • When [x] is a vlist (?is_vls), its elements must be atomic.")}
  } else {stop("\n • [x] must be an atomic vlist (?ivls), atomic dtf (?idtf), or some other atomic object.")}
}

#' @describeIn na. Is \code{x} an \code{NA} scalar? Returns \code{TRUE} or
#'   \code{FALSE} according to whether \code{x} is an \code{NA} scalar.
#' @export
nas <- function(x) {if (n1(x) & iatm(x)) {is.na(x)} else {F}}

#' @describeIn na. Is \code{x} a non-\code{NA} scalar? Returns \code{TRUE} or
#'   \code{FALSE} according to whether \code{x} is a non-\code{NA} scalar.
#' @export
oks <- function(x) {if (n1(x) & iatm(x)) {!is.na(x)} else {F}}

#' @describeIn na. Remove \code{NA} values of \code{x}. If there are no
#'   \code{NA} values in \code{x}, this function returns \code{x} unchanged.
#'   \strong{However}, if there are \code{NA} values in \code{x}, by removing
#'   \code{NA} values from \code{x}, this function changes the dimensions of
#'   and/or class of \code{x}. If \code{x} is an array or an
#'   \code{\link[itab]{atomic tabular}}, its class is reduced to an atomic
#'   vector sans \code{NA} values. If \code{x} is an \code{\link[ivls]{atomic
#'   vlist}}, any elements with \code{NA} values are reduced to atomic vectors
#'   sans their \code{NA} values.
#' @export
rm_na <- function(x) {
  if (is.atomic(x)) {return(x[!is.na(x)])}
  else if (is.data.frame(x)) {if (all(apply(x, 2, is.atomic))) {
    if (!any(is.na(x))) {return(x)}
    x <- av(x)
    return(x[!is.na(x)])
  }}
  else if (any_vls(x)) {if (all(sapply(x, is.atomic))) {return(lapply(x, rm_na))}}
  stop("\n • [x] must be populated (?ipop) and either an atomic object, an atomic vlist (?ivls), or an atomic dtf (?idtf).")
}
