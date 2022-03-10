#' @name na_vals
#' @family meta
#' @title Manage NA and Non-NA Values
#' @description Identify \code{NA} values, identify OK values (i.e., not
#'   \code{NA}), substitute a value for \code{NA} values, or remove \code{NA}
#'   values.
#' @details All functions in this group take atomic objects,
#'   \link[=is_atm_tibble]{atomic tibbles} and
#'   \link[=is_atm_vlist]{atomic vlists}.
#'   \cr\cr
#'   \strong{\code{na}}
#'   \cr Returns an object with the same dimensions as \code{x} where all atomic
#'   values are replaced by either \code{TRUE} or \code{FALSE} indicating
#'   whether the original atomic value was \code{NA}.
#'   \cr\cr
#'   \strong{\code{ok}}
#'   \cr Returns an object with the same dimensions as \code{x} where all atomic
#'   values are replaced by either \code{TRUE} or \code{FALSE} indicating
#'   whether the original atomic value was OK (i.e., not \code{NA}).
#'   \cr\cr
#'   \strong{\code{sub.na}}
#'   \cr Returns an object with the same dimensions as \code{x} where all
#'   \code{NA} values are replaced by \code{s}. Note that the modes of \code{x}
#'   and \code{s} must be \link[=compatible]{compatible}.
#'   \cr\cr
#'   \strong{\code{rm.na}}
#'   If there are no \code{NA} values in \code{x}, this function returns
#'   \code{x} unchanged.
#'   \cr\cr
#'   However, if there are \code{NA} values in \code{x}, by removing \code{NA}
#'   values from \code{x}, this function changes the dimensions of and/or class
#'   of \code{x}. If \code{x} is an array or an atomic tibble, its class is
#'   reduced to an atomic vector sans \code{NA} values. If \code{x} is an atomic
#'   vlist, any elements with \code{NA} values are reduced to atomic vectors
#'   sans their \code{NA} values.
#' @param x The argument to be inspected/managed.
#' @param s Atomic scalar to replace \code{NA} values. Mode must be
#'   \link[=compatible]{compatible} with \code{x}.
#' @return An atomic scalar, an atomic vector, an atomic array, an
#'   \link[=is_atm_tibble]{atomic tibble}, or an
#'   \link[=is_atm_vlist]{atomic vlist}.
#' @export
na <- function(x) {
  if (length(x) > 0) {
    if (is.atomic(x)) {return(is.na(x))}
    else if (is.data.frame(x)) {
      if (all(apply(x, 2, is.atomic))) {
        return(apply(x, 2, is.na))
      }
    }
    if (is_vlist(x)) {
      if (all(lengths(x) > 0)) {
        if (all(sapply(x, is.atomic(x)))) {
          return(all(lapply(x, is.na)))
        }
      }
    }
  }
  stop("\n  * [x] must be non empty and atomic, a non-empty atomic vlist, or a non-empty atomic tibble.")
}

#' @rdname na_vals
#' @export
ok <- function(x) {
  if (length(x) > 0) {
    if (is.atomic(x)) {return(!is.na(x))}
    else if (is.data.frame(x)) {
      if (all(apply(x, 2, is.atomic))) {
        return(apply(apply(x, 2, is.na), not))
      }
    }
    if (is_vlist(x)) {
      if (all(lengths(x) > 0)) {
        if (all(sapply(x, is.atomic(x)))) {
          return(all(lapply(lapply(x, is.na), not)))
        }
      }
    }
  }
  stop("\n  * [x] must be non empty and atomic, a non-empty atomic vlist, or a non-empty atomic tibble.")
}

#' @rdname na_vals
#' @export
sub_na <- function(x, s) {
  if (length(x) == 0) {return(x)}
  if (is.atomic(x)) {
    if (compatible(x, s)) {
      x[is.na(x)] <- s
      return(x)
    }
    else {stop("\n  * [x] and [s] are of incompatible modes.")}
  }
  else if (is.data.frame(x)) {
    if (all(apply(x, 2, is.atomic))) {
      if (all(apply(x, 2, compatible, s))) {
        x[is.na(x)] <- s
        return(x)
      }
      else {stop("\n  * [s] is incompatible with one or more columns of [x].")}
    }
    else {stop("\n  * When [x] is a data.frame, its columns must be atomic.")}
  }
  else if (is_vlist(x)) {
    if (all(sapply(x, is.atomic))) {
      if (all(sapply(x, compatible, s))) {
        return(lapply(x, sub_na, s))
      }
      else {stop("\n  * [s] is incompatible with one or more elements of [x].")}
    }
    else {stop("\n  * When [x] is a vlist, its elements must be atomic.")}
  }
  else {stop("\n  * [x] must be atomic, an atomic vlist, or an atomic data.frame.")}
}

#' @rdname na_vals
#' @export
rm_na <- function(x) {
  if (is.atomic(x)) {return(x[!is.na(x)])}
  else if (is.data.frame(x)) {
    if (all(apply(x, 2, is.atomic))) {
      if (!any(is.na(x))) {
        return(x)
      }
      x <- av(x)
      return(x[!is.na(x)])
    }
  }
  else if (is_vlist(x)) {
    if (all(sapply(x, is.atomic))) {
      return(lapply(x, rm_na))
    }
  }
  stop("\n  * [x] must be non empty and atomic, a non-empty atomic vlist, or a non-empty atomic tibble.")
}
