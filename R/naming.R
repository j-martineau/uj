#' @name naming
#' @title Get or set element, row, and/or column names.
#' @description Name elements of a vector.
#' @param x Vector or vlist for \code{namev} and \code{vn}; matrix or data.frame
#'   for \code{namer}, \code{namec}, \code{namerc}, \code{rn}, and \code{cn}; or
#'   any object for \code{namel}.
#' @param n Character scalar element-name.
#' @param vn Character vector of element names.
#' @param rn,cn Character vectors of row names or column names, respectively.
#' @return Named/renamed version of \code{x}.
#' @export
namev <- function(x, vn) {
  bank_funs(imvc, x = x)
  bank_funs(atm_vec, vn = vn, n. = length(x))
  err_check()
  names(x) <- vn
  x
}

#' @describeIn naming Name the rows of a matrix or data frame.
#' @export
namer <- function(x, rn) {
  bank_funs(id2D, x = x)
  bank_funs(atm_vec, rn = rn, n. = NROW(x))
  err_check()
  rownames(x) <- rn
  x
}

#' @describeIn naming Name the columns of a matrix or data frame.
#' @export
namec <- function(x, cn) {
  bank_funs(id2D, x = x)
  bank_funs(atm_vec, cn = cn, n. = NCOL(x))
  err_check()
  colnames(x) <- cn
  x
}

#' @describeIn naming Create a 1-element list from \code{x} and name that
#'   element with the value of \code{e}
#' @export
namel <- function(x, n) {
  bank_funs(atm_scl, n)
  err_check()
  x <- list(x)
  names(x) <- n
  x
}

#' @describeIn naming Name the rows and columns of a matrix or data frame.
#' @export
namerc <- function(x, r, c) {namer(namec(x, c), r)}

#' @describeIn naming Get element names.
#' @export.
vn <- function(x) {names(x)}

#' @describeIn naming Get column names.
#' @export.
cn <- function(x) {colnames(x)}

#' @describeIn naming Get row names.
#' @export.
rn <- function(x) {rownames(x)}
