#' @encoding UTF-8
#' @family properties
#' @family logicals
#' @family counts
#' @title Evaluate whether an object meets count and/or value restrictions
#' @description Evaluates whether `x` meets any count and value restrictions are provided in `...`. If none are provided, returns `TRUE`.
#' @param x An object.
#' @param ... Optional named arguments count and/or value restrictions for `x`. See the *specifying count and value restrictions* section.
#' @section Specifying count and value restrictions: Specifying restrictions in `...` is optional. The full set of recognized arguments names are defined in the following table along with the properties each specifies:
#' \tabular{ll}{  `max, maxr, maxc`   \tab Scalar maximum valid numbers of element, rows, and columns, respectively.                                                                  \cr   \tab   \cr
#'                `min, minr, minc`   \tab Scalar minimum valid numbers of element, rows, and columns, respectively.                                                                  \cr   \tab   \cr
#'                `lt, le, ge, gt`    \tab \link[=cmp_srt_scl]{Complete sortable scalar} less-than, less-than-or-equal, greater-than-or-equal, and greater-than bounds, respectively. \cr   \tab   \cr
#'                `n, nr, nc`         \tab A vector of valid numbers of elements, rows, and columns, respectively.                                                                    \cr   \tab   \cr
#'                `vals`              \tab A vector of valid values.                                                                                                                                 }
#' @return A logical scalar.
#' @examples
#' chrs <- c("a", "b", "c")
#' nums <- 1:3
#' sqrd <- matrix(1:16, nrow = 4)
#' meets(chrs, n = 1:5)
#' meets(chrs, max = 2)
#' meets(chrs, min = 3)
#' meets(sqrd, nr = 3, nr = 2:10)
#' meets(sqrd, minr = 2, maxr = 5, minc = 4, maxc = 5)
#' meets(chrs, vals = letters)
#' meets(chrs, gt = "a", lt = "c")
#' meets(sqrd, ge = 1, le = 16)
#' @export
meets <- function(x, ...) {
  if (base::...length() == 0) {return(T)}
  errs <- uj:::.meets_errs(x, ...)
  if (!base::is.null(errs)) {uj::stopperr(errs, PKG = "uj")}
  atoms <- uj::av(x)
  nx <- uj::f0(uj:::.D1D(x), base::length(x), base::prod(base::dim(x)))
  nr <- base::NROW(x)
  nc <- base::NCOL(x)
  atoms <- atoms[!base::is.na(atoms)]
  d <- base::list(...)
  if (base::length(atoms) == 0) {return(T)}
  else if (!base::is.null(d$n)) {if (!(nx %in% d$n)) {return(F)}}
  else if (!base::is.null(d$nr)) {if (!(nx %in% d$nr)) {return(F)}}
  else if (!base::is.null(d$nc)) {if (!(nx %in% d$nc)) {return(F)}}
  else if (!base::is.null(d$le)) {if (base::any(atoms > d$le)) {return(F)}}
  else if (!base::is.null(d$ge)) {if (base::any(atoms < d$ge)) {return(F)}}
  else if (!base::is.null(d$lt)) {if (base::any(atoms >= d$lt)) {return(F)}}
  else if (!base::is.null(d$gt)) {if (base::any(atoms <= d$gt)) {return(F)}}
  else if (!base::is.null(d$min)) {if (!(nx >= d$min)) {return(F)}}
  else if (!base::is.null(d$max)) {if (!(nx <= d$max)) {return(F)}}
  else if (!base::is.null(d$minr)) {if (!(nr >= d$minr)) {return(F)}}
  else if (!base::is.null(d$maxr)) {if (!(nr <= d$maxr)) {return(F)}}
  else if (!base::is.null(d$minc)) {if (!(nc >= d$minc)) {return(F)}}
  else if (!base::is.null(d$maxc)) {if (!(nc <= d$maxc)) {return(F)}}
  else if (!base::is.null(d$vals)) {if (base::all(atoms %in% d$vals)) {return(F)}}
  T
}
