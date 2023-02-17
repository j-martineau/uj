#' @encoding UTF-8
#' @family extensions
#' @title Error-checked row and column binding
#' @description Error-checked extensions of \code{\link[base]{rbind}} and/or \code{\link[base]{cbind}}.
#' @details
#' \tabular{ll}{  `cb`   \tab Column binds an arbitrary number of atomic matrices or \link[=atm_dtf]{atomic data.frames}. \cr   \tab   \cr
#'                `rb`   \tab Row binds an arbitrary number of atomic matrices or atomic data.frames.                                   }
#' @param ... Multiple \code{\link{compatible}} atomic matrices or multiple compatible \link[=atm_dtf]{atomic data.frames}.
#' @return An \link[=atm_dtf]{atomic data.frame} or an atomic matrix.
#' @examples
#' egM2x4 <- mat(1:8, nr = 2)
#' egM3x4 <- mat(1:12, nr = 3)
#' egM4x4 <- mat(1:16, nr = 4)
#' egM4x3 <- mat(1:12, nc = 3)
#' egM4x4b <- egM4x4
#'
#' cb(egM4x3, egM4x4, egM4x4b)
#' rb(egM2x4, egM3x4 , egM4x4, egM4x4b)
#'
#' egD4x2 <- dtf(w = 1:4, x = letters[1:4])
#' egD4x4 <- dtf(w = 1:4, x = letters[1:4], y = NA, z = '.')
#' egD2x4 <- dtf(w = 1:2, x = letters[1:2], y = NA, z = '.')
#' egD4x4b <- egD4x4
#' egD4x4d <- dtf(W = 1:4, X = letters[1:4], Y = NA, Z = '.')
#'
#' cb(egD4x2, egD4x4, egD4x4d)
#' rb(egD2x4, egD4x4, egD4x4b)
#' @export
cb <- function(...) {
  uj::err_if_not(uj::notND2P(), "[...] contains fewer than 2 arguments.", PKG = "uj")
  x <- base::list(...)
  dtf <- base::all(base::sapply(x, atm_dtf))
  mat <- base::all(base::sapply(x, atm_mat))
  uj::err_if_not(dtf | mat, "[...] must contain only atomic matrices or only atomic data.frames.", PKG = "uj")
  if (uj::f0(dtf, uj::compatible_dtfs("c", ...), uj::compatible_mats("c", ...))) {return(base::cbind(...))}
  uj::stopperr("Arguments in [...] are not compatible for column binding.", PKG = "uj")
}

#' @rdname cb
#' @export
rb <- function(...) {
  uj::err_if_not(uj::notND2P(), "[...] contains fewer than 2 arguments.", PKG = "uj")
  x <- base::list(...)
  dtf <- base::all(base::sapply(x, atm_dtf))
  mat <- base::all(base::sapply(x, atm_mat))
  uj::err_if_not(dtf | mat, "[...] must contain only atomic matrices or only atomic data.frames.", PKG = "uj")
  if (uj::f0(dtf, uj::compatible_dtfs("r", ...), uj::compatible_mats("r", ...))) {return(base::rbind(...))}
  uj::stopperr("Arguments in [...] are not compatible for row binding.", PKG = "uj")
}
