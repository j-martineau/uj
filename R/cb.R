#' @encoding UTF-8
#' @family extensions
#' @title Error-checked row and column binding
#' @description Error-checked extensions of \code{\link[base:rbind]{base::rbind}} and/or \code{\link[base:cbind]{base::cbind}}:
#' \tabular{rl}{
#'       `cb`   \tab Column binds an arbitrary number of atomic matrices or \link[=atm_dtf]{atomic data.frames}.
#'   \cr `rb`   \tab Row binds an arbitrary number of atomic matrices or atomic data.frames.
#' }
#' @param ... Multiple \code{\link{compatible}} atomic matrices or multiple compatible \link[=atm_dtf]{atomic data.frames}.
#' @return An \link[=atm_dtf]{atomic data.frame} or an atomic matrix.
#' @examples
#' m2x4 <- mat(1:8, nr = 2)
#' m3x4 <- mat(1:12, nr = 3)
#' m4x4 <- mat(1:16, nr = 4)
#' m4x3 <- mat(1:12, nc = 3)
#' m4x4b <- m4x4
#'
#' cb(m4x3, m4x4, m4x4b)
#' rb(m2x4, m3x4 , m4x4, m4x4b)
#'
#' d4x2 <- dtf(w = 1:4, x = letters[1:4])
#' d4x4 <- dtf(w = 1:4, x = letters[1:4], y = NA, z = '.')
#' d2x4 <- dtf(w = 1:2, x = letters[1:2], y = NA, z = '.')
#' d4x4b <- d4x4
#' d4x4d <- dtf(W = 1:4, X = letters[1:4], Y = NA, Z = '.')
#'
#' cb(d4x2, d4x4, d4x4d)
#' rb(d2x4, d4x4, d4x4b)
#' @export
cb <- function(...) {
  if (base::...length() < 2) {stop(uj::format_errs(pkg = "uj", "[...] contains fewer than 2 arguments."))}
  x <- base::list(...)
  dtf <- base::all(base::sapply(x, atm_dtf))
  mat <- base::all(base::sapply(x, atm_mat))
  if (!dtf & !mat) {stop(uj::format_errs(pkg = "uj", "[...] must contain only atomic matrices or only atomic data.frames."))}
  if (uj::f0(dtf, uj::compatible_dtfs("c", ...), uj::compatible_mats("c", ...))) {base::cbind(...)}
  else {stop(uj::format_errs(pkg = "uj", "Arguments in [...] are not compatible for column binding."))}
}

#' @rdname cb
#' @export
rb <- function(...) {
  if (base::...length() < 2) {stop(uj::format_errs(pkg = "uj", "[...] contains fewer than 2 arguments."))}
  x <- base::list(...)
  dtf <- base::all(base::sapply(x, atm_dtf))
  mat <- base::all(base::sapply(x, atm_mat))
  if (!dtf & !mat) {stop(uj::format_errs(pkg = "uj", "[...] must contain only atomic matrices or only atomic data.frames."))}
  if (uj::f0(dtf, uj::compatible_dtfs("r", ...), uj::compatible_mats("r", ...))) {base::rbind(...)}
  else {stop(uj::format_errs(pkg = "uj", "Arguments in [...] are not compatible for row binding."))}
}
