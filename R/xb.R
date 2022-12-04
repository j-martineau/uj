#' @family validated
#' @title Error-checked row and column binding
#' @description Error-checked extensions of \code{\link[base:rbind]{base::rbind}} and/or \code{\link[base:cbind]{base::cbind}}.
#' @section Functions:`xb`\cr Row binds if `...` arguments are compatible only for row binding. Column binds if arguments are compatible only for column binding. Throws an error if arguments are either compatible for both row and column binding or not compatible for binding at all.
#'   \cr\cr`cb`\cr Column binds an arbitrary number of atomic matrices or \link[=atm_dtf]{atomic data.frames}.
#'   \cr\cr`rb`\cr Row binds an arbitrary number of atomic matrices or atomic data.frames.
#' @param ... Multiple \code{\link{compatible}} atomic matrices or multiple compatible \link[=atm_dtf]{atomic data.frames}.
#' @return An \link[=atm_dtf]{atomic data.frame} or an atomic matrix.
#' @examples
#' # MATRIX BINDING
#'
#' # (setup)
#' m2x4 <- mat(1:8, nr = 2)
#' m3x4 <- mat(1:12, nr = 3)
#' m4x4 <- mat(1:16, nr = 4)
#' m4x4b <- m4x4
#' m3x4c <- as.character(m4x4)
#'
#' # >> valid cbind / xbind
#' cb(m2x4, m3x4 , m4x4 , m4x4b)
#' xb(m2x4, m3x4 , m4x4 , m4x4b)
#'
#' # >> valid rbind / xbind
#' rb(m2x4, m3x4 , m4x4 , m4x4b)
#' xb(m2x4, m3x4 , m4x4 , m4x4b)
#'
#' # !! mode mismatched cbind / rbind
#' rb(m4x4, m3x4c)
#'
#' # !! ambiguous xbind
#' xb(m4x4, m4x4b)
#'
#' # DATA.FRAME BINDING
#'
#' # >> setup
#' d4x2 <- dtf(w = 1:4, x = letters[1:4])
#' d4x4 <- dtf(w = 1:4, x = letters[1:4], y = NA, z = '')
#' d2x4 <- dtf(w = 1:2, x = letters[1:2], y = NA, z = '')
#' d4x4b <- d4x4
#' d4x4c <- dtf(w = as.character(1:4), x = letters[1:4], y = NA, z = '')
#' d4x4d <- dtf(W = 1:4, X = letters[1:4], Y = NA, Z = '')
#'
#' # >> valid cbind / xbind
#' cb(d4x4, d4x4d)
#' xb(d4x4, d4x4d)
#'
#' # >> valid rbind / xbind
#' rb(d4x2 , d4x4 , d4x4b)
#' xb(d4x2 , d4x4 , d4x4b)
#'
#' # !! rbind with ncol mismatch
#' rb(d4x2, d4x4)
#'
#' #!! rbind with nrow mismatch
#' cb(d2x4, d4x2)
#'
#' # !! rbind with mode mismatch
#' rb(d4x4, d4x4c)
#'
#' # !! rbind with name mismatch
#' rb(d4x4, d4x4d)
#'
#' # !! cbind with duplicate column names
#' cb(d4x4, d4x4b)
#' @export
xb <- function(...) {
  if (...length() < 2) {stop("\n \u2022 [...] contains fewer than 2 arguments.")}
  x <- list(...)
  dtf <- all(sapply(x, atm_dtf))
  mat <- all(sapply(x, atm_mat))
  if (!dtf & !mat) {stop("\n \u2022 [...] must contain only atomic matrices or only atomic data.frames.")}
  ok.r <- f0(dtf, compatible_dtfs("r", ...), compatible_mats("r", ...))
  ok.c <- f0(dtf, compatible_dtfs("c", ...), compatible_mats("c", ...))
  if (ok.r & ok.c) {stop("\n \u2022 Arguments in [...] are compatible for both row and column binding. Use rb(...) or cb(...) instead.")}
  else if (ok.r) {rbind(...)}
  else if (ok.c) {rbind(...)}
  else {stop("\n \u2022 Arguments in [...] are not compatible for either row or column binding.")}
}

#' @rdname xb
#' @export
cb <- function(...) {
  if (...length() < 2) {stop("\n \u2022 [...] contains fewer than 2 arguments.")}
  x <- list(...)
  dtf <- all(sapply(x, atm_dtf))
  mat <- all(sapply(x, atm_mat))
  if (!dtf & !mat) {stop("\n \u2022 [...] must contain only atomic matrices or only atomic data.frames.")}
  if (f0(dtf, compatible_dtfs("c", ...), compatible_mats("c", ...))) {cbind(...)}
  else {stop("\n \u2022 Arguments in [...] are not compatible for column binding.")}
}

#' @rdname xb
#' @export
rb <- function(...) {
  if (...length() < 2) {stop("\n \u2022 [...] contains fewer than 2 arguments.")}
  x <- list(...)
  dtf <- all(sapply(x, atm_dtf))
  mat <- all(sapply(x, atm_mat))
  if (!dtf & !mat) {stop("\n \u2022 [...] must contain only atomic matrices or only atomic data.frames.")}
  if (f0(dtf, compatible_dtfs("r", ...), compatible_mats("r", ...))) {cbind(...)}
  else {stop("\n \u2022 Arguments in [...] are not compatible for row binding.")}
}
