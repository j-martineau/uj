#' @name xb
#' @family extensions
#' @title Error-checked row and column binding
#' @description \itemize{
#'   \item **`xb`**: Row binds if `...` arguments are compatible only for row binding. Column binds if arguments are compatible only for column binding. Throws an error if arguments are either compatible for both row and column binding or not compatible for binding at all.
#'   \item **`cb`**: Column binds an arbitrary number of atomic matrices or \link[=atm_dtf]{atomic data.frames}.
#'   \item **`rb`**: Row binds an arbitrary number of atomic matrices or atomic data.frames.
#' }
#' @param ... Multiple \code{\link{compatible}} atomic matrices or multiple compatible \link[=atm_dtf]{atomic data.frames}.
#' @return An \link[=atm_dtf]{atomic data.frame} or an atomic matrix.
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
