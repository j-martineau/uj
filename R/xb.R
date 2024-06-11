#' @encoding UTF-8
#' @title Error-Checked Row and Column Binding of 1+ of \link[=compatible_dtfs]{Compatible Data Frames} or 1+ \link[=compatible_mats]{Compatible Matrices}
#' @description Error-checked extensions of \code{\link[base]{rbind}} and/or \code{\link[base]{cbind}} that can handle `NULL` arguments as long as there is at least one non-`NULL` `...` argument.
#' @param ... Multiple \code{\link{compatible}} atomic matrices or multiple compatible \link[=atm_dtf]{atomic data.frames}.
#' @return An \link[=atm_dtf]{atomic data.frame} or an atomic matrix.
#' @examples
#' egXb <- function() {
#'   m2x2  <- base::matrix(1:4, nrow = 2)
#'   m3x2  <- base::matrix(1:6, nrow = 3)
#'   m3x3  <- base::matrix(1:9, nrow = 3)
#'   m4x4  <- base::matrix(1:16, nrow = 4)
#'   d4x2  <- tibble::tibble(w = 1:4, x = letters[1:4])
#'   d4x4  <- tibble::tibble(w = 1:4, x = letters[1:4], y = NA, z = '.')
#'   d2x4  <- tibble::tibble(w = 1:2, x = letters[1:2], y = NA, z = '.')
#'   d4x4b <- tibble::tibble(W = 1:4, X = letters[1:4], Y = NA, Z = '.')
#'
#'   list(m2x2  = uj::cb(m2x2, NULL, NULL),
#'        m2x4  = uj::cb(m2x2, m2x2, NULL),
#'        m2x6  = uj::cb(m2x2, m2x2, m2x2),
#'        m3x2  = uj::rb(m3x2, NULL, NULL),
#'        m5x2  = uj::rb(m3x2, m2x2, NULL),
#'        m7x2  = uj::rb(m3x2, m2x2, m2x2),
#'        m12x7 = uj::cb(uj::rb(m4x4, m4x4, m4x4), uj::rb(m3x3, m3x3, m3x3, m3x3)),
#'        d4x2  = uj::cb(d4x2, NULL , NULL),
#'        d4x6  = uj::cb(d4x2, d4x4b, NULL),
#'        d4x8  = uj::cb(d4x4, d4x4b, NULL),
#'        d6x4  = uj::rb(d2x4, d4x4 , NULL),
#'        d8x4  = uj::rb(d2x4, d4x4 , d2x4))
#' }
#' egXb <- egXb()
#' @export
xb_help <- function() {utils::help("xb_help", package = "uj")}

#' @describeIn xb_help Columns binds any compatible non-`NULL` `...` arguments. If there is only one non`NULL` argument and it is a matrix or data.frame, returns that argument.
#' @export
cb <- function(...) {
  if (base::...length() > 0) {
    x <- base::list(...)
    i <- base::which(!base::sapply(x, base::is.null))
    x <- x[i]
    if (base::length(x) > 1) {
      dtf <- base::all(base::sapply(x, uj::atm_dtf))
      mat <- base::all(base::sapply(x, uj::atm_mat))
      if (!dtf & !mat) {uj::stopperr("[...] must contain only atomic matrices or only atomic data.frames.")}
      if (dtf) {code <- base::paste0("uj::compatible_dtfs(2, ", base::paste0(base::paste0("x[[", i, "]]"), collapse = ", "), ")")}
      else     {code <- base::paste0("uj::compatible_mats(2, ", base::paste0(base::paste0("x[[", i, "]]"), collapse = ", "), ")")}
      ok <- uj::run(code)
      if (ok) {
        code <- base::paste0("base::cbind(", base::paste0(base::paste0("x[[", i, "]]"), collapse = ", "), ")")
        uj::run(code)
      } else {uj::stopperr("Arguments in [...] are not atomic matrices or atomic data frames compatible for column binding.")}
    } else if (base::length(x) == 1) {
      if (uj::atm_dtf(x[[1]]) | uj::atm_mat(x[[1]])) {x[[1]]}
      else {uj::stopper("[...] contains only one argument, which is neither an atomic data.frame nor an atomic matrix.")}
    } else {uj::stopper("[...] contains only [NULL] objects.")}
  } else {uj::stopper("There are no [...] arguments.")}
}

#' @describeIn xb_help Row binds any compatible non-`NULL` `...` arguments. If there is only one non`NULL` argument and it is a matrix or data.frame, returns that argument.
#' @export
rb <- function(...) {
  if (base::...length() > 0) {
    x <- base::list(...)
    i <- base::which(!base::sapply(x, base::is.null))
    x <- x[i]
    if (base::length(x) > 1) {
      dtf <- base::all(base::sapply(x, uj::atm_dtf))
      mat <- base::all(base::sapply(x, uj::atm_mat))
      if (!dtf & !mat) {uj::stopperr("[...] must contain only atomic matrices or only atomic data.frames.")}
      if (dtf) {code <- base::paste0("uj::compatible_dtfs(1, ", base::paste0(base::paste0("x[[", i, "]]"), collapse = ", "), ")")}
      else     {code <- base::paste0("uj::compatible_mats(1, ", base::paste0(base::paste0("x[[", i, "]]"), collapse = ", "), ")")}
      ok <- uj::run(code)
      if (ok) {
        code <- base::paste0("base::rbind(", base::paste0(base::paste0("x[[", i, "]]"), collapse = ", "), ")")
        uj::run(code)
      } else {uj::stopperr("Arguments in [...] are not atomic matrices or atomic data frames for row binding.")}
    } else if (base::length(x) == 1) {
      if (uj::atm_dtf(x[[1]]) | uj::atm_mat(x[[1]])) {x[[1]]}
      else {uj::stopper("[...] contains only one argument, which is neither an atomic data.frame nor an atomic matrix.")}
    } else {uj::stopper("[...] contains only [NULL] objects.")}
  } else {uj::stopper("There are no [...] arguments.")}
}
