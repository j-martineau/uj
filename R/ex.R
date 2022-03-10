#' @name ex
#' @family meta
#' @title Extract rows, columns, and/or elements
#' @description Extract specific rows and/or columns of matrices and tibbles
#'   or extract specific elements of vects or vlists.
#' @param x Any valid R object.
#' @param r A logical or integer vector indexing rows to extract from \code{x}
#'   of \link[=ddim]{defined dimensionality} 2.
#' @param c A logical or integer vector indexing columns to extract from
#'   \code{x} of \link[=ddim]{defined dimensionality} 2.
#' @param e A logical or integer vector indexing elements to extract from
#'   \code{x} of \link[=ddim]{defined dimensionality} 1.
#' @examples
#' Mat <- matrix(1:100, nrow = 10)
#' Dat   <- as.tibble(MATRIX)
#' iR <- 1:10 %in% 1:5
#' iC <- 1:10 %in% 6:10
#' iE <- which(1:26 %in% 1:13)
#' ex(mat, r = 1:5, c = 6:10)
#' ex(mat, r = 1:5, c = 6:10)
#' ex(mat, r = 1:5, c = 6:10)
#' ex(mat, iR = 1:5, iC = 6:10)
#' ex(mat, r = 20)
#' ex(Dat, c = 6:10)
#' ex(Dat, e = 1:10)
#' ex(letters, e = 1:13)
#' ex(letters, e = iE)
#' ex(letters, r = iR, c = iC)
#' @export
ex <- function(x, r = NULL, c = NULL, e = NULL) {
  VX <- xpop(x)
  VR <- f0(xnll(r), T, cmp_psw_vec(r))
  VC <- f0(xnll(c), T, cmp_psw_vec(c))
  VE <- f0(xnll(e), T, cmp_psw_vec(e))
  E  <- NULL
  if (!VX) {E <- c(E, "\n  * [x] must be populated.")}
  if (!VR) {E <- c(E, "\n  * [r] must be NULL or a positive, whole-number scalar/vector.")}
  if (!VC) {E <- c(E, "\n  * [c] must be NULL or a positive, whole-number scalar/vector.")}
  if (!VE) {E <- c(E, "\n  * [e] must be NULL or a positive, whole-number scalar/vector.")}
  if (xdef(E)) {stop(E)}
  if (xd2D(x)) {
    VE <- xnll(e)
    VX <- xdef(r) | xdef(c)
    VR <- f0(xnll(r), T, all(r <= nrow(x)))
    VC <- f0(xnll(c), T, all(c <= ncol(x)))
    E <- NULL
    if (!VE) {c(E, "\n  * [x] is rectangular, but [e] specifies individual elements to extract from [x].")}
    if (!VX) {c(E, "\n  * [x] is rectangular, but both [r] and [c] are NULL.")}
    if (!VR) {c(E, "\n  * [r] contains a value that does not point to any row in [x].")}
    if (!VC) {c(E, "\n  * [c] contains a value that does not point to any column in [x].")}
    if (xdef(E)) {stop(E)}
    if (xnll(r)) {r <- 1:nrow(x)}
    if (xnll(c)) {c <- 1:ncol(x)}
    x <- x[r, c]
  }
  else {
    VD <- xvec(x) | is_vlist(x)
    VX <- f0(!VD, T, xdef(e))
    VR <- xnll(r)
    VC <- xnll(c)
    VE <- f0(!VX, T, all(e <= length(x)))
    E <- NULL
    if (!VD) {c(E, "\n  * [x] is neither rectangular nor a populated vector or vlist.")}
    if (!VX) {c(E, "\n  * [x] is a populated vector or vlist, but [e = NULL].")}
    if (!VR) {c(E, "\n  * [r] is a populated vector or vlist, but [r] specifies rows to extract.")}
    if (!VC) {c(E, "\n  * [c] is a populated vector or vlist, but [c] specifies columns to extract.")}
    if (!VE) {c(E, "\n  * [e] contains values that do not point to any element of [x].")}
    if (xdef(E)) {stop(E)}
    x[e]
  }
}
