#' @name ex.
#' @family extensions
#' @title Extract rows, columns, and/or elements
#' @param x Any valid R object.
#' @param r \link[cmp_ind_vec]{Complete indexer vec} indicating rows to extract
#'   from \code{x} of \link[id2D]{defined dimensionality 2}.
#' @param c \link[cmp_ind_vec]{Complete indexer vec} indicating columns to
#'   extract from \code{x} of \link[id2D]{defined dimensionality 2}.
#' @param r \link[cmp_ind_vec]{Complete indexer vec} indicating elements to
#'   extract from \code{x} of \link[id1D]{defined dimensionality 1}.
#' @examples
#' Mat <- matrix(1:100, nrow = 10)
#' Dat   <- as_tibble(Mat)
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
ex. <- function() {help("ex.", package = "uj")}

#' @describeIn ex. Extract specific rows and/or columns of matrices and
#'   \link[idtf]{dtfs} or extract specific elements of \link[ivec]{vecs} or
#'   \link[ivls]{vlists}.
ex <- function(x, r = NULL, c = NULL, e = NULL) {
  vx <- ipop(x)
  vr <- f0(inll(r), T, cmp_psw_vec(r))
  vc <- f0(inll(c), T, cmp_psw_vec(c))
  ve <- f0(inll(e), T, cmp_psw_vec(e))
  err  <- NULL
  if (!fx) {err <- c(err, "\n • [x] must be populated (?ipop).")}
  if (!vr) {err <- c(err, "\n • [r] must be NULL or a positive whole-number scalar/vector (?cmp_psw_scl, ?cmp_psw_vec).")}
  if (!vc) {err <- c(err, "\n • [c] must be NULL or a positive whole-number scalar/vector (?cmp_psw_scl, ?cmp_psw_vec).")}
  if (!ve) {err <- c(err, "\n • [e] must be NULL or a positive whole-number scalar/vector (?cmp_psw_scl, ?cmp_psw_vec).")}
  if (idef(err)) {stop(err)}
  if (id2D(x)) {
    ve <- inll(e)
    vr <- f0(inll(r), T, all(r <= nrow(x)))
    vc <- f0(inll(c), T, all(c <= ncol(x)))
    err <- NULL
    if (!ve) {err <- c(err, "\n • [x] is rectangular, but [e] specifies individual elements to extract from [x].")}
    if (!vx) {err <- c(err, "\n • [x] is rectangular, but both [r] and [c] are NULL.")}
    if (!vr) {err <- c(err, "\n • [r] contains a value that does not point to any row in [x].")}
    if (!vc) {err <- c(err, "\n • [c] contains a value that does not point to any column in [x].")}
    if (idef(err)) {stop(err)}
    if (inll(r)) {r <- 1:nrow(x)}
    if (inll(c)) {c <- 1:ncol(x)}
    x <- x[r, c]
  }
  else {
    vc <- ivec(x) | ivls(x)
    vd <- f0(!vx, T, idef(e))
    vr <- inll(r)
    vc <- inll(c)
    ve <- f0(!vx, T, all(e <= length(x)))
    err <- NULL
    if (!vd) {c(err, "\n • [x] is neither rectangular nor a populated vector or vlist.")}
    if (!vx) {c(err, "\n • [x] is a populated vector or vlist (?pop_vec, ?pop_vls), but [e = NULL].")}
    if (!vr) {c(err, "\n • [r] is a populated vector or vlist (?pop_vec, ?pop_vls), but [r] specifies rows to extract.")}
    if (!vc) {c(err, "\n • [c] is a populated vector or vlist (?pop_vec, ?pop_vls), but [c] specifies columns to extract.")}
    if (!ve) {c(err, "\n • [e] contains values that do not point to any element of [x].")}
    if (idef(err)) {stop(err)}
    x[e]
  }
}
