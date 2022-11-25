#' @family extensions
#' @description ex. Extract rows and/or columns of \link[=id2D]{two-dimensional
#'   objects} or elements of \link[=ie1D]{effectively one-dimensional objects}.
#' @title Extract Rows, Columns, and/or Elements
#' @param x Any valid R object.
#' @param r A \link[=cmp_ind_vec]{complete indexer vec} indicating rows to
#'   extract from `x` of \link[=id2D]{defined dimensionality `2`}.
#' @param c A \link[=cmp_ind_vec]{complete indexer vec} indicating columns to
#'   extract from `x` of \link[=id2D]{defined dimensionality `2`}.
#' @param r A \link[=cmp_ind_vec]{complete indexer vec} indicating elements to
#'   extract from `x` of \link[=id1D]{defined dimensionality `1`}.
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
ex <- function(x, r = NULL, c = NULL, e = NULL) {
  errs <- c(f0(ipop(x)                 , NULL, "\n \u2022 [x] must be populated (?ipop)."),
            f0(inll(r) | cmp_psw_vec(r), NULL, "\n \u2022 [r] must be NULL or a positive whole-number scalar/vector (?cmp_psw_scl, ?cmp_psw_vec)."),
            f0(inll(c) | cmp_psw_vec(c), NULL, "\n \u2022 [c] must be NULL or a positive whole-number scalar/vector (?cmp_psw_scl, ?cmp_psw_vec)."),
            f0(inll(e) | cmp_psw_vec(e), NULL, "\n \u2022 [e] must be NULL or a positive whole-number scalar/vector (?cmp_psw_scl, ?cmp_psw_vec)."))
  if (!is.null(errs)) {stop(errs)}
  if (imat(x) | idtf(x)) {
    errs <- c(f0(inll(e)                          , NULL, "\n \u2022 [x] is not a vector of vlist, but [e] specifies elements to extract."),
              f0(idef(r) | idef(c)                , NULL, "\n \u2022 [x] is a matrix or data frame, but both [r] and [c] are NULL."       ),
              f0(f0(inll(r), T, all(r <= nrow(x))), NULL, "\n \u2022 [r] contains a value that does not point to any row in [x]."         ),
              f0(f0(inll(c), T, all(c <= ncol(x))), NULL, "\n \u2022 [c] contains a value that does not point to any column in [x]."      ))
    if (!is.null(errs)) {stop(errs)}
    if (inll(r)) {r <- 1:nrow(x)}
    if (inll(c)) {c <- 1:ncol(x)}
    x <- x[r, c]
  } else if (ivec(x) & ivls(x)) {
    ok.e <- f0(psw_vec(e), all(e <= length(x), na.rm = TRUE), idef(e))
    errs <- c(f0(idef(e), NULL, "\n \u2022 [x] is a vector or vlist (?ivls), but [e = NULL]."),
              f0(inll(c), NULL, "\n \u2022 [x] is a not a matrix or data frame, but [c] specifies columns to extract."),
              f0(inll(r), NULL, "\n \u2022 [x] is a not a matrix or data frame, but [r] specifies rows to extract."),
              f0(ok.e   , NULL, "\n \u2022 [e] contains values that do not point to any element of [x]."))
    if (!is.null(errs)) {stop(errs)}
    x[e]
  }
  else {stop("\n \u2022 [x] is neither a matrix, data.frame, vector, or vlist (?ivls).")}
}
