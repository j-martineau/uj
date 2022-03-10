#' @name cmp_xcl
#' @family props
#' @title Complete + xcl
#' @description See \code{\link{cmp}}, and \code{\link{xcl}}.
#' @param x An object
#' @return \code{TRUE} or \code{FALSE}
#' @export
cmp_xcl_vals <- function() {
  x <- paste0("", c("cmp", xcl_vals()))
  names(x) <- rep.int("cmp_xcl", length(x))
  x
}

#' @rdname cmp_xcl
#' @export
cmp_arr <- function(x) {is_cmp_array(x)}

#' @rdname cmp_xcl
#' @export
cmp_agn <- function(x) {is_cmp_generic(x)}

#' @rdname cmp_xcl
#' @export
cmp_atb <- function(x) {is_cmp_tibble(x)}

#' @rdname cmp_xcl
#' @export
cmp_avl <- function(x) {is_cmp_vlist(x)}

#' @rdname cmp_xcl
#' @export
cmp_avt <- function(x) {is_cmp_vtype(x)}

#' @rdname cmp_xcl
#' @export
cmp_mat <- function(x) {is_cmp_matrix(x)}

#' @rdname cmp_xcl
#' @export
cmp_mvc <- function(x) {is_cmp_mvect(x)}

#' @rdname cmp_xcl
#' @export
cmp_scl <- function(x) {is_cmp_scalar(x)}

#' @rdname cmp_xcl
#' @export
cmp_vec <- function(x) {is_cmp_vect(x)}
