#' @name cmp_ccc
#' @family props
#' @title Complete + Extended Class (cmp + ccc)
#' @description See \code{\link{cmp}}, and \code{\link{ccc}}.
#' @param x An object
#' @return \code{TRUE} or \code{FALSE}
#' @export
cmp_ccc_vals <- function() {
  x <- paste0("", c("cmp", ccc_vals()))
  names(x) <- rep.int("cmp_ccc", length(x))
  x
}

#' @rdname cmp_ccc
#' @export
cmp_arr <- function(x) {is_cmp_array(x)}

#' @rdname cmp_ccc
#' @export
cmp_agn <- function(x) {is_cmp_generic(x)}

#' @rdname cmp_ccc
#' @export
cmp_atb <- function(x) {is_cmp_tibble(x)}

#' @rdname cmp_ccc
#' @export
cmp_avl <- function(x) {is_cmp_vlist(x)}

#' @rdname cmp_ccc
#' @export
cmp_avt <- function(x) {is_cmp_vtype(x)}

#' @rdname cmp_ccc
#' @export
cmp_mat <- function(x) {is_cmp_matrix(x)}

#' @rdname cmp_ccc
#' @export
cmp_mvc <- function(x) {is_cmp_mvect(x)}

#' @rdname cmp_ccc
#' @export
cmp_scl <- function(x) {is_cmp_scalar(x)}

#' @rdname cmp_ccc
#' @export
cmp_vec <- function(x) {is_cmp_vect(x)}
