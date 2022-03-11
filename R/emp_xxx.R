#' @name emp_xxx
#' @title Empty + Fundamental Type or Extended Class (emp + fff or ccc)
#' @description See \code{\link{xemp}}, \code{link{xatm}}, \code{link{xrcr}},
#'   \code{link{is_vtype}}, \code{link{is.matrix}},
#'   \code{link[tibble]{is_tibble}}, and \code{\link{is_vlist}}.
#' @param x An object
#' @export
emp_xxx_vals <- function() {
  x <- c('emp_arr', 'emp_atm', 'emp_mat', 'emp_rcr',
         'emp_tib', 'emp_vls', 'emp_vtp')
  names(x) <- rep.int("emp_xxx", length(x))
  x
}

#' @rdname emp_xxx
#' @export
emp_arr <- function(x) {length(x) == 0 & is.array(x)}

#' @rdname emp_xxx
#' @export
emp_atm <- function(x) {length(x) == 0 & is.atomic(x)}

#' @rdname emp_xxx
#' @export
emp_gen <- function(x) {is_emp_generic(x)}

#' @rdname emp_xxx
#' @export
emp_mat <- function(x) {is_emp_matrix(x)}

#' @rdname emp_xxx
#' @export
emp_rcr <- function(x) {length(x) == 0 & is.recursive(x)}

#' @rdname emp_xxx
#' @export
emp_tib <- function(x) {is_emp_tibble(x)}

#' @rdname emp_xxx
#' @export
emp_vls <- function(x) {is_emp_vlist()}

#' @rdname emp_xxx
#' @export
emp_vtp <- function(x) {is_emp_vtype(x)}
