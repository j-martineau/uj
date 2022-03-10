#' @name pop_type
#' @family props
#' @title populated + ftype or xcl
#' @description See \code{\link{pop}}, \code{link{atm}}, \code{link{rcr}},
#'   \code{link{is_vtype}}, \code{link{is.matrix}},
#'   \code{link[tibble]{is_tibble}}, and \code{\link{is_vlist}}.
#' @param x An object
#' @export
pop_type_vals <- function() {
  x <- c('pop_arr', 'pop_atm', 'pop_mat', 'pop_rcr',
         'pop_tib', 'pop_vls', 'pop_vtp')
  names(x) <- rep.int("pop_type", length(x))
  x
}

#' @rdname pop_type
#' @export
pop_arr <- function(x) {length(x) > 0 & is.array(x)}

#' @rdname pop_type
#' @export
pop_atm <- function(x) {length(x) > 0 & is.atomic(x)}

#' @rdname pop_type
#' @export
pop_mat <- function(x) {length(x) > 0 & is.matrix(x)}

#' @rdname pop_type
#' @export
pop_rcr <- function(x) {length(x) > 0 & is.recursive(x)}

#' @rdname pop_type
#' @export
pop_tib <- function(x) {length(x) > 0 & tibble::is_tibble(x)}

#' @rdname pop_type
#' @export
pop_vls <- function(x) {length(x) > 0 & is_vlist(x)}

#' @rdname pop_type
#' @export
pop_vtp <- function(x) {length(x) > 0 & is_vtype(x)}
