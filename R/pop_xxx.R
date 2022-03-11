#' @name pop_xxx
#' @family props
#' @title Populated + Fundamental Type or Extended Class (pop + ttt or ccc)
#' @description See \code{\link{xpop}}, \code{link{xatm}}, \code{link{xrcr}},
#'   \code{link{is_vtype}}, \code{link{is.matrix}},
#'   \code{link[tibble]{is_tibble}}, and \code{\link{is_vlist}}.
#' @param x An object
#' @export
pop_xxx_vals <- function() {
  x <- c('pop_arr', 'pop_atm', 'pop_mat', 'pop_rcr', 'pop_tib', 'pop_vls', 'pop_vtp')
  names(x) <- rep.int("pop_xxx", length(x))
  x
}

#' @rdname pop_xxx
#' @export
pop_arr <- function(x) {length(x) > 0 & is.array(x)}

#' @rdname pop_xxx
#' @export
pop_atm <- function(x) {length(x) > 0 & is.atomic(x)}

#' @rdname pop_xxx
#' @export
pop_mat <- function(x) {length(x) > 0 & is.matrix(x)}

#' @rdname pop_xxx
#' @export
pop_rcr <- function(x) {length(x) > 0 & is.recursive(x)}

#' @rdname pop_xxx
#' @export
pop_tib <- function(x) {length(x) > 0 & tibble::is_tibble(x)}

#' @rdname pop_xxx
#' @export
pop_vls <- function(x) {length(x) > 0 & is_vlist(x)}

#' @rdname pop_xxx
#' @export
pop_vtp <- function(x) {length(x) > 0 & is_vtype(x)}
