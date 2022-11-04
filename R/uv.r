#' @name uv_uj
#' @title Reduce ... to a vector of unique atomic values
#' @description Atomizes \code{...}, reducing the set of arguments to a single
#'   vector of unique constituent atomic values.
#' @param ... One or more objects to be reduced to a single atomic vector and
#'   then further reduced to unique atomic values.
#' @return An atomic vector.
#' @examples uv(0:5, 5:10, 10:15, 15:20)
#' @export
uv_uj <- function() {help("uv_uj", package = "uj")}

#' @rdname uv_uj
#' @export
uv <- function(...) {unique(av(...))}

#' @rdname uv_uj
#' @export
uav <- uv
