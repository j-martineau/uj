#' @family values
#' @title Reduce all arguments to a single vector of unique atomic values
#' @description \code{\link[=av]{Atomizes}} \code{...}, reducing the set of
#'   arguments to a single vector of constituent atomic values.
#' @param ... One or more objects to be reduced to a single atomic vector and
#'   then further reduced to unique atomic values.
#' @return An atomic vector.
#' @examples uv(0:5, 5:10, 10:15, 15:20)
#' @export
uv <- function(...) {unique(av(...))}
