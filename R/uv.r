#' @name uv
#' @title Reduce `...` to a vector of unique atomic values
#' @description \link[=av]{Atomize} `...` and reduce the result to unique constituent atomic values.
#' @param ... One or more objects to be reduced to a single atomic vector and then further reduced to unique atomic values.
#' @return An atomic vector.
#' @examples uv(0:5, 5:10, 10:15, 15:20)
#' @export
uv <- function(...) {unique(av(...))}

#' @rdname uv
#' @export
uav <- uv
