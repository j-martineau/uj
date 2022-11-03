#' @title Atomize (reduce to atomic values)
#' @family meta
#' @description Reduce all arguments in \code{...} to a single atomic vector of
#'   atomic elements without any additional attributes.
#' @param ... Arguments to be atomized.
#' @return A single atomic vector containing all constituent atomic values in
#'   the full set of arguments supplied in \code{...}.
#' @examples
#' Fruits <- c("apples" , "bananas", "oranges")
#' Digits <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
#' Both   <- list(Fruits, Digits)
#' av(Fruits)
#' av(Digits)
#' av(Both)
#' av("celery", "beets", "lettuce")
#' av("celery", "beets", "lettuce")
#' @export
av <- function(...) {x. <- as.vector(unlist(list(...), T, F)); attributes(x.) <- NULL; x.}
