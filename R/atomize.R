#' @title Atomize (reduce arguments to an atomic vector)
#' @description \tabular{rl}{
#'     `atomize`   \tab Gets a vector of all
#'   \cr `atoms`   \tab atomic values in all
#'   \cr    `av`   \tab `...` arguments.
#'   \cr     `a`   \tab   
#'   \cr           \tab   
#'   \cr   `sav`   \tab Gets a sorted vector of
#'   \cr           \tab all atomic values in all
#'   \cr           \tab `...` arguments.
#' }
#' @param ... Arguments to be atomized.
#' @return A single atomic vector containing all constituent atomic values in the full set of arguments supplied in `...`.
#' @export
#' @examples
#' x <- list("a", "b", "c")
#' y <- data.frame(1:3)
#'
#' x
#' y
#'
#' a(x)
#' av(y)
#' atoms(x, y)
#' atomize(x, "d", y, 4)
#' sav(x, "d", y, 4)
atomize <- function(...) {x <- as.vector(unlist(list(...), T, F)); attributes(x) <- NULL; x}

#' @rdname atomize
#' @export
atoms <- atomize

#' @rdname atomize
#' @export
av <- atomize

#' @rdname atomize
#' @export
a <- atomize

#' @rdname atomize
#' @export
sav <- function(...) {sort(atomize(...))}
