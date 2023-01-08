#' @encoding UTF-8
#' @family extensions
#' @family values
#' @title Atomize (reduce arguments to an atomic vector)
#' @description \tabular{rl}{
#'      `atomize, a`   \tab Gets a vector of all atomic
#'   \cr `atoms, av`   \tab values in all `...` arguments.
#'   \cr               \tab   
#'   \cr       `sav`   \tab Gets `sort(a(...))`.
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
atomize <- function(...) {x <- base::as.vector(base::unlist(base::list(...), T, F)); base::attributes(x) <- NULL; x}

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
sav <- function(...) {base::sort(uj::atomize(...))}
