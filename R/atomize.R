#' @encoding UTF-8
#' @family extensions
#' @family values
#' @title Atomize
#' @description Get (and maybe process) a vector of all atoms contained in the full set of `...` args.
#' @details Functions in this family are:
#' \tabular{ll}{  `atomize, atoms, av, a`   \tab Gets a vector of atomic values in `...` args. \cr
#'                `unique_av, uav, uv`      \tab Calls `unique(av(...))`                       \cr
#'                `which_av, wav, wv`       \tab Calls `which(av(...))`                        \cr
#'                `paste_av, pav, pv`       \tab Calls `paste0(p, av(...), s)`                 \cr
#'                `sort_av, sav, sv`        \tab Calls `sort(av(...))`.                        \cr
#'                `glue_av, gav, gv`        \tab Calls `paste0(av(...) collapse = g)`          \cr
#'                `sort_uv, suv`            \tab Calls `sort(unique(av(...)))`                   }
#' @param ... Arguments to be atomized. Expected to atomize to a logical vector for `which_av` and its aliases `wav` and `wv`.
#' @param g Character scalar glue.
#' @param p Character scalar prefix.
#' @param s Character scalar suffix.
#' @return **A character scalar** \cr\cr `glue_av, gav, gv`
#' \cr\cr  **A character vector** \cr\cr `paste_av, pav, pv`
#' \cr\cr  **An integer vector**  \cr\cr `which_av, wav, wv`
#' \cr\cr  **An atomic vector**   \cr\cr  All others
#' @examples
#' x <- list("a", "b", "c")
#' y <- data.frame(1:3)
#' x
#' y
#' a(x)
#' av(y)
#' atoms(x, y)
#' atomize(x, "d", y, 4)
#' glue_av(letters, LETTERS, g = "-")
#' paste_av(letters, p = "\u2022 ", s = ".")
#' sort_av(letters, LETTERS, 0:9)
#' sort_uv(letters, LETTERS, 0:9)
#' unique_av(letters, "d", 0:9, 5)
#' @export
atomize <- function(...) {
  x <- base::as.vector(base::unlist(base::list(...), T, F))
  base::attributes(x) <- NULL
  x
}

#' @rdname atomize
#' @export
glue_av <- function(..., g = "") {
  if (!uj:::.cmp_chr_scl(g)) {g <- ""}
  x <- base::as.vector(base::unlist(base::list(...), T, F))
  base::attributes(x) <- NULL
  base::paste0(x, collapse = g)
}

#' @rdname atomize
#' @export
paste_av <- function(..., p = "", s = "") {
  if (!uj:::.cmp_chr_scl(p)) {p <- ""}
  if (!uj:::.cmp_chr_scl(s)) {s <- ""}
  x <- base::as.vector(base::unlist(base::list(...), T, F))
  base::attributes(x) <- NULL
  base::paste0(base::paste(x, sep = p), s)
}

#' @rdname atomize
#' @export
sort_uv <- function(...) {
  x <- base::as.vector(base::unlist(base::list(...), T, F))
  base::attributes(x) <- NULL
  base::sort(base::unique(x))
}

#' @rdname atomize
#' @export
which_av <- function(...) {
  x <- base::as.vector(base::unlist(base::list(...), T, F))
  base::attributes(x) <- NULL
  if (!base::is.logical(x)) {NULL} else {which(x)}
}

#' @rdname atomize
#' @export
sort_av <- function(...) {
  x <- base::as.vector(base::unlist(base::list(...), T, F))
  base::attributes(x) <- NULL
  base::sort(x)
}

#' @rdname atomize
#' @export
unique_av <- function(...) {
  x <- base::as.vector(base::unlist(base::list(...), T, F))
  base::attributes(x) <- NULL
  base::unique(x)
}

#' @rdname atomize
#' @export
atoms <- atomize

#' @rdname atomize
#' @export
av <- atomize

#' @rdname atomize
#' @export
sv <- sort_av

#' @rdname atomize
#' @export
gv <- glue_av

#' @rdname atomize
#' @export
pv <- paste_av

#' @rdname atomize
#' @export
uv <- unique_av

#' @rdname atomize
#' @export
suv <- sort_uv

#' @rdname atomize
#' @export
wv <- which_av

#' @rdname atomize
#' @export
a <- atomize
