#' @encoding UTF-8
#' @title Atomize
#' @description Get (and maybe process) a vector of all atoms contained in the full set of `...` args.
#' @param ... Arguments to be atomized. Expected to atomize to a logical vector for `which_av` and its aliases `wav` and `wv`.
#' @param g Character scalar glue for gluing.
#' @param p Character scalar prefix.
#' @param s Character scalar suffix.
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
atomize_help <- function() {utils::help("atomize_help", package = "uj")}

#' @describeIn atomize_help Flattens `...` args to an atomic vector of the constituent atomic elements of `...` without any attributes. Returns an atomic vector.
#' @export
atomize <- function(...) {
  x <- base::as.vector(base::unlist(base::list(...), T, F))
  base::attributes(x) <- NULL
  x
}

#' @describeIn atomize_help Atomizes `...` and glues the resulting atomic vector into an atomic scalar using `g` as the glue. Thinly wraps `paste0(av(...), collapse = "g")`. Returns a character scalar.
#' @export
glue_av <- function(..., g = "") {
  if (!uj::.cmp_chr_scl(g)) {g <- ""}
  x <- base::as.vector(base::unlist(base::list(...), T, F))
  base::attributes(x) <- NULL
  base::paste0(x, collapse = g)
}

#' @describeIn atomize_help Atomizes `...` and pastes a prefix and a suffix to the resulting values. Thinly wraps `paste(p, av(...), s)`. Returns a character vector.
#' @export
paste_av <- function(..., p = "", s = "") {
  if (!uj::.cmp_chr_scl(p)) {p <- ""}
  if (!uj::.cmp_chr_scl(s)) {s <- ""}
  base::paste0(p, uj::av(...), s)
}

#' @describeIn atomize_help Atomizes `...` and sorts the unique values of the result. Thinly wraps `sort(unique(av(...)))`. Returns a sorted unique atomic vector.
#' @export
sort_uv <- function(...) {
  x <- base::as.vector(base::unlist(base::list(...), T, F))
  base::attributes(x) <- NULL
  base::sort(base::unique(x))
}

#' @describeIn atomize_help Atomizes `...` and integer indexes the `TRUE` values in the result. Thinly wraps `which(av(...))`. Returns a logical vector.
#' @export
which_av <- function(...) {
  x <- base::as.vector(base::unlist(base::list(...), T, F))
  base::attributes(x) <- NULL
  if (!base::is.logical(x)) {NULL} else {which(x)}
}

#' @describeIn atomize_help Atomizes `...` and sorts the result. Thinly wraps `sort(av(...))`. Returns a sorted atomic vector.
#' @export
sort_av <- function(...) {
  x <- base::as.vector(base::unlist(base::list(...), T, F))
  base::attributes(x) <- NULL
  base::sort(x)
}

#' @describeIn atomize_help Atomizes `...` and returns the unique values of the result. Thinly wraps `unique(av(...))`. Returns a unique atomic vector.
#' @export
unique_av <- function(...) {
  x <- base::as.vector(base::unlist(base::list(...), T, F))
  base::attributes(x) <- NULL
  base::unique(x)
}

#' @describeIn atomize_help An alias for `atomize`.
#' @export
atoms <- atomize

#' @describeIn atomize_help An alias for `atomize`.
#' @export
av <- atomize

#' @describeIn atomize_help An alias for `atomize`.
#' @export
a <- atomize

#' @describeIn atomize_help An alias for `sort_av`.
#' @export
sv <- sort_av

#' @describeIn atomize_help An alias for `glue_av`.
#' @export
gv <- glue_av

#' @describeIn atomize_help An alias for `paste_av`.
#' @export
pv <- paste_av

#' @describeIn atomize_help An alias for `unique_av`.
#' @export
uv <- unique_av

#' @describeIn atomize_help An alias for `sort_uv`.
#' @export
suv <- sort_uv

#' @describeIn atomize_help An alias for `which_av`.
#' @export
wv <- which_av
