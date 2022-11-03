#' @name pgrid
#' @family strings
#' @title \code{expand.grid} for \code{paste} and \code{paste0}
#' @description Convert arguments in \code{...} to character, separating them
#'   into their constituent characters (if \code{ch = T}), add a blank string
#'   (if \code{add.blank = T}), and create a character vector containing all
#'   possible combinations of elements in \code{...} pasted together using the
#'   'paste' \code{p}. The first part of the character scalar in each element of
#'   the result is from \code{...1}, the second from \code{..2}, and so on.
#' @param ... Non-empty atomic objects.
#' @param p. A character scalar to use as the 'paste'.
#' @param ch. A logical scalar indicating whether to split arguments in
#'   \code{...} into consituent characters after conversion to mode 'character'.
#' @param add.blank. Whether to add a blank ("") to each argument in \code{...}.
#' @return A character vector.
#' @export
pgrid <- function(..., p. = " ", ch. = F, add.blank. = F) {
  x. <- list(...)
  vn. <- length(x.) > 0
  vns. <- f0(vn., all(lengths(x.) > 0), T)
  vd. <- f0(vn. & vns., all(sapply(x., is.atomic)), T)
  vp. <- cmp_chr_scl(p.)
  vc. <- isTF(ch.)
  va. <- isTF(add.blank.)
  err. <- NULL
  if (!vn. ) {err. <- c(err., "\n • [...] is empty.")}
  if (!vns.) {err. <- c(err., "\n • [...] contains an empty element.")}
  if (!vd. ) {err. <- c(err., "\n • Arguments in [...] must be atomic.")}
  if (!vp. ) {err. <- c(err., "\n • [p.] must be a character scalar.")}
  if (!vc. ) {err. <- c(err., "\n • [ch.] must be TRUE or FALSE.")}
  if (!va. ) {err. <- c(err., "\n • [add.blank.] must be TRUE or FALSE.")}
  if (idef(err.)) {stop(err.)}
  x. <- sapply(x., as.character)
  x. <- sapply(x., unlist, T, F)
  if (ch.) {x. <- sapply(x., ch.)}
  if (add.blank.) {x. <- sapply(x., c, "")}
  call. <- paste0("x[[", 1:length(x.), "]]")
  call. <- paste0(call., collapse = ", ")
  call. <- paste0("expand.grid(", call., ", stringsAsFactors = F)")
  x. <- run(call.)
  av(apply(x., 2, paste, sep = p.))
}

#' @describeIn pgrid Call \code{pgrid} with \code{p. = ""} (a blank string).
#' @export
pgrid0 <- function(..., ch. = F, add.blank. = F) {pgrid(..., p. = "", ch. = ch., add.blank. = add.blank.)}
