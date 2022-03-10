#' @name pgrid
#' @family strings
#' @title \code{expand.grid} for \code{paste} and \code{paste0}
#' @details \strong{\code{p.grid}}
#'   \cr Converts arguments in \code{...} to character, separating them into
#'   their constituent characters (if \code{ch = T}), add a blank string (if
#'   \code{add.blank = T}), and create a character vector containing all
#'   possible combinations of elements in \code{...} pasted together using the
#'   'paste' \code{p}. The first part of the character scalar in each element of
#'   the result is from \code{...1}, the second from \code{..2}, and so on.
#'   \cr\cr
#'   \strong{\code{pgrid0}}
#'   \cr Calls \code{pgrid} with \code{p} specified as a blank string (i.e.,
#'   \code{""}).
#' @param ... Non-empty atomic objects.
#' @param p A character scalar to use as the 'paste'.
#' @param ch A logical scalar indicating whether to split arguments in
#'   \code{...} into consituent characters after conversion to mode 'character'.
#' @param add.blank Whether to add a blank ("") to each argument in \code{...}.
#' @return A character vector.
#' @export
pgrid <- function(..., p = " ", ch = F, add.blank = F) {
  x  <- list(...)
  VN <- length(x) > 0
  VL <- f0(VN, all(lengths(x) > 0), T)
  VX <- f0(VN & VL, all(sapply(x, is.atomic)), T)
  VP <- cmp_chr_scl(p)
  VC <- isTF(ch)
  VA <- isTF(add.blank)
  E <- NULL
  if (!VN) {E <- c(E, "\n  * [...] is empty.")}
  if (!VL) {E <- c(E, "\n  * [...] contains an empty element.")}
  if (!VX) {E <- c(E, "\n  * Arguments in [...] must be atomic.")}
  if (!VP) {E <- c(E, "\n  * [p] must be a character scalar.")}
  if (!VC) {E <- c(E, "\n  * [ch] must be TRUE or FALSE.")}
  if (!VA) {E <- c(E, "\n  * [add.blank] must be TRUE or FALSE.")}
  if (xdef(E)) {stop(E)}
  x <- sapply(x, as.character)
  x <- sapply(x, unlist, T, F)
  if (ch) {x <- sapply(x, ch)}
  if (add.blank) {x <- sapply(x, c, "")}
  Call <- paste0("x[[", 1:length(x), "]]")
  Call <- paste0(Call, collapse = ", ")
  Call <- paste0("expand.grid(", Call, ", stringsAsFactors = F)")
  x <- run(Call)
  av(apply(x, 2, paste, sep = p))
}

#' @rdname pgrid
#' @export
pgrid0 <- function(..., ch = F, add.blank = F) {pgrid(..., p = "", ch = ch, add.blank = add.blank)}
