#' @name dups.
#' @family values
#' @title Identify Duplicate Values.
#' @description Extended functionality for \code{duplicated}.
#' @param ... One or more atomic vectors to be evaluated for duplicated values.
#' @param x An atomic vector.
#' @param int A logical scalar indicating whether to return a logical vector
#'   (when \code{int = FALSE}) or an integer vector (when \code{int = TRUE}).
#' @return An atomic vector containing 1 copy of each duplicated element
#'   (\code{dups}), a logical vector indexing elements of \code{x} that are
#'   duplicates of earlier elements (\code{idups(x, int = FALSE)}), or an
#'   integer vector indexing elements of \code{x} that are duplicates of earlier
#'   elements (\code{idups(x, int = TRUE)}).
#' @examples
#' dups(0:5, 5:10, 10:15, 15:20)
#' idups(c(0:5, 5:10, 10:15, 15:20))
#' idups(c(0:5, 5:10, 10:15, 15:20), int = T)
#' @export
dups. <- function() {help("dups.", package = "uj")}

#' @describeIn dups. Atomizes \code{...} (i.e., reduces it to a single atomic
#'   vector) and reduces the resulting vector to contain one copy of each
#'   duplicated element.
#' @export
dups <- function(...) {out. <- av(...); unique(out.[duplicated(out.)])}

#' @describeIn dups. Indexes all elements of \code{x} that are duplicates of
#'   elements appearing earlier in \code{x}, either as a logical vector of the
#'   same length as \code{x} (when \code{int = FALSE}) or as an integer vector
#'   whose length is the same as the number of elements in \code{x} that are
#'   duplicates of earlier elements (when \code{int = TRUE}).
#' @export
idups <- function(x., int. = F) {
  vx. <- iatm(x.)
  vi. <- isTF(int.)
  err. <- NULL
  if (!vx.) {err. <- c(err., "\n • [x.] must be atomic.")}
  if (!vi.) {err <- c(err., "\n • [int.] must be TRUE or FALSE.")}
  if (idef(err)) {stop(err.)}
  f0(int., which(duplicated(x.)), duplicated(x.))
}
