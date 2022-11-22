#' @name dups
#' @family values
#' @title Identify Duplicate Values.
#' @description Extended functionality for \code{duplicated}.
#' @section Functions in This Family:
#'   \strong{\code{dups}}
#'   Atomizes \code{...} (i.e., reduces it to a single atomic vector) and
#'   reduces the resulting vector to contain one copy of each duplicated
#'   element.
#'   \cr\cr
#'   \strong{\code{idups}}
#'   Indexes all elements of \code{x} that are duplicates of elements appearing
#'   earlier in \code{x}, either as a logical vector of the same length as
#'   \code{x} (when \code{int = FALSE}) or as an integer vector whose length is
#'   the same as the number of elements in \code{x} that are duplicates of
#'   earlier elements (when \code{int = TRUE}).
#' @param ... One or more atomic vectors to be evaluated for duplicated values.
#' @param x \link[=atm_vec]{Atomic vec}.
#' @param int A non-\code{NA} logical scalar indicating whether to return a
#'   logical vector (\code{int = FALSE}) or an integer vector (\code{int =
#'   TRUE}).
#' @return \strong{\code{dups}}
#'   \cr An atomic vector.
#'   \cr\cr
#'   \strong{\code{idups}}
#'   \cr A logical vector (when \code{int = FALSE}); an integer vector (when
#'   \code{int = TRUE}).
#' @examples
#' dups(0:5, 5:10, 10:15, 15:20)
#' idups(c(0:5, 5:10, 10:15, 15:20))
#' idups(c(0:5, 5:10, 10:15, 15:20), int = T)
#' @export
dups <- function(...) {x <- av(...); unique(x[duplicated(x)])}

#' @rdname dups
#' @export
idups <- function(x, int = F) {
  errs <- c(f0(atm_vec(x), NULL, "\n \u2022 [x] must be an atomic vec (?atm_vec)."),
            f0(isTF(int), NULL, "\n \u2022 [int] must be TRUE or FALSE."))
  if (!is.null(errs)) {stop(errs)}
  f0(int, which(duplicated(x)), duplicated(x))
}
