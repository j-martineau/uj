#' @name comparable
#' @family props
#' @title Are objects comparable?
#' @details Determines whether modes of all arguments in \code{...} are
#'   comparable (i.e., sortable and compatible with each other), meaning that
#'   all are character, logical, numeric, or ordered factor with the same set of
#'   levels (in the same order).
#' @param ... An arbitrary number of arguments to be checked for comparability
#'   with each other.
#' @param recycle A logical scalar indicating whether arguments in \code{...}
#'   must be recyclable to be comparable.
#' @return A logical scalar.
#' @export
comparable <- function(..., recycle = T) {
  x <- list(...)                                                                 # arguments in [...] as a list
  N <- length(x)
  E <- NULL
  if (N < 2) {E <- c(E, "\n  * [...] must contain multiple arguments.")}
  if (!isTF(recycle)) {E <- c(E, "\n  * [recycle] must be TRUE or FALSE.")}
  if (xdef(E)) {stop(E)}
  if (recycle) {
    R <- unique(lengths(x))                                                      # unique set of argument length
    R <- max(R) / R                                                              # number of replications needed for recycling
    if (any(R != round(R))) {return(F)}                                          # if arguments must be recyclable and any rep is fractional, not comparable
  }
  CH <- all(sapply(x, is.character))
  LG <- all(sapply(x, is.logical))
  NM <- all(sapply(x, is.numeric))
  OF <- all(sapply(x, is.ordered))
  if ( CH |  LG | NM) {return(T)}
  if (!OF) {return(F)}
  LV <- sapply(x, levels)
  for (i in 2:N) {if (!identical(LV[[i]], LV[[i - 1]])) {return(F)}}
  T
}
