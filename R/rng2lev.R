#' @title Convert sortable values in specific ranges to a specific level
#' @description Convert values of \code{x} to the \code{n} levels in \code{levs}
#'   distinguished by the \code{n-1} thresholds in \code{cuts}.
#' @param x Atomic object of sortable values.
#' @param cuts Atomic vector of \code{n-1} cuts for \code{n} levels.
#' @param levs Atomic vector of \code{n} level labels.
#' @details When values of \code{x} are equal to a value of \code{cuts},
#'   assignment to level is dependent on whether the value of \code{cut} is less
#'   than 0 vs. greater than or equal to 0. For a value of \code{cut} less than
#'   0, the level assigned is the lower of the levels distinguished by that
#'   value of \code{cuts}. Otherwise, the level assigned is the upper of the
#'   levels distinguished by that value of \code{cuts}.
#' @return Atomic object of the same dimension as \code{x} and the same mode as
#'   \code{levs}.
#' @export
rng2lev <- function(x, cuts, levs) {
  bank_funs(cmp_num_vec, x = x)
  bank_funs(cmp_atm_vec, cuts = cuts)
  bank_funs(cmp_atm_vec, levs = levs)
  err_check()
  if (!all(sort(cuts) == cuts)) {bank_err("[cuts] must be in increasing order")}
  if (length(unique(cuts)) != length(cuts)) {bank_err("[cuts] contains one or more duplicate values")}
  if (length(cuts) != length(levs) - 1) {bank_err("There must be one fewer value in [cuts] than in [levs]")}
  err_check()
  cuts <- c(-Inf, cuts, Inf)                                                     # infinite lower and upper cuts
  tolev <- function(y) {                                                         # FUN to get a level for an individual value
    for (i in 1:length(levs)) {                                                  # : FOR each level
      lb <- cuts[i]                                                              # : : get its lower bound
      ub <- cuts[i + 1]                                                          # : : get its upper bound
      if (lb < y & y < ub) {return(levs[i])}                                     # : : IF the value is between the bounds > return this level
      if (y < 0) {                                                               # : : IF the value is less than zero
        if      (y == lb) {return(levs[i - 1])}                                  # : : : IF     the value is equal to the lower bound > return one level lower
        else if (y == ub) {return(levs[i])}                                      # : : : BUT IF the value is equal to the upper bound > return this level
      } else {                                                                   # : : ELSE (value is 0 or greater)
        if      (y == lb) {return(levs[i])}                                      # : : : IF     the value is equal to the lower bound > return this level
        else if (y == ub) {return(levs[i + 1])}                                  # : : : BUT IF the value is equal to the upper bound > return this level + 1
  }}}                                                                            # END:END:END
  sapply(x, tolev)                                                               # apply the cut scores to each value in succession
}
