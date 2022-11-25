#' @name rng2lev
#' @title Convert Sortable Values in Specific Ranges to a Specific Level
#' @description Convert values of `x` to the `n` levels in `levs` distinguished
#'   by the `n-1` thresholds in `cuts`.
#' @param x An atomic object of \link[=isrt]{sortable} values.
#' @param cuts An \link[=num_vec]{atomic vec} of `n-1` cuts for `n` levels.
#' @param levs An \link[=atm_vec]{atomic vec} of `n` level labels.
#' @description When values of `x` are equal to a value of `cuts`, assignment to
#'   level is dependent on whether the value of each element of `cut` is less
#'   than `0` vs. greater than or equal to `0`. For a value in `cuts` less than
#'   `0`, the level assigned is the lower of the levels distinguished by that
#'   value of `cuts`. Otherwise, the level assigned is the upper of the levels
#'   distinguished by that value of `cuts`.
#' @return An atomic object of the same dimension as `x` and the same mode as
#'   `levs`.
#' @export
rng2lev <- function(x, cuts, levs) {
  errs <- c(f0(isrt(x)          , NULL, " \u2022 [x] must be an atomic sortable object (?isrt)."),
            f0(cmp_srt_vec(cuts), NULL, " \u2022 [cuts] must be a complete sortable vec (?cmp_srt_vec)."),
            f0(cmp_srt_vec(levs), NULL, " \u2022 [levs] must be a complete sortable vec (?cmp_srt_vec)."))
  if (!is.null(errs)) {stop(errs)}
  errs <- c(f0(all(sort(cuts) == cuts)                , NULL, " \u2022 [cuts] must be in increasing order."),
            f0(length(unique(cuts)) != length(cuts)   , NULL, " \u2022 [cuts] contains duplicate values."),
            f0(length(cuts) != length(levs) - 1       , NULL, " \u2022 There must be one fewer value in [cuts] than in [levs]."),
            f0(compatible(x, cuts, levs, recycle. = F), NULL, " \u2022 [x], [cuts], and [levs] must be of comparable modes (?comparable)."))
  if (!is.null(errs)) {stop(errs)}
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
  if (is.array(x)) {apply(x, 1:length(dim(x)), tolev)} else {sapply(x, tolev)}
}
