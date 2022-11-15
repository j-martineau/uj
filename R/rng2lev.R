#' @name rng2lev.
#' @title Convert sortable values in specific ranges to a specific level
#' @param x Atomic object of \link[isrt]{sortable} values.
#' @param cuts \link[num_vec]{Atomic vec} of \code{n-1} cuts for \code{n}
#'   levels.
#' @param levs \link[atm_vec]{Atomic vec} of \code{n} level labels.
#' @description When values of \code{x} are equal to a value of \code{cuts},
#'   assignment to level is dependent on whether the value of \code{cut} is less
#'   than 0 vs. greater than or equal to 0. For a value of \code{cut} less than
#'   0, the level assigned is the lower of the levels distinguished by that
#'   value of \code{cuts}. Otherwise, the level assigned is the upper of the
#'   levels distinguished by that value of \code{cuts}.
#' @return Atomic object of the same dimension as \code{x} and the same mode as
#'   \code{levs}.
#' @export
rng2lev. <- function() {help("rng2lev.", package = "uj")}

#' @describeIn rng2lev. Convert values of \code{x} to the \code{n} levels in
#'   \code{levs} distinguished by the \code{n-1} thresholds in \code{cuts}.
#' @export
rng2lev <- function(x, cuts, levs) {
  errs <- c(f0(isrt(x)          , NULL, " \u2022 [x] must be an atomic sortable object (?isrt)."),
            f0(cmp_srt_vec(cuts), NULL, " \u2022 [cuts] must be a complete sortable vec (?cmp_srt_vec)."),
            f0(cmp_srt_vec(levs), NULL, " \u2022 [levs] must be a complete sortable vec (?cmp_srt_vec)."))
  if (idef(errs)) {stop(errs)}
  errs <- c(f0(all(sort(cuts) == cuts)                , NULL, " \u2022 [cuts] must be in increasing order."),
            f0(length(unique(cuts)) != length(cuts)   , NULL, " \u2022 [cuts] contains duplicate values."),
            f0(length(cuts) != length(levs) - 1       , NULL, " \u2022 There must be one fewer value in [cuts] than in [levs]."),
            f0(compatible(x, cuts, levs, recycle. = F), NULL, " \u2022 [x], [cuts], and [levs] must be of comparable modes (?comparable)."))
  if (idef(errs)) {stop(errs)}
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
