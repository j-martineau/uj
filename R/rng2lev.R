#' @name rng2lev.
#' @title Convert sortable values in specific ranges to a specific level
#' @param x Atomic object of \link[isrt]{sortable} values.
#' @param cuts \link[num_vec]{Atomic vec} of \code{n-1} cuts for \code{n} levels.
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
  err <- NULL
  if (!isrt(x)) {err <- c(err, " • [x] must be an atomic sortable object (?isrt).")}
  if (!cmp_srt_vec(cuts)) {err <- c(err, " • [cuts] must be a complete sortable vec (?cmp_srt_vec).")}
  if (!cmp_srt_vec(levs)) {err <- c(err, " • [levs] must be a complete sortable vec (?cmp_srt_vec).")}
  if (idef(err)) {stop(err)}
  err <- NULL
  if (!all(sort(cuts) == cuts)) {err <- c(err, " • [cuts] must be in increasing order")}
  if (length(unique(cuts)) != length(cuts)) {err <- c(err, " • [cuts] contains duplicate values")}
  if (length(cuts) != length(levs) - 1) {err <- c(err, " • There must be one fewer value in [cuts] than in [levs]")}
  if (!compatible(x, cuts, levs, recycle. = F)) {err <- c(err, " • [x], [cuts], and [levs] must be of comparable modes (?comparable).")}
  if (idef(err)) {stop(err)}
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
  if (is.array(x)) {apply(x, 1:length(dim(x)), tolev)}                           # apply the cut scores to each value in succession
  else {sapply(x, tolev)}
}
