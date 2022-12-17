#' @name rng2lev
#' @title Convert sortable values in levels by range
#' @description Convert values of `x` to the `n` levels in `levs` distinguished by the `n-1` thresholds in `cuts`.
#' \cr
#' \cr When values of `x` are equal to a value of `cuts`, assignment to level is dependent on whether the value of each element of `cut` is less than `0` vs. greater than or equal to `0`.
#' \cr
#' \cr For a value in `cuts` less than `0`, the level assigned is the lower of the levels distinguished by that value of `cuts`.
#' \cr
#' \cr Otherwise, the level assigned is the upper of the levels distinguished by that value of `cuts`.
#' @param x An atomic object of \link[=isrt]{sortable} values.
#' @param cuts An \link[=num_vec]{atomic vec} of `n-1` cuts for `n` levels.
#' @param levs An atomic vec of `n` level labels.
#' @return An atomic object of the same dimension as `x` and the same mode as `levs`.
#' @examples
#' num_vals. <- -5:5
#' chr_vals. <- letters[1:11]
#' ord_vals. <- factor(chr_vals., levels = chr_vals., ordered = TRUE)
#' num_cuts. <- c(-3, 0, 3)
#' chr_cuts. <- c("c", "f", "i")
#' ord_cuts. <- factor(chr_cuts., levels = chr_vals., ordered = TRUE)
#' rng_levs. <- c("A", "B", "C", "D")
#' rng2lev(num_vals., num_cuts., rng_levs.)
#' rng2lev(chr_vals., chr_cuts., rng_levs.)
#' rng2lev(ord_vals., ord_cuts., rng_levs.)
#' @export
rng2lev <- function(x, cuts, levs) {
  errs <- c(f0(pop_srt(x)       , NULL, "[x] must be an atomic sortable object (?isrt)."),
            f0(cmp_srt_vec(cuts), NULL, "[cuts] must be a complete sortable vec (?cmp_srt_vec)."),
            f0(cmp_srt_vec(levs), NULL, "[levs] must be a complete sortable vec (?cmp_srt_vec)."))
  if (!is.null(errs)) {stop(.errs(errs))}
  errs <- c(f0(all(sort(cuts) == cuts)             , NULL, "[cuts] must be sorted in increasing order."),
            f0(length(unique(cuts)) == length(cuts), NULL, "[cuts] contains duplicate values."),
            f0(length(cuts) == length(levs) - 1    , NULL, "length(cuts) must equal length(levs) - 1."),
            f0(comparable(x, cuts)                 , NULL, "[x] and [cuts] must be of comparable modes (?comparable)."))
  if (!is.null(errs)) {stop(.errs(errs))}
  y <- x
  y[1:length(y)] <- NA
  if (   is.character(levs)) {y <- as.character(y)}
  else if (is.ordered(levs)) {y <- ordered(y, levels = levels(levs))}
  else if (is.numeric(levs)) {y <- as.numeric(y)}
  for (i in 1:length(cuts)) {
    cut <- cuts[i]
    lo <- levs[i]
    hi <- levs[i + 1]
    eq <- f0(!is.numeric(x), hi, f0(x < 0, lo, hi))
    if (i == 1) {y[x < cut] <- lo}
    y[x > cut] <- hi
    y[x == cut] <- eq
  }
  y
}
