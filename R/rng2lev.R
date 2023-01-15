#' @encoding UTF-8
#' @family conversions
#' @title Convert sortable values in levels by range
#' @description Convert values of `x` to the `n` levels in `levs` distinguished by the `n-1` thresholds in `cuts`.
#' \cr\cr When values of `x` are equal to a value of `cuts`, assignment to level is dependent on whether the value of each element of `cut` is less than `0` vs. greater than or equal to `0`.
#' \cr\cr For a value in `cuts` less than `0`, the level assigned is the lower of the levels distinguished by that value of `cuts`.
#' \cr\cr Otherwise, the level assigned is the upper of the levels distinguished by that value of `cuts`.
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
  errs <- base::c(uj::f0(uj::pop_srt(x)       , NULL, "[x] must be an atomic sortable object (?isrt)."),
                  uj::f0(uj::cmp_srt_vec(cuts), NULL, "[cuts] must be a complete sortable vec (?cmp_srt_vec)."),
                  uj::f0(uj::cmp_srt_vec(levs), NULL, "[levs] must be a complete sortable vec (?cmp_srt_vec)."))
  if (!base::is.null(errs)) {stop(uj::format_errs(pkg = "uj", errs))}
  errs <- base::c(uj::f0(base::all(base::sort(cuts) == cuts)                   , NULL, "[cuts] must be sorted in increasing order."),
                  uj::f0(base::length(base::unique(cuts)) == base::length(cuts), NULL, "[cuts] contains duplicate values."),
                  uj::f0(base::length(cuts) == base::length(levs) - 1          , NULL, "length(cuts) must equal length(levs) - 1."),
                  uj::f0(uj::comparable(x, cuts)                               , NULL, "[x] and [cuts] must be of comparable modes (?comparable)."))
  if (!base::is.null(errs)) {stop(uj::format_errs(pkg = "uj", errs))}
  y <- x
  y[1:base::length(y)] <- NA
  if (base::is.character(levs)) {y <- base::as.character(y)}
  else if (base::is.ordered(levs)) {y <- base::ordered(y, levels = base::levels(levs))}
  else if (base::is.numeric(levs)) {y <- base::as.numeric(y)}
  for (i in 1:base::length(cuts)) {
    cut <- cuts[i]
    lo <- levs[i]
    hi <- levs[i + 1]
    eq <- uj::f0(!base::is.numeric(x), hi, uj::f0(x < 0, lo, hi))
    if (i == 1) {y[x < cut] <- lo}
    y[x > cut] <- hi
    y[x == cut] <- eq
  }
  y
}
