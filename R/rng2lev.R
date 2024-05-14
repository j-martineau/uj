#' @encoding UTF-8
#' @family conversions
#' @title Convert sortable values in levels by range
#' @description Convert values of `x` to the `N` levels in `levs` distinguished by the `N-1` thresholds in `cuts`.
#' \cr\cr When values of `x` are equal to a value of `cuts`, assignment to level is dependent on whether the value of each element of `cuts` is less than `0` vs. greater than or equal to `0`.
#' \cr\cr For a value in `cuts` less than `0`, the level assigned is the lower of the levels distinguished by that value of `cuts`.
#' \cr\cr Otherwise, the level assigned is the upper of the levels distinguished by that value of `cuts`.
#' @param x An \link[=atm_srt]{atomic object of sortable} values.
#' @param cuts An \link[=srt_vec]{atomic vec} of `N-1` cuts for `N` levels.
#' @param levs An atomic vec of `N` level labels.
#' @return An atomic object of the same dimension as `x` and the same mode as `levs`.
#' @examples
#' numVALS <- -5:5
#' chrVALS <- letters[1:11]
#' ordVALS <- factor(chrVALS, levels = chrVALS, ordered = TRUE)
#' numCUTS <- c(-3, 0, 3)
#' chrCUTS <- c("c", "f", "i")
#' ordCUTS <- factor(chrVALS, levels = chrVALS, ordered = TRUE)
#' rngLEVS <- c("A", "B", "C", "D")
#' rng2lev(numVALS, numCUTS, rngLEVS)
#' rng2lev(chrVALS, chrCUTS, rngLEVS)
#' rng2lev(ordVALS, ordCUTS, rngLEVS)
#' @export
rng2lev <- function(x, cuts, levs) {
  errs <- NULL
  if (!uj::.pop_srt(x)) {errs <- base::c(errs, "[x] must be a sortable atomic object (?pop_srt).")}
  if (!uj::.cmp_srt_vec(cuts)) {errs <- base::c(errs, "[cuts] must be a complete sortable vec (?cmp_srt_vec).")}
  if (!uj::.cmp_srt_vec(levs)) {errs <- base::c(errs, "[levs] must be a complete sortable vec (?cmp_srt_vec).")}
  if (!base::is.null(errs)) {uj::stopperr(errs)}
  if (!base::all(base::sort(cuts) == cuts)) {errs <- base::c(errs, "[cuts] must be sorted in increasing order.")}
  if (base::length(cuts) != base::length(base::unique(cuts))) {errs <- base::c(errs, "[cuts] contains duplicate values.")}
  if (base::length(cuts) != base::length(levs) - 1) {errs <- base::c(errs, "length(cuts) must equal length(levs) - 1.")}
  if (!uj::comparable(x, cuts)) {errs <- base::c(errs, "[x] and [cuts] must be of comparable modes (?comparable).")}
  if (!base::is.null(errs)) {uj::stopperr(errs)}
  y <- x
  y[1:base::length(y)] <- NA
  if (base::is.character(x)) {y <- base::as.character(y)}
  else if (base::is.ordered(levs)) {y <- base::factor(y, levels = base::levels(levs))}
  else if (base::is.null(levs)) {y <- base::as.numeric(y)}
  for (i in 1:base::length(cuts)) {
    cut <- cuts[i]
    lo <- levs[i]
    hi <- levs[i + 1]
    if (!base::is.numeric(x)) {eq <- hi} else if (x < 0) {eq <- lo} else {eq <- hi}
    if (i == 1) {y[x < cut] <- lo}
    y[x > cut] <- hi
    y[x == cut] <- eq
  }
  y
}
