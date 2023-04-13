#' @encoding UTF-8
#' @family conversions
#' @title Convert sortable values in levels bY range
#' @description Convert values of `X` to the `N` levels in `Levs` distinguished by the `N-1` thresholds in `Cuts`.
#' \cr\cr When values of `X` are equal to a value of `Cuts`, assignment to level is dependent on whether the value of each element of `Cuts` is less than `0` vs. greater than or equal to `0`.
#' \cr\cr For a value in `Cuts` less than `0`, the level assigned is the lower of the levels distinguished by that value of `Cuts`.
#' \cr\cr Otherwise, the level assigned is the upper of the levels distinguished by that value of `Cuts`.
#' @param X An \link[=atm_srt]{atomic object of sortable} values.
#' @param Cuts An \link[=srt_vec]{atomic vec} of `N-1` cuts for `N` levels.
#' @param Levs An atomic vec of `N` level labels.
#' @return An atomic object of the same dimension as `X` and the same mode as `Levs`.
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
rng2lev <- function(X, Cuts, Levs) {
  Errors <- NULL
  if (!uj:::.pop_srt(X)) {Errors <- base::c(Errors, "[X] must be a sortable atomic object (?pop_srt).")}
  if (!uj:::.cmp_srt_vec(Cuts)) {Errors <- base::c(Errors, "[Cuts] must be a complete sortable vec (?cmp_srt_vec).")}
  if (!uj:::.cmp_srt_vec(Levs)) {Errors <- base::c(Errors, "[Levs] must be a complete sortable vec (?cmp_srt_vec).")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  if (!base::all(base::sort(Cuts) == Cuts)) {Errors <- base::c(Errors, "[Cuts] must be sorted in increasing order.")}
  if (base::length(Cuts) != base::length(base::unique(Cuts))) {Errors <- base::c(Errors, "[Cuts] contains duplicate values.")}
  if (base::length(Cuts) != base::length(Levs) - 1) {Errors <- base::c(Errors, "length(Cuts) must equal length(Levs) - 1.")}
  if (!uj::comparable(X, Cuts)) {Errors <- base::c(Errors, "[X] and [Cuts] must be of comparable modes (?comparable).")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  Y <- X
  Y[1:base::length(Y)] <- NA
  if (base::is.character(X)) {Y <- base::as.character(Y)}
  else if (base::is.ordered(Levs)) {Y <- base::factor(Y, levels = base::levels(Levs))}
  else if (base::is.null(Levs)) {Y <- base::as.numeric(Y)}
  for (i in 1:base::length(Cuts)) {
    Cut <- Cuts[i]
    Lo <- Levs[i]
    Hi <- Levs[i + 1]
    if (!base::is.numeric(X)) {Eq <- Hi} else if (X < 0) {Eq <- Lo} else {Eq <- Hi}
    if (i == 1) {Y[X < Cut] <- Lo}
    Y[X > Cut] <- Hi
    Y[X == Cut] <- Eq
  }
  Y
}
