#' @encoding UTF-8
#' @family conversions
#' @title Convert sortable values in levels by range
#' @description Convert values of `x` to the `n` levels in `levs` distinguished by the `n-1` thresholds in `cuts`.
#' \cr\cr When values of `x` are equal to a value of `cuts`, assignment to level is dependent on whether the value of each element of `cut` is less than `0` vs. greater than or equal to `0`.
#' \cr\cr For a value in `cuts` less than `0`, the level assigned is the lower of the levels distinguished by that value of `cuts`.
#' \cr\cr Otherwise, the level assigned is the upper of the levels distinguished by that value of `cuts`.
#' @param x An \link[=atm_srt]{atomic object of sortable} values.
#' @param cuts An \link[=srt_vec]{atomic vec} of `n-1` cuts for `n` levels.
#' @param levs An atomic vec of `n` level labels.
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
  uj::errs_if_nots(uj::pop_srt(x)                   , "[x] must be a sortable atomic object (?pop_srt)."         ,
                   uj::cmp_srt_vec(cuts)            , "[cuts] must be a complete sortable vec (?cmp_srt_vec)."   ,
                   uj::cmp_srt_vec(levs)            , "[levs] must be a complete sortable vec (?cmp_srt_vec)."   , PKG = "uj")
  uj::errs_if_nots(uj::isVEQ(base::sort(cuts), cuts), "[cuts] must be sorted in increasing order."               ,
                   uj::UNQ(cuts)                    , "[cuts] contains duplicate values."                        ,
                   uj::N(cuts) == uj::N(levs) - 1   , "length(cuts) must equal length(levs) - 1."                ,
                   uj::comparable(x, cuts)          , "[x] and [cuts] must be of comparable modes (?comparable).", PKG = "uj")
  y <- x
  y[1:uj::N(y)] <- NA
  if (uj::isCHR(levs)) {y <- uj::asCHR(y)}
  else if (uj::isORD(levs)) {y <- uj::asORD(y, levs = uj::LEVS(levs))}
  else if (uj::NLL(levs)) {y <- uj::asNUM(y)}
  for (i in 1:uj::N(cuts)) {
    cut <- cuts[i]
    lo <- levs[i]
    hi <- levs[i + 1]
    eq <- uj::f0(uj::notNUM(x), hi, uj::f0(x < 0, lo, hi))
    if (i == 1) {y[x < cut] <- lo}
    y[x > cut] <- hi
    y[x == cut] <- eq
  }
  y
}
