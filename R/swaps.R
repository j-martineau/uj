#' @encoding UTF-8
#' @family extensions
#' @family forks
#' @title Swap by Row Index, Column Index, Element Index, and Atomic Value.
#' @param x A non-empty data.frame, matrix, vlist (?VLS), or an atomic object of any dimension.
#' @param from,to Uniquely valued non-zero whole number vectors indexing source (`from`) and destination (`to`) positions, rows, or columns. `from < 0` and `to < 0` index from the last value rather than the first.
#' @param old \link[=unq_atm_vec]{A uniquely valued atomic vec} of values to be replaced.
#' @param new \link[=atm_vec]{An atomic vec} of replacement values. `length(new)` must be in `c(1, length(old))`.
#' @param dim Dimension on which swapping occurs. `0` indicates swapping by element index of a non-empty vector, vlist, or other atomic object. `1` and `2` indicate, respectively, swapping rows and columns of a data.frame or matrix.
#' @param all `TRUE` or `FALSE` indicating whether all values of `x` must be contained in `old` (ignored when `old` and `new` are `NULL`).
#' @return An object of the same class and dimensions as `x`.
#' @examples
#' AF <- LETTERS[1:6]
#' M6 <- matrix(1:36, nrow = 6)
#'
#' i1 <- 1:3
#' i2 <- 6:4
#'
#' v1 <- c("A", "B", "C")
#' v2 <- c("a", "b", "c")
#'
#' AF
#' swap_vals(AF, v1, v2)
#' swap_elts(AF, i1, i2)
#' swap(AF, v1, v2, type = "v")
#' swap(AF, i1, i2, type = "i")
#'
#' M6
#' swap_rows(M6, i1, i2)
#' swap_cols(M6, i1, i2)
#' @export
swaps <- function() {utils::help("swap", package = "uj")}

#' @describeIn swaps Swap elements of `x` either by index (when `type == "i"`) or by value (when `type == "v"`).
#' @export
swap <- function(x, s1, s2, type = "i") {
  okAV <- uj::.pop_atm(x)
  okTY <- uj::is_EQ(type, "i") | uj::is_EQ(type, "v")
  errs <- NULL
  if (!okAV) {errs <- base::c(errs, "[x] must be a populated vec (?pop_vec).")}
  if (!okTY) {errs <- base::c(errs, "[type] must be either 'i' or 'v'.")}
  if (uj::.DEF(errs)) {uj::stopper(errs)}
  if (type == "i") {uj::swap_elts(x, s1, s2)} else {uj::swap_vals(x, s1, s2)}
}

#' @describeIn swaps Swap elements of `x` either by value, meaning that elements of `x` matching `v1[i]` are replaced by `v2[i]` and vice versa.
#' @export
swap_vals <- function(x, v1, v2) {
  okAV <- uj::.pop_atm(x)
  ok1P <- uj::.unq_atm_vec(v1)
  ok2P <- uj::.unq_atm_vec(v2)
  ok12 <- uj::.unq_atm_vec(base::c(v1, v2))
  okNN <- base::length(v1) == base::length(v2)
  errs <- NULL
  if (!okAV) {errs <- base::c(errs, "[x] must be a populated vec (?pop_vec).")}
  if (!ok1P) {errs <- base::c(errs, "[v1] must be a complete positive whole-number vec (?unq_psw_vec).")}
  if (!ok2P) {errs <- base::c(errs, "[v2] must be a complete positive whole-number vec (?unq_psw_vec).")}
  if (!ok12) {errs <- base::c(errs, "A value from [v1] is duplicated in [v2].")}
  if (!okNN) {errs <- base::c(errs, "[v1] and [v2] must be of equal length.")}
  if (uj::.DEF(errs)) {uj::stopper(errs)}
  old <- x
  for (i in 1:base::length(v1)) {
    i1 <- old == v1[i]
    i2 <- old == v2[i]
    x[i1] <- v2[i]
    x[i2] <- v1[i]
  }
  x
}

#' @describeIn swaps Swap elements of `x` by index, meaning that element `i1[i]` of `x` is replaced by element `i2[i]` of and vice versa.
#' @export
swap_elts <- function(x, i1, i2) {
  ok1D <- uj::.pop_vec(x)
  ok1P <- uj::.unq_psw_vec(i1)
  ok2P <- uj::.unq_psw_vec(i1)
  ok1V <- uj::f0(!ok1P, T, base::all(i1 <= base::length(x)))
  ok2V <- uj::f0(!ok2P, T, base::all(i2 <= base::length(x)))
  ok12 <- uj::.unq_psw_vec(base::c(i1, i2))
  okNN <- base::length(i1) == base::length(i2)
  errs <- NULL
  if (!ok1D) {errs <- base::c(errs, "[x] must be a populated vec (?pop_vec).")}
  if (!ok1P) {errs <- base::c(errs, "[i1] must be a complete positive whole-number vec (?unq_psw_vec).")}
  if (!ok2P) {errs <- base::c(errs, "[i2] must be a complete positive whole-number vec (?unq_psw_vec).")}
  if (!ok1V) {errs <- base::c(errs, "[i1] contains a value greater than [length(x)].")}
  if (!ok2V) {errs <- base::c(errs, "[i2] contains a value greater than [length(x)].")}
  if (!ok12) {errs <- base::c(errs, "A value from [i1] is duplicated in [i2].")}
  if (!okNN) {errs <- base::c(errs, "[i1] and [i2] must be of equal length.")}
  if (uj::.DEF(errs)) {uj::stopper(errs)}
  old <- x
  for (i in 1:base::length(i1)) {
    x[i1[i]] <- old[i2[i]]
    x[i2[i]] <- old[i1[i]]
  }
  x
}

#' @describeIn swaps Swap rows of `x` by index, meaning that row `i1[i]` of `x` is replaced by row `i2[i]` of `x` and vice versa.
#' @export
swap_rows <- function(x, i1, i2) {
  ok2D <- uj::.pop_mat(x) | uj::.pop_dtf(x)
  okFP <- uj::.unq_psw_vec(i1)
  okTP <- uj::.unq_psw_vec(i2)
  okFV <- uj::f0(!okFP, T, base::all(i1 <= base::nrow(x)))
  okTV <- uj::f0(!okTP, T, base::all(i2 <= base::nrow(x)))
  okFT <- uj::.unq_psw_vec(base::c(i1, i2))
  okNN <- base::length(i1) == base::length(i2)
  errs <- NULL
  if (!ok2D) {errs <- base::c(errs, "[x] must be a populated matrix or a populated data.frame.")}
  if (!okFP) {errs <- base::c(errs, "[i1] must be a complete positive whole-number vec (?unq_psw_vec).")}
  if (!okTP) {errs <- base::c(errs, "[i2] must be a complete positive whole-number vec (?unq_psw_vec).")}
  if (!okFV) {errs <- base::c(errs, "[i1] contains a value greater than [nrow(x)].")}
  if (!okTV) {errs <- base::c(errs, "[i2] contains a value greater than [nrow(x)].")}
  if (!okFT) {errs <- base::c(errs, "A value from [i1] is duplicated in [i2].")}
  if (!okNN) {errs <- base::c(errs, "[i1] and [i2] must be of equal length.")}
  if (uj::.DEF(errs)) {uj::stopper(errs)}
  old <- x
  for (i in 1:base::length(i1)) {
    x[i1[i], ] <- old[i2[i], ]
    x[i2[i], ] <- old[i1[i], ]
  }
  x
}

#' @describeIn swaps Swap columns of `x` by index, meaning that column `i1[i]` of `x` is replaced by column `i2[i]` of `x` and vice versa.
#' @export
swap_cols <- function(x, i1, i2) {
  ok2D <- uj::.pop_mat(x) | uj::.pop_dtf(x)
  ok1P <- uj::.unq_psw_vec(i1)
  ok2P <- uj::.unq_psw_vec(i2  )
  ok1V <- uj::f0(!ok1P, T, base::all(i1 <= base::ncol(x)))
  ok2V <- uj::f0(!ok2P, T, base::all(i2 <= base::ncol(x)))
  ok12 <- uj::.unq_psw_vec(base::c(i1, i2))
  okNN <- base::length(i1) == base::length(i2)
  errs <- NULL
  if (!ok2D) {errs <- base::c(errs, "[x] must be a populated matrix or a populated data.frame.")}
  if (!ok1P) {errs <- base::c(errs, "[i1] must be a complete positive whole-number vec (?unq_psw_vec).")}
  if (!ok2P) {errs <- base::c(errs, "[i2] must be a complete positive whole-number vec (?unq_psw_vec).")}
  if (!ok1V) {errs <- base::c(errs, "[i1] contains a value greater than [ncol(x)].")}
  if (!ok2V) {errs <- base::c(errs, "[i2] contains a value greater than [ncol(x)].")}
  if (!ok12) {errs <- base::c(errs, "A value from [i1] is duplicated in [i2].")}
  if (!okNN) {errs <- base::c(errs, "[i1] and [i2] must be of equal length.")}
  if (uj::.DEF(errs)) {uj::stopper(errs)}
  old <- x
  for (i in 1:base::length(i1)) {
    x[ , i1[i]] <- old[ , i2[i]]
    x[ , i2[i]] <- old[ , i1[i]]
  }
  x
}
