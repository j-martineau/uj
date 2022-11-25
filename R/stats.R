#' @name stats0
#' @family extensions
#' @family na_values
#' @title Compute Stats Ignoring `NA` Values
#' @param ... Scalars, vectors, or matrices. Reduced a single vector of atomic
#'   values for `sd0`, `min0`, `max0`, and `mean0`.
#' @param n Positive whole-number scalar size of the complete set each subset is
#'   drawn from.
#' @param x A \link[=cmp_num_vec]{complete numeric vec} or a
#'   \link[=cmp_num_mat]{complete numeric matrix}.
#' @param y An optional \link[=cmp_num_vec]{complete numeric vec} or a
#'   \link[=cmp_num_mat]{complete numeric matrix}.
#' @export
stats0 <- NULL

#' @describeIn stats0 Standard deviation of collective non-`NA` atomic elements
#'   in `...`.
#' @export
sum0 <- function(...) {sum(av(...), na.rm = T)}

#' @describeIn stats0 Standard deviation of collective non-`NA` atomic elements
#'   in `...`.
#' @export
sd0 <- function(...) {sd(av(...), na.rm = T)}

#' @describeIn stats0 Correlation of complete cases in `x` and/or `y`.
#' @export
cor0 <- function(x, y = NULL) {cor(x, y, use = "complete.obs")}

#' @describeIn stats0 Covariance of complete cases in `x` and/or `y`.
#' @export
cov0 <- function(x, y = NULL) {cov(x, y, use = "complete.obs")}

#' @describeIn stats0 Minimum value of collective non-`NA` atomic elements in
#'   `...`.
#' @export
min0 <- function(...) {min(av(...), na.rm = T)}

#' @describeIn stats0 Maximum value of collective non-`NA` atomic elements in
#'   `...`.
#' @export
max0 <- function(...) {max(av(...), na.rm = T)}

#' @describeIn stats0 Variance of collective non-`NA` atomic elements in `...`.
#' @export
var0 <- function(...) {var(av(...), na.rm = T)}

#' @describeIn stats0 Pairwise minimum values ignoring pairs containing any `NA`
#'   values.
#' @export
pmin0 <- function(...) {pmin(..., na.rm = T)}

#' @describeIn stats0 Pairwise maximum values ignoring pairs containing any `NA`
#'   values.
#' @export
pmax0 <- function(...) {pmax(..., na.rm = T)}

#' @describeIn stats0 Mean value of collective non-`NA` atomic elements in
#'   `...`.
#' @export
mean0 <- function(...) {mean(av(...), na.rm = T)}

#' @describeIn stats0 Column standard deviations of `x` removing non-`NA` values
#'   separately for each column.
#' @export
csds0 <- function(x) {apply(x, 2, sd, na.rm = T)}

#' @describeIn stats0 Row standard deviations of `x` removing non-`NA` values
#'   separately for each row.
#' @export
rsds0 <- function(x) {apply(x, 1, sd, na.rm = T)}

#' @describeIn stats0 Column sums of `x` removing non-`NA` values separately for
#'   each column.
#' @export
csums0 <- function(x) {colSums(x, na.rm = T)}

#' @describeIn stats0 Row sums of `x` removing non-`NA` values separately for
#'   each row.
#' @export
rsums0 <- function(x) {rowSums(x, na.rm = T)}

#' @describeIn stats0 Column means of `x` removing non-`NA` values separately
#'   for each column.
#' @export
cmeans0 <- function(x) {colMeans(x, na.rm = T)}

#' @describeIn stats0 Row means of `x` removing non-`NA` values separately for
#'   each row.
#' @export
rmeans0 <- function(x) {rowMeans(x, na.rm = T)}

