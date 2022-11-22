#' @name stats0
#' @family extensions
#' @family na_values
#' @title Compute stats ignoring NA
#' @param ... Scalars, vectors, or matrices. Reduced a single vector of atomic
#'   values for \code{sd0}, \code{min0}, \code{max0}, and \code{mean0}.
#' @param n Positive whole-number scalar size of the complete set each subset is
#'   drawn from.
#' @param x \link[=cmp_num_vec]{Complete numeric vec} or
#'   \link[=cmp_num_mat]{complete numeric matrix}.
#' @param y Optional \link[=cmp_num_vec]{complete numeric vec} or
#'   \link[=cmp_num_mat]{complete numeric matrix}.
#' @export
stats0 <- NULL

#' @describeIn stats0 Standard deviation of non-\code{NA} atomic elements
#'   contained in \code{...} as a whole.
#' @export
sum0 <- function(...) {sum(av(...), na.rm = T)}

#' @describeIn stats0 Standard deviation of non-\code{NA} atomic elements
#'   contained in \code{...} as a whole.
#' @export
sd0 <- function(...) {sd(av(...), na.rm = T)}

#' @describeIn stats0 Correlation of complete cases in \code{x.} and/or
#'   \code{y.}.
#' @export
cor0 <- function(x, y = NULL) {cor(x, y, use = "complete.obs")}

#' @describeIn stats0 Covariance of complete cases in \code{x.} and/or
#'   \code{y.}.
#' @export
cov0 <- function(x, y = NULL) {cov(x, y, use = "complete.obs")}

#' @describeIn stats0 Minimum value of non-\code{NA} atomic elements contained
#'   in \code{...} as a whole.
#' @export
min0 <- function(...) {min(av(...), na.rm = T)}

#' @describeIn stats0 Maximum value of non-\code{NA} atomic elements contained
#'   in \code{...} as a whole.
#' @export
max0 <- function(...) {max(av(...), na.rm = T)}

#' @describeIn stats0 Variance of non-\code{NA} atomic elements contained in
#'   \code{...} as a whole.
#' @export
var0 <- function(...) {var(av(...), na.rm = T)}

#' @describeIn stats0 Pairwise minimum values ignoring \code{NA} values.
#' @export
pmin0 <- function(...) {pmin(..., na.rm = T)}

#' @describeIn stats0 Pairwise maximum values ignoring \code{NA} values.
#' @export
pmax0 <- function(...) {pmax(..., na.rm = T)}

#' @describeIn stats0 Mean value of non-\code{NA} atomic elements contained in
#'   \code{...} as a whole.
#' @export
mean0 <- function(...) {mean(av(...), na.rm = T)}

#' @describeIn stats0 Column standard deviations of \code{x} removing
#'   non-\code{NA} values separately for each column.
#' @export
csds0 <- function(x) {apply(x, 2, sd, na.rm = T)}

#' @describeIn stats0 Row standard deviations of \code{x} removing non-\code{NA}
#'   values separately for each row.
#' @export
rsds0 <- function(x) {apply(x, 1, sd, na.rm = T)}

#' @describeIn stats0 Column sums of \code{x} removing non-\code{NA} values
#'   separately for each column.
#' @export
csums0 <- function(x) {colSums(x, na.rm = T)}

#' @describeIn stats0 Row sums of \code{x} removing non-\code{NA} values
#'   separately for each row.
#' @export
rsums0 <- function(x) {rowSums(x, na.rm = T)}

#' @describeIn stats0 Column means of \code{x} removing non-\code{NA} values
#'   separately for each column.
#' @export
cmeans0 <- function(x) {colMeans(x, na.rm = T)}

#' @describeIn stats0 Row means of \code{x} removing non-\code{NA} values
#'   separately for each row.
#' @export
rmeans0 <- function(x) {rowMeans(x, na.rm = T)}

