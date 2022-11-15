#' @name stats.
#' @family math
#' @family extensions
#' @title Compute stats ignoring NA
#' @param ... Scalars, vectors, or matrices. Reduced a single vector of atomic
#'   values for \code{sd0}, \code{min0}, \code{max0}, and \code{mean0}.
#' @param n Positive whole-number scalar size of the complete set each subset is
#'   drawn from.
#' @param x \link[cmp_num_vec]{Complete numeric vec} or
#'   \link[cmp_num_mat]{complete numeric matrix}.
#' @param y Optional \link[cmp_num_vec]{complete numeric vec} or
#'   \link[cmp_num_mat]{complete numeric matrix}.
#' @export
stats. <- function() {help("stats.", package = "uj")}

#' @describeIn stats. Standard deviation of non-\code{NA} atomic elements
#'   contained in \code{...} as a whole.
#' @export
sum0 <- function(...) {sum(av(...), na.rm = T)}

#' @describeIn stats. Standard deviation of non-\code{NA} atomic elements
#'   contained in \code{...} as a whole.
#' @export
sd0 <- function(...) {sd(av(...), na.rm = T)}

#' @describeIn stats. Correlation of complete cases in \code{x.} and/or
#'   \code{y.}.
#' @export
cor0 <- function(x, y = NULL) {cor(x, y, use = "complete.obs")}

#' @describeIn stats. Covariance of complete cases in \code{x.} and/or
#'   \code{y.}.
#' @export
cov0 <- function(x, y = NULL) {cov(x, y, use = "complete.obs")}

#' @describeIn stats. Minimum value of non-\code{NA} atomic elements contained
#'   in \code{...} as a whole.
#' @export
min0 <- function(...) {min(av(...), na.rm = T)}

#' @describeIn stats. Maximum value of non-\code{NA} atomic elements contained
#'   in \code{...} as a whole.
#' @export
max0 <- function(...) {max(av(...), na.rm = T)}

#' @describeIn stats. Variance of non-\code{NA} atomic elements contained in
#'   \code{...} as a whole.
#' @export
var0 <- function(...) {var(av(...), na.rm = T)}

#' @describeIn stats. Pairwise minimum values ignoring \code{NA} values.
#' @export
pmin0 <- function(...) {pmin(..., na.rm = T)}

#' @describeIn stats. Pairwise maximum values ignoring \code{NA} values.
#' @export
pmax0 <- function(...) {pmax(..., na.rm = T)}

#' @describeIn stats. Mean value of non-\code{NA} atomic elements contained in
#'   \code{...} as a whole.
#' @export
mean0 <- function(...) {mean(av(...), na.rm = T)}

#' @describeIn stats. Column standard deviations of \code{x} removing
#'   non-\code{NA} values separately for each column.
#' @export
csds0 <- function(x) {apply(x, 2, sd, na.rm = T)}

#' @describeIn stats. Row standard deviations of \code{x} removing non-\code{NA}
#'   values separately for each row.
#' @export
rsds0 <- function(x) {apply(x, 1, sd, na.rm = T)}

#' @describeIn stats. Column sums of \code{x} removing non-\code{NA} values
#'   separately for each column.
#' @export
csums0 <- function(x) {colSums(x, na.rm = T)}

#' @describeIn stats. Row sums of \code{x} removing non-\code{NA} values
#'   separately for each row.
#' @export
rsums0 <- function(x) {rowSums(x, na.rm = T)}

#' @describeIn stats. Column means of \code{x} removing non-\code{NA} values
#'   separately for each column.
#' @export
cmeans0 <- function(x) {colMeans(x, na.rm = T)}

#' @describeIn stats. Row means of \code{x} removing non-\code{NA} values
#'   separately for each row.
#' @export
rmeans0 <- function(x) {rowMeans(x, na.rm = T)}

#' @describeIn stats. Create a list containing elements of indices to create all
#'   possible combinations of 1 or more elements of an object of size \code{n}.
#' @export
sets <- function(n) {
  if (!cmp_psw_scl(n)) {stop("\n \u2022 [n] must be a positive whole-number scalar.")}
  x <- lapply(1:n, function(x) combn(n, x))                                      # : all possible combinations from size 1 to size [n]
  y <- NULL                                                                      # : initialize the result
  for (i in 1:nx(x)) {                                                           # : FOR EACH element of the list returned in [x]
    xi <- x[[i]]                                                                 # : : get that element and
    for (j in 1:nc(xi)) {y <- c(y, list(av(xi[ , j])))}                          # : : FOR EACH column (i.e., unique combination) > append as a list element to [y]
  }                                                                              # : END
  y                                                                              # : return value
}                                                                                # END

