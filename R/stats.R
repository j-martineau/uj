#' @name stats_tools
#' @title Compute basic statistics by dropping \code{NA} values (complete cases
#'   only for \code{cor0}, and \code{cov0}).
#' @description Standard deviation of non-\code{NA} atomic elements contained in
#'   \code{...} as a whole.
#' @param ... Scalars, vectors, or matrices. Reduced a single vector of atomic
#'   values for \code{sd0}, \code{min0}, \code{max0}, and \code{mean0}.
#' @param x. Numeric vector or matrix.
#' @param y. Optional numeric vector of matrix.
#' @export
sd0 <- function(...) {sd(av(...), na.rm = T)}

#' @describeIn stats_tools Correlation of complete cases in \code{x.}
#'   and/or \code{y.}.
#' @export
cor0 <- function(x., y. = NULL) {cor(x., y., use = "complete.obs")}

#' @describeIn stats_tools Covariance of complete cases in \code{x.}
#'   and/or \code{y.}.
#' @export
cov0 <- function(x., y. = NULL) {cov(x., y., use = "complete.obs")}

#' @describeIn stats_tools Minimum value of non-\code{NA} atomic
#'   elements contained in \code{...} as a whole.
#' @export
min0 <- function(...) {min(av(...), na.rm = T)}

#' @describeIn stats_tools Maximum value of non-\code{NA} atomic
#'   elements contained in \code{...} as a whole.
#' @export
max0 <- function(...) {max(av(...), na.rm = T)}

#' @describeIn stats_tools Variance of non-\code{NA} atomic elements
#'   contained in \code{...} as a whole.
#' @export
var0 <- function(...) {var(av(...), na.rm = T)}

#' @describeIn stats_tools Pairwise minimum values ignoring \code{NA}
#'   values.
#' @export
pmin0 <- function(...) {pmin(..., na.rm = T)}

#' @describeIn stats_tools Pairwise maximum values ignoring \code{NA}
#'   values.
#' @export
pmax0 <- function(...) {pmax(..., na.rm = T)}

#' @describeIn stats_tools Mean value of non-\code{NA} atomic elements
#'   contained in \code{...} as a whole.
#' @export
mean0 <- function(...) {mean(av(...), na.rm = T)}

#' @describeIn stats_tools Column standard deviations of \code{x}
#'   removing non-\code{NA} values separately for each column.
csds0 <- function(x.) {apply(x., 2, sd, na.rm = T)}

#' @describeIn stats_tools Row standard deviations of \code{x} removing
#'   non-\code{NA} values separately for each row.
rsds0 <- function(x.) {apply(x., 1, sd, na.rm = T)}

#' @describeIn stats_tools Column sums of \code{x} removing
#'   non-\code{NA} values separately for each column.
csums0 <- function(x.) {colSums(x., na.rm = T)}

#' @describeIn stats_tools Row sums of \code{x} removing non-\code{NA}
#'   values separately for each row.
rsums0 <- function(x.) {rowSums(x., na.rm = T)}

#' @describeIn stats_tools Column means of \code{x} removing
#'   non-\code{NA} values separately for each column.
cmeans0 <- function(x.) {colMeans(x., na.rm = T)}

#' @describeIn stats_tools Row means of \code{x} removing non-\code{NA}
#'   values separately for each row.
rmeans0 <- function(x.) {rowMeans(x., na.rm = T)}

#' @title Sets from combinations
#' @description Create a list containing elements of indices to create all
#'   possible combinations of 1 or more elements of an object of size \code{n}.
#' @param n Size of the complete set each subset is drawn from.
sets <- function(n.) {                                                           # : BODY
  x. <- lapply(1:n., function(x) combn(n., x))                                   # : all possible combinations from size 1 to size [n]
  y. <- NULL                                                                     # : initialize the result
  for (i. in 1:nx(x.)) {                                                         # : FOR EACH element of the list returned in [x]
    xi. <- x.[[i.]]                                                              # : : get that element and
    for (j. in 1:nc(xi.)) {y. <- c(y., list(av(xi.[ , j.])))}                    # : : FOR EACH column (i.e., unique combination) > append as a list element to [y]
  }                                                                              # : END
  y.                                                                             # : return value
}                                                                                # END

