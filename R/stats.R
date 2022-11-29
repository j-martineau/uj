#' @name stats0
#' @family extensions
#' @family na_values
#' @title Compute stats ignoring `NA` values
#' @description \itemize{
#'   \item **`sum0`**: calculates sum of non-`NA` atomic values in `...`.
#'   \item **`sd0`**: calculates standard deviation of non-`NA` atomic values in `...`.
#'   \item **`var0`**: calculates variance of non-`NA` atomic values in `...`.
#'   \item **`min0`**: calculates minimum of non-`NA` atomic values in `...`.
#'   \item **`max0`**: calculates maximum of non-`NA` atomic values in `...`.
#'   \item **`mean0`**: calculates mean of all non-`NA` atomic values in `...`.
#'   \item **`cor0`**: calls `cor(x, y, use = "complete.obs")`.
#'   \item **`cov0`**: calls `cov(x, y, use = "complete.obs")`.
#'   \item **`csds0`**: calls `apply(x, 2, sd, na.rm = T)`.
#'   \item **`rsds0`**: calls `apply(x, 1, sd, na.rm = T)`.
#'   \item **`pmin0`**: calls `pmin(..., na.rm = T)`
#'   \item **`pmax0`**: calls `pmax(..., na.rm = T)`
#'   \item **`csums0`**: calls `colSums(x, na.rm = T)`.
#'   \item **`rsums0`**: calls `rowSums(x, na.rm = T)`.
#'   \item **`cmeans0`**: calls `colMeans(x, na.rm = T)`.
#'   \item **`rmeans0`**: calls `rowMeans(x, na.rm = T)`.
#' }
#' @param ... Scalars, vectors, or matrices. Reduced a single vector of atomic values for `sd0`, `min0`, `max0`, and `mean0`.
#' @param n Positive whole-number scalar size of the complete set each subset is drawn from.
#' @param x A \link[=cmp_num_vec]{complete numeric vec} or a \link[=cmp_num_mat]{complete numeric matrix}.
#' @param y An optional complete numeric vec or a complete numeric matrix.
#' @return \itemize{
#'   \item **`sum0, sd0, var0, min0, max0, mean0`**: a numeric scalar.
#'   \item **`cor0, cov0, csds0, rsds0, pmin0, pmax0, csums0, rsums0, cmeans0, rmeans0`**: a numeric scalar or vector.
#' }
#' @export
sum0 <- function(...) {sum(av(...), na.rm = T)}

#' @rdname stats0
#' @export
sd0 <- function(...) {sd(av(...), na.rm = T)}

#' @rdname stats0
#' @export
cor0 <- function(x, y = NULL) {cor(x, y, use = "complete.obs")}

#' @rdname stats0
#' @export
cov0 <- function(x, y = NULL) {cov(x, y, use = "complete.obs")}

#' @rdname stats0
#' @export
min0 <- function(...) {min(av(...), na.rm = T)}

#' @rdname stats0
#' @export
max0 <- function(...) {max(av(...), na.rm = T)}

#' @rdname stats0
#' @export
var0 <- function(...) {var(av(...), na.rm = T)}

#' @rdname stats0
#' @export
pmin0 <- function(...) {pmin(..., na.rm = T)}

#' @rdname stats0
#' @export
pmax0 <- function(...) {pmax(..., na.rm = T)}

#' @rdname stats0
#' @export
mean0 <- function(...) {mean(av(...), na.rm = T)}

#' @rdname stats0
#' @export
csds0 <- function(x) {apply(x, 2, sd, na.rm = T)}

#' @rdname stats0
#' @export
rsds0 <- function(x) {apply(x, 1, sd, na.rm = T)}

#' @rdname stats0
#' @export
csums0 <- function(x) {colSums(x, na.rm = T)}

#' @rdname stats0
#' @export
rsums0 <- function(x) {rowSums(x, na.rm = T)}

#' @rdname stats0
#' @export
cmeans0 <- function(x) {colMeans(x, na.rm = T)}

#' @rdname stats0
#' @export
rmeans0 <- function(x) {rowMeans(x, na.rm = T)}
