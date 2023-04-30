#' @name sum0
#' @encoding UTF-8
#' @family missingness
#' @family extensions
#' @title Compute stats ignoring `NA` values
#' @description Simple wrappers for calculating statistics ignoring any `NA` values.
#' @details
#' \tabular{ll}{  `cmeans0`   \tab Calls `colMeans(X, na.rm = T)`          \cr
#'                `rmeans0`   \tab Calls `rowMeans(X, na.rm = T)`          \cr
#'                `csums0`    \tab Calls `colSums(X, na.rm = T)`           \cr
#'                `rsums0`    \tab Calls `rowSums(X, na.rm = T)`           \cr
#'                `csds0`     \tab Calls `apply(X, 2, sd, na.rm = T)`      \cr
#'                `rsds0`     \tab Calls `apply(X, 1, sd, na.rm = T)`      \cr
#'                `pmin0`     \tab Calls `pmin(..., na.rm = T)`            \cr
#'                `pmax0`     \tab Calls `pmax(..., na.rm = T)`            \cr
#'                `mean0`     \tab Calls `mean(av(...), na.rm = T)`        \cr
#'                `min0`      \tab Calls `min(av(...), na.rm = T)`         \cr
#'                `max0`      \tab Calls `max(av(...), na.rm = T)`         \cr
#'                `sum0`      \tab Calls `sum(av(...), na.rm = T)`         \cr
#'                `var0`      \tab Calls `var(av(...), na.rm = T)`         \cr
#'                `cor0`      \tab Calls `cor(X, Y, use = 'complete.obs')` \cr
#'                `cov0`      \tab Calls `cov(X, Y, use = 'complete.obs')` \cr
#'                `sd0`       \tab Calls `sd(av(...), na.rm = T)`            }
#' @param ... Scalars, vectors, or matrices. Reduced a single vector of atomic values for `sd0`, `min0`, `max0`, and `mean0`.
#' @param X A \link[=cmp_num_vec]{complete numeric vec} or a \link[=cmp_num_mat]{complete numeric matrix}.
#' @param Y An optional complete numeric vec or a complete numeric matrix.
#' @return **A numeric vector**           \cr\cr `cmeans0, csums0, cvars0, csds0` \cr `rmeans0, rsums0, rvars0, rsds0` \cr `mean0, sum0, var0, sd0` \cr `pmin0, min0` \cr `pmax0, max0`
#' \cr\cr  **A numeric vector or matrix** \cr\cr `cor0, cov0`
#' @examples
#' egVec1 <- sample(0:99, 10)
#' egVec2 <- sample(0:99, 20)
#' egVec3 <- sample(0:99, 20)
#' egMat1 <- sample(0:99, 100)
#'
#' egVec1[sample(1:10, 1)] <- NA
#' egVec2[sample(1:20, 1)] <- NA
#' egVec3[sample(1:20, 2)] <- NA
#' egMat1[sample(1:100, 5)] <- NA
#' egMat1 <- matrix(mat1, nrow = 10)
#' rownames(egMat1) <- paste0("R", 1:10)
#' colnames(egMat1) <- paste0("C", 1:10)
#' egDtf1 <- as.data.frame(egMat1)
#' egX <- list(v1 = egVec1, v2 = egVec2, v3 = egVec3, m = egMat1, d = egDtf1)
#'
#' egVec1
#' egVec2
#' egVec3
#' egMat1
#' egDtf1
#' egX
#'
#' matrix(c(cmeans0(egMat1), cmeans0(egDtf1)), ncol = 2)
#' matrix(c(rmeans0(egMat1), rmeans0(egDtf1)), ncol = 2)
#' matrix(c(csums0( egMat1), csums0( egDtf1)), ncol = 2)
#' matrix(c(rsums0( egMat1), rsums0( egDtf1)), ncol = 2)
#' matrix(c(csds0(  egMat1), csds0(  egDtf1)), ncol = 2)
#' matrix(c(rsds0(  egMat1), rsds0(  egDtf1)), ncol = 2)
#'
#' pmin0(egVec1, egVec2, egVec3)
#' pmax0(egVec2, egVec2, egVec3)
#'
#' cor0(egVec2, egVec3)
#' cov0(egVec2, egVec3)
#'
#' cor0(egMat1)
#' cov0(egMat1)
#'
#' list(mean = mean0(egX), min = min0(egX), max = max0(egX),
#'       sum = sum0( egX), var = var0(egX), sd  = sd0( egX))
#' @export
sum0 <- function(...) {base::sum(uj::av(...), na.rm = T)}

#' @rdname sum0
#' @export
sd0 <- function(...) {stats::sd(uj::av(...), na.rm = T)}

#' @rdname sum0
#' @export
cor0 <- function(X, Y = NULL) {stats::cor(X, Y, use = "complete.obs")}

#' @rdname sum0
#' @export
cov0 <- function(X, Y = NULL) {stats::cov(X, Y, use = "complete.obs")}

#' @rdname sum0
#' @export
min0 <- function(...) {base::min(uj::av(...), na.rm = T)}

#' @rdname sum0
#' @export
max0 <- function(...) {base::max(uj::av(...), na.rm = T)}

#' @rdname sum0
#' @export
var0 <- function(...) {base::var(uj::av(...), na.rm = T)}

#' @rdname sum0
#' @export
pmin0 <- function(...) {base::pmin(..., na.rm = T)}

#' @rdname sum0
#' @export
pmax0 <- function(...) {base::pmax(..., na.rm = T)}

#' @rdname sum0
#' @export
mean0 <- function(...) {base::mean(uj::av(...), na.rm = T)}

#' @rdname sum0
#' @export
csds0 <- function(X) {base::apply(X, 2, stats::sd, na.rm = T)}

#' @rdname sum0
#' @export
rsds0 <- function(X) {base::apply(X, 1, stats::sd, na.rm = T)}

#' @rdname sum0
#' @export
cvars0 <- function(X) {base::apply(X, 2, stats::var, na.rm = T)}

#' @rdname sum0
#' @export
rvars0 <- function(X) {base::apply(X, 1, stats::var, na.rm = T)}

#' @rdname sum0
#' @export
csums0 <- function(X) {base::colSums(X, na.rm = T)}

#' @rdname sum0
#' @export
rsums0 <- function(X) {base::rowSums(X, na.rm = T)}

#' @rdname sum0
#' @export
cmeans0 <- function(X) {base::colMeans(X, na.rm = T)}

#' @rdname sum0
#' @export
rmeans0 <- function(X) {base::rowMeans(X, na.rm = T)}
