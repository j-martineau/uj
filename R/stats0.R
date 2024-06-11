#' @encoding UTF-8
#' @family missingness
#' @title Compute Statistics Ignoring `NA` Calues
#' @description Simple wrappers for calculating statistics ignoring any `NA` values.
#' @param ... Scalars, vectors, or matrices. Reduced a single vector of atomic values for `sd0`, `min0`, `max0`, and `mean0`.
#' @param x A \link[=cmp_num_vec]{complete numeric vec} or a \link[=cmp_num_mat]{complete numeric matrix}.
#' @param y An optional complete numeric vec or a complete numeric matrix.
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
#' egMat1 <- matrix(egMat1, nrow = 10)
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
stats0_help <- function() {utils::help("stats0_help", package = "uj")}

#' @describeIn stats0_help Calculate sum ignoring `NA` values.
#' @export
sum0 <- function(...) {base::sum(uj::av(...), na.rm = T)}

#' @describeIn stats0_help Calculate standard deviation ignoring `NA` values.
#' @export
sd0 <- function(...) {stats::sd(uj::av(...), na.rm = T)}

#' @describeIn stats0_help Calculate correlation ignoring `NA` values.
#' @export
cor0 <- function(x, y = NULL) {stats::cor(x, y, use = "complete.obs")}

#' @describeIn stats0_help Calculate covariance ignoring `NA` values.
#' @export
cov0 <- function(x, y = NULL) {stats::cov(x, y, use = "complete.obs")}

#' @describeIn stats0_help Calculate minimum value ignoring `NA` values.
#' @export
min0 <- function(...) {base::min(uj::av(...), na.rm = T)}

#' @describeIn stats0_help Calculate maximum ignoring `NA` values.
#' @export
max0 <- function(...) {base::max(uj::av(...), na.rm = T)}

#' @describeIn stats0_help Calculate variance ignoring `NA` values.
#' @export
var0 <- function(...) {stats::var(uj::av(...), na.rm = T)}

#' @describeIn stats0_help Calculate pairmin values ignoring `NA` values.
#' @export
pmin0 <- function(...) {base::pmin(..., na.rm = T)}

#' @describeIn stats0_help Calculate pairmax values ignoring `NA` values.
#' @export
pmax0 <- function(...) {base::pmax(..., na.rm = T)}

#' @describeIn stats0_help Calculate mean value ignoring `NA` values.
#' @export
mean0 <- function(...) {base::mean(uj::av(...), na.rm = T)}

#' @describeIn stats0_help Calculate column standard deviations ignoring `NA` values.
#' @export
csds0 <- function(x) {base::apply(x, 2, stats::sd, na.rm = T)}

#' @describeIn stats0_help Calculate row standard deviations ignoring `NA` values.
#' @export
rsds0 <- function(x) {base::apply(x, 1, stats::sd, na.rm = T)}

#' @describeIn stats0_help Calculate column variances ignoring `NA` values.
#' @export
cvars0 <- function(x) {base::apply(x, 2, stats::var, na.rm = T)}

#' @describeIn stats0_help Calculate row variances ignoring `NA` values.
#' @export
rvars0 <- function(x) {base::apply(x, 1, stats::var, na.rm = T)}

#' @describeIn stats0_help Calculate column sums ignoring `NA` values.
#' @export
csums0 <- function(x) {base::colSums(x, na.rm = T)}

#' @describeIn stats0_help Calculate row sums ignoring `NA` values.
#' @export
rsums0 <- function(x) {base::rowSums(x, na.rm = T)}

#' @describeIn stats0_help Calculate column means ignoring `NA` values.
#' @export
cmeans0 <- function(x) {base::colMeans(x, na.rm = T)}

#' @describeIn stats0_help Calculate row means ignoring `NA` values.
#' @export
rmeans0 <- function(x) {base::rowMeans(x, na.rm = T)}
