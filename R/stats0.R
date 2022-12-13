#' @name stats0
#' @family unchecked
#' @family missingness
#' @title Compute stats ignoring `NA` values
#' @description \tabular{rl}{
#'       `cmeans0`   \tab Calls `colMeans(x, na.rm = T)`.
#'   \cr `rmeans0`   \tab Calls `rowMeans(x, na.rm = T)`.
#'   \cr  `csums0`   \tab Calls `colSums(x, na.rm = T)`.
#'   \cr  `rsums0`   \tab Calls `rowSums(x, na.rm = T)`.
#'   \cr   `csds0`   \tab Calls `apply(x, 2, sd, na.rm = T)`.
#'   \cr   `rsds0`   \tab Calls `apply(x, 1, sd, na.rm = T)`.
#'   \cr   `pmin0`   \tab Calls `pmin(..., na.rm = T)`.
#'   \cr   `pmax0`   \tab Calls `pmax(..., na.rm = T)`.
#'   \cr   `mean0`   \tab Calls `mean(av(...), na.rm = T)`.
#'   \cr    `min0`   \tab Calls `min(av(...), na.rm = T)`.
#'   \cr    `max0`   \tab Calls `max(av(...), na.rm = T)`.
#'   \cr    `sum0`   \tab Calls `sum(av(...), na.rm = T)`.
#'   \cr    `var0`   \tab Calls `var(av(...), na.rm = T)`.
#'   \cr     `sd0`   \tab Calls `sd(av(...), na.rm = T)`.
#'   \cr    `cor0`   \tab Calls `cor(x, y, use = "complete.obs")`.
#'   \cr    `cov0`   \tab Calls `cov(x, y, use = "complete.obs")`.
#' }
#' @param ... Scalars, vectors, or matrices. Reduced a single vector of atomic values for `sd0`, `min0`, `max0`, and `mean0`.
#' @param n Positive whole-number scalar size of the complete set each subset is drawn from.
#' @param x A \link[=cmp_num_vec]{complete numeric vec} or a \link[=cmp_num_mat]{complete numeric matrix}.
#' @param y An optional complete numeric vec or a complete numeric matrix.
#' @return \tabular{rl}{
#'     `cmeans0,rmeans0`   \tab A numeric vector.
#'   \cr `csums0,rsums0`   \tab  
#'   \cr   `csds0,rsds0`   \tab  
#'   \cr   `pmin0,pmax0`   \tab  
#'   \cr                   \tab  
#'   \cr    `mean0,sum0`   \tab A numeric scalar.
#'   \cr     `min0,max0`   \tab  
#'   \cr      `sd0,var0`   \tab  
#'   \cr                   \tab  
#'   \cr     `cor0,cov0`   \tab A numeric vector/matrix.
#' }
#' @examples
#' vec1. <- sample(0:99, 10)
#' vec2. <- sample(0:99, 20)
#' vec3. <- sample(0:99, 20)
#' mat1. <- sample(0:99, 100)
#'
#' vec1.[sample(1:10, 1)] <- NA
#' vec2.[sample(1:20, 1)] <- NA
#' vec3.[sample(1:20, 2)] <- NA
#' mat1.[sample(1:100, 5)] <- NA
#' mat1. <- matrix(mat1., nrow = 10)
#' rownames(mat1.) <- paste0("R", 1:10)
#' colnames(mat1.) <- paste0("C", 1:10)
#' dtf1. <- as.data.frame(mat1.)
#' x. <- list(v1 = vec1., v2 = vec2., v3 = vec3., m = mat1., d = dtf1.)
#'
#' vec1.
#' vec2.
#' vec3.
#' mat1.
#' dtf1.
#' x.
#'
#' matrix(c(cmeans0(mat1.), cmeans0(dtf1.)), ncol = 2)
#' matrix(c(rmeans0(mat1.), rmeans0(dtf1.)), ncol = 2)
#' matrix(c(csums0(mat1.), csums0(dtf1.)), ncol = 2)
#' matrix(c(rsums0(mat1.), rsums0(dtf1.)), ncol = 2)
#' matrix(c(csds0(mat1.), csds0(dtf1.)), ncol = 2)
#' matrix(c(rsds0(mat1.), rsds0(dtf1.)), ncol = 2)
#'
#' pmin0(vec2., vec3.)
#' pmax0(vec2., vec3.)
#'
#' cor0(vec2., vec3.)
#' cov0(vec2., vec3.)
#'
#' cor0(mat1.)
#' cov0(mat1.)
#'
#' list(mean = mean0(x.), min = min0(x.), max = max0(x.), sum = sum0(x.), var = var0(x.), sd = sd0(x.))
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
