#' @name getnames
#' @family naming
#' @family meta
#' @title Get element, row, and/or column names
#' @details \strong{\code{enames}}
#'   \cr Gets element names of an atomic or list vector.
#'   \cr\cr
#'   strong{\code{rnames, cnames}}
#'   \cr Gets row and column names, respectively, of a tibble or matrix.
#'   \cr \cr
#'   \strong{\code{rcnames}}
#'   \cr Get row and column names of a tibble or matrix in a 2-element list.
#' @param x An object to get names from.
#' @return A character vector containing names or a 2-element list of character
#'   vectors.
#' @export
getnames <- function(x, d = 0, u = T, err = F) {
  VX <- xpop(x)
  VD <- isIN(d, c(0, 1, 2, 12))
  VU <- isTF(u)
  VE <- isTF(err)
  E <- NULL
  if (!VX) {E <- c(E, "\n  * [x] must be populated.")}
  if (!VD) {E <- c(E, "\n  * [d] must be 0, 1, 2, or 12.")}
  if (!VU) {E <- c(E, "\n  * [u] must be TRUE or FALSE.")}
  if (!VE) {E <- c(E, "\n  * [err] must be TRUE or FALSE.")}
  if (xdef(E)) {stop(E)}
  if (d == 0) {
    N <- names(x)
    if (err & length(N) > 0) {stop("\n  * Elements of [x] are not named.")}
    if (u & !is_unique(N)) {stop("\n  * Element names of [x] are not unique.")}
    return(N)
  }
  if (!xd2D(x)) {stop("\n  * [x] must be a matrix or tibble")}
  if (d %in% c(1, 12)) {
    R <- rownames(x)
    if (err & length(R) > 0) {stop("\n  * Row of [x] are not named.")}
    if (u & !is_unique(R)) {stop("\n  * Row names of [x] are not unique.")}
    if (d == 1) {return(R)}
  }
  if (d %in% c(2, 12)) {
    C <- colnames(x)
    if (err & length(C) > 0) {stop("\n  * Column of [x] are not named.")}
    if (u & !is_unique(C)) {stop("\n  * Column names of [x] are not unique.")}
    if (d == 2) {return(C)}
  }
  list(r = R, c = C)
}

#' @rdname getnames
#' @export
enames <- function(x, u = T, err = F) {getnames(x, 0, u, err)}

#' @rdname getnames
#' @export
rnames <- function(x, u = T, err = F) {getnames(x, 1, u, err)}

#' @rdname getnames
#' @export
cnames <- function(x, u = T, err = F) {getnames(x, 2, u, err)}

#' @rdname getnames
#' @export
rcnames <- function(x, u = T, err = F) {getnames(x, 12, u, err)}
