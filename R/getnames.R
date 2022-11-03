#' @name getnames
#' @family naming
#' @family meta
#' @title Get element, row, and/or column names
#' @param x An object to get names from.
#' @return A character vector containing names or a 2-element list of character
#'   vectors.
#' @export
getnames <- function(x, d = 0, u = T, err = F) {
  vx <- ipop(x)
  vd <- isIN(d, c(0, 1, 2, 12))
  vu <- isTF(u)
  ve <- isTF(err)
  err. <- NULL
  if (!vx) {err. <- c(err., "\n • [x] must be populated.")}
  if (!vd) {err. <- c(err., "\n • [d] must be 0, 1, 2, or 12.")}
  if (!vu) {err. <- c(err., "\n • [u] must be TRUE or FALSE.")}
  if (!ve) {err. <- c(err., "\n • [err] must be TRUE or FALSE.")}
  if (idef(err.)) {stop(err.)}
  if (d == 0) {
    en <- names(x)
    if (err & length(en) > 0) {stop("\n • elements of [x] are not named.")}
    if (u & !is_unique(en)) {stop("\n • element names of [x] are not unique.")}
    return(en)
  }
  if (!id2D(x)) {stop("\n • [x] must be a matrix or tibble")}
  if (d %in% c(1, 12)) {
    rn <- rownames(x)
    if (err & length(rn) > 0) {stop("\n • row of [x] are not named.")}
    if (u & !is_unique(rn)) {stop("\n • row names of [x] are not unique.")}
    if (d == 1) {return(rn)}
  }
  if (d %in% c(2, 12)) {
    cn <- colnames(x)
    if (err & length(cn) > 0) {stop("\n • column of [x] are not named.")}
    if (u & !is_unique(cn)) {stop("\n • column names of [x] are not unique.")}
    if (d == 2) {return(cn)}
  }
  list(r = rn, c = cn)
}

#' @describeIn getnames Get element names.
#' @export
enames <- function(x, u = T, err = F) {getnames(x, 0, u, err)}

#' @describeIn getnames Get row names.
#' @export
rnames <- function(x, u = T, err = F) {getnames(x, 1, u, err)}

#' @describeIn getnames Get column names.
#' @export
cnames <- function(x, u = T, err = F) {getnames(x, 2, u, err)}

#' @describeIn getnames Get row and column names (in a 2-element list).
#' @export
rcnames <- function(x, u = T, err = F) {getnames(x, 12, u, err)}
