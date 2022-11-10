#' @name naming.
#' @family extensions
#' @title Get or set element, row, and/or column names.
#' @param x Vector or list for \code{namev} and \code{vn}; matrix or data.frame
#'   for \code{rn}, \code{namer}, \code{cn}, \code{namec}, \code{namerc}; or any
#'   object for \code{namel}.
#' @param ... Arbitrary number of arguments to be inspected for names.
#' @param err \link[cmp_lgl_scl]{Complete logical} scalar indicating whether
#'   names must be non-missing.
#' @param d \link[cmp_psw_scl]{Complete positive whole-number scalar} giving the
#'   dimension(s) to name: \code{0} for elements (of atomic or list vectors
#'   only), \code{1} for rows of matrices or tibbles, \code{2} for columns of
#'   matrices or dtfs, or \code{12} for rows and columns of matrices or dtfs.
#' @param u \link[cmp_lgl_scl]{Complete logical scalar} indicating whether names
#'   must be unique.
#' @param blank \link[cmp_lgl_scl]{Complete logical scalar} indicating whether blank
#'   strings (\code{""}) are allowed as names.
#' @param ln \link[cmp_chr_scl]{Complete character scalar} element name.
#' @param en \link[cmp_chr_vec]{Complete character vec} of element names.
#' @param rn,cn \link[cmp_chr_vec]{Complete character vecs} of row or column
#'   names, respectively.
#' @return Character vector or a named/renamed version of \code{x}.
#' @export
naming. <- function() {help("naming.", package = "uj")}

#' @describeIn naming. Name the elements of a vector.
#' @export
namev <- function(x, en) {
  bank_funs(imvc, x = x)
  bank_funs(atm_vec, en = en, n = length(x))
  err_check()
  names(x) <- en
  x
}

#' @describeIn naming. Name the rows of a matrix or data frame.
#' @export
namer <- function(x, rn) {
  bank_funs(id2D, x = x)
  bank_funs(atm_vec, rn = rn, n = NROW(x))
  err_check()
  rownames(x) <- rn
  x
}

#' @describeIn naming. Name the columns of a matrix or data frame.
#' @export
namec <- function(x, cn) {
  bank_funs(id2D, x = x)
  bank_funs(atm_vec, cn = cn, n = NCOL(x))
  err_check()
  colnames(x) <- cn
  x
}

#' @describeIn naming. Create a 1-element list from \code{x} and name that
#'   element with the value of \code{e}
#' @export
namel <- function(x, ln) {
  bank_funs(atm_scl, ln = ln)
  err_check()
  x <- list(x)
  names(x) <- ln
  x
}

#' @describeIn naming. Name the rows and columns of a matrix or data frame.
#' @export
namerc <- function(x, rn, cn) {namer(namec(x, cn), rn)}

#' @describeIn naming. Determine whether an objects elements, rows, and/or
#'   columns are named, accounting for whether names must be unique, whether the
#'   object may contain empty elements, and whether names may be blank strings
#'   ("").
#' @export
named <- function(x, d = 0, u = T, blank = F) {
  vx <- ivec(x) | pop_vls(x) | (is.array(x) & length(dim(x)) == 1)
  vd <- isIN(d, c(0, 1, 2, 12))
  v1 <- ANY(isEQ(d, 0), id1D(x))
  v2 <- ANY(isIN(d, c(1, 2, 12)), id2D(x))
  vu <- isTF(u)
  vb <- isTF(blank)
  errn <- NULL
  if (!vx) {errn <- c(errn, "\n • [x] must be a vector, populated vlist (?pop_vls), matrix, or data.frame.")}
  if (!vd) {errn <- c(errn, "\n • [d] must be 0, 1, 2, or 12.")}
  if (!v1) {errn <- c(errn, "\n • [d] must be 0 when [x] is a vector, vlist (?is_vls), or 1D array.")}
  if (!v2) {errn <- c(errn, "\n • [d] must be 1, 2, or 12 when [x] is a matrix or data.frame.")}
  if (!vu) {errn <- c(errn, "\n • [u] must be TRUE or FALSE.")}
  if (!vb) {errn <- c(errn, "\n • [blank] must be TRUE or FALSE.")}
  if (idef(errn)) {stop(errn)}
  leOK <- ueOK <- beOK <- lrOK <- urOK <- brOK <- lcOK <- ucOK <- bcOK <- T      # initialize result scalars
  if (length(x) == 0) {return(F)}                                                # length 0 is unnamed by default
  if (d == 0) {                                                                  # if inspecting for element names
    elabs <- names(x)                                                            # > get element names
    leOK <- length(elabs) > 0                                                    # > are elements named?
    ueOK <- f0(leOK & u, isEQ(elabs, uv(elabs)), F)                              # > do names meet uniqueness specification?
    beOK <- f0(leOK & blank, !any(elabs == ""), F)                               # > do names meet blankness specification?
  }
  if (d %in% c(1, 12)) {
    rlabs <- rownames(x)
    lrOK <- length(rlabs) > 0
    urOK <- f0(lrOK & u, isEQ(rlabs, uv(rlabs)), F)
    brOK <- f0(lrOK & blank, !any(rlabs == ""), F)
  }
  if (d %in% c(2, 12)) {
    clabs <- colnames(x)
    lcOK <- length(clabs) > 0
    ucOK <- f0(lcOK & u, isEQ(clabs, uv(clabs)), F)
    bcOK <- f0(lcOK & blank, !any(clabs == ""), F)
  }
  leOK & ueOK & beOK & lrOK & urOK & brOK & lcOK & ucOK & bcOK
}

#' @describeIn naming. Are elements of \code{x} named?
#' @export
enamed <- function(x, u = T, blank = F) {named(x, 0, u, blank)}

#' @describeIn naming. Are rows of \code{x} named?
#' @export
rnamed <- function(x, u = T, blank = F) {named(x, 1, u, blank)}

#' @describeIn naming. Are columns of \code{x} named?
#' @export
cnamed <- function(x, u = T, blank = F) {named(x, 2, u, blank)}

#' @describeIn naming. Are both rows and columns of \code{x} named?
#' @export
rcnamed <- function(x, u = T, blank = F) {named(x, 12, u, blank)}

#' @describeIn naming. Are all \code{...} arguments named?
#' @export
named. <- function(..., u = T, blank = F) {named(list(...), 0, u, blank)}

#' @describeIn naming. Get element, row, and/or column names with optional
#'   restrictions.
#' @export
getnames <- function(x, d = 0, u = T, err = F) {
  vx <- ipop(x)
  vd <- isIN(d, c(0, 1, 2, 12))
  vu <- isTF(u)
  ve <- isTF(err)
  errs <- NULL
  if (!vx) {errs <- c(errs., "\n • [x] must be populated (?ipop).")}
  if (!vd) {errs <- c(errs., "\n • [d] must be 0, 1, 2, or 12.")}
  if (!vu) {errs <- c(errs., "\n • [u] must be TRUE or FALSE.")}
  if (!ve) {errs <- c(errs., "\n • [err] must be TRUE or FALSE.")}
  if (idef(errs)) {stop(errs)}
  if (d == 0) {
    en <- names(x)
    if (err & length(en) > 0) {stop("\n • elements of [x] are not named.")}
    if (u & !is_unq(en)) {stop("\n • element names of [x] are not unique.")}
    return(en)
  }
  if (!id2D(x)) {stop("\n • [x] must be a matrix or a dtf (?is_dtf)")}
  if (d %in% c(1, 12)) {
    rn <- rownames(x)
    if (err & length(rn) > 0) {stop("\n • row of [x] are not named.")}
    if (u & !is_unq(rn)) {stop("\n • row names of [x] are not unique.")}
    if (d == 1) {return(rn)}
  }
  if (d %in% c(2, 12)) {
    cn <- colnames(x)
    if (err & length(cn) > 0) {stop("\n • column of [x] are not named.")}
    if (u & !is_unq(cn)) {stop("\n • column names of [x] are not unique.")}
    if (d == 2) {return(cn)}
  }
  list(r = rn, c = cn)
}

#' @describeIn naming. Get element names.
#' @export
en <- function(x, u = T, err = F) {getnames(x, 0, u, err)}

#' @describeIn naming. Get row names.
#' @export
rn <- function(x, u = T, err = F) {getnames(x, 1, u, err)}

#' @describeIn naming. Get column names.
#' @export
cn <- function(x, u = T, err = F) {getnames(x, 2, u, err)}

#' @describeIn naming. Get row and column names (in a 2-element list).
#' @export
rcn <- function(x, u = T, err = F) {getnames(x, 12, u, err)}
