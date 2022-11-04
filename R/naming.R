#' @name names_uj
#' @family meta
#' @title Get or set element, row, and/or column names.
#' @param x Vector or vlist for \code{namev} and \code{vn}; matrix or data.frame
#'   for \code{rn}, \code{namer}, \code{cn}, \code{namec}, \code{namerc}; or any
#'   object for \code{namel}.
#' @param x. Argument to be inspected for names or to be named..
#' @param ... Arbitrary number of arguments to be inspected for names.
#' @param d. Integer scalar indicating dimensions to get names for. Valid values
#'   are \code{c(1, 2, 12)} for row, column, and both.
#' @param err. Logical scalar indicating whether names must be non-missing.
#' @param dim. Dimension \code{0} for elements (of atomic or list vectors only),
#'   \code{1} for rows of matrices or tibbles, \code{2} for columns of matrices
#'   or tibbles, or \code{12} for rows and columns of matrices or tibbles.
#' @param u. Logical scalar indicating whether names must be unique.
#' @param blank. Logical scalar indicating whether blank strings (\code{""}) are
#'   allowed as names.
#' @param n. Character scalar element-name.
#' @param v. Character vector of element names.
#' @param r.,c. Character vectors of element, row, or column names,
#'   respectively.
#' @return Character vector or a named/renamed version of \code{x}.
#' @export
names_uj <- function() {help("names_uj", package = "uj")}

#' @describeIn names_uj Name the elements of a vector.
#' @export
namev <- function(x., v.) {
  bank_funs(imvc, x. = x.)
  bank_funs(atm_vec, v. = v., n. = length(x.))
  err_check()
  names(x.) <- v.
  x.
}

#' @describeIn names_uj Name the rows of a matrix or data frame.
#' @export
namer <- function(x., r.) {
  bank_funs(id2D, x. = x.)
  bank_funs(atm_vec, r. = r., n. = NROW(x.))
  err_check()
  rownames(x.) <- r.
  x.
}

#' @describeIn names_uj Name the columns of a matrix or data frame.
#' @export
namec <- function(x., c.) {
  bank_funs(id2D, x. = x.)
  bank_funs(atm_vec, c. = c., n. = NCOL(x.))
  err_check()
  colnames(x.) <- c.
  x.
}

#' @describeIn names_uj Create a 1-element list from \code{x} and name that
#'   element with the value of \code{e}
#' @export
namel <- function(x., n.) {
  bank_funs(atm_scl, n.)
  err_check()
  x. <- list(x.)
  names(x.) <- n.
  x.
}

#' @describeIn names_uj Name the rows and columns of a matrix or data frame.
#' @export
namerc <- function(x., r., c.) {namer(namec(x., c.), r.)}

#' @describeIn names_uj Determine whether an objects elements, rows, and/or
#'   columns are named, accounting for whether names must be unique, whether the
#'   object may contain empty elements, and whether names may be blank strings
#'   ("").
#' @export
named <- function(x., dim. = 0, u. = T, blank. = F) {
  vx. <- ivec(x.) | pop_vls(x.) | (is.array(x.) & length(dim(x.)) == 1)
  vd. <- isIN(dim., c(0, 1, 2, 12))
  v1. <- ANY(isEQ(dim., 0), id1D(x.))
  v2. <- ANY(isIN(dim., c(1, 2, 12)), id2D(x.))
  vu. <- isTF(u.)
  vb. <- isTF(blank.)
  err. <- NULL
  if (!vx.) {err. <- c(err., "\n • [x] must be a vect, populated vlist, matrix, or data.frame.")}
  if (!vd.) {err. <- c(err., "\n • [dim.] must be 0, 1, 2, or 12.")}
  if (!v1.) {err. <- c(err., "\n • [dim.] must be 0 when [x] is a vector, vlist, or 1D array.")}
  if (!v2.) {err. <- c(err., "\n • [dim.] must be 1, 2, or 12 when [x] is a matrix or data.frame.")}
  if (!vu.) {err. <- c(err., "\n • [u.] must be TRUE or FALSE.")}
  if (!vb.) {err. <- c(err., "\n • [blank.] must be TRUE or FALSE.")}
  if (idef(err.)) {stop(err.)}
  en. <- eu. <- eb. <- rn. <- ru. <- rb. <- cn. <- cu. <- cb. <- T               # initialize result scalars
  if (length(x.) == 0) {return(F)}                                               # length 0 is unnamed by default
  if (dim. == 0) {                                                               # if inspecting for element names
    n.  <- names(x.)                                                             # > get element names
    en. <- length(n.) > 0                                                        # > are elements named?
    eu. <- f0(eb. & u., isEQ(n., uv(n.)), F)                                     # > do names meet uniqueness specification?
    eb. <- f0(en. & blank., !any(. == ""), F)                                    # > do names meet blankness specification?
  }
  if (dim. %in% c(1, 12)) {                                                      # if inspecting for row names
    n.  <- rownames(x.)                                                          # > get row names
    rn. <- length(n.) > 0                                                        # > are rows named?
    ru. <- f0(rn. & u., isEQ(n., uv(n.)), F)                                     # > do names meet uniqueness specification?
    rb. <- f0(rn. & blank., !any(n. == ""), F)                                   # > do names meet blankness specification?
  }
  if (dim. %in% c(2, 12)) {                                                      # if inspecting for column names
    n.  <- colnames(x.)                                                          # > get column names
    cn. <- length(n.) > 0                                                        # > are column named?
    cu. <- f0(cn. & u., isEQ(n., uv(n.)), F)                                     # > do names meet uniqueness specification?
    cb. <- f0(cn. & blank., !any(n. == ""), F)                                   # > do names meet blankness specification?
  }
  en. & rn. & cn. & eu. & ru. & cu. & eb. & rb. & cb.                            # whether all requirements are met
}

#' @describeIn names_uj Are elements of \code{x} named?
#' @export
enamed <- function(x., u. = T, blank. = F) {named(x., 0, u., blank.)}

#' @describeIn names_uj Are rows of \code{x} named?
#' @export
rnamed <- function(x., u. = T, blank. = F) {named(x., 1, u., blank.)}

#' @describeIn names_uj Are columns of \code{x} named?
#' @export
cnamed <- function(x., u. = T, blank. = F) {named(x., 2, u., blank.)}

#' @describeIn names_uj Are both rows and columns of \code{x} named?
#' @export
rcnamed <- function(x., u. = T, blank. = F) {named(x., 12, u., blank.)}

#' @describeIn names_uj Are all \code{...} arguments named?
#' @export
named. <- function(..., u. = T, blank. = F) {named(list(...), 0, u., blank.)}

#' @describeIn names_uj Get element, row, and/or column names with optional
#'   restrictions.
#' @export
getnames <- function(x., d. = 0, u. = T, err. = F) {
  vx. <- ipop(x.)
  vd. <- isIN(d., c(0, 1, 2, 12))
  vu. <- isTF(u.)
  ve. <- isTF(err.)
  errs. <- NULL
  if (!vx.) {errs. <- c(errs., "\n • [x.] must be populated.")}
  if (!vd.) {errs. <- c(errs., "\n • [d.] must be 0, 1, 2, or 12.")}
  if (!vu.) {errs. <- c(errs., "\n • [u.] must be TRUE or FALSE.")}
  if (!ve.) {errs. <- c(errs., "\n • [err.] must be TRUE or FALSE.")}
  if (idef(errs.)) {stop(errs.)}
  if (d. == 0) {
    en. <- names(x.)
    if (err. & length(en.) > 0) {stop("\n • elements of [x.] are not named.")}
    if (u. & !is_unique(en.)) {stop("\n • element names of [x.] are not unique.")}
    return(en.)
  }
  if (!id2D(x.)) {stop("\n • [x.] must be a matrix or tibble")}
  if (d. %in% c(1, 12)) {
    rn. <- rownames(x.)
    if (err. & length(rn.) > 0) {stop("\n • row of [x.] are not named.")}
    if (u. & !is_unique(rn.)) {stop("\n • row names of [x.] are not unique.")}
    if (d. == 1) {return(rn.)}
  }
  if (d. %in% c(2, 12)) {
    cn. <- colnames(x.)
    if (err. & length(cn.) > 0) {stop("\n • column of [x.] are not named.")}
    if (u. & !is_unique(cn.)) {stop("\n • column names of [x.] are not unique.")}
    if (d. == 2) {return(cn.)}
  }
  list(r = rn., c = cn.)
}

#' @describeIn names_uj Get element names.
#' @export
en <- function(x., u. = T, err. = F) {getnames(x., 0, u., err.)}

#' @describeIn names_uj Get row names.
#' @export
rn <- function(x., u. = T, err. = F) {getnames(x., 1, u., err.)}

#' @describeIn names_uj Get column names.
#' @export
cn <- function(x., u. = T, err. = F) {getnames(x., 2, u., err.)}

#' @describeIn names_uj Get row and column names (in a 2-element list).
#' @export
rc <- function(x., u. = T, err. = F) {getnames(x., 12, u., err.)}
