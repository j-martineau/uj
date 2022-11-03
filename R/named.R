#' @rdname named
#' @family naming
#' @family meta
#' @title Are elements of an object named?
#' @description Determine whether an objects elements, rows, and/or columns are
#'   named, accounting for whether names must be unique, whether the object may
#'   contain empty elements, and whether names may be blank strings ("").
#' @param x. Argument to be inspected for names.
#' @param ... Arbitrary number of arguments to be inspected for names.
#' @param dim. Dimension \code{0} for elements (of atomic or list vectors only),
#'   \code{1} for rows of matrices or tibbles, \code{2} for columns of matrices
#'   or tibbles, or \code{12} for rows and columns of matrices or tibbles.
#' @param u. Logical scalar indicating whether names must be unique.
#' @param blank. Logical scalar indicating whether blank strings (\code{""}) are
#'   allowed as names.
#' @return Logical scalar.
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

#' @describeIn named Are elements of \code{x} named?
#' @export
enamed <- function(x., u. = T, blank. = F) {named(x., 0, u., blank.)}

#' @describeIn named Are rows of \code{x} named?
#' @export
rnamed <- function(x., u. = T, blank. = F) {named(x., 1, u., blank.)}

#' @describeIn named Are columns of \code{x} named?
#' @export
cnamed <- function(x., u. = T, blank. = F) {named(x., 2, u., blank.)}

#' @describeIn named Are both rows and columns of \code{x} named?
#' @export
rcnamed <- function(x., u. = T, blank. = F) {named(x., 12, u., blank.)}

#' @describeIn named Are all \code{...} arguments named?
#' @export
named. <- function(..., u. = T, blank. = F) {named(list(...), 0, u., blank.)}
