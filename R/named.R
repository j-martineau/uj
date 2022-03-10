#' @rdname named
#' @family naming
#' @family meta
#' @title Are elements of an object named?
#' @details \strong{\code{named}}
#'   \cr Determine whether an objects elements, rows, and/or columns are named,
#'   accounting for whether names must be unique, whether the object may contain
#'   empty elements, and whether names may be blank strings ("").
#'   \cr\cr
#'   \strong{\code{rnamed,cnamed,rcnamed}}
#'   \cr Convenience functions to specify rows, columns, or both.
#'   \cr\cr
#'   \strong{\code{named.}}
#'   \cr Determine whether all arguments in \code{...} are named.
#' @param x Argument to be inspected for names.
#' @param ... Arbitrary number of arguments to be inspected for names.
#' @param dim. Dimension \code{0} for elements (of atomic or list vectors only),
#'   \code{1} for rows of matrices or tibbles, \code{2} for columns of matrices
#'   or tibbles, or \code{12} for rows and columns of matrices or tibbles.
#' @param u. Logical scalar indicating whether names must be unique.
#' @param blank. Logical scalar indicating whether blank strings (\code{""}) are
#'   allowed as names.
#' @return Logical scalar.
#' @export
named <- function(x, dim. = 0, u. = T, blank. = F) {
  VX <- xvec(x) | is_pop_vlist(x) | (is.array(x) & length(dim(x)) == 1)
  VD <- isIN(dim., c(0, 1, 2, 12))
  V1 <- ANY(isEQ(dim., 0), xd1D(x))
  V2 <- ANY(isIN(dim., c(1, 2, 12)), xd2D(x))
  VU <- isTF(u.)
  VB <- isTF(blank.)
  E  <- NULL
  if (!VX) {E <- c(E, "\n  * [x] must be a vect, populated vlist, matrix, or data.frame.")}
  if (!VD) {E <- c(E, "\n  * [dim.] must be 0, 1, 2, or 12.")}
  if (!V1) {E <- c(E, "\n  * [dim.] must be 0 when [x] is a vector, vlist, or 1D array.")}
  if (!V2) {E <- c(E, "\n  * [dim.] must be 1, 2, or 12 when [x] is a matrix or data.frame.")}
  if (!VU) {E <- c(E, "\n  * [u.] must be TRUE or FALSE.")}
  if (!VB) {E <- c(E, "\n  * [blank.] must be TRUE or FALSE.")}
  if (xdef(E)) {stop(E)}
  EN <- EU <- EB <- RN <- RU <- RB <- CN <- CU <- CB <- T                        # initialize result scalars
  if (length(x) == 0) {return(F)}                                                # length 0 is unnamed by default
  if (dim. == 0) {                                                               # if inspecting for element names
    N  <- names(x)                                                               # > get element names
    EN <- length(N) > 0                                                          # > are elements named?
    EU <- f0(EN & u., isEQ(N, uv(N)), F)                                         # > do names meet uniqueness specification?
    EB <- f0(EN & blank., !any(N == ""), F)                                      # > do names meet blankness specification?
  }
  if (dim. %in% c(1, 12)) {                                                      # if inspecting for row names
    N  <- rownames(x)                                                            # > get row names
    RN <- length(N) > 0                                                          # > are rows named?
    RU <- f0(RN & u., isEQ(N, uv(N)), F)                                         # > do names meet uniqueness specification?
    RB <- f0(RN & blank., !any(N == ""), F)                                      # > do names meet blankness specification?
  }
  if (dim. %in% c(2, 12)) {                                                      # if inspecting for column names
    N  <- colnames(x)                                                            # > get column names
    CN <- length(N) > 0                                                          # > are column named?
    CU <- f0(CN & u., isEQ(N, uv(N)), F)                                         # > do names meet uniqueness specification?
    CB <- f0(CN & blank., !any(N == ""), F)                                      # > do names meet blankness specification?
  }
  EN & RN & CN & EU & RU & CU & EB & RB & CB                                     # whether all requirements are met
}

#' @rdname named
#' @export
enamed <- function(x, u. = T, blank. = F) {named(x, 0, u., blank.)}

#' @rdname named
#' @export
rnamed <- function(x, u. = T, blank. = F) {named(x, 1, u., blank.)}

#' @rdname named
#' @export
cnamed <- function(x, u. = T, blank. = F) {named(x, 2, u., blank.)}

#' @rdname named
#' @export
rcnamed <- function(x, u. = T, blank. = F) {named(x, 12, u., blank.)}

#' @rdname named
#' @export
named. <- function(..., u. = T, blank. = F) {named(list(...), 0, u., blank.)}
