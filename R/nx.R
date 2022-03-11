#' @name nx
#' @family meta
#' @title Dedicated counting functions
#' @details \strong{\code{n_is}}
#'   \cr Checks for specific counts, minimum counts, maximum counts, and equal
#'   counts. Returns counts themselves if no checks are specified.
#'   \cr\cr
#'   \strong{\code{n.}, \code{nr}, and \code{nc}}
#'   \cr Get or check counts of elements, rows, and columns, respectively.
#'   \cr\cr
#'   \strong{\code{ns}, \code{nmin}, and \code{nmax}}
#'   \cr Get the number of elements in each argument in \code{...}, the minimum
#'   length of any argument in \code{...}, and the maximum length of any
#'   argument in \code{...}, respectively.
#'   \cr\cr
#'   \strong{\code{nsame}}
#'   \cr Checks all arguments in \code{...} for the same length.
#'   \cr\cr
#'   \strong{\code{nw}, \code{nt}, \code{nf}, \code{nu}, \code{nav}, \code{nna},
#'   and \code{nok}}
#'   \cr Get or check counts of \code{TRUE}, \code{TRUE}, \code{FALSE}, unique
#'   atomic, atomic, \code{NA}, and non-\code{NA} values, respectively.
#'   \cr\cr
#'   \strong{\code{n0}, \code{n1}, \code{n1p}, and \code{n2p}}
#'   \cr Check for counts of \code{0}, \code{1}, \code{≥ 1}, and \code{≥ 2},
#'   respectively.
#'   \cr\cr
#'   \strong{\code{nch}}
#'   \cr Gets or checks number of characters in each element of character
#'   scalars, vectors, matrices, and arrays.
#' @param x A non-negative whole-number object.
#' @param ... One or more arguments to be examined for counts.
#' @param n. Optional non-negative whole-number vector of valid element, row, or
#'   column counts.
#' @param min.,max. Optional non-negative whole-number scalars giving minimum
#'   and maximum valid element, row, or column counts.
#' @param eq.,na.,a. Logical scalars indicating, respectively, whether all
#'   counts must be equal, whether \code{NA} values are allowed, and whether to
#'   atomize \code{...} to create a single atomic vector before processing. If
#'   \code{a} is \code{FALSE}, each argument in \code{...} is processed
#'   separately.
#' @param vals. Optional atomic vector indicating specific values to be counted.
#' @param lt.,le,.ge.,gt. Optional atomic scalars indicating specific values
#'   elements of arguments in \code{...} must be less than, less than or equal
#'   to, greater than or equal to, or greater than, respectively, to be counted.
#' @return An integer or logical scalar or vector.
#' @examples
#' N <- 0:15
#' n_is(N, n. = 0:5)
#' n_is(N, min. = 3, max. = 12)
#' n_is(N, eq. = T)
#' n_is(rep(0, 3), eq. = T)
#' nx(letters, LETTERS, 0:9, NULL)
#' nx(letters, LETTERS, 0:9, vals. = letters)
#' nx(letters, LETTERS, vals. = letters)
#' nx(letters, LETTERS, le. = "M", ge. = "m")
#' nx(letters, LETTERS, lt. = "M", gt. = "m")
#' n0(letters, LETTERS, TRUE, 0:1, 0:2, 0:9)
#' n1(letters, LETTERS, TRUE, 0:1, 0:2, 0:9)
#' n1p(letters, LETTERS, TRUE, 0:1, 0:2, 0:9)
#' n2p(letters, LETTERS, TRUE, 0:1, 0:2, 0:9)
#' nmin(letters, LETTERS, 0:9)
#' nmax(letters, LETTERS, 0:9)
#' nsame(letters, LETTERS, 0:9)
#' nsame(letters, LETTERS)
#' nsame(letters, LETTERS, lt. = "M", gt. = "m")
#' nmin(letters, LETTERS, 0:9, min. = 11)
#' nmax(letters, LETTERS, 0:9, max. = 11)
#' nsame(letters, LETTERS, 0:9, min. = 11, max. = 25)
#' nw(0:99 %in% 50:59, 0:99 %in% 41:49)
#' nw(0:99 %in% 50:59, 0:99 %in% 41:49, n. = 0:9)
#' nw(0:99 %in% 50:59, 0:99 %in% 41:49, min. = 9, max. = 19)
#' nw(0:99 %in% 50:59, 0:99 %in% 41:49, min. = 9, max. = 19, eq. = T)
#' nf(0:99 %in% 50:59, 0:99 %in% 41:49)
#' nf(0:99 %in% 50:59, 0:99 %in% 41:49, n = 0:9)
#' nf(0:99 %in% 50:59, 0:99 %in% 41:49, min. = 9, max. = 19)
#' nf(0:99 %in% 50:59, 0:99 %in% 41:49, min. = 9, max. = 19, eq. = T)
#' nr(tibble(letters, LETTERS), matrix(c(letters, LETTERS), nrow = 2))
#' nc(tibble(letters, LETTERS), matrix(c(letters, LETTERS), nrow = 2))
#' nch(letters, eq. = T)
#' nch(letters, "a string")
#' nch(letters, "a string", a. = T)
n_is <- function(x, n. = NULL, min. = NULL, max. = NULL, eq. = F) {
  VX <- cmp_nnw(x)
  VN <- f0(xnll(n.  ), T, cmp_nnw_vec(n.  ))
  VI <- f0(xnll(min.), T, cmp_nnw_scl(min.))
  VA <- f0(xnll(max.), T, cmp_nnw_scl(max.))
  VE <- isTF(eq.)
  E  <- NULL
  if (!VX) {E <- c(E, "\n  * [x] must contain only non-negative whole numbers.")}
  if (!VN) {E <- c(E, "\n  * [n.] must be NULL or a non-negative whole-number vect.")}
  if (!VI) {E <- c(E, "\n  * [min.] must be NULL or a non-negative whole-number scalar.")}
  if (!VA) {E <- c(E, "\n  * [max.] must be NULL or a non-negative whole-number scalar.")}
  if (!VE) {E <- c(E, "\n  * [eq.] must be TRUE or FALSE.")}
  if (xdef(E)) {stop(E)}
  if (length(c(n., min., max.)) == 0 & !eq.) {return(x)}                         # if no restrictions are specified, return the raw counts
  N <- f0(xnll(n.  ), T, all(x %in% n.))                                         # whether exact argument length restriction is met
  M <- f0(xnll(min.), T, all(x >= min.))                                         # whether min argument length restriction is met
  X <- f0(xnll(max.), T, all(x <= max.))                                         # whether max argument length restriction is met
  E <- f0(eq., length(unique(x)) == 1, T)                                        # whether argument length equality restriction is met
  (N & M & X & E)                                                                # whether all argument length restrictions are met
}

#' @rdname nx
#' @export
nx <- function(..., n. = NULL, min. = NULL, max. = NULL, eq. = F, a. = F, na. = F, vals. = NULL, lt. = NULL, le. = NULL, ge. = NULL, gt. = NULL) {
  x <- list(...); A <- av(x)                                                     # extract ... args and an atomized version
  VDT <- ...length() > 0
  VNN <- f0(xnll(n.  ), T, cmp_nnw_vec(n.  ))
  VMN <- f0(xnll(min.), T, cmp_nnw_scl(min.))
  VMX <- f0(xnll(max.), T, cmp_nnw_scl(max.))
  VMM <- f0(!VMN | !VMX, T, f0(xnll(min.) | xnll(max.), T, max. >= min.))
  VEQ <- isTF(eq.)
  VAA <- isTF(a.)
  VNA <- isTF(na.)
  VXX <- f0(!isF(na.), T, !any(is.na(A)))
  VVL <- f0(xnll(vals.), T, compatible(..., vals., recycle = F))
  VLT <- f0(xnll(lt.), T, comparable(..., lt., recycle = F))
  VLE <- f0(xnll(le.), T, comparable(..., le., recycle = F))
  VGE <- f0(xnll(ge.), T, comparable(..., ge., recycle = F))
  VGT <- f0(xnll(gt.), T, comparable(..., gt., recycle = F))
  E   <- NULL
  if (!VDT) {E <- c(E, "\n  * [...] is empty.")}
  if (!VNN) {E <- c(E, "\n  * [n.] must be NULL or contain only non-negative whole numbers.")}
  if (!VMN) {E <- c(E, "\n  * [min.] must be NULL or a non-negative whole number.")}
  if (!VMX) {E <- c(E, "\n  * [max.] must be NULL or a non-negative whole number.")}
  if (!VMM) {E <- c(E, "\n  * [max.] must be greater than or equal to [min.].")}
  if (!VEQ) {E <- c(E, "\n  * [eq.] must be TRUE or FALSE.")}
  if (!VAA) {E <- c(E, "\n  * [a.] must be TRUE or FALSE.")}
  if (!VNA) {E <- c(E, "\n  * [na.] must be TRUE or FALSE.")}
  if (!VXX) {E <- c(E, "\n  * [na. = FALSE] but arguments in [...] contains NA values.")}
  if (!VVL) {E <- c(E, "\n  * [vals.] must be NULL or compatible with arguments in [...].")}
  if (!VLT) {E <- c(E, "\n  * [lt.] must be NULL or comparable with arguments in [...].")}
  if (!VLE) {E <- c(E, "\n  * [le.] must be NULL or comparable with arguments in [...].")}
  if (!VGE) {E <- c(E, "\n  * [ge.] must be NULL or comparable with arguments in [...].")}
  if (!VGT) {E <- c(E, "\n  * [gt.] must be NULL or comparable with arguments in [...].")}
  if (xdef(E)) {stop(E)}
  if (a.) {x <- list(A)}                                                         # if atomization is specified, replace x with the atomized version in a 1-element list
  for (i in 1:length(x)) {
    X <- x[[i]]
    if (xdef(vals.)) {X <- X[X %in% vals.]}
    if (xdef(lt.)) {X <- X[X <  lt.]}
    if (xdef(le.)) {X <- X[X <= le.]}
    if (xdef(ge.)) {X <- X[X >= ge.]}
    if (xdef(gt.)) {X <- X[X >  gt.]}
    x[[i]] <- X
  }
  x <- lengths(x, F)                                                             # get the lengths of the arguments
  n_is(x, n. = n., min. = min., max. = max., eq. = eq.)                          # and check them against any length restrictions
}

#' @rdname nx
#' @export
ns <- function(..., n. = NULL, min. = NULL, max. = NULL, na. = F, vals. = NULL, lt. = NULL, le. = NULL, ge. = NULL, gt. = NULL) {nx(..., n. = n., min. = min., max. = max., na. = na., a. = F, vals. = vals., lt. = lt., le. = le., ge. = ge., gt. = gt.)}

#' @rdname nx
#' @export
nmin <- function(..., na. = F, vals. = NULL, lt. = NULL, le. = NULL, ge. = NULL, gt. = NULL) {min(nx(..., na. = na., a. = F, vals. = vals., lt. = lt., le. = le., ge. = ge., gt. = gt.))}

#' @rdname nx
#' @export
nmax <- function(..., na. = F, vals. = NULL, lt. = NULL, le. = NULL, ge. = NULL, gt. = NULL) {max(nx(..., na. = na., a. = F, vals. = vals., lt. = lt., le. = le., ge. = ge., gt. = gt.))}

#' @rdname nx
#' @export
nsame <- function(..., min. = NULL, max. = NULL, na. = F, vals. = NULL, lt. = NULL, le. = NULL, ge. = NULL, gt. = NULL) {nx(..., min. = min., max. = max., eq. = T, na. = na., a. = F, vals. = vals., lt. = lt., le. = le., ge. = ge., gt. = gt.)}

#' @rdname nx
#' @export
nw <- function(..., n. = NULL, min. = NULL, max. = NULL, eq. = F, na. = F, a. = T) {
  x <- list(...)
  A <- av(x)
  VDT <- all(sapply(x, xlgc))
  VNN <- f0(xnll(n.  ), T, cmp_nnw_vec(n.  ))
  VMN <- f0(xnll(min.), T, cmp_nnw_scl(min.))
  VMX <- f0(xnll(max.), T, cmp_nnw_scl(max.))
  VEQ <- isTF(eq.)
  VAA <- isTF(a.)
  VNA <- isTF(na.)
  VXX <- f0(!isF(na.), T, !any(is.na(A)))
  E   <- NULL
  if (!VDT) {E <- c(E, "\n  * [x] must contain only non-empty logical objects.")}
  if (!VNN) {E <- c(E, "\n  * [n.] must be NULL or a non-negative whole-number vect.")}
  if (!VMN) {E <- c(E, "\n  * [min.] must be NULL or a non-negative whole-number scalar.")}
  if (!VMX) {E <- c(E, "\n  * [max.] must be NULL or a non-negative whole-number scalar.")}
  if (!VEQ) {E <- c(E, "\n  * [eq.] must be TRUE or FALSE.")}
  if (!VAA) {E <- c(E, "\n  * [a.] must be TRUE or FALSE.")}
  if (!VNA) {E <- c(E, "\n  * [na.] must be TRUE or FALSE.")}
  if (!VXX) {E <- c(E, "\n  * [na. = FALSE] but arguments in [...] contains NA values.")}
  if (xdef(E)) {stop(E)}
  if (isT(a.)) {x <- list(A)}
  N <- sapply(lapply(x, which), length)
  n_is(N, n., min., max., eq.)
}

#' @rdname nx
#' @export
nt <- nw

#' @rdname nx
#' @export
nf <- function(..., n. = NULL, min. = NULL, max. = NULL, eq. = F, na. = F, a. = T) {
  x <- list(...)
  A <- av(x)
  VDT <- all(sapply(x, xlgc))
  VNN <- f0(xnll(n.  ), T, cmp_nnw_vec(n.  ))
  VMN <- f0(xnll(min.), T, cmp_nnw_scl(min.))
  VMX <- f0(xnll(max.), T, cmp_nnw_scl(max.))
  VEQ <- isTF(eq.)
  VAA <- isTF(a.)
  VNA <- isTF(na.)
  VXX <- f0(!isF(na.), T, !any(is.na(A)))
  E   <- NULL
  if (!VDT) {E <- c(E, "\n  * [x] must contain only non-empty logical objects.")}
  if (!VNN) {E <- c(E, "\n  * [n.] must be NULL or a non-negative whole-number vect.")}
  if (!VMN) {E <- c(E, "\n  * [min.] must be NULL or a non-negative whole-number scalar.")}
  if (!VMX) {E <- c(E, "\n  * [max.] must be NULL or a non-negative whole-number scalar.")}
  if (!VEQ) {E <- c(E, "\n  * [eq.] must be TRUE or FALSE.")}
  if (!VAA) {E <- c(E, "\n  * [a.] must be TRUE or FALSE.")}
  if (!VNA) {E <- c(E, "\n  * [na.] must be TRUE or FALSE.")}
  if (!VXX) {E <- c(E, "\n  * [na. = FALSE] but arguments in [...] contains NA values.")}
  if (xdef(E)) {stop(E)}
  if (isT(a.)) {x <- list(A)}
  N <- sapply(lapply(lapply(x, not), which), length)
  n_is(N, n., min., max., eq.)
}

#' @rdname nx
#' @export
nu <- function(..., n. = NULL, min. = NULL, max. = NULL, eq. = F, na. = F, a. = T) {
  x <- list(...)
  A <- av(x)
  VDT <- all(sapply(x, pop_atm))
  VNN <- f0(xnll(n.  ), T, cmp_nnw_vec(n.  ))
  VMN <- f0(xnll(min.), T, cmp_nnw_scl(min.))
  VMX <- f0(xnll(max.), T, cmp_nnw_scl(max.))
  VEQ <- isTF(eq.)
  VAA <- isTF(a.)
  VNA <- isTF(na.)
  VXX <- f0(!isF(na.), T, !any(is.na(A)))
  E   <- NULL
  if (!VDT) {E <- c(E, "\n  * [x] must contain only non-empty atomic objects.")}
  if (!VNN) {E <- c(E, "\n  * [n.] must be NULL or a non-negative whole-number vect.")}
  if (!VMN) {E <- c(E, "\n  * [min.] must be NULL or a non-negative whole-number scalar.")}
  if (!VMX) {E <- c(E, "\n  * [max.] must be NULL or a non-negative whole-number scalar.")}
  if (!VEQ) {E <- c(E, "\n  * [eq.] must be TRUE or FALSE.")}
  if (!VAA) {E <- c(E, "\n  * [a.] must be TRUE or FALSE.")}
  if (!VNA) {E <- c(E, "\n  * [na.] must be TRUE or FALSE.")}
  if (!VXX) {E <- c(E, "\n  * [na. = FALSE] but arguments in [...] contains NA values.")}
  if (xdef(E)) {stop(E)}
  if (isT(a.)) {x <- list(A)}
  N <- sapply(lapply(x, unique), length)
  n_is(N, n., min., max., eq.)
}

#' @rdname nx
#' @export
nr <- function(..., n. = NULL, min. = NULL, max. = NULL, eq. = F) {
  x <- list(...)
  VDT <- all(sapply(x, xd2D))
  VNN <- f0(xnll(n.  ), T, cmp_nnw_vec(n.  ))
  VMN <- f0(xnll(min.), T, cmp_nnw_scl(min.))
  VMX <- f0(xnll(max.), T, cmp_nnw_scl(max.))
  VEQ <- isTF(eq.)
  E   <- NULL
  if (!VDT) {E <- c(E, "\n  * [x] must contain only matrices or data.frames.")}
  if (!VNN) {E <- c(E, "\n  * [n.] must be NULL or a non-negative whole-number vect.")}
  if (!VMN) {E <- c(E, "\n  * [min.] must be NULL or a non-negative whole-number scalar.")}
  if (!VMX) {E <- c(E, "\n  * [max.] must be NULL or a non-negative whole-number scalar.")}
  if (!VEQ) {E <- c(E, "\n  * [eq.] must be TRUE or FALSE.")}
  if (xdef(E)) {stop(E)}
  N <- sapply(x, nrow)
  n_is(N, n., min., max., eq.)
}

#' @rdname nx
#' @export
nc <- function(..., n. = NULL, min. = NULL, max. = NULL, eq. = F) {
  x <- list(...)
  VDT <- all(sapply(x, xd2D))
  VNN <- f0(xnll(n.  ), T, cmp_nnw_vec(n.  ))
  VMN <- f0(xnll(min.), T, cmp_nnw_scl(min.))
  VMX <- f0(xnll(max.), T, cmp_nnw_scl(max.))
  VEQ <- isTF(eq.)
  E   <- NULL
  if (!VDT) {E <- c(E, "\n  * [x] must contain only matrices or data.frames.")}
  if (!VNN) {E <- c(E, "\n  * [n.] must be NULL or a non-negative whole-number vect.")}
  if (!VMN) {E <- c(E, "\n  * [min.] must be NULL or a non-negative whole-number scalar.")}
  if (!VMX) {E <- c(E, "\n  * [max.] must be NULL or a non-negative whole-number scalar.")}
  if (!VEQ) {E <- c(E, "\n  * [eq.] must be TRUE or FALSE.")}
  if (xdef(E)) {stop(E)}
  N <- sapply(x, ncol)
  n_is(N, n., min., max., eq.)
}

#' @rdname nx
#' @export
nch <- function(..., n. = NULL, min. = NULL, max. = NULL, eq. = F, na. = F, a. = T) {
  x <- list(...)
  A <- av(x)
  VDT <- all(sapply(x, xchr))
  VNN <- f0(xnll(n.  ), T, cmp_nnw_vec(n.  ))
  VMN <- f0(xnll(min.), T, cmp_nnw_scl(min.))
  VMX <- f0(xnll(max.), T, cmp_nnw_scl(max.))
  VEQ <- isTF(eq.)
  VAA <- isTF(a.)
  VNA <- isTF(na.)
  VXX <- f0(!isF(na.), T, !any(is.na(A)))
  E   <- NULL
  if (!VDT) {E <- c(E, "\n  * [x] must contain only non-empty atomic objects.")}
  if (!VNN) {E <- c(E, "\n  * [n.] must be NULL or a non-negative whole-number vect.")}
  if (!VMN) {E <- c(E, "\n  * [min.] must be NULL or a non-negative whole-number scalar.")}
  if (!VMX) {E <- c(E, "\n  * [max.] must be NULL or a non-negative whole-number scalar.")}
  if (!VEQ) {E <- c(E, "\n  * [eq.] must be TRUE or FALSE.")}
  if (!VAA) {E <- c(E, "\n  * [a.] must be TRUE or FALSE.")}
  if (!VNA) {E <- c(E, "\n  * [na.] must be TRUE or FALSE.")}
  if (!VXX) {E <- c(E, "\n  * [na = FALSE] but arguments in [...] contains NA values.")}
  if (xdef(E)) {stop(E)}
  if (isT(a.)) {x <- list(A)}
  N <- sapply(lapply(x, ch), length)
  n_is(N, n., min., max., eq.)
}

#' @rdname nx
#' @export
nna <- function(..., n. = NULL, min. = NULL, max. = NULL, eq. = F, a. = T) {
  x <- list(...)
  A <- av(x)
  VDT <- all(sapply(x, xchr))
  VNN <- f0(xnll(n.  ), T, cmp_nnw_vec(n.  ))
  VMN <- f0(xnll(min.), T, cmp_nnw_scl(min.))
  VMX <- f0(xnll(max.), T, cmp_nnw_scl(max.))
  VEQ <- isTF(eq.)
  VAA <- isTF(a.)
  E   <- NULL
  if (!VDT) {E <- c(E, "\n  * [x] must contain only non-empty atomic objects.")}
  if (!VNN) {E <- c(E, "\n  * [n.] must be NULL or a non-negative whole-number vect.")}
  if (!VMN) {E <- c(E, "\n  * [min.] must be NULL or a non-negative whole-number scalar.")}
  if (!VMX) {E <- c(E, "\n  * [max.] must be NULL or a non-negative whole-number scalar.")}
  if (!VEQ) {E <- c(E, "\n  * [eq.] must be TRUE or FALSE.")}
  if (!VAA) {E <- c(E, "\n  * [a.] must be TRUE or FALSE.")}
  if (xdef(E)) {stop(E)}
  if (isT(a.)) {x <- list(A)}
  N <- sapply(lapply(x, is.na), length)
  n_is(N, n., min., max., eq.)
}

#' @rdname nx
#' @export
nok <- function(..., n. = NULL, min. = NULL, max. = NULL, eq. = F, a. = T) {
  x <- list(...)
  A <- av(x)
  VDT <- all(sapply(x, xchr))
  VNN <- f0(xnll(n.  ), T, cmp_nnw_vec(n.  ))
  VMN <- f0(xnll(min.), T, cmp_nnw_scl(min.))
  VMX <- f0(xnll(max.), T, cmp_nnw_scl(max.))
  VEQ <- isTF(eq.)
  VAA <- isTF(a.)
  E   <- NULL
  if (!VDT) {E <- c(E, "\n  * [x] must contain only non-empty atomic objects.")}
  if (!VNN) {E <- c(E, "\n  * [n.] must be NULL or a non-negative whole-number vect.")}
  if (!VMN) {E <- c(E, "\n  * [min.] must be NULL or a non-negative whole-number scalar.")}
  if (!VMX) {E <- c(E, "\n  * [max.] must be NULL or a non-negative whole-number scalar.")}
  if (!VEQ) {E <- c(E, "\n  * [eq.] must be TRUE or FALSE.")}
  if (!VAA) {E <- c(E, "\n  * [a.] must be TRUE or FALSE.")}
  if (xdef(E)) {stop(E)}
  if (isT(a.)) {x <- list(A)}
  N <- sapply(lapply(lapply(x, is.na), not), length)
  n_is(N, n., min., max., eq.)
}

#' @rdname nx
#' @export
nat <- function(..., n. = NULL, min. = NULL, max. = NULL, eq. = F, a. = T) {nx(..., n. = n., min. = min., max. = max., eq. = eq., a. = T)}

#' @rdname nx
#' @export
n0 <- function(..., na. = F, a. = T) {nx(..., n. = 0, na. = na., a. = a.)}

#' @rdname nx
#' @export
n1 <- function(..., na. = F, a. = T) {nx(..., n. = 1, na. = na., a. = a.)}

#' @rdname nx
#' @export
n1p <- function(..., na. = F, eq. = T, a. = T) {nx(..., min. = 1, na. = na., a. = a.)}

#' @rdname nx
#' @export
n2p <- function(..., na. = F, eq. = T, a. = T) {nx(..., min. = 2, na. = na., a. = a.)}
