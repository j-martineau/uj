#' @name make
#' @family meta
#' @title Make atomic vectors, atomic matrices, atomic tibbles, and atomic or
#'   recursive vlists
#' @description Extended functionality for creating vectors, matrices,
#'   tibbles and vlists.
#' @details \strong{\code{vec(..., r, en.)}}
#'   \cr Creates an atomic vector from the atomic objects in \code{...}
#'   collapsed into a vector, replicated \code{r.} times, with optional element
#'   names.
#'   \cr\cr
#'   \strong{\code{vec_na(r.)}}
#'   \cr Creates a vector of \code{r.} \code{NA} values.
#'   \cr\cr
#'   \strong{\code{mat(..., nr., nc., br., rn., cn.)}}
#'   \cr Creates an atomic matrix from the atomic elements in \code{...} with
#'   \code{r.} rows, \code{c.} columns, optionally filling by row, with optional
#'   row and column names.
#'   \cr\cr
#'   \strong{\code{atb0(cn.)}}
#'   \cr Creates an \link[=is_atm_tibble]{atomic tibble} with 0 rows and columns
#'   with names from \code{cn.}, which may be pipe delimited.
#'   \cr\cr
#'   \strong{\code{atb_na(cn., nr.)}}
#'   \cr Creates an \link[=is_atm_tibble]{atomic tibble} with \code{nr.} rows
#'   and columns with names from \code{cn.} (which may be pipe delimited). All
#'   cells of the resulting tibble are populated with \code{NA}.
#'   \cr\cr
#'   \strong{\code{atb(..., rn., cn.)}}
#'   \cr Creates an \link[=is_atm_tibble]{atomic tibble} from the atomic vect
#'   arguments in \code{...} with optional row names, plus optional column names
#'   to replace any names of arguments in \code{...}.
#'   \cr\cr
#'   \strong{\code{vls(..., en. = NULL)}}
#'   \cr Creates a \link[=is_vlist]{vlist} from the arguments in \code{...} with
#'   optional element names to replace any names of arguments in \code{...}.
#'   \cr\cr
#'   \strong{\code{mk_diag(x = 1, n = 1)}}
#'   \cr Creates a square, atomic diagonal matrix. For numeric \code{x},
#'   off-diagonals are \code{0}. For logical \code{x}, off diagonals are
#'   \code{FALSE}. For character \code{x}, off diagonals are blank strings
#'   (\code{""}). For all others, off diagonals are \code{NA}.
#' @param ... Objects to placed in an atomic vect, atomic matrix, atomib tibble,
#'   vlist, or square atomic diagonal matrix.
#' @param en. Optional character vector of names to apply to vector or list
#'   elements. May be pipe-delimited in character vector.
#' @param cn. Optional character vector of names to apply to columns of a matrix
#'   or tibble. May be pipe-delimited in character vector. For tibbles, this
#'   argument can be used only if the arguments in \code{...} are not named.
#' @param rn. Optional character vector of names to apply to rows of a matrix or
#'   tibble. May be pipe-delimited in character vector.
#' @param r. Number of replications.
#' @param nr. Number of rows.
#' @param nc. Number of columns.
#' @param br. Whether to fill matrices by row.
#' @param x An numeric, logical, or character object of length 1 or greater.
#' @param n A positive whole number indicating the number of replications of
#'   \code{x}.
#' @return An \link[=is_atm_tibble]{atomic tibble}, an
#'   \link[=is_atm_vect]{atomic vect}, an \link[=is_atm_matrix]{atomic matrix}
#'   (\code{mat.}), or a \link[=is_vlist]{vlist.} All others return the value of
#'   function they are thin wrappers for.
#' @export
vec <- function(..., r. = 1, en. = NULL) {
  x <- av(...)
  if (length(x) == 0 | isEQ(r., 0)) {x <- vector()}
  VR <- cmp_nnw_scl(r.)
  VN <- f0(xnll(en.), T, cmp_chr_vec(en.))
  E <- NULL
  if (!VR) {E <- c(E, "\n  * [r.] must be a non-negative, whole-number scalar.")}
  if (!VN) {E <- c(E, "\n  * [en.] must be NULL or a character scalar/vector.")}
  if (xdef(E)) {stop(E)}
  if (r. > 1) {x <- rep.int(x, r.)}
  if (xdef(en.)) {
    en. <- ssP(en.)
    if (length(x) != length(en.)) {stop("\n  * [en.] must be the same length as the resulting vector.")}
    names(x) <- en.
  }
  x
}

#' @rdname make
#' @export
vec_na <- function(r.) {vec(NA, r. = r.)}

#' @rdname make
#' @export
mat <- function(..., nr. = 1, nc. = NULL, br. = F, rn. = NULL, cn. = NULL) {
  x  <- av(...)
  if (length(x) == 0) {X <- NA} else {X <- x}
  VNR <- f0(xnll(nr.), T, cmp_nnw_scl(nr.))
  VNC <- f0(xnll(nc.), T, cmp_nnw_scl(nc.))
  VBR <- isTF(br.)
  VRN <- f0(xnll(rn.), T, cmp_chr_vec(rn.))
  VCN <- f0(xnll(cn.), T, cmp_chr_vec(cn.))
  E <- NULL
  if (!VNR) {E <- c(E, "\n  * [nr.] must be NULL or a non-negative, whole number scalar.")}
  if (!VNC) {E <- c(E, "\n  * [nc.] must be NULL or a non-negative, whole number scalar.")}
  if (!VBR) {E <- c(E, "\n  * [br.] must be TRUE or FALSE.")}
  if (xdef(E)) {stop(E)}
  if      (is.null(nr.) & is.null(nc.)) {NR <- 1  ; NC <- length(x)      }
  else if (is.null(nr.)               ) {NC <- nc.; NR <- length(x) / nc.}
  else if (               is.null(nc.)) {NR <- nr.; NC <- length(x) / nr.}
  rn. <- ssP(rn.)
  cn. <- ssP(cn.)
  VRC <- round(NR) == NR & round(NC) == NC & NR * NC == length(x)
  VRN <- f0(xnll(rn.), T, f0(!VRC, T, length(rn.) == NR))
  VCN <- f0(xnll(cn.), T, f0(!VRC, T, length(cn.) == NC))
  if (VRN & VCN) {
    NT <- nr. * nc.
    if (NT > 0) {VRP <- (NT / length(X)) == round(NT / length(X))}
    else {VRP <- length(x) == 0}
  }
  else {VRP <- T}
  if (!VRC) {
    if      (xnll(nr.)) {E <- c(E, "\n  * [nc.] is not a divisor of length(av(...))." )}
    else if (xnll(nc.)) {E <- c(E, "\n  * [nr.] is not a divisor of length(av(...))." )}
    else                {E <- c(E, "\n  * [nr. * nc.] does not equal length(av(...)).")}
  }
  if (!VRN) {E <- c(E, "\n  * [rn.] must be NULL or a complete vect of length equal to the number of resulting rows."   )}
  if (!VCN) {E <- c(E, "\n  * [cn.] must be NULL or a complete vect of length equal to the number of resulting columns.")}
  if (xdef(E)) {stop(E)}
  matrix(x, nrow = nr., ncol = nc., byrow = br.)
  rownames(x) <- rn.
  colnames(x) <- cn.
}

#' @rdname make
#' @export
atb0 <- function(cn.) {
  if (!cmp_chr_vec(cn.)) {stop("\n  * [cn.] must be a non-NA character scalar or vector.")}
  run("tibble::tibble(", daw(ss(cn.), " = NA", w. = ", "), ", .rows = 0)")
}

#' @rdname make
#' @export
atb_na <- function(cn., nr.) {
  VCN <- cmp_chr_vec(cn.)
  VNR <- cmp_psw_scl(nr.)
  E   <- NULL
  if (!VCN) {E <- c(E, "\n  * [cn.] must be a non-NA character scalar or vector.")}
  if (!VNR) {E <- c(E, "\n  * [nr.] must be NULL or a positive, whole number scalar.")}
  if (!xnll(E)) {stop(E)}
  Val <- rep.int(NA, nr.)
  run("tibble::tibble(", daw(ss(cn.), " = Val", w. = ", "), ")")
}

#' @rdname make
#' @export
atb <- function(..., rn. = NULL, cn. = NULL) {
  x <- list(...)
  rn. <- f0(cmp_chr_vec(rn.), ss(rn.), rn.)
  cn. <- f0(cmp_chr_vec(cn.), ss(cn.), cn.)
  N <- length(x)
  L <- lengths(x)
  M <- max(L)
  VXN <- ...length() > 0
  VXL <- f0(!VXN, T, all(lengths(x)))
  VXV <- f0(!VXN | !VXL, T, all(sapply(x, xvec)))
  VXR <- f0(N == 0, F, f0(any(L == 0), F, all(M / L == round(M / L))))
  VRN <- f0(xnll(rn.), T, f0(!cmp_chr_vec(rn.), F, length(rn.) == M))
  VCN <- f0(xnll(cn.), T, f0(!cmp_chr_vec(cn.), F, length(cn.) == N))
  E <- NULL
  if (!VXN) {E <- c(E, "\n  * [...] is empty.")}
  if (!VXL) {E <- c(E, "\n  * [...] contains an empty argument.")}
  if (!VXV) {E <- c(E, "\n  * Arguments in [...] must be atomic vects.")}
  if (!VXR) {E <- c(E, "\n  * Arguments in [...] are not recyclable .")}
  if (!VRN) {E <- c(E, "\n  * [rn.] must be NULL or match the number of rows produced.")}
  if (!VCN) {E <- c(E, "\n  * [cn.] must be NULL or match the number of arguments in [...].")}
  if (xdef(E)) {stop(E)}
  x <- tibble::tibble(...)
  if (xdef(rn.)) {rownames(x) <- rn.}
  if (xdef(cn.)) {colnames(x) <- cn.}
}

#' @rdname make
#' @export
vls <- function(..., en. = NULL) {
  x <- list(...)
  en. <- f0(cmp_chr_vec(en.), ss(en.), en.)
  N <- length(x)
  VXN <- ...length() > 0
  VNN <- f0(xnll(en.), T, f0(!cmp_chr_vec(en.), F, length(en.) == N))
  E <- NULL
  if (!VXN) {E <- c(E, "\n  * [...] is empty.")}
  if (!VNN) {E <- c(E, "\n  * [en.] must be NULL or match the number of arguments in [...].")}
  if (xdef(E)) {stop(E)}
  if (xdef(en.)) {names(x) <- en.}
  x
}

#' @rdname make
#' @export
mk_diag <- function(x = 1, n = 1) {
  VX <- xnum(x) | xlgc(x) | xchr(x)
  VN <- cmp_psw_scl(n)
  VL <- f0(length(x) > 1, T, f0(VN, T, n > 1))
  E <- NULL
  if (!VX) {E <- c(E, "\n  * [x] must be an numeric, logical, or character.")}
  if (!VN) {E <- c(E, "\n  * [n] must a positive, whole-number scalar.")}
  if (!VL) {E <- c(E, "\n  * Neither [length(x)] nor [n] is greater than 1.")}
  if (xdef(E)) {stop(E)}
  NR <- nrow(x); NC <- ncol(x)
  if (is.matrix(x)) {if (NR > 0 & NR == NC) {x <- diag(x, NR, NC)}}
  x <- av(x)
  if (n > 1) {x <- rep(x, n)}
  if (all(is.na(x))) {x[is.na(x)] <- NA_real_}
  N <- length(x)
  Blank <- f0(xnum(x), 0, f0(xlgc(x), F, f0(xchr(x), "", NA)))
  R <- matrix(Blank, nrow = N, ncol = N)
  diag(R) <- x
  return(R)
}
