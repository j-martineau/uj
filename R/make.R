#' @name make
#' @family meta
#' @title Extended functionality for creating atomic vectors, atomic matrices,
#'   atomic tibbles, and atomic or recursive vlists
#' @description Create an atomic vector.
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
#'   \strong{\code{mkvls(...)}}
#'   \cr Creates a named vlist by taking the original call and parsing arguments
#'   to get element names. Allows for more concise code such as
#'   \code{mkvls(letters, LETTERS)} giving the same result as \code{list(letters
#'   = letters, LETTERS = LETTERS)}.
#'   \cr\cr
#'   \strong{\code{mkdiag(x. = 1, n. = 1)}}
#'   \cr Creates a square, atomic diagonal matrix. For numeric \code{x.},
#'   off-diagonals are \code{0}. For logical \code{x.}, off diagonals are
#'   \code{FALSE}. For character \code{x.}, off diagonals are blank strings
#'   (\code{""}). For all others, off diagonals are \code{NA}.
#' @param ... Objects to placed in an atomic vect, atomic matrix, atomic tibble,
#'   vlist, or square atomic diagonal matrix.
#' @param vn. Optional character vector of names to apply to vector or list
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
#' @param x. An numeric, logical, or character object of length 1 or greater.
#' @param n. A positive whole number indicating the number of replications of
#'   \code{x}.
#' @return An \link[=is_atm_tibble]{atomic tibble}, an
#'   \link[=is_atm_vect]{atomic vect}, an \link[=is_atm_matrix]{atomic matrix}
#'   (\code{mat.}), or a \link[=is_vlist]{vlist.} All others return the value of
#'   function they are thin wrappers for.
#' @export
vec <- function(..., r. = 1, vn. = NULL) {
  x. <- av(...)
  if (length(x.) == 0 | isEQ(r., 0)) {x. <- vector()}
  vr. <- cmp_nnw_scl(r.)
  vvn. <- f0(inll(vn.), T, cmp_chr_vec(vn.))
  err. <- NULL
  if (!vr.) {err. <- c(err., "\n • [r.] must be a non-negative, whole-number scalar.")}
  if (!vvn.) {err. <- c(err., "\n • [en.] must be NULL or a character scalar/vector.")}
  if (idef(err.)) {stop(err.)}
  if (r. > 1) {x. <- rep.int(x., r.)}
  if (idef(vn.)) {
    vn. <- ssP(vn.)
    if (length(x.) != length(vn.)) {stop("\n • [vn.] must be the same length as the resulting vector.")}
    names(x.) <- vn.
  }
  x.
}

#' @describeIn make Create a vector of missing/\code{NA} values.
#' @export
vec_na <- function(r.) {vec(NA, r. = r.)}

#' @describeIn make Create an atomic matrix.
#' @export
mat <- function(..., nr. = 1, nc. = NULL, br. = F, rn. = NULL, cn. = NULL) {
  av. <- av(...)
  if (length(av.) == 0) {x. <- NA} else {x. <- av.}
  vnr. <- f0(inll(nr.), T, cmp_nnw_scl(nr.))
  vnc. <- f0(inll(nc.), T, cmp_nnw_scl(nc.))
  vbr. <- isTF(br.)
  vrn. <- f0(inll(rn.), T, cmp_chr_vec(rn.))
  vcn. <- f0(inll(cn.), T, cmp_chr_vec(cn.))
  err. <- NULL
  if (!vnr.) {err. <- c(err., "\n • [nr.] must be NULL or a non-negative, whole number scalar.")}
  if (!vnc.) {err. <- c(err., "\n • [nc.] must be NULL or a non-negative, whole number scalar.")}
  if (!vbr.) {err. <- c(err., "\n • [br.] must be TRUE or FALSE.")}
  if (idef(err.)) {stop(err.)}
  if      (is.null(nr.) & is.null(nc.)) {nr0. <- 1  ; nc0. <- length(av.)      }
  else if (is.null(nr.)               ) {nc0. <- nc.; nr0. <- length(av.) / nc.}
  else if (               is.null(nc.)) {nr0. <- nr.; nc0. <- length(av.) / nr.}
  rn. <- ssP(rn.)
  cn. <- ssP(cn.)
  vrc. <- round(nr0.) == nr0. & round(nc0.) == nc0. & nr0. * nc0. == length(av.)
  vrn. <- f0(inll(rn.), T, f0(!vrc., T, length(rn.) == nr0.))
  vcn. <- f0(inll(cn.), T, f0(!vrc., T, length(cn.) == nc0.))
  if (vrn. & vcn.) {
    nt. <- nr. * nc.
    if (nt. > 0) {vrp. <- (nt. / length(x.)) == round(nt. / length(x.))}
    else {vrp. <- length(x.) == 0}
  }
  else {vrp. <- T}
  if (!vrc.) {
    if      (inll(nr.)) {err. <- c(err., "\n • [nc.] is not a divisor of length(av(...))." )}
    else if (inll(nc.)) {err. <- c(err., "\n • [nr.] is not a divisor of length(av(...))." )}
    else                {err. <- c(err., "\n • [nr. * nc.] does not equal length(av(...)).")}
  }
  if (!vrn.) {err. <- c(err., "\n • [rn.] must be NULL or a complete vect of length equal to the number of resulting rows."   )}
  if (!vcn.) {err. <- c(err., "\n • [cn.] must be NULL or a complete vect of length equal to the number of resulting columns.")}
  if (idef(err.)) {stop(err.)}
  x. <- matrix(av., nrow = nr., ncol = nc., byrow = br.)
  rownames(x.) <- rn.
  colnames(x.) <- cn.
}

#' @describeIn make Create an atomic tibble with zero rows.
#' @export
atb0 <- function(cn.) {
  if (!cmp_chr_vec(cn.)) {stop("\n • [cn.] must be a non-NA character scalar or vector.")}
  run("tibble::tibble(", daw(ss(cn.), " = NA", w. = ", "), ", .rows = 0)")
}

#' @describeIn make Create an atomic tibble containing only missing values.
#' @export
atb_na <- function(cn., nr.) {
  vc. <- cmp_chr_vec(cn.)
  vn. <- cmp_psw_scl(nr.)
  err. <- NULL
  if (!vc.) {err. <- c(err., "\n • [cn.] must be a non-NA character scalar or vector.")}
  if (!vn.) {err. <- c(err., "\n • [nr.] must be NULL or a positive, whole number scalar.")}
  if (!inll(err.)) {stop(err.)}
  val. <- rep.int(NA, nr.)
  run("tibble::tibble(", daw(ss(cn.), " = val.", w. = ", "), ")")
}

#' @describeIn make Create an atomic tibble from named arguments or from column
#'   names in \code{cn.}.
#' @export
atb <- function(..., cn. = NULL) {
  x. <- list(...)
  cn. <- f0(cmp_chr_vec(cn.), ss(cn.), cn.)
  n. <- length(x.)
  ns. <- lengths(x.)
  mxn. <- max(ns.)
  v0. <- ...length() > 0
  vns. <- f0(!v0., T, all(lengths(x.) %in% ns.))
  vxv. <- f0(!v0. | !vns., T, all(sapply(x., xvec)))
  vxr. <- f0(n. == 0, F, f0(any(ns. == 0), F, all(mxn. / ns. == round(mxn. / ns.))))
  vcn. <- f0(inll(cn.), T, f0(!cmp_chr_vec(cn.), F, length(cn.) == n.))
  err. <- NULL
  if (!v0.) {err. <- c(err., "\n • [...] is empty.")}
  if (!vns.) {err. <- c(err., "\n • [...] contains an empty argument.")}
  if (!vxv.) {err. <- c(err., "\n • Arguments in [...] must be atomic vects.")}
  if (!vxr.) {err. <- c(err., "\n • Arguments in [...] are not recyclable .")}
  if (!vcn.) {err. <- c(err., "\n • [cn.] must be NULL or match the number of arguments in [...].")}
  if (idef(err.)) {stop(err.)}
  x. <- tibble::tibble(...)
  if (idef(cn.)) {colnames(x.) <- cn.}
  x.
}

#' @describeIn make Create a named vlist from named arguments or from unnamed
#'   arguments and element names provided in \code{en.}.
#' @export
vls <- function(..., en. = NULL) {
  x. <- list(...)
  en. <- f0(cmp_chr_vec(en.), ss(en.), en.)
  n. <- length(x.)
  v0. <- ...length() > 0
  vn. <- f0(inll(en.), T, f0(!cmp_chr_vec(en.), F, length(en.) == n.))
  err. <- NULL
  if (!v0.) {err. <- c(err., "\n • [...] is empty.")}
  if (!vn.) {err. <- c(err., "\n • [en.] must be NULL or match the number of arguments in [...].")}
  if (idef(err.)) {stop(err.)}
  if (idef(en.)) {names(x.) <- en.}
  x.
}

#' @describeIn make Make a list where element names are derived from the
#'   original function call. For example, \code{mkls(letters, LETTERS)} results
#'   in the same output as \code{list(letters = letters, LETTERS = LETTERS)}
#' @export
mkls <- function(...) {
  x. <- as_chr(match.call())                                                     # get a function call object and convert to character
  x. <- x.[2:nx(x.)]                                                             # remove the function call leaving the variables as named in the calling function
  x. <- dw0("list(", glst(peq(x., x.)), ")")                                     # create the call [list(<var1> = <var1>, <var2> = <var2>, ...)]
  eval.parent(parse(text = x., n = 1))                                           # and evaluate it in the environment of the calling function
}

#' @describeIn make Create a diagonal matrix.
#' @export
mkdiag <- function(x. = 1, n. = 1) {
  vx. <- xnum(x.) | xlgl(x.) | xchr(x.)
  vn. <- cmp_psw_scl(n.)
  vns. <- f0(length(x.) > 1, T, f0(vn., T, n. > 1))
  err. <- NULL
  if (!vx.) {err. <- c(err., "\n • [x.] must be an numeric, logical, or character.")}
  if (!vn.) {err. <- c(err., "\n • [n.] must a positive, whole-number scalar.")}
  if (!vns.) {err. <- c(err., "\n • Neither [length(x.)] nor [n.] is greater than 1.")}
  if (idef(err.)) {stop(err.)}
  nr. <- nrow(x.); nc. <- ncol(x.)
  if (is.matrix(x.)) {if (nr. > 0 & nr. == nc.) {x. <- diag(x., nr., nc.)}}
  x. <- av(x.)
  if (n. > 1) {x. <- rep(x., n.)}
  if (all(is.na(x.))) {x.[is.na(x.)] <- NA_real_}
  l. <- length(x.)
  blank. <- f0(xnum(x.), 0, f0(xlgl(x.), F, f0(xchr(x.), "", NA)))
  out. <- matrix(blank., nrow = l., ncol = l.)
  diag(out.) <- x.
  out.
}
