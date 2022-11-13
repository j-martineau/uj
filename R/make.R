#' @name make.
#' @family extensions
#' @title Extended object creation functionality
#' @param ... Objects to placed in an atomic vect, atomic matrix, atomic dtf,
#'   vlist, or square atomic diagonal matrix.
#' @param vn. Optional \link[cmp_chr_vec]{complete character vec} of names to
#'   apply to vector or list elements. May be pipe-delimited.
#' @param cn,cn. Optional \link[cmp_chr_vec]{complete character vec} of names to
#'   apply to columns of a matrix or dtf May be pipe-delimited. For tibbles,
#'   this argument can be used only if the arguments in \code{...} are not
#'   named.
#' @param rn. Optional \link[cmp_chr_vec]{complete character vec} of names to
#'   apply to rows of a matrix or dtf May be pipe-delimited.
#' @param r,r. \link[cmp_psw_scl]{Complete positive whole-number scalar} giving
#'   number of replications.
#' @param nr,nr. \link[cmp_psw_scl]{Complete positive whole-number scalar} giving
#'   number of rows.
#' @param nc,nc. \link[cmp_psw_scl]{Complete positive whole-number scalar}
#'   giving number of columns.
#' @param br. \link[cmp_lgl_scl]{Complete logical scalar} indicating whether to
#'   fill matrices by row.
#' @param x A numeric, logical, or character object of length 1 or greater.
#' @return An \link[=idtf]{atomic dtf}, an \link[ivec]{atomic vect}, an
#'   \link[imat]{atomic matrix}, or a \link[is_vls]{atomic vlist}. All others
#'   return the value of the function they are thin wrappers for.
#' @export
make. <- function() {help("make.", package = "uj")}

#' @describeIn make. Creates an atomic vec from the atomic objects in \code{...}
#'   collapsed into a vector, replicated \code{r.} times, with optional element
#'   names.
#' @export
vec <- function(..., r. = 1, vn. = NULL) {
  x <- av(...)
  if (length(x) == 0 | isEQ(r., 0)) {x <- vector()}
  vr <- cmp_nnw_scl(r.)
  vn <- f0(inll(vn.), T, cmp_chr_vec(vn.))
  err <- NULL
  if (!vr) {err <- c(err, "\n • [r.] must be a non-negative whole-number scalar (?cmp_nnw_scl).")}
  if (!vn) {err <- c(err, "\n • [vn.] must be NULL or a complete character vec (?cmp_chr_vec).")}
  if (idef(err)) {stop(err)}
  if (r. > 1) {x <- rep.int(x, r.)}
  if (idef(vn.)) {
    vn. <- ssP(vn.)
    if (length(x) != length(vn.)) {stop("\n • [vn.] must be the same length as the resulting vector.")}
    names(x) <- vn.
  }
  x
}

#' @describeIn make. Creates a named atomic vec by taking the original call
#'   and parsing arguments to get element names. Allows for more concise code
#'   such as \code{vec.(a, b)} giving the same result as \code{list(a = a, b =
#'   b)}.
#' @export
vec. <- function(...) {
  x <- as_chr(match.call())                                                      # get a function call object and convert to character
  x <- x[2:nx(x)]                                                                # remove the function call leaving the variables as named in the calling function
  x <- paste(x, "=", x)
  x <- paste0(x, collapse = ", ")
  x <- paste0("c(", x, ")")
  eval.parent(parse(text = x, n = 1))                                            # and evaluate it in the environment of the calling function
}

#' @describeIn make. Creates a vector of \code{r.} \code{NA} values.
#' @export
vec_na <- function(r.) {vec(NA, r. = r.)}

#' @describeIn make. Creates an atomic matrix from the atomic elements in
#'   \code{...} with \code{r.} rows, \code{c.} columns, optionally filling by
#'   row, with optional row and column names.
#' @export
mat <- function(..., nr. = 1, nc. = NULL, br. = F, rn. = NULL, cn. = NULL) {
  av <- av(...)
  if (length(av) == 0) {x <- NA} else {x <- av}
  vnr <- f0(inll(nr.), T, cmp_nnw_scl(nr.))
  vnc <- f0(inll(nc.), T, cmp_nnw_scl(nc.))
  vbr <- isTF(br.)
  vrn <- f0(inll(rn.), T, cmp_chr_vec(rn.))
  vcn <- f0(inll(cn.), T, cmp_chr_vec(cn.))
  err <- NULL
  if (!vnr) {err <- c(err, "\n • [nr.] must be NULL or a non-negative whole number scalar (?cmp_nnw_scl).")}
  if (!vnc) {err <- c(err, "\n • [nc.] must be NULL or a non-negative whole number scalar (?cmp_nnw_scl).")}
  if (!vbr) {err <- c(err, "\n • [br.] must be TRUE or FALSE.")}
  if (idef(err)) {stop(err)}
  if  (is.null(nr.) & is.null(nc.)) {nr0 <- 1; nc0 <- length(av)}
  else if (is.null(nr.)) {nc0 <- nc.; nr0 <- length(av) / nc.}
  else if (is.null(nc.)) {nr0 <- nr.; nc0 <- length(av) / nr.}
  rn. <- av(strsplit(rn., "|", fixed = T))
  cn. <- av(strsplit(cn., "|", fixed = T))
  vrc <- round(nr0) == nr0 & round(nc0) == nc0 & nr0 * nc0 == length(av)
  vrn <- f0(inll(rn.), T, f0(!vrc, T, length(rn.) == nr0))
  vcn <- f0(inll(cn.), T, f0(!vrc, T, length(cn.) == nc0))
  if (vrn & vcn) {
    nt <- nr. * nc.
    if (nt > 0) {vrp <- (nt / length(x)) == round(nt / length(x))}
    else {vrp <- length(x) == 0}
  }
  else {vrp <- T}
  if (!vrc) {
    if (inll(nr.)) {err <- c(err, "\n • [nc.] is not a divisor of length(av(...))." )}
    else if (inll(nc.)) {err <- c(err, "\n • [nr.] is not a divisor of length(av(...))." )}
    else {err <- c(err, "\n • [nr. * nc.] does not equal length(av(...)).")}
  }
  if (!vrn) {err <- c(err, "\n • [rn.] must be NULL or a complete vect of length equal to the number of resulting rows.")}
  if (!vcn) {err <- c(err, "\n • [cn.] must be NULL or a complete vect of length equal to the number of resulting columns.")}
  if (idef(err)) {stop(err)}
  x <- matrix(av, nrow = nr., ncol = nc., byrow = br.)
  rownames(x) <- rn.
  colnames(x) <- cn.
  x
}

#' @describeIn make. Creates an  with 0 rows and columns
#'   with names from \code{cn.}, which may be pipe delimited.
#' @export
dtf0 <- function(cn) {
  if (!cmp_chr_vec(cn)) {stop("\n • [cn] must be a complete character vector (?cmp_chr_vec).")}
  cn <- av(strsplit(cn, "|", fixed = T))
  cmd <- paste0("tibble::tibble(", paste0(paste0(cn, " = NA"), collapse = ", "), ", .rows = 0")
  run(cmd)
}

#' @describeIn make. Creates an \link[idtf]{atomic dtf} with \code{nr.} rows and
#'   columns with names from \code{cn.} (which may be pipe delimited). All cells
#'   of the resulting tibble are populated with \code{NA}. values.
#' @export
dtf_na <- function(cn, nr) {
  vc <- cmp_chr_vec(cn)
  vn <- cmp_psw_scl(nr)
  err <- NULL
  if (!vc) {err <- c(err, "\n • [cn] must be a complete character vector (?cmp_chr_vec).")}
  if (!vn) {err <- c(err, "\n • [nr] must be NULL or a positive whole number scalar (?cmp_psw_scl).")}
  if (!inll(err)) {stop(err)}
  val <- rep.int(NA, nr)
  cn <- av(strsplit(cn, "|", fixed = T))
  cmd <- paste0("tibble::tibble(", paste0(paste0(cn, " = val"), collapse = ", "), ")")
  run(cmd)
}

#' @describeIn make. Creates an \link[idtf]{atomic dtf} from the
#'   \link[ivec]{atomic vec} arguments in \code{...} with optional row names,
#'   plus optional column names to replace any names of arguments in \code{...}.
#' @export
dtf <- function(..., cn. = NULL) {
  x <- list(...)
  cn. <- f0(cmp_chr_vec(cn.), ss(cn.), cn.)
  n <- length(x)
  ns <- lengths(x)
  mxn <- max(ns)
  v0 <- ...length() > 0
  vns <- f0(!v0, T, all(lengths(x.) %in% ns))
  vxv <- f0(!v0 | !vns, T, all(sapply(x., ivec)))
  vxr <- f0(n == 0, F, f0(any(n == 0), F, all(mxn / ns == round(mxn / ns))))
  vcn <- f0(inll(cn.), T, f0(!cmp_chr_vec(cn.), F, length(cn.) == n))
  err <- NULL
  if (!v0) {err <- c(err, "\n • [...] is empty.")}
  if (!vns) {err <- c(err, "\n • [...] contains an empty argument.")}
  if (!vxv) {err <- c(err, "\n • Arguments in [...] must be atomic vecs (?ivec).")}
  if (!vxr) {err <- c(err, "\n • Arguments in [...] are not recyclable .")}
  if (!vcn) {err <- c(err, "\n • [cn.] must be NULL or match the number of arguments in [...].")}
  if (idef(err)) {stop(err)}
  x <- tibble::tibble(...)
  if (idef(cn.)) {colnames(x) <- cn.}
  x
}

#' @describeIn make. Creates an atomic \link[ivls]{vlist} from the arguments in
#'   \code{...} with optional element names to replace any names of arguments in
#'   \code{...}.
#' @export
vls <- function(..., en. = NULL) {
  x <- list(...)
  en. <- f0(cmp_chr_vec(en.), av(strsplit(en., "|", fixed = T)), en.)
  n <- length(x)
  v0 <- ...length() > 0
  vn <- f0(inll(en.), T, f0(!cmp_chr_vec(en.), F, length(en.) == n))
  err <- NULL
  if (!v0) {err <- c(err, "\n • [...] is empty.")}
  if (!vn) {err <- c(err, "\n • [en.] must be NULL or match the number of arguments in [...].")}
  if (idef(err)) {stop(err)}
  if (idef(en.)) {names(x) <- en.}
  x
}

#' @describeIn make. Creates a named \link[ivls]{atomic vlist} by taking the
#'   original call and parsing arguments to get element names. Allows for more
#'   concise code such as \code{mkvls(letters, LETTERS)} giving the same result
#'   as \code{list(letters = letters, LETTERS = LETTERS)}.
#' @export
vls. <- function(...) {
  x <- as_chr(match.call())                                                      # get a function call object and convert to character
  x <- x[2:nx(x)]                                                                # remove the function call leaving the variables as named in the calling function
  x <- paste0("list(", paste0(paste(x, "=", x), collapse = ", "), ")")           # create the call [list(<var1> = <var1>, <var2> = <var2>, ...)]
  eval.parent(parse(text = x, n = 1))                                            # and evaluate it in the environment of the calling function
}

#' @describeIn make. Creates a square atomic diagonal matrix. For numeric
#'   \code{x}, off-diagonals are \code{0}. For logical \code{x}, off diagonals
#'   are \code{FALSE}. For character \code{x}, off diagonals are blank strings
#'   (\code{""}). For all others, off diagonals are \code{NA}.
#' @export
diag. <- function(x = 1, r = 1) {
  vx <- inum(x) | ilgl(x) | ichr(x)
  vr <- cmp_psw_scl(r)
  vR <- f0(length(x) > 1, T, f0(vr, T, r > 1))
  err <- NULL
  if (!vx) {err <- c(err, "\n • [x] must be an numeric, logical, or character.")}
  if (!vr) {err <- c(err, "\n • [r] must a positive whole-number scalar (?cmp_psw_scl).")}
  if (!vR) {err <- c(err, "\n • Neither [length(x)] nor [r] is greater than 1.")}
  if (idef(err)) {stop(err)}
  nr <- nrow(x); nc <- ncol(x)
  if (is.matrix(x)) {if (nr > 0 & nr == nc) {x <- diag(x, nr, nc)}}
  x <- av(x)
  if (r > 1) {x <- rep(x, r)}
  if (all(is.na(x))) {x[is.na(x)] <- NA_real_}
  n <- length(x)
  blank <- f0(inum(x), 0, f0(ilgl(x), F, f0(ichr(x), "", NA)))
  out <- matrix(blank, nrow = n, ncol = n)
  diag(out) <- x
  out
}
