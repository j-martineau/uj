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
  errs <- c(f0(cmp_nnw_scl(r.), NULL, "\n \u2022 [r.] must be a non-negative whole-number scalar (?cmp_nnw_scl)."),
            f0(inll(vn.) | cmp_chr_vec(vn.), NULL, "\n \u2022 [vn.] must be NULL or a complete character vec (?cmp_chr_vec)." ))
  if (idef(errs)) {stop(errs)}
  if (r. > 1) {x <- rep.int(x, r.)}
  if (idef(vn.)) {
    vn. <- ssP(vn.)
    if (length(x) != length(vn.)) {stop("\n \u2022 [vn.] must be the same length as the vector resulting from atomizing (?av) [...].")}
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
  errs <- c(f0(inll(nr.) | cmp_nnw_scl(nr.), NULL, "\n \u2022 [nr.] must be NULL or a non-negative whole number scalar (?cmp_nnw_scl)."),
            f0(inll(nc.) | cmp_nnw_scl(nc.), NULL, "\n \u2022 [nc.] must be NULL or a non-negative whole number scalar (?cmp_nnw_scl)."),
            f0(isTF(br.)                   , NULL, "\n \u2022 [br.] must be TRUE or FALSE."),
            f0(inll(rn.) | cmp_chr_vec(rn.), NULL, "\n \u2022 [rn.] must be NULL or a complete character vector (?cmp_chr_vec)."),
            f0(inll(cn.) | cmp_chr_vec(cn.), NULL, "\n \u2022 [cn.] must be NULL or a complete character vector (?cmp_chr_vec)."))
  if (idef(errs)) {stop(errs)}
  if  (is.null(nr.) & is.null(nc.)) {nr0 <- 1; nc0 <- length(av)}
  else if (is.null(nr.)) {nc0 <- nc.; nr0 <- length(av) / nc.}
  else if (is.null(nc.)) {nr0 <- nr.; nc0 <- length(av) / nr.}
  rn. <- av(strsplit(rn., "|", fixed = T))
  cn. <- av(strsplit(cn., "|", fixed = T))
  ok.ns <- round(nr0) == nr0 & round(nc0) == nc0 & nr0 * nc0 == length(av)
  ok.rn <- f0(inll(rn.), T, f0(!ok.ns, T, length(rn.) == nr0))
  ok.cn <- f0(inll(cn.), T, f0(!ok.ns, T, length(cn.) == nc0))
  if (ok.rn & ok.cn) {
    nt <- nr. * nc.
    if (nt > 0) {ok.reps <- (nt / length(x)) == round(nt / length(x))}
    else {ok.reps <- length(x) == 0}
  }
  else {ok.reps <- T}
  errs <- NULL
  if (!ok.ns) {
    errs <- c(f0(inll(nr.), NULL, "\n \u2022 [nc.] is not a divisor of length(av(...))." ),
              f0(inll(nc.), NULL, "\n \u2022 [nr.] is not a divisor of length(av(...))." ),
              f0(idef(nr.) & idef(nc.), NULL, "\n \u2022 [nr. * nc.] does not equal length(av(...))."))
  } else {errs <- NULL}
  errs <- c(errs,
            f0(ok.rn, NULL, "\n \u2022 [rn.] must be NULL or a complete vect of length equal to the number of resulting rows."),
            f0(ok.cn, NULL, "\n \u2022 [cn.] must be NULL or a complete vect of length equal to the number of resulting columns."))
  if (idef(errs)) {stop(errs)}
  x <- matrix(av, nrow = nr., ncol = nc., byrow = br.)
  rownames(x) <- rn.
  colnames(x) <- cn.
  x
}

#' @describeIn make. Creates an  with 0 rows and columns
#'   with names from \code{cn.}, which may be pipe delimited.
#' @export
dtf0 <- function(cn) {
  if (!cmp_chr_vec(cn)) {stop("\n \u2022 [cn] must be a complete character vector (?cmp_chr_vec).")}
  cn <- av(strsplit(cn, "|", fixed = T))
  cmd <- paste0("tibble::tibble(", paste0(paste0(cn, " = NA"), collapse = ", "), ", .rows = 0")
  run(cmd)
}

#' @describeIn make. Creates an \link[idtf]{atomic dtf} with \code{nr.} rows and
#'   columns with names from \code{cn.} (which may be pipe delimited). All cells
#'   of the resulting tibble are populated with \code{NA}. values.
#' @export
dtf_na <- function(cn, nr) {
  errs <- c(f0(cmp_chr_vec(cn), NULL, "\n \u2022 [cn] must be a complete character vector (?cmp_chr_vec)."),
            f0(cmp_psw_scl(nr), NULL, "\n \u2022 [nr] must be NULL or a positive whole number scalar (?cmp_psw_scl)."))
  if (idef(errs)) {stop(errs)}
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
  cn. <- f0(cmp_chr_vec(cn.), ss(cn.), cn.)
  dots <- list(...)
  n.dots <- length(dots)
  dot.ns <- lengths(dots)
  max.n <- max(dot.ns)
  ok.0 <- n.dots > 0
  ok.ns <- f0(!ok.0, T, all(dot.ns > 0))
  ok.vec <- f0(!ok.0 | !ok.ns, T, all(sapply(dots, ivec)))
  ok.reps <- f0(n.dots == 0, F, f0(any(n.dots == 0), F, all(max.n / dot.ns == round(max.n / dot.ns))))
  ok.cn <- f0(inll(cn.), T, f0(!cmp_chr_vec(cn.), F, length(cn.) == n.dots))
  errs <- c(f0(ok.0   , NULL, "\n \u2022 [...] is empty."),
            f0(ok.ns  , NULL, "\n \u2022 [...] contains an empty argument."),
            f0(ok.vec , NULL, "\n \u2022 Arguments in [...] must be atomic vecs (?ivec)."),
            f0(ok.reps, NULL, "\n \u2022 Arguments in [...] are not recyclable ."),
            f0(ok.cn  , NULL, "\n \u2022 [cn.] must be NULL or match the number of arguments in [...]."))
  if (idef(errs)) {stop(errs)}
  out <- tibble::tibble(...)
  if (idef(cn.)) {colnames(out) <- cn.}
  out
}

#' @describeIn make. Creates an atomic \link[ivls]{vlist} from the arguments in
#'   \code{...} with optional element names to replace any names of arguments in
#'   \code{...}.
#' @export
vls <- function(..., en. = NULL) {
  en. <- f0(cmp_chr_vec(en.), av(strsplit(en., "|", fixed = T)), en.)
  dots <- list(...)
  n.dots <- length(dots)
  ok.en <- f0(inll(en.), T, f0(!cmp_chr_vec(en.), F, length(en.) == n.dots))
  errs <- c(f0(n.dots > 0, NULL, "\n \u2022 [...] is empty."),
            f0(ok.en     , NULL, "\n \u2022 [en.] must be NULL or match the number of arguments in [...]."))
  if (idef(errs)) {stop(errs)}
  if (idef(en.)) {names(dots) <- en.}
  dots
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
  ok.x <- inum(x) | ilgl(x) | ichr(x)
  ok.r <- cmp_psw_scl(r)
  ok.ge1 <- f0(length(x) > 1, T, f0(ok.r, T, r > 1))
  errs <- c(f0(ok.x  , NULL, "\n \u2022 [x] must be an numeric, logical, or character."),
            f0(ok.r  , NULL, "\n \u2022 [r] must a positive whole-number scalar (?cmp_psw_scl)."),
            f0(ok.ge1, NULL, "\n \u2022 Neither [length(x)] nor [r] is greater than 1."))
  if (idef(errs)) {stop(errs)}
  nr <- nrow(x)
  nc <- ncol(x)
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
