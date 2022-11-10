#' @name nx.
#' @family extensions
#' @title Dedicated counting functions
#' @param x \link[innw]{Non-negative whole-number} object.
#' @param ... One or more arguments to be examined for counts.
#' @param n Optional \link[cmp_nnw_vec]{complete non-negative whole-number vect}
#'   of valid element, row, or column counts.
#' @param min,max Optional \link[cmp_nnw_scl]{complete non-negative whole-number
#'   scalars} giving minimum and maximum valid element, row, or column counts.
#' @param eq,na,a \link[cmp_lgl_scl]{Complete logical scalars} indicating,
#'   respectively, whether all counts must be equal, whether \code{NA} values
#'   are allowed, and whether to atomize \code{...} to create a single atomic
#'   vector before processing. If \code{a} is \code{FALSE}, each argument in
#'   \code{...} is processed separately.
#' @param vals Optional \link[is_vec]{atomic vec} indicating specific values to
#'   be counted.
#' @param lt,le,.ge,gt Optional \link[cmp_scl]{complete atomic scalars}
#'   indicating specific values elements of arguments in \code{...} must be less
#'   than, less than or equal to, greater than or equal to, or greater than,
#'   respectively, to be counted.
#' @return An integer or logical scalar or vector.
#' @examples
#' N <- 0:15
#' n_is(N, n = 0:5)
#' n_is(N, min = 3, max = 12)
#' n_is(N, eq = T)
#' n_is(rep(0, 3), eq = T)
#' nx(letters, LETTERS, 0:9, NULL)
#' nx(letters, LETTERS, 0:9, vals = letters)
#' nx(letters, LETTERS, vals = letters)
#' nx(letters, LETTERS, le = "M", ge = "m")
#' nx(letters, LETTERS, lt = "M", gt = "m")
#' n0(letters, LETTERS, TRUE, 0:1, 0:2, 0:9)
#' n1(letters, LETTERS, TRUE, 0:1, 0:2, 0:9)
#' n1p(letters, LETTERS, TRUE, 0:1, 0:2, 0:9)
#' n2p(letters, LETTERS, TRUE, 0:1, 0:2, 0:9)
#' nmin(letters, LETTERS, 0:9)
#' nmax(letters, LETTERS, 0:9)
#' nsame(letters, LETTERS, 0:9)
#' nsame(letters, LETTERS)
#' nsame(letters, LETTERS, lt = "M", gt = "m")
#' nmin(letters, LETTERS, 0:9, min = 11)
#' nmax(letters, LETTERS, 0:9, max = 11)
#' nsame(letters, LETTERS, 0:9, min = 11, max = 25)
#' nw(0:99 %in% 50:59, 0:99 %in% 41:49)
#' nw(0:99 %in% 50:59, 0:99 %in% 41:49, n = 0:9)
#' nw(0:99 %in% 50:59, 0:99 %in% 41:49, min = 9, max = 19)
#' nw(0:99 %in% 50:59, 0:99 %in% 41:49, min = 9, max = 19, eq = T)
#' nf(0:99 %in% 50:59, 0:99 %in% 41:49)
#' nf(0:99 %in% 50:59, 0:99 %in% 41:49, n = 0:9)
#' nf(0:99 %in% 50:59, 0:99 %in% 41:49, min = 9, max = 19)
#' nf(0:99 %in% 50:59, 0:99 %in% 41:49, min = 9, max = 19, eq = T)
#' nr(tibble(letters, LETTERS), matrix(c(letters, LETTERS), nrow = 2))
#' nc(tibble(letters, LETTERS), matrix(c(letters, LETTERS), nrow = 2))
#' nch(letters, eq = T)
#' nch(letters, "a string")
#' nch(letters, "a string", a = T)
#' @export
nx. <- function() {help("nx.", package = "uj")}

#' @describeIn nx. Check for specific counts, minimum counts, maximum counts,
#'   and equal counts. Returns counts themselves if no checks are specified.
#' @export
n_is <- function(x, n = NULL, min = NULL, max = NULL, eq = F) {
  vx <- cmp_nnw(x)
  vn <- f0(inll(n), T, cmp_nnw_vec(n))
  veq <- isTF(eq)
  vmn <- f0(inll(min), T, cmp_nnw_scl(min))
  vmx <- f0(inll(max), T, cmp_nnw_scl(max))
  err <- NULL
  if (!vx) {err <- c(err, "\n • [x] must contain only non-negative whole numbers.")}
  if (!vn) {err <- c(err, "\n • [n] must be NULL or a non-negative whole-number vec (?cmp_nnw_vec).")}
  if (!vmn) {err <- c(err, "\n • [min] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl).")}
  if (!vmx) {err <- c(err, "\n • [max] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl).")}
  if (!veq) {err <- c(err, "\n • [eq] must be TRUE or FALSE.")}
  if (idef(err)) {stop(err)}
  if (length(c(n, min, max)) == 0 & !eq) {return(x)}                             # if no restrictions are specified, return the raw counts
  vn <- f0(inll(n), T, all(x %in% n))                                            # whether exact argument length restriction is met
  vmn <- f0(inll(min), T, all(x >= min))                                         # whether min argument length restriction is met
  vmx <- f0(inll(max), T, all(x <= max))                                         # whether max argument length restriction is met
  veq <- f0(eq, length(unique(x)) == 1, T)                                       # whether argument length equality restriction is met
  (vn & vmn & vmx & veq)                                                         # whether all argument length restrictions are met
}

#' @describeIn nx. Length of arguments.
#' @export
nx <- function(..., n = NULL, min = NULL, max = NULL, eq = F, a = F, na = F, vals = NULL, lt = NULL, le = NULL, ge = NULL, gt = NULL) {
  x <- list(...); av <- av(x)                                                     # extract ... args and an atomized version
  v0 <- ...length() > 0
  va <- isTF(a)
  vn <- f0(inll(n), T, cmp_nnw_vec(n))
  vmn <- f0(inll(min), T, cmp_nnw_scl(min))
  vmx <- f0(inll(max), T, cmp_nnw_scl(max))
  vmm <- f0(!vmn | !vmx, T, f0(inll(min) | inll(max), T, max >= min))
  veq <- isTF(eq)
  vna <- isTF(na)
  vxx <- f0(!isF(na), T, !any(is.na(av)))
  vlt <- f0(inll(lt), T, comparable(..., lt, recycle = F))
  vle <- f0(inll(le), T, comparable(..., le, recycle = F))
  vge <- f0(inll(ge), T, comparable(..., ge, recycle = F))
  vgt <- f0(inll(gt), T, comparable(..., gt, recycle = F))
  vvl <- f0(inll(vals), T, compatible(..., vals, recycle = F))
  err <- NULL
  if (!v0) {err <- c(err, "\n • [...] is empty.")}
  if (!va) {err <- c(err, "\n • [a] must be TRUE or FALSE.")}
  if (!vn) {err <- c(err, "\n • [n] must be NULL or contain only non-negative whole numbers.")}
  if (!vmn) {err <- c(err, "\n • [min] must be NULL or a non-negative whole number.")}
  if (!vmx) {err <- c(err, "\n • [max] must be NULL or a non-negative whole number.")}
  if (!vmm) {err <- c(err, "\n • [max] must be greater than or equal to [min].")}
  if (!veq) {err <- c(err, "\n • [eq] must be TRUE or FALSE.")}
  if (!vna) {err <- c(err, "\n • [na] must be TRUE or FALSE.")}
  if (!vxx) {err <- c(err, "\n • [na = FALSE] but arguments in [...] contains NA values.")}
  if (!vvl) {err <- c(err, "\n • [vals] must be NULL or compatible with arguments in [...].")}
  if (!vlt) {err <- c(err, "\n • [lt] must be NULL or comparable with arguments in [...].")}
  if (!vle) {err <- c(err, "\n • [le] must be NULL or comparable with arguments in [...].")}
  if (!vge) {err <- c(err, "\n • [ge] must be NULL or comparable with arguments in [...].")}
  if (!vgt) {err <- c(err, "\n • [gt] must be NULL or comparable with arguments in [...].")}
  if (idef(err)) {stop(err)}
  if (a) {x <- list(av)}                                                         # if atomization is specified, replace x with the atomized version in a 1-element list
  for (i in 1:length(x)) {
    xi <- x[[i]]
    if (idef(vals)) {xi <- xi[xi %in% vals]}
    if (idef(lt)) {xi <- xi[xi <  lt]}
    if (idef(le)) {xi <- xi[xi <= le]}
    if (idef(ge)) {xi <- xi[xi >= ge]}
    if (idef(gt)) {xi <- xi[xi >  gt]}
    x[[i]] <- xi
  }
  x <- lengths(x, F)                                                             # get the lengths of the arguments
  n_is(x, n = n, min = min, max = max, eq = eq)                                  # and check them against any length restrictions
}

#' @describeIn nx. Lengths of \code{...} arguments.
#' @export
ns <- function(..., n = NULL, min = NULL, max = NULL, na = F, vals = NULL, lt = NULL, le = NULL, ge = NULL, gt = NULL) {nx(..., n = n, min = min, max = max, na = na, a = F, vals = vals, lt = lt, le = le, ge = ge, gt = gt)}

#' @describeIn nx. Minimum length of any \code{...} argument.
#' @export
nmin <- function(..., na = F, vals = NULL, lt = NULL, le = NULL, ge = NULL, gt = NULL) {min(nx(..., na = na, a = F, vals = vals, lt = lt, le = le, ge = ge, gt = gt))}

#' @describeIn nx. Maximum length of any \code{...} argument.
#' @export
nmax <- function(..., na = F, vals = NULL, lt = NULL, le = NULL, ge = NULL, gt = NULL) {max(nx(..., na = na, a = F, vals = vals, lt = lt, le = le, ge = ge, gt = gt))}

#' @describeIn nx. Whether all \code{...} arguments have the same length.
#' @export
nsame <- function(..., min = NULL, max = NULL, na = F, vals = NULL, lt = NULL, le = NULL, ge = NULL, gt = NULL) {nx(..., min = min, max = max, eq = T, na = na, a = F, vals = vals, lt = lt, le = le, ge = ge, gt = gt)}

#' @describeIn nx. Number of \code{TRUE} elements.
#' @export
nw <- function(..., n = NULL, min = NULL, max = NULL, eq = F, na = F, a = T) {
  x <- list(...)
  av <- av(x)
  vd <- all(sapply(x, ilgl))
  vn <- f0(inll(n), T, cmp_nnw_vec(n  ))
  vmn <- f0(inll(min), T, cmp_nnw_scl(min))
  vmx <- f0(inll(max), T, cmp_nnw_scl(max))
  veq <- isTF(eq)
  va <- isTF(a)
  vna <- isTF(na)
  vxx <- f0(!isF(na), T, !any(is.na(av)))
  err <- NULL
  if (!va) {err <- c(err, "\n • [a] must be TRUE or FALSE.")}
  if (!vd) {err <- c(err, "\n • [...] must contain only non-empty logical objects.")}
  if (!vn) {err <- c(err, "\n • [n] must be NULL or a non-negative whole-number vec (?cmp_nnw_vec).")}
  if (!vmn) {err <- c(err, "\n • [min] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl).")}
  if (!vmx) {err <- c(err, "\n • [max] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl).")}
  if (!veq) {err <- c(err, "\n • [eq] must be TRUE or FALSE.")}
  if (!vna) {err <- c(err, "\n • [na] must be TRUE or FALSE.")}
  if (!vxx) {err <- c(err, "\n • [na = FALSE] but arguments in [...] contains NA values.")}
  if (idef(err)) {stop(err)}
  if (isT(a)) {x <- list(av)}
  x <- sapply(lapply(x, which), length)
  n_is(x, n, min, max, eq)
}

#' @describeIn nx. Number of \code{TRUE} elements.
#' @export
nt <- nw

#' @describeIn nx. Number of \code{FALSE} elements.
#' @export
nf <- function(..., n = NULL, min = NULL, max = NULL, eq = F, na = F, a = T) {
  x <- list(...)
  av <- av(x)
  vd <- all(sapply(x, ilgl))
  vn <- f0(inll(n  ), T, cmp_nnw_vec(n  ))
  vmn <- f0(inll(min), T, cmp_nnw_scl(min))
  vmx <- f0(inll(max), T, cmp_nnw_scl(max))
  veq <- isTF(eq)
  va <- isTF(a)
  vna <- isTF(na)
  vxx <- f0(!isF(na), T, !any(is.na(av)))
  err <- NULL
  if (!va) {err <- c(err, "\n • [a] must be TRUE or FALSE.")}
  if (!vd) {err <- c(err, "\n • [...] must contain only non-empty logical objects.")}
  if (!vn) {err <- c(err, "\n • [n] must be NULL or a non-negative whole-number vec (?cmp_nnw_vec).")}
  if (!vmn) {err <- c(err, "\n • [min] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl).")}
  if (!vmx) {err <- c(err, "\n • [max] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl).")}
  if (!veq) {err <- c(err, "\n • [eq] must be TRUE or FALSE.")}
  if (!vna) {err <- c(err, "\n • [na] must be TRUE or FALSE.")}
  if (!vxx) {err <- c(err, "\n • [na = FALSE] but arguments in [...] contains NA values.")}
  if (idef(err)) {stop(err)}
  if (isT(a)) {x <- list(av)}
  x <- sapply(lapply(lapply(x, not), which), length)
  n_is(x, n, min, max, eq)
}

#' @describeIn nx. Number of unique elements.
#' @export
nu <- function(..., n = NULL, min = NULL, max = NULL, eq = F, na = F, a = T) {
  x <- list(...)
  av <- av(x)
  vd <- all(sapply(x, pop_atm))
  vn <- f0(inll(n  ), T, cmp_nnw_vec(n  ))
  vmn <- f0(inll(min), T, cmp_nnw_scl(min))
  vmx <- f0(inll(max), T, cmp_nnw_scl(max))
  veq <- isTF(eq)
  va <- isTF(a)
  vna <- isTF(na)
  vxx <- f0(!isF(na), T, !any(is.na(av)))
  err <- NULL
  if (!va) {err <- c(err, "\n • [a] must be TRUE or FALSE.")}
  if (!vd) {err <- c(err, "\n • [...] must contain only non-empty atomic objects.")}
  if (!vn) {err <- c(err, "\n • [n] must be NULL or a non-negative whole-number vec (?cmp_nnw_vec).")}
  if (!vmn) {err <- c(err, "\n • [min] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl).")}
  if (!vmx) {err <- c(err, "\n • [max] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl).")}
  if (!veq) {err <- c(err, "\n • [eq] must be TRUE or FALSE.")}
  if (!vna) {err <- c(err, "\n • [na] must be TRUE or FALSE.")}
  if (!vxx) {err <- c(err, "\n • [na = FALSE] but arguments in [...] contains NA values.")}
  if (idef(err)) {stop(err)}
  if (isT(a)) {x <- list(av)}
  x <- sapply(lapply(x, unique), length)
  n_is(x, n, min, max, eq)
}

#' @describeIn nx. Number of rows.
#' @export
nr <- function(..., n = NULL, min = NULL, max = NULL, eq = F) {
  x <- list(...)
  vd <- all(sapply(x, id2D))
  vn <- f0(inll(n  ), T, cmp_nnw_vec(n  ))
  vmn <- f0(inll(min), T, cmp_nnw_scl(min))
  vmx <- f0(inll(max), T, cmp_nnw_scl(max))
  veq <- isTF(eq)
  err <- NULL
  if (!vd) {err <- c(err, "\n • [...] must contain only matrices or dtfs (?is_dtf).")}
  if (!vn) {err <- c(err, "\n • [n] must be NULL or a non-negative whole-number vec (?cmp_nnw_vec).")}
  if (!vmn) {err <- c(err, "\n • [min] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl).")}
  if (!vmx) {err <- c(err, "\n • [max] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl).")}
  if (!veq) {err <- c(err, "\n • [eq] must be TRUE or FALSE.")}
  if (idef(err)) {stop(err)}
  x <- sapply(x, nrow)
  n_is(x, n, min, max, eq)
}

#' @describeIn nx. Number of columns.
#' @export
nc <- function(..., n = NULL, min = NULL, max = NULL, eq = F) {
  x <- list(...)
  vd <- all(sapply(x, id2D))
  vn <- f0(inll(n), T, cmp_nnw_vec(n  ))
  vmn <- f0(inll(min), T, cmp_nnw_scl(min))
  vmx <- f0(inll(max), T, cmp_nnw_scl(max))
  veq <- isTF(eq)
  err <- NULL
  if (!vd) {err <- c(err, "\n • [...] must contain only matrices or dtfs (?is_dtf)")}
  if (!vn) {err <- c(err, "\n • [n] must be NULL or a non-negative whole-number vec (?cmp_nnw_vec).")}
  if (!vmn) {err <- c(err, "\n • [min] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl).")}
  if (!vmx) {err <- c(err, "\n • [max] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl).")}
  if (!veq) {err <- c(err, "\n • [eq] must be TRUE or FALSE.")}
  if (idef(err)) {stop(err)}
  x <- sapply(x, ncol)
  n_is(x, n, min, max, eq)
}

#' @describeIn nx. Number of characters in each element.
#'   argument.
#' @export
nch <- function(..., n = NULL, min = NULL, max = NULL, eq = F, na = F, a = T) {
  x <- list(...)
  av <- av(x)
  vd <- all(sapply(x, ichr))
  vn <- f0(inll(n), T, cmp_nnw_vec(n  ))
  vmn <- f0(inll(min), T, cmp_nnw_scl(min))
  vmx <- f0(inll(max), T, cmp_nnw_scl(max))
  veq <- isTF(eq)
  va <- isTF(a)
  vna <- isTF(na)
  vxx <- f0(!isF(na), T, !any(is.na(av)))
  err <- NULL
  if (!va) {err <- c(err, "\n • [a] must be TRUE or FALSE.")}
  if (!vd) {err <- c(err, "\n • [...] must contain only non-empty atomic objects.")}
  if (!vn) {err <- c(err, "\n • [n] must be NULL or a non-negative whole-number vec (?cmp_nnw_vec).")}
  if (!vmn) {err <- c(err, "\n • [min] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl).")}
  if (!vmx) {err <- c(err, "\n • [max] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl).")}
  if (!veq) {err <- c(err, "\n • [eq] must be TRUE or FALSE.")}
  if (!vna) {err <- c(err, "\n • [na] must be TRUE or FALSE.")}
  if (!vxx) {err <- c(err, "\n • [na = FALSE] but arguments in [...] contains NA values.")}
  if (idef(err)) {stop(err)}
  if (isT(a)) {x <- list(av)}
  x <- sapply(lapply(x, ch), length)
  n_is(x, n, min, max, eq)
}

#' @describeIn nx. Number of \code{NA} values.
#' @export
nna <- function(..., n = NULL, min = NULL, max = NULL, eq = F, a = T) {
  x <- list(...)
  av <- av(x)
  vd <- all(sapply(x, ichr))
  vn <- f0(inll(n  ), T, cmp_nnw_vec(n  ))
  vmn <- f0(inll(min), T, cmp_nnw_scl(min))
  vmx <- f0(inll(max), T, cmp_nnw_scl(max))
  veq <- isTF(eq)
  va <- isTF(a)
  err <- NULL
  if (!va) {err <- c(err, "\n • [a] must be TRUE or FALSE.")}
  if (!vd) {err <- c(err, "\n • [...] must contain only non-empty atomic objects.")}
  if (!vn) {err <- c(err, "\n • [n] must be NULL or a non-negative whole-number vec (?cmp_nnw_vec).")}
  if (!vmn) {err <- c(err, "\n • [min] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl).")}
  if (!vmx) {err <- c(err, "\n • [max] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl).")}
  if (!veq) {err <- c(err, "\n • [eq] must be TRUE or FALSE.")}
  if (idef(err)) {stop(err)}
  if (isT(a)) {x <- list(av)}
  x <- sapply(lapply(x, is.na), length)
  n_is(x, n, min, max, eq)
}

#' @describeIn nx. Number of non-\code{NA} values.
#' @export
nok <- function(..., n = NULL, min = NULL, max = NULL, eq = F, a = T) {
  x <- list(...)
  av <- av(x)
  vd <- all(sapply(x, ichr))
  vn <- f0(inll(n), T, cmp_nnw_vec(n))
  vmn <- f0(inll(min), T, cmp_nnw_scl(min))
  vmx <- f0(inll(max), T, cmp_nnw_scl(max))
  veq <- isTF(eq)
  va <- isTF(a)
  err <- NULL
  if (!va) {err <- c(err, "\n • [a] must be TRUE or FALSE.")}
  if (!vd) {err <- c(err, "\n • [...] must contain only non-empty atomic objects.")}
  if (!vn) {err <- c(err, "\n • [n] must be NULL or a non-negative whole-number vec (?cmp_nnw_vec).")}
  if (!vmn) {err <- c(err, "\n • [min] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl).")}
  if (!vmx) {err <- c(err, "\n • [max] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl).")}
  if (!veq) {err <- c(err, "\n • [eq] must be TRUE or FALSE.")}
  if (idef(err)) {stop(err)}
  if (isT(a)) {x <- list(av)}
  x <- sapply(lapply(lapply(x, is.na), not), length)
  n_is(x, n, min, max, eq)
}

#' @describeIn nx. Number of atomic elements.
#' @export
nat <- function(..., n = NULL, min = NULL, max = NULL, eq = F, a = T) {nx(..., n = n, min = min, max = max, eq = eq, a = T)}

#' @describeIn nx. Is the number of elements 0?
#' @export
n0 <- function(..., na = F, a = T) {nx(..., n = 0, na = na, a = a)}

#' @describeIn nx. Is the number of elements 1?
#' @export
n1 <- function(..., na = F, a = T) {nx(..., n = 1, na = na, a = a)}

#' @describeIn nx. Is the number of elements 2?
#' @export
n2 <- function(..., na = F, a = T) {nx(..., n = 2, na = na, a = a)}

#' @describeIn nx. Is the number of elements 3?
#' @export
n3 <- function(..., na = F, a = T) {nx(..., n = 3, na = na, a = a)}

#' @describeIn nx. Is the number of elements 1 or gerater?
#' @export
n1p <- function(..., na = F, eq = T, a = T) {nx(..., min = 1, na = na, a = a)}

#' @describeIn nx. Is the number of elements 2 or greater?
#' @export
n2p <- function(..., na = F, eq = T, a = T) {nx(..., min = 2, na = na, a = a)}

#' @describeIn nx. Is the number of elements 3 or greater?
#' @export
n3p <- function(..., na = F, eq = T, a = T) {nx(..., min = 3, na = na, a = a)}

#' @describeIn nx. Is the number of dot arguments 0?
#' @export
nd <- function() {eval.parent(...length())}

#' @describeIn nx. Is the number of dot arguments 0?
#' @export
nd0 <- function() {eval.parent(...length() == 0)}

#' @describeIn nx. Is the number of dot arguments 1?
#' @export
nd1 <- function() {eval.parent(...length() == 1)}

#' @describeIn nx. Is the number of dot arguments 2?
#' @export
nd2 <- function() {eval.parent(...length() == 2)}

#' @describeIn nx. Is the number of dot arguments 2?
#' @export
nd3 <- function() {eval.parent(...length() == 3)}

#' @describeIn nx. Is the number of dot arguments 1 or greater?
#' @export
nd1p <- function() {eval.parent(...length() >= 1)}

#' @describeIn nx. Is the number of dot arguments 2 or greater?
#' @export
nd2p <- function() {eval.parent(...length() >= 2)}

#' @describeIn nx. Is the number of dot arguments 3 or greater?
#' @export
nd3p <- function() {eval.parent(...length() >= 3)}
