#' @name n_is
#' @family extensions
#' @title Dedicated Counting Functions
#' @description
#'   Counts functions can be used with a single unnamed argument, multiple
#'   unnamed arguments, and with restrictions in named arguments, in any
#'   combination. The functions in this family are: \tabular{ll}{
#'   FUNCTION(S)            \tab QUESTION(S) ANSWERED                        \cr
#'   `n_is`                 \tab Do counts in `x` meet criteria in `n`, `min`,
#'                               `max`, and/or `eq`?                         \cr
#'   `nd`                   \tab How many `...` arguments?                   \cr
#'   `nx`                   \tab How many values in `...` arguments?         \cr
#'   `ns`                   \tab How many values in each `...` argument?     \cr
#'   `nmin, nmax`           \tab What is the min or max `...` argument length,
#'                               respectively?                               \cr
#'   `nsame`                \tab Are lengths of all `...` arguments the same?\cr
#'   `nw, nt`               \tab How many `TRUE` values?                     \cr
#'   `nf`                   \tab How many `FALSE` values?                    \cr
#'   `nu, nuv`              \tab How many unique atomic values?              \cr
#'   `nr, nc`               \tab How many rows or columns, respectively?     \cr
#'   `nch`                  \tab How many characters in each element?        \cr
#'   `nna, nok`             \tab How many `NA` or non-`NA` values,
#'                               respectively?                               \cr
#'   `nat`                  \tab How many values after \link[=av]{atomizing}?\cr
#'   `n0, n1, n2, n3`       \tab Is length `0`, `1`, `2`, or `3`,
#'                               respectively?                               \cr
#'   `n1p, n2p, n3p`        \tab Is length `1+`, `2+`, or `3+`, respectively?\cr
#'   `nd0, nd1, nd2, nd3`   \tab Is the number of `...` arguments `0`, `1`, `2`,
#'                               or `3`, respectively?                       \cr
#'   `nd1p, nd2p, nd3p`     \tab Is the number of `...` arguments `1+`, `2+`, or
#'                               `3+`, respectively.                           }
#' @param x \link[=innw]{non-negative whole-number} object.
#' @param ... One or more arguments to be examined for counts.
#' @param n Optional \link[=cmp_nnw_vec]{complete non-negative whole-number
#'   vec} of valid element, row, or column counts.
#' @param min,max Optional \link[=cmp_nnw_scl]{complete non-negative
#'   whole-number scalars} giving minimum and maximum valid element, row, or
#'   column counts.
#' @param eq,na,a Non-\code{NA} scalars indicating, respectively, whether all
#'   counts must be equal, whether \code{NA} values are allowed, and whether to
#'   atomize \code{...} to create a single atomic vector before processing. If
#'   \code{a} is \code{FALSE}, each argument in \code{...} is processed
#'   separately.
#' @param vals Optional \link[=atm_vec]{atomic vec} indicating specific values
#'   to be counted.
#' @param lt,le,ge,gt Optional \link[=cmp_srt_scl]{complete sortable scalars}
#'   indicating specific values elements of \code{...} arguments must be less
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
n_is <- function(x, n = NULL, min = NULL, max = NULL, eq = F) {
  errs <- c(f0(cmp_nnw(x), NULL, "\n \u2022 [x] must contain only non-negative whole numbers."),
            f0(inll(n) | cmp_nnw_vec(n), NULL, "\n \u2022 [n] must be NULL or a non-negative whole-number vec (?cmp_nnw_vec)."),
            f0(inll(min) | cmp_nnw_scl(min), NULL, "\n \u2022 [min] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl)."),
            f0(inll(max) | cmp_nnw_scl(max), NULL, "\n \u2022 [max] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl)."),
            f0(isTF(eq), NULL, "\n \u2022 [eq] must be TRUE or FALSE."))
  if (!is.null(errs)) {stop(errs)}
  if (length(c(n, min, max)) == 0 & !eq) {return(x)}                             # if no restrictions are specified, return the raw counts
  ok.n <- f0(inll(n), T, all(x %in% n))                                          # whether exact argument length restriction is met
  ok.min <- f0(inll(min), T, all(x >= min))                                      # whether min argument length restriction is met
  ok.max <- f0(inll(max), T, all(x <= max))                                      # whether max argument length restriction is met
  ok.eq <- f0(eq, length(unique(x)) == 1, T)                                     # whether argument length equality restriction is met
  ok.n & ok.min & ok.max & ok.eq                                                 # whether all argument length restrictions are met
}

#' @rdname n_is
#' @export
nx <- function(..., n = NULL, min = NULL, max = NULL, eq = F, a = F, na = F, vals = NULL, lt = NULL, le = NULL, ge = NULL, gt = NULL) {
  dots <- list(...)
  av.dots <- av(dots)
  ok.min <- inll(min) | cmp_nnw_scl(min)
  ok.max <- inll(max) | cmp_nnw_scl(max)
  ok.mm <- f0(!ok.min | !ok.max, T, f0(inll(min) | inll(max), T, max >= min))
  errs <- c(f0(...length() > 0                                      , NULL, "\n \u2022 [...] is empty."),
            f0(isTF(a)                                              , NULL, "\n \u2022 [a] must be TRUE or FALSE."),
            f0(inll(n) | cmp_nnw_vec(n)                             , NULL, "\n \u2022 [n] must be NULL or contain only non-negative whole numbers."),
            f0(ok.min                                               , NULL, "\n \u2022 [min] must be NULL or a non-negative whole number."),
            f0(ok.max                                               , NULL, "\n \u2022 [max] must be NULL or a non-negative whole number."),
            f0(ok.mm                                                , NULL, "\n \u2022 [max] must be greater than or equal to [min]."),
            f0(isTF(eq)                                             , NULL, "\n \u2022 [eq] must be TRUE or FALSE."),
            f0(isTF(na)                                             , NULL, "\n \u2022 [na] must be TRUE or FALSE."),
            f0(f0(!isF(na), T, !any(is.na(av)))                     , NULL, "\n \u2022 [na = FALSE] but arguments in [...] contains NA values."),
            f0(f0(inll(vals), T, compatible(..., vals, recycle = F)), NULL, "\n \u2022 [vals] must be NULL or compatible with arguments in [...]."),
            f0(f0(inll(lt  ), T, comparable(..., lt  , recycle = F)), NULL, "\n \u2022 [lt] must be NULL or comparable with arguments in [...]."),
            f0(f0(inll(le  ), T, comparable(..., le  , recycle = F)), NULL, "\n \u2022 [le] must be NULL or comparable with arguments in [...]."),
            f0(f0(inll(ge  ), T, comparable(..., ge  , recycle = F)), NULL, "\n \u2022 [ge] must be NULL or comparable with arguments in [...]."),
            f0(f0(inll(gt  ), T, comparable(..., gt  , recycle = F)), NULL, "\n \u2022 [gt] must be NULL or comparable with arguments in [...]."))
  if (!is.null(errs)) {stop(errs)}
  if (a) {dots <- list(av.dots)}
  for (i in 1:length(dots)) {
    dot <- dots[[i]]
    if (idef(vals)) {dot <- dot[dot %in% vals]}
    if (idef(lt)) {dot <- dot[dot <  lt]}
    if (idef(le)) {dot <- dot[dot <= le]}
    if (idef(ge)) {dot <- dot[dot >= ge]}
    if (idef(gt)) {dot <- dot[dot >  gt]}
    dots[[i]] <- dot
  }
  n_is(lengths(dots, F), n = n, min = min, max = max, eq = eq)
}

#' @rdname n_is
#' @export
ns <- function(..., n = NULL, min = NULL, max = NULL, na = F, vals = NULL, lt = NULL, le = NULL, ge = NULL, gt = NULL) {nx(..., n = n, min = min, max = max, na = na, a = F, vals = vals, lt = lt, le = le, ge = ge, gt = gt)}

#' @rdname n_is
#' @export
nmin <- function(..., na = F, vals = NULL, lt = NULL, le = NULL, ge = NULL, gt = NULL) {min(nx(..., na = na, a = F, vals = vals, lt = lt, le = le, ge = ge, gt = gt))}

#' @rdname n_is
#' @export
nmax <- function(..., na = F, vals = NULL, lt = NULL, le = NULL, ge = NULL, gt = NULL) {max(nx(..., na = na, a = F, vals = vals, lt = lt, le = le, ge = ge, gt = gt))}

#' @rdname n_is
#' @export
nsame <- function(..., min = NULL, max = NULL, na = F, vals = NULL, lt = NULL, le = NULL, ge = NULL, gt = NULL) {nx(..., min = min, max = max, eq = T, na = na, a = F, vals = vals, lt = lt, le = le, ge = ge, gt = gt)}

#' @rdname n_is
#' @export
nw <- function(..., n = NULL, min = NULL, max = NULL, eq = F, na = F, a = T) {
  dots <- list(...)
  errs <- c(f0(all(sapply(dots, pop_lgl))  , NULL, "\n \u2022 [...] must contain only non-empty logical objects."),
            f0(isTF(a)                     , NULL, "\n \u2022 [a] must be TRUE or FALSE."),
            f0(inll(n) | cmp_nnw_vec(n)    , NULL, "\n \u2022 [n] must be NULL or a non-negative whole-number vec (?cmp_nnw_vec)."),
            f0(inll(min) | cmp_nnw_scl(min), NULL, "\n \u2022 [min] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl)."),
            f0(inll(max) | cmp_nnw_scl(max), NULL, "\n \u2022 [max] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl)."),
            f0(isTF(eq)                    , NULL, "\n \u2022 [eq] must be TRUE or FALSE."),
            f0(isTF(na)                    , NULL, "\n \u2022 [na] must be TRUE or FALSE."),
            f0(f0(!isF(na), T, !any(is.na(av))), NULL, "\n \u2022 [na = FALSE] but arguments in [...] contains NA values."))
  if (!is.null(errs)) {stop(errs)}
  if (isT(a)) {dots <- list(av(dots))}
  n_is(sapply(lapply(dots, which), length), n, min, max, eq)
}

#' @rdname n_is
#' @export
nt <- nw

#' @rdname n_is
#' @export
nf <- function(..., n = NULL, min = NULL, max = NULL, eq = F, na = F, a = T) {
  dots <- list(...)
  errs <- c(f0(all(sapply(dots, pop_lgl))      , NULL, "\n \u2022 [...] must contain only non-empty logical objects."),
            f0(isTF(a)                         , NULL, "\n \u2022 [a] must be TRUE or FALSE."),
            f0(inll(n) | cmp_nnw_vec(n)        , NULL, "\n \u2022 [n] must be NULL or a non-negative whole-number vec (?cmp_nnw_vec)."),
            f0(inll(min) | cmp_nnw_scl(min)    , NULL, "\n \u2022 [min] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl)."),
            f0(inll(max) | cmp_nnw_scl(max)    , NULL, "\n \u2022 [max] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl)."),
            f0(isTF(eq)                        , NULL, "\n \u2022 [eq] must be TRUE or FALSE."),
            f0(isTF(na)                        , NULL, "\n \u2022 [na] must be TRUE or FALSE."),
            f0(f0(!isF(na), T, !any(is.na(av))), NULL, "\n \u2022 [na = FALSE] but arguments in [...] contains NA values."))
  if (!is.null(errs)) {stop(errs)}
  if (isT(a)) {dots <- list(av(dots))}
  n_is(sapply(lapply(lapply(dots, not), which), length), n, min, max, eq)
}

#' @rdname n_is
#' @export
nu <- function(..., n = NULL, min = NULL, max = NULL, eq = F, na = F, a = T) {
  dots <- list(...)
  errs <- c(f0(all(sapply(dots, pop_lgl))      , NULL, "\n \u2022 [...] must contain only non-empty logical objects."),
            f0(isTF(a)                         , NULL, "\n \u2022 [a] must be TRUE or FALSE."),
            f0(inll(n) | cmp_nnw_vec(n)        , NULL, "\n \u2022 [n] must be NULL or a non-negative whole-number vec (?cmp_nnw_vec)."),
            f0(inll(min) | cmp_nnw_scl(min)    , NULL, "\n \u2022 [min] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl)."),
            f0(inll(max) | cmp_nnw_scl(max)    , NULL, "\n \u2022 [max] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl)."),
            f0(isTF(eq)                        , NULL, "\n \u2022 [eq] must be TRUE or FALSE."),
            f0(isTF(na)                        , NULL, "\n \u2022 [na] must be TRUE or FALSE."),
            f0(f0(!isF(na), T, !any(is.na(av))), NULL, "\n \u2022 [na = FALSE] but arguments in [...] contains NA values."))
  if (!is.null(errs)) {stop(errs)}
  if (isT(a)) {dots <- list(av(dots))}
  n_is(sapply(lapply(dots, unique), length), n, min, max, eq)
}

#' @rdname n_is
#' @export
nuv <- nu

#' @rdname n_is
#' @export
nr <- function(..., n = NULL, min = NULL, max = NULL, eq = F) {
  dots <- list(...)
  errs <- c(f0(all(sapply(dots, id2D))     , NULL, "\n \u2022 [...] must contain only matrices or dtfs (?is_dtf)."),
            f0(inll(n) | cmp_nnw_vec(n)    , NULL, "\n \u2022 [n] must be NULL or a non-negative whole-number vec (?cmp_nnw_vec)."),
            f0(inll(min) | cmp_nnw_scl(min), NULL, "\n \u2022 [min] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl)."),
            f0(inll(max) | cmp_nnw_scl(max), NULL, "\n \u2022 [max] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl)."),
            f0(isTF(eq)                    , NULL, "\n \u2022 [eq] must be TRUE or FALSE."))
  if (!is.null(errs)) {stop(errs)}
  n_is(sapply(dots, nrow), n, min, max, eq)
}

#' @rdname n_is
#' @export
nc <- function(..., n = NULL, min = NULL, max = NULL, eq = F) {
  dots <- list(...)
  errs <- c(f0(all(sapply(dots, id2D))     , NULL, "\n \u2022 [...] must contain only matrices or dtfs (?is_dtf)."),
            f0(inll(n) | cmp_nnw_vec(n)    , NULL, "\n \u2022 [n] must be NULL or a non-negative whole-number vec (?cmp_nnw_vec)."),
            f0(inll(min) | cmp_nnw_scl(min), NULL, "\n \u2022 [min] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl)."),
            f0(inll(max) | cmp_nnw_scl(max), NULL, "\n \u2022 [max] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl)."),
            f0(isTF(eq)                    , NULL, "\n \u2022 [eq] must be TRUE or FALSE."))
  if (!is.null(errs)) {stop(errs)}
  n_is(sapply(dots, ncol), n, min, max, eq)
}

#' @rdname n_is
#' @export
nch <- function(..., n = NULL, min = NULL, max = NULL, eq = F, na = F, a = T) {
  dots <- list(...)
  av.dots <- av(dots)
  errs <- c(f0(isTF(a)                        , NULL, "\n \u2022 [a] must be TRUE or FALSE."),
            f0(all(sapply(dots, ichr))        , NULL, "\n \u2022 [...] must contain only non-empty character objects."),
            f0(inll(n) | cmp_nnw_vec(n)       , NULL, "\n \u2022 [n] must be NULL or a non-negative whole-number vec (?cmp_nnw_vec)."),
            f0(inll(min) | cmp_nnw_scl(min)   , NULL, "\n \u2022 [min] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl)."),
            f0(inll(max) | cmp_nnw_scl(max)   , NULL, "\n \u2022 [max] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl)."),
            f0(isTF(eq)                       , NULL, "\n \u2022 [eq] must be TRUE or FALSE."),
            f0(isTF(na)                       , NULL, "\n \u2022 [na] must be TRUE or FALSE."),
            f0(!isF(na) | !any(is.na(av.dots)), NULL, "\n \u2022 [na = FALSE] but arguments in [...] contains NA values."))
  if (!is.null(errs)) {stop(errs)}
  if (isT(a)) {dots <- list(av.dots)}
  ns <- sapply(lapply(dots, ch), length)
  n_is(ns, n, min, max, eq)
}

#' @rdname n_is
#' @export
nna <- function(..., n = NULL, min = NULL, max = NULL, eq = F, a = T) {
  dots <- list(...)
  av.dots <- av(dots)
  errs <- c(f0(isTF(a)                        , NULL, "\n \u2022 [a] must be TRUE or FALSE."),
            f0(all(sapply(dots, ichr))        , NULL, "\n \u2022 [...] must contain only non-empty character objects."),
            f0(inll(n) | cmp_nnw_vec(n)       , NULL, "\n \u2022 [n] must be NULL or a non-negative whole-number vec (?cmp_nnw_vec)."),
            f0(inll(min) | cmp_nnw_scl(min)   , NULL, "\n \u2022 [min] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl)."),
            f0(inll(max) | cmp_nnw_scl(max)   , NULL, "\n \u2022 [max] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl)."),
            f0(isTF(eq)                       , NULL, "\n \u2022 [eq] must be TRUE or FALSE."))
  if (!is.null(errs)) {stop(errs)}
  if (isT(a)) {dots <- list(av.dots)}
  ns <- sapply(lapply(dots, is.na), length)
  n_is(ns, n, min, max, eq)
}

#' @rdname n_is
#' @export
nok <- function(..., n = NULL, min = NULL, max = NULL, eq = F, a = T) {
  dots <- list(...)
  av.dots <- av(dots)
  errs <- c(f0(isTF(a)                        , NULL, "\n \u2022 [a] must be TRUE or FALSE."),
            f0(all(sapply(dots, ichr))        , NULL, "\n \u2022 [...] must contain only non-empty character objects."),
            f0(inll(n) | cmp_nnw_vec(n)       , NULL, "\n \u2022 [n] must be NULL or a non-negative whole-number vec (?cmp_nnw_vec)."),
            f0(inll(min) | cmp_nnw_scl(min)   , NULL, "\n \u2022 [min] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl)."),
            f0(inll(max) | cmp_nnw_scl(max)   , NULL, "\n \u2022 [max] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl)."),
            f0(isTF(eq)                       , NULL, "\n \u2022 [eq] must be TRUE or FALSE."))
  if (!is.null(errs)) {stop(errs)}
  if (isT(a)) {dots <- list(av.dots)}
  ns <- sapply(lapply(lapply(dots, is.na), not), length)
  n_is(ns, n, min, max, eq)
}

#' @rdname n_is
#' @export
nat <- function(..., n = NULL, min = NULL, max = NULL, eq = F, a = T) {nx(..., n = n, min = min, max = max, eq = eq, a = T)}

#' @rdname n_is
#' @export
n0 <- function(..., na = F, a = T) {nx(..., n = 0, na = na, a = a)}

#' @rdname n_is
#' @export
n1 <- function(..., na = F, a = T) {nx(..., n = 1, na = na, a = a)}

#' @rdname n_is
#' @export
n2 <- function(..., na = F, a = T) {nx(..., n = 2, na = na, a = a)}

#' @rdname n_is
#' @export
n3 <- function(..., na = F, a = T) {nx(..., n = 3, na = na, a = a)}

#' @rdname n_is
#' @export
n1p <- function(..., na = F, eq = T, a = T) {nx(..., min = 1, na = na, a = a)}

#' @rdname n_is
#' @export
n2p <- function(..., na = F, eq = T, a = T) {nx(..., min = 2, na = na, a = a)}

#' @rdname n_is
#' @export
n3p <- function(..., na = F, eq = T, a = T) {nx(..., min = 3, na = na, a = a)}

#' @rdname n_is
#' @export
nd <- function() {eval.parent(...length())}

#' @rdname n_is
#' @export
nd0 <- function() {eval.parent(...length() == 0)}

#' @rdname n_is
#' @export
nd1 <- function() {eval.parent(...length() == 1)}

#' @rdname n_is
#' @export
nd2 <- function() {eval.parent(...length() == 2)}

#' @rdname n_is
#' @export
nd3 <- function() {eval.parent(...length() == 3)}

#' @rdname n_is
#' @export
nd1p <- function() {eval.parent(...length() >= 1)}

#' @rdname n_is
#' @export
nd2p <- function() {eval.parent(...length() >= 2)}

#' @rdname n_is
#' @export
nd3p <- function() {eval.parent(...length() >= 3)}
