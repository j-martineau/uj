#' @name n_is
#' @title Dedicated counting functions
#' @description Counting functions can be used with a single unnamed argument, multiple unnamed arguments, and with restrictions in named arguments, in any combination. The functions in this family answer the questions described below.
#' \tabular{rl}{
#'   \cr `nu,nuv`   \tab Number of unique atomic values.
#'   \cr  `nw,nt`   \tab Number of `TRUE` values.
#'   \cr     `nf`   \tab Number of `FALSE` values.
#'   \cr    `nna`   \tab Number of atomic `NA` values.
#'   \cr    `nok`   \tab Number of atomic non-`NA` values.
#'   \cr    `nch`   \tab Number of characters in each element.
#'   \cr     `nr`   \tab Number of rows.
#'   \cr     `nc`   \tab Number of columns.
#'   \cr            \tab   
#'   \cr     `nd`   \tab Gets the number of `...` args.
#'   \cr    `nd0`   \tab Are there `0 ...` args?
#'   \cr    `nd1`   \tab Are there `1 ...` args?
#'   \cr    `nd2`   \tab Are there `2 ...` args?
#'   \cr    `nd3`   \tab Are there `3 ...` args?
#'   \cr   `nd1p`   \tab Are there `1+ ...` args?
#'   \cr   `nd2p`   \tab Are there `2+ ...` args?
#'   \cr   `nd3p`   \tab Are there `3+ ...` args?
#'   \cr            \tab   
#'   \cr     `n0`   \tab Is length `0`?
#'   \cr     `n1`   \tab Is length `1`?
#'   \cr     `n2`   \tab Is length `2`?
#'   \cr     `n3`   \tab Is length `3`?
#'   \cr    `n1p`   \tab Is length `1+`?
#'   \cr    `n2p`   \tab Is length `2+`?
#'   \cr    `n3p`   \tab Is length `3+`?
#'   \cr            \tab   
#'   \cr   `nmin`   \tab Min `...` arg length.
#'   \cr   `nmax`   \tab Max `...` arg length.
#'   \cr            \tab   
#'   \cr  `nsame`   \tab Are `...` arg lengths the same?
#'   \cr    `nat`   \tab Length after \link[=av]{atomizing}.
#'   \cr            \tab   
#'   \cr     `nx`   \tab Length(s) of `...` args.
#'   \cr     `ns`   \tab Lengths of `...` args.
#'   \cr            \tab   
#'   \cr   `n_is`   \tab Do counts in `x` meet criteria in `n, min, max, eq`?
#' }
#' @param x \link[=innw]{non-negative whole-number} object.
#' @param ... One or more arguments to be examined for counts.
#' @param n Optional \link[=cmp_nnw_vec]{complete non-negative whole-number vec} of valid element, row, or column counts.
#' @param min Optional complete non-negative whole-number scalar giving minimum valid element, row, or column counts.
#' @param max Optional complete non-negative whole-number scalar giving maximum valid element, row, or column counts.
#' @param eq Non-`NA` scalar indicating whether all counts must be equal.
#' @param na Non-`NA` scalar whether `NA` values are allowed.
#' @param a Non-`NA` scalar indicating whether to \link[=av]{atomize} `...` to create a single atomic vector before processing. If `a = FALSE`, each argument in `...` is processed separately.
#' @param vals Optional \link[=atm_vec]{atomic vec} indicating specific values to be counted.
#' @param lt Optional \link[=cmp_srt_scl]{complete sortable scalar} indicating specific values elements of `...` arguments must be less than in order to be counted.
#' @param le Optional complete sortable scalar indicating specific values elements of `...` arguments must be less than or equal to in order to be counted
#' @param ge Optional complete sortable scalar indicating specific values elements of `...` arguments must be greater than or equal to in order to be counted.
#' @param gt Optional complete sortable scalar indicating specific values elements of `...` arguments must be greater than in order to be counted.
#' @return An integer or logical scalar or vector.
#' @export
#' @examples
#' N <- 0:15
#' n_is(N, n = 0:5)
#' n_is(N, min = 3, max = 12)
#' n_is(N, eq = T)
#' n_is(rep(0, 3), eq = T)
#' nx(letters, LETTERS, 0:9, NULL)
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
#' nt(0:99 %in% 50:59, 0:99 %in% 41:49)
#' nw(0:99 %in% 50:59, 0:99 %in% 41:49, n = 0:9)
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
n_is <- function(x, n = NULL, min = NULL, max = NULL, eq = FALSE) {
  errs <- c(f0(cmp_nnw(x)                  , NULL, "[x] must contain only non-negative whole numbers."),
            f0(inll(n) | cmp_nnw_vec(n)    , NULL, "[n] must be NULL or a non-negative whole-number vec (?cmp_nnw_vec)."),
            f0(inll(min) | cmp_nnw_scl(min), NULL, "[min] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl)."),
            f0(inll(max) | cmp_nnw_scl(max), NULL, "[max] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl)."),
            f0(isTF(eq)                    , NULL, "[eq] must be TRUE or FALSE."))
  if (!is.null(errs)) {stop(.errs(errs))}
  if (length(c(n, min, max)) == 0 & !eq) {return(x)}                             # if no restrictions are specified, return the raw counts
  ok.n <- f0(inll(n), T, all(x %in% n))                                          # whether exact argument length restriction is met
  ok.min <- f0(inll(min), T, all(x >= min))                                      # whether min argument length restriction is met
  ok.max <- f0(inll(max), T, all(x <= max))                                      # whether max argument length restriction is met
  ok.eq <- f0(eq, length(unique(x)) == 1, T)                                     # whether argument length equality restriction is met
  ok.n & ok.min & ok.max & ok.eq                                                 # whether all argument length restrictions are met
}

#' @rdname n_is
#' @export
nx <- function(..., n = NULL, min = NULL, max = NULL, eq = FALSE, a = FALSE, na = TRUE, vals = NULL, lt = NULL, le = NULL, ge = NULL, gt = NULL) {
  dots <- list(...)
  atoms <- av(dots)
  ok.d <- ...length() > 0
  ok.n <- inll(n) | cmp_nnw_vec(n)
  ok.min <- inll(min) | cmp_nnw_scl(min)
  ok.max <- inll(max) | cmp_nnw_scl(max)
  ok.mm <- f0(!ok.min | !ok.max, T, f0(inll(min) | inll(max), T, max >= min))
  ok.vals <- f0(is.null(vals), T, all(for (dot in dots) {compatible(dot, vals)}))
  ok.lt <- f0(is.null(vals), T, all(for (dot in dots) {comparable(lt, vals)}))
  ok.le <- f0(is.null(vals), T, all(for (dot in dots) {comparable(le, vals)}))
  ok.ge <- f0(is.null(vals), T, all(for (dot in dots) {comparable(ge, vals)}))
  ok.gt <- f0(is.null(vals), T, all(for (dot in dots) {comparable(gt, vals)}))
  ok.na <- f0(!isF(na), T, !any(is.na(atoms)))
  errs <- c(f0(ok.d    , NULL, "[...] is empty."),
            f0(isTF(a) , NULL, "[a] must be TRUE or FALSE."),
            f0(ok.n    , NULL, "[n] must be NULL or contain only non-negative whole numbers."),
            f0(ok.min  , NULL, "[min] must be NULL or a non-negative whole number."),
            f0(ok.max  , NULL, "[max] must be NULL or a non-negative whole number."),
            f0(ok.mm   , NULL, "[max] must be greater than or equal to [min]."),
            f0(isTF(eq), NULL, "[eq] must be TRUE or FALSE."),
            f0(isTF(na), NULL, "[na] must be TRUE or FALSE."),
            f0(ok.na   , NULL, "[na = FALSE] but arguments in [...] contains NA values."),
            f0(ok.vals , NULL, "[vals] must be NULL or compatible with arguments in [...]."),
            f0(ok.lt   , NULL, "[lt] must be NULL or comparable with arguments in [...]."),
            f0(ok.le   , NULL, "[le] must be NULL or comparable with arguments in [...]."),
            f0(ok.ge   , NULL, "[ge] must be NULL or comparable with arguments in [...]."),
            f0(ok.gt   , NULL, "[gt] must be NULL or comparable with arguments in [...]."))
  if (!is.null(errs)) {stop(.errs(errs))}
  if (a) {dots <- list(atoms)}
  for (i in 1:length(dots)) {
    dot <- dots[[i]]
    if (idef(vals)) {dot <- dot[dot %in% vals]}
    if (idef(lt)) {dot <- dot[dot <  lt]}
    if (idef(le)) {dot <- dot[dot <= le]}
    if (idef(ge)) {dot <- dot[dot >= ge]}
    if (idef(gt)) {dot <- dot[dot >  gt]}
    if (!is.null(dot)) {dots[[i]] <- dot}
  }
  n_is(lengths(dots, F), n = n, min = min, max = max, eq = eq)
}

#' @rdname n_is
#' @export
ns <- function(..., n = NULL, min = NULL, max = NULL, na = TRUE, vals = NULL, lt = NULL, le = NULL, ge = NULL, gt = NULL) {nx(..., n = n, min = min, max = max, na = na, a = F, vals = vals, lt = lt, le = le, ge = ge, gt = gt)}

#' @rdname n_is
#' @export
nmin <- function(..., na = TRUE, vals = NULL, lt = NULL, le = NULL, ge = NULL, gt = NULL) {min(nx(..., na = na, a = F, vals = vals, lt = lt, le = le, ge = ge, gt = gt))}

#' @rdname n_is
#' @export
nmax <- function(..., na = TRUE, vals = NULL, lt = NULL, le = NULL, ge = NULL, gt = NULL) {max(nx(..., na = na, a = F, vals = vals, lt = lt, le = le, ge = ge, gt = gt))}

#' @rdname n_is
#' @export
nsame <- function(..., min = NULL, max = NULL, na = TRUE, vals = NULL, lt = NULL, le = NULL, ge = NULL, gt = NULL) {nx(..., min = min, max = max, eq = T, na = na, a = F, vals = vals, lt = lt, le = le, ge = ge, gt = gt)}

#' @rdname n_is
#' @export
nw <- function(..., n = NULL, min = NULL, max = NULL, eq = FALSE, na = FALSE, a = TRUE) {
  dots <- list(...)
  atoms <- av(dots)
  errs <- c(f0(all(sapply(dots, pop_lgl))         , NULL, "[...] must contain only non-empty logical objects."),
            f0(isTF(a)                            , NULL, "[a] must be TRUE or FALSE."),
            f0(inll(n) | cmp_nnw_vec(n)           , NULL, "[n] must be NULL or a non-negative whole-number vec (?cmp_nnw_vec)."),
            f0(inll(min) | cmp_nnw_scl(min)       , NULL, "[min] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl)."),
            f0(inll(max) | cmp_nnw_scl(max)       , NULL, "[max] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl)."),
            f0(isTF(eq)                           , NULL, "[eq] must be TRUE or FALSE."),
            f0(isTF(na)                           , NULL, "[na] must be TRUE or FALSE."),
            f0(f0(!isF(na), T, !any(is.na(atoms))), NULL, "[na = FALSE] but arguments in [...] contains NA values."))
  if (!is.null(errs)) {stop(.errs(errs))}
  if (isT(a)) {dots <- list(atoms)}
  n_is(sapply(lapply(dots, which), length), n = n, min = min, max = max, eq = eq)
}

#' @rdname n_is
#' @export
nt <- nw

#' @rdname n_is
#' @export
nf <- function(..., n = NULL, min = NULL, max = NULL, eq = FALSE, na = FALSE, a = TRUE) {
  dots <- list(...)
  atoms <- av(dots)
  errs <- c(f0(all(sapply(dots, pop_lgl))         , NULL, "[...] must contain only non-empty logical objects."),
            f0(isTF(a)                            , NULL, "[a] must be TRUE or FALSE."),
            f0(inll(n) | cmp_nnw_vec(n)           , NULL, "[n] must be NULL or a non-negative whole-number vec (?cmp_nnw_vec)."),
            f0(inll(min) | cmp_nnw_scl(min)       , NULL, "[min] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl)."),
            f0(inll(max) | cmp_nnw_scl(max)       , NULL, "[max] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl)."),
            f0(isTF(eq)                           , NULL, "[eq] must be TRUE or FALSE."),
            f0(isTF(na)                           , NULL, "[na] must be TRUE or FALSE."),
            f0(f0(!isF(na), T, !any(is.na(atoms))), NULL, "[na = FALSE] but arguments in [...] contains NA values."))
  if (!is.null(errs)) {stop(.errs(errs))}
  if (isT(a)) {dots <- list(av(dots))}
  n_is(sapply(lapply(lapply(dots, not), which), length), n = n, min = min, max = max, eq = eq)
}

#' @rdname n_is
#' @export
nu <- function(..., n = NULL, min = NULL, max = NULL, eq = FALSE, na = TRUE, a = TRUE) {
  dots <- list(...)
  atoms <- av(dots)
  errs <- c(f0(all(sapply(dots, pop_lgl))         , NULL, "[...] must contain only non-empty logical objects."),
            f0(isTF(a)                            , NULL, "[a] must be TRUE or FALSE."),
            f0(inll(n) | cmp_nnw_vec(n)           , NULL, "[n] must be NULL or a non-negative whole-number vec (?cmp_nnw_vec)."),
            f0(inll(min) | cmp_nnw_scl(min)       , NULL, "[min] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl)."),
            f0(inll(max) | cmp_nnw_scl(max)       , NULL, "[max] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl)."),
            f0(isTF(eq)                           , NULL, "[eq] must be TRUE or FALSE."),
            f0(isTF(na)                           , NULL, "[na] must be TRUE or FALSE."),
            f0(f0(!isF(na), T, !any(is.na(atoms))), NULL, "[na = FALSE] but arguments in [...] contains NA values."))
  if (!is.null(errs)) {stop(.errs(errs))}
  if (isT(a)) {dots <- list(av(dots))}
  n_is(sapply(lapply(dots, unique), length), n = n, min = min, max = max, eq = eq)
}

#' @rdname n_is
#' @export
nuv <- nu

#' @rdname n_is
#' @export
nr <- function(..., n = NULL, min = NULL, max = NULL, eq = FALSE) {
  dots <- list(...)
  errs <- c(f0(all(sapply(dots, id2D))     , NULL, "[...] must contain only matrices or dtfs (?is_dtf)."),
            f0(inll(n) | cmp_nnw_vec(n)    , NULL, "[n] must be NULL or a non-negative whole-number vec (?cmp_nnw_vec)."),
            f0(inll(min) | cmp_nnw_scl(min), NULL, "[min] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl)."),
            f0(inll(max) | cmp_nnw_scl(max), NULL, "[max] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl)."),
            f0(isTF(eq)                    , NULL, "[eq] must be TRUE or FALSE."))
  if (!is.null(errs)) {stop(.errs(errs))}
  n_is(sapply(dots, nrow), n = n, min = min, max = max, eq = eq)
}

#' @rdname n_is
#' @export
nc <- function(..., n = NULL, min = NULL, max = NULL, eq = FALSE) {
  dots <- list(...)
  errs <- c(f0(all(sapply(dots, id2D))     , NULL, "[...] must contain only matrices or dtfs (?is_dtf)."),
            f0(inll(n) | cmp_nnw_vec(n)    , NULL, "[n] must be NULL or a non-negative whole-number vec (?cmp_nnw_vec)."),
            f0(inll(min) | cmp_nnw_scl(min), NULL, "[min] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl)."),
            f0(inll(max) | cmp_nnw_scl(max), NULL, "[max] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl)."),
            f0(isTF(eq)                    , NULL, "[eq] must be TRUE or FALSE."))
  if (!is.null(errs)) {stop(.errs(errs))}
  n_is(sapply(dots, ncol), n = n, min = min, max = max, eq = eq)
}

#' @rdname n_is
#' @export
nch <- function(..., n = NULL, min = NULL, max = NULL, eq = FALSE, na = FALSE, a = TRUE) {
  dots <- list(...)
  av.dots <- av(dots)
  errs <- c(f0(isTF(a)                        , NULL, "[a] must be TRUE or FALSE."),
            f0(all(sapply(dots, ichr))        , NULL, "[...] must contain only non-empty character objects."),
            f0(inll(n) | cmp_nnw_vec(n)       , NULL, "[n] must be NULL or a non-negative whole-number vec (?cmp_nnw_vec)."),
            f0(inll(min) | cmp_nnw_scl(min)   , NULL, "[min] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl)."),
            f0(inll(max) | cmp_nnw_scl(max)   , NULL, "[max] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl)."),
            f0(isTF(eq)                       , NULL, "[eq] must be TRUE or FALSE."),
            f0(isTF(na)                       , NULL, "[na] must be TRUE or FALSE."),
            f0(!isF(na) | !any(is.na(av.dots)), NULL, "[na = FALSE] but arguments in [...] contains NA values."))
  if (!is.null(errs)) {stop(.errs(errs))}
  if (isT(a)) {dots <- list(av.dots)}
  ns <- sapply(lapply(dots, ch), length)
  n_is(ns, n = n, min = min, max = max, eq = eq)
}

#' @rdname n_is
#' @export
nna <- function(..., n = NULL, min = NULL, max = NULL, eq = FALSE, a = TRUE) {
  dots <- list(...)
  av.dots <- av(dots)
  errs <- c(f0(isTF(a)                        , NULL, "[a] must be TRUE or FALSE."),
            f0(all(sapply(dots, ichr))        , NULL, "[...] must contain only non-empty character objects."),
            f0(inll(n) | cmp_nnw_vec(n)       , NULL, "[n] must be NULL or a non-negative whole-number vec (?cmp_nnw_vec)."),
            f0(inll(min) | cmp_nnw_scl(min)   , NULL, "[min] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl)."),
            f0(inll(max) | cmp_nnw_scl(max)   , NULL, "[max] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl)."),
            f0(isTF(eq)                       , NULL, "[eq] must be TRUE or FALSE."))
  if (!is.null(errs)) {stop(.errs(errs))}
  if (isT(a)) {dots <- list(av.dots)}
  ns <- sapply(lapply(dots, is.na), length)
  n_is(ns, n = n, min = min, max = max, eq = eq)
}

#' @rdname n_is
#' @export
nok <- function(..., n = NULL, min = NULL, max = NULL, eq = FALSE, a = TRUE) {
  dots <- list(...)
  av.dots <- av(dots)
  errs <- c(f0(isTF(a)                        , NULL, "[a] must be TRUE or FALSE."),
            f0(all(sapply(dots, ichr))        , NULL, "[...] must contain only non-empty character objects."),
            f0(inll(n) | cmp_nnw_vec(n)       , NULL, "[n] must be NULL or a non-negative whole-number vec (?cmp_nnw_vec)."),
            f0(inll(min) | cmp_nnw_scl(min)   , NULL, "[min] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl)."),
            f0(inll(max) | cmp_nnw_scl(max)   , NULL, "[max] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl)."),
            f0(isTF(eq)                       , NULL, "[eq] must be TRUE or FALSE."))
  if (!is.null(errs)) {stop(.errs(errs))}
  if (isT(a)) {dots <- list(av.dots)}
  ns <- sapply(lapply(lapply(dots, is.na), not), length)
  n_is(ns, n = n, min = min, max = max, eq = eq)
}

#' @rdname n_is
#' @export
nat <- function(..., n = NULL, min = NULL, max = NULL, eq = FALSE, a = TRUE) {nx(..., n = n, min = min, max = max, eq = eq, a = T)}

#' @rdname n_is
#' @export
n0 <- function(..., na = TRUE, a = TRUE) {nx(..., n = 0, na = na, a = a)}

#' @rdname n_is
#' @export
n1 <- function(..., na = TRUE, a = TRUE) {nx(..., n = 1, na = na, a = a)}

#' @rdname n_is
#' @export
n2 <- function(..., na = TRUE, a = TRUE) {nx(..., n = 2, na = na, a = a)}

#' @rdname n_is
#' @export
n3 <- function(..., na = TRUE, a = TRUE) {nx(..., n = 3, na = na, a = a)}

#' @rdname n_is
#' @export
n1p <- function(..., na = TRUE, eq = FALSE, a = TRUE) {nx(..., min = 1, na = na, a = a)}

#' @rdname n_is
#' @export
n2p <- function(..., na = TRUE, eq = FALSE, a = TRUE) {nx(..., min = 2, na = na, a = a)}

#' @rdname n_is
#' @export
n3p <- function(..., na = TRUE, eq = FALSE, a = TRUE) {nx(..., min = 3, na = na, a = a)}

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
