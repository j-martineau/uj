#' @name n_is
#' @encoding UTF-8
#' @family extensions
#' @family counts
#' @family dots
#' @title Dedicated counting functions
#' @description Counting functions can be used with a single unnamed argument, multiple unnamed arguments, and with restrictions in named arguments, in any combination. The functions in this family answer the questions described below.
#' \tabular{rl}{
#'      `nu, nuv`   \tab Number of unique atomic values.
#'   \cr `nw, nt`   \tab Number of `TRUE` values.
#'   \cr     `nf`   \tab Number of `FALSE` values.
#'   \cr    `nna`   \tab Number of atomic `NA` values.
#'   \cr    `nok`   \tab Number of atomic non-`NA` values.
#'   \cr    `nch`   \tab Number of characters in each element.
#'   \cr     `nc`   \tab Number of columns.
#'   \cr     `nr`   \tab Number of rows.
#'   \cr            \tab  
#'   \cr     `nd`   \tab Gets the number of `...` args.
#'   \cr    `nd0`   \tab Are there `0 ...` args?
#'   \cr    `nd1`   \tab Are there `1 ...` args?
#'   \cr    `nd2`   \tab Are there `2 ...` args?
#'   \cr    `nd3`   \tab Are there `3 ...` args?
#'   \cr   `nd1p`   \tab Are there `1+ ...` args?
#'   \cr   `nd2p`   \tab Are there `2+ ...` args?
#'   \cr   `nd3p`   \tab Are there `3+ ...` args?
#'   \cr            \tab  
#'   \cr     `n0`   \tab Is length `0`?
#'   \cr     `n1`   \tab Is length `1`?
#'   \cr     `n2`   \tab Is length `2`?
#'   \cr     `n3`   \tab Is length `3`?
#'   \cr    `n1p`   \tab Is length `1+`?
#'   \cr    `n2p`   \tab Is length `2+`?
#'   \cr    `n3p`   \tab Is length `3+`?
#'   \cr            \tab  
#'   \cr   `nmin`   \tab Min `...` arg length.
#'   \cr   `nmax`   \tab Max `...` arg length.
#'   \cr            \tab  
#'   \cr  `nsame`   \tab Are `...` arg lengths the same?
#'   \cr    `nat`   \tab Length after \link[=av]{atomizing}.
#'   \cr            \tab  
#'   \cr     `nx`   \tab Length(s) of `...` args.
#'   \cr     `ns`   \tab Lengths of `...` args.
#'   \cr            \tab  
#'   \cr   `n_is`   \tab Do counts meet criteria in `n, min, max, eq`?
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
#' @return May be non-negative integer or a logical scalar or vector, depending the properties of primary argument(s) (i.e., `x` and `...`) and optional arguments (i.e., all others).
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
  errs <- base::c(uj::f0(uj::cmp_nnw(x)                , NULL, "[x] must contain only non-negative whole numbers."),
            uj::f0(uj::inll(n) | uj::cmp_nnw_vec(n)    , NULL, "[n] must be NULL or a non-negative whole-number vec (?cmp_nnw_vec)."),
            uj::f0(uj::inll(min) | uj::cmp_nnw_scl(min), NULL, "[min] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl)."),
            uj::f0(uj::inll(max) | uj::cmp_nnw_scl(max), NULL, "[max] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl)."),
            uj::f0(uj::isTF(eq)                        , NULL, "[eq] must be TRUE or FALSE."))
  if (!base::is.null(errs)) {stop(uj::.errs(errs))}
  if (base::length(base::c(n, min, max)) == 0 & !eq) {return(x)}
  ok.n <- uj::f0(uj::inll(n), T, base::all(x %in% n))
  ok.min <- uj::f0(uj::inll(min), T, base::all(x >= min))
  ok.max <- uj::f0(uj::inll(max), T, base::all(x <= max))
  ok.eq <- uj::f0(eq, base::length(base::unique(x)) == 1, T)
  ok.n & ok.min & ok.max & ok.eq
}

#' @rdname n_is
#' @export
nx <- function(..., n = NULL, min = NULL, max = NULL, eq = FALSE, a = FALSE, na = TRUE, vals = NULL, lt = NULL, le = NULL, ge = NULL, gt = NULL) {
  dots <- base::list(...)
  atoms <- uj::av(dots)
  ok.d <- base::...length() > 0
  ok.n <- uj::inll(n) | uj::cmp_nnw_vec(n)
  ok.min <- uj::inll(min) | uj::cmp_nnw_scl(min)
  ok.max <- uj::inll(max) | uj::cmp_nnw_scl(max)
  ok.mm <- uj::f0(!ok.min | !ok.max, T, uj::f0(uj::inll(min) | uj::inll(max), T, max >= min))
  ok.vals <- uj::f0(base::is.null(vals), T, base::all(for (dot in dots) {uj::compatible(dot, vals)}))
  ok.lt <- uj::f0(base::is.null(vals), T, base::all(for (dot in dots) {uj::comparable(lt, vals)}))
  ok.le <- uj::f0(base::is.null(vals), T, base::all(for (dot in dots) {uj::comparable(le, vals)}))
  ok.ge <- uj::f0(base::is.null(vals), T, base::all(for (dot in dots) {uj::comparable(ge, vals)}))
  ok.gt <- uj::f0(base::is.null(vals), T, base::all(for (dot in dots) {uj::comparable(gt, vals)}))
  ok.na <- uj::f0(!uj::isF(na), T, !base::any(base::is.na(atoms)))
  errs <- base::c(uj::f0(ok.d        , NULL, "[...] is empty."),
                  uj::f0(uj::isTF(a) , NULL, "[a] must be TRUE or FALSE."),
                  uj::f0(ok.n        , NULL, "[n] must be NULL or contain only non-negative whole numbers."),
                  uj::f0(ok.min      , NULL, "[min] must be NULL or a non-negative whole number."),
                  uj::f0(ok.max      , NULL, "[max] must be NULL or a non-negative whole number."),
                  uj::f0(ok.mm       , NULL, "[max] must be greater than or equal to [min]."),
                  uj::f0(uj::isTF(eq), NULL, "[eq] must be TRUE or FALSE."),
                  uj::f0(uj::isTF(na), NULL, "[na] must be TRUE or FALSE."),
                  uj::f0(ok.na       , NULL, "[na = FALSE] but arguments in [...] contains NA values."),
                  uj::f0(ok.vals     , NULL, "[vals] must be NULL or compatible with arguments in [...]."),
                  uj::f0(ok.lt       , NULL, "[lt] must be NULL or comparable with arguments in [...]."),
                  uj::f0(ok.le       , NULL, "[le] must be NULL or comparable with arguments in [...]."),
                  uj::f0(ok.ge       , NULL, "[ge] must be NULL or comparable with arguments in [...]."),
                  uj::f0(ok.gt       , NULL, "[gt] must be NULL or comparable with arguments in [...]."))
  if (!base::is.null(errs)) {stop(uj:::.errs(errs))}
  if (a) {dots <- base::list(atoms)}
  for (i in 1:base::length(dots)) {
    dot <- dots[[i]]
    if (uj::idef(vals)) {dot <- dot[dot %in% vals]}
    if (uj::idef(lt)) {dot <- dot[dot <  lt]}
    if (uj::idef(le)) {dot <- dot[dot <= le]}
    if (uj::idef(ge)) {dot <- dot[dot >= ge]}
    if (uj::idef(gt)) {dot <- dot[dot >  gt]}
    if (!base::is.null(dot)) {dots[[i]] <- dot}
  }
  uj::n_is(base::lengths(dots, F), n = n, min = min, max = max, eq = eq)
}

#' @rdname n_is
#' @export
ns <- function(..., n = NULL, min = NULL, max = NULL, na = TRUE, vals = NULL, lt = NULL, le = NULL, ge = NULL, gt = NULL) {uj::nx(..., n = n, min = min, max = max, na = na, a = F, vals = vals, lt = lt, le = le, ge = ge, gt = gt)}

#' @rdname n_is
#' @export
nmin <- function(..., na = TRUE, vals = NULL, lt = NULL, le = NULL, ge = NULL, gt = NULL) {base::min(uj::nx(..., na = na, a = F, vals = vals, lt = lt, le = le, ge = ge, gt = gt))}

#' @rdname n_is
#' @export
nmax <- function(..., na = TRUE, vals = NULL, lt = NULL, le = NULL, ge = NULL, gt = NULL) {base::max(uj::nx(..., na = na, a = F, vals = vals, lt = lt, le = le, ge = ge, gt = gt))}

#' @rdname n_is
#' @export
nsame <- function(..., min = NULL, max = NULL, na = TRUE, vals = NULL, lt = NULL, le = NULL, ge = NULL, gt = NULL) {uj::nx(..., min = min, max = max, eq = T, na = na, a = F, vals = vals, lt = lt, le = le, ge = ge, gt = gt)}

#' @rdname n_is
#' @export
nw <- function(..., n = NULL, min = NULL, max = NULL, eq = FALSE, na = FALSE, a = TRUE) {
  dots <- list(...)
  atoms <- uj::av(dots)
  errs <- c(uj::f0(base::all(base::sapply(dots, pop_lgl))                 , NULL, "[...] must contain only non-empty logical objects."),
            uj::f0(uj::isTF(a)                                            , NULL, "[a] must be TRUE or FALSE."),
            uj::f0(uj::inll(n) | uj::cmp_nnw_vec(n)                       , NULL, "[n] must be NULL or a non-negative whole-number vec (?cmp_nnw_vec)."),
            uj::f0(uj::inll(min) | uj::cmp_nnw_scl(min)                   , NULL, "[min] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl)."),
            uj::f0(uj::inll(max) | uj::cmp_nnw_scl(max)                   , NULL, "[max] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl)."),
            uj::f0(uj::isTF(eq)                                           , NULL, "[eq] must be TRUE or FALSE."),
            uj::f0(uj::isTF(na)                                           , NULL, "[na] must be TRUE or FALSE."),
            uj::f0(uj::f0(!uj::isF(na), T, !base::any(base::is.na(atoms))), NULL, "[na = FALSE] but arguments in [...] contains NA values."))
  if (!base::is.null(errs)) {stop(uj:::.errs(errs))}
  if (uj::isT(a)) {dots <- base::list(atoms)}
  x <- uj::av(base::sapply(dots, function(x) {base::length(base::which(x))}))
  uj::n_is(x, n = n, min = min, max = max, eq = eq)
}

#' @rdname n_is
#' @export
nt <- nw

#' @rdname n_is
#' @export
nf <- function(..., n = NULL, min = NULL, max = NULL, eq = FALSE, na = FALSE, a = TRUE) {
  dots <- base::list(...)
  atoms <- uj::av(dots)
  errs <- base::c(f0(base::all(base::sapply(dots, uj::pop_lgl)), NULL, "[...] must contain only non-empty logical objects."),
                  uj::f0(uj::isTF(a)                           , NULL, "[a] must be TRUE or FALSE."),
                  uj::f0(uj::inll(n) | uj::cmp_nnw_vec(n)      , NULL, "[n] must be NULL or a non-negative whole-number vec (?cmp_nnw_vec)."),
                  uj::f0(uj::inll(min) | uj::cmp_nnw_scl(min)  , NULL, "[min] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl)."),
                  uj::f0(uj::inll(max) | uj::cmp_nnw_scl(max)  , NULL, "[max] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl)."),
                  uj::f0(uj::isTF(eq)                          , NULL, "[eq] must be TRUE or FALSE."),
                  uj::f0(uj::isTF(na)                          , NULL, "[na] must be TRUE or FALSE."),
                  uj::f0(uj::f0(!uj::isF(na), T, !base::any(base::is.na(atoms))), NULL, "[na = FALSE] but arguments in [...] contains NA values."))
  if (!base::is.null(errs)) {stop(uj:::.errs(errs))}
  if (uj::isT(a)) {dots <- base::list(uj::av(dots))}
  x <- uj::av(base::sapply(dots, function(x) {base::length(base::which(!x))}))
  uj::n_is(x, n = n, min = min, max = max, eq = eq)
}

#' @rdname n_is
#' @export
nu <- function(..., n = NULL, min = NULL, max = NULL, eq = FALSE, na = TRUE, a = TRUE) {
  dots <- base::list(...)
  atoms <- uj::av(dots)
  errs <- base::c(uj::f0(base::all(base::sapply(dots, uj::pop_lgl)), NULL, "[...] must contain only non-empty logical objects."),
                  uj::f0(uj::isTF(a)                               , NULL, "[a] must be TRUE or FALSE."),
                  uj::f0(uj::inll(n) | uj::cmp_nnw_vec(n)          , NULL, "[n] must be NULL or a non-negative whole-number vec (?cmp_nnw_vec)."),
                  uj::f0(uj::inll(min) | uj::cmp_nnw_scl(min)      , NULL, "[min] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl)."),
                  uj::f0(uj::inll(max) | uj::cmp_nnw_scl(max)      , NULL, "[max] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl)."),
                  uj::f0(uj::isTF(eq)                              , NULL, "[eq] must be TRUE or FALSE."),
                  uj::f0(uj::isTF(na)                              , NULL, "[na] must be TRUE or FALSE."),
                  uj::f0(uj::f0(!uj::isF(na), T, !base::any(base::is.na(atoms))), NULL, "[na = FALSE] but arguments in [...] contains NA values."))
  if (!base::is.null(errs)) {stop(uj:::.errs(errs))}
  if (uj::isT(a)) {dots <- base::list(uj::av(dots))}
  x <- uj::av(base::sapply(dots, function(x) {base::length(base::unique(x))}))
  uj::n_is(x, n = n, min = min, max = max, eq = eq)
}

#' @rdname n_is
#' @export
nuv <- nu

#' @rdname n_is
#' @export
nr <- function(..., n = NULL, min = NULL, max = NULL, eq = FALSE) {
  dots <- base::list(...)
  errs <- base::c(uj::f0(base::all(base::sapply(dots, uj::id2D)), NULL, "[...] must contain only matrices or dtfs (?is_dtf)."),
                  uj::f0(uj::inll(n) | uj::cmp_nnw_vec(n)       , NULL, "[n] must be NULL or a non-negative whole-number vec (?cmp_nnw_vec)."),
                  uj::f0(uj::inll(min) | uj::cmp_nnw_scl(min)   , NULL, "[min] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl)."),
                  uj::f0(uj::inll(max) | uj::cmp_nnw_scl(max)   , NULL, "[max] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl)."),
                  uj::f0(uj::isTF(eq)                           , NULL, "[eq] must be TRUE or FALSE."))
  if (!base::is.null(errs)) {stop(uj:::.errs(errs))}
  x <- uj::av(base::sapply(dots, nrow))
  uj::n_is(x, n = n, min = min, max = max, eq = eq)
}

#' @rdname n_is
#' @export
nc <- function(..., n = NULL, min = NULL, max = NULL, eq = FALSE) {
  dots <- base::list(...)
  errs <- base::c(uj::f0(base::all(base::sapply(dots, uj::id2D)), NULL, "[...] must contain only matrices or dtfs (?is_dtf)."),
                  uj::f0(uj::inll(n) | uj::cmp_nnw_vec(n)       , NULL, "[n] must be NULL or a non-negative whole-number vec (?cmp_nnw_vec)."),
                  uj::f0(uj::inll(min) | uj::cmp_nnw_scl(min)   , NULL, "[min] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl)."),
                  uj::f0(uj::inll(max) | uj::cmp_nnw_scl(max)   , NULL, "[max] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl)."),
                  uj::f0(uj::isTF(eq)                           , NULL, "[eq] must be TRUE or FALSE."))
  if (!base::is.null(errs)) {stop(uj:::.errs(errs))}
  x <- uj::av(base::sapply(dots, ncol))
  uj::n_is(x, n = n, min = min, max = max, eq = eq)
}

#' @rdname n_is
#' @export
nch <- function(..., n = NULL, min = NULL, max = NULL, eq = FALSE, na = FALSE, a = TRUE) {
  dots <- base::list(...)
  av.dots <- uj::av(dots)
  errs <- base::c(uj::f0(uj::isTF(a)                         , NULL, "[a] must be TRUE or FALSE."),
                  uj::f0(base::all(base::sapply(dots, ichr)) , NULL, "[...] must contain only non-empty character objects."),
                  uj::f0(uj::inll(n) | uj::cmp_nnw_vec(n)    , NULL, "[n] must be NULL or a non-negative whole-number vec (?cmp_nnw_vec)."),
                  uj::f0(uj::inll(min) | uj::cmp_nnw_scl(min), NULL, "[min] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl)."),
                  uj::f0(uj::inll(max) | uj::cmp_nnw_scl(max), NULL, "[max] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl)."),
                  uj::f0(uj::isTF(eq)                        , NULL, "[eq] must be TRUE or FALSE."),
                  uj::f0(uj::isTF(na)                        , NULL, "[na] must be TRUE or FALSE."),
                  uj::f0(!uj::isF(na) | !base::any(base::is.na(av.dots)), NULL, "[na = FALSE] but arguments in [...] contains NA values."))
  if (!base::is.null(errs)) {stop(uj:::.errs(errs))}
  if (uj::isT(a)) {dots <- base::list(av.dots)}
  x <- uj::av(base::sapply(dots, nchar))
  uj::n_is(x, n = n, min = min, max = max, eq = eq)
}

#' @rdname n_is
#' @export
nna <- function(..., n = NULL, min = NULL, max = NULL, eq = FALSE, a = TRUE) {
  dots <- base::list(...)
  av.dots <- uj::av(dots)
  errs <- base::c(uj::f0(uj::isTF(a)                            , NULL, "[a] must be TRUE or FALSE."),
                  uj::f0(base::all(base::sapply(dots, uj::ichr)), NULL, "[...] must contain only non-empty character objects."),
                  uj::f0(uj::inll(n) | uj::cmp_nnw_vec(n)       , NULL, "[n] must be NULL or a non-negative whole-number vec (?cmp_nnw_vec)."),
                  uj::f0(uj::inll(min) | uj::cmp_nnw_scl(min)   , NULL, "[min] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl)."),
                  uj::f0(uj::inll(max) | uj::cmp_nnw_scl(max)   , NULL, "[max] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl)."),
                  uj::f0(uj::isTF(eq)                           , NULL, "[eq] must be TRUE or FALSE."))
  if (!base::is.null(errs)) {stop(uj:::.errs(errs))}
  if (uj::isT(a)) {dots <- base::list(av.dots)}
  x <- uj::av(base::sapply(dots, function(x) {base::length(base::which(base::is.na(x)))}))
  uj::n_is(x, n = n, min = min, max = max, eq = eq)
}

#' @rdname n_is
#' @export
nok <- function(..., n = NULL, min = NULL, max = NULL, eq = FALSE, a = TRUE) {
  dots <- base::list(...)
  av.dots <- uj::av(dots)
  errs <- base::c(uj::f0(uj::isTF(a)                            , NULL, "[a] must be TRUE or FALSE."),
                  uj::f0(base::all(base::sapply(dots, uj::ichr)), NULL, "[...] must contain only non-empty character objects."),
                  uj::f0(uj::inll(n) | uj::cmp_nnw_vec(n)       , NULL, "[n] must be NULL or a non-negative whole-number vec (?cmp_nnw_vec)."),
                  uj::f0(uj::inll(min) | uj::cmp_nnw_scl(min)   , NULL, "[min] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl)."),
                  uj::f0(uj::inll(max) | uj::cmp_nnw_scl(max)   , NULL, "[max] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl)."),
                  uj::f0(uj::isTF(eq)                           , NULL, "[eq] must be TRUE or FALSE."))
  if (!base::is.null(errs)) {stop(uj:::.errs(errs))}
  if (uj::isT(a)) {dots <- base::list(av.dots)}
  x <- uj::av(base::sapply(dots, function(x) {base::length(base::which(!base::is.na(x)))}))
  uj::n_is(x, n = n, min = min, max = max, eq = eq)
}

#' @rdname n_is
#' @export
nat <- function(..., n = NULL, min = NULL, max = NULL, eq = FALSE, a = TRUE) {uj::nx(..., n = n, min = min, max = max, eq = eq, a = T)}

#' @rdname n_is
#' @export
n0 <- function(..., na = TRUE, a = TRUE) {uj::nx(..., n = 0, na = na, a = a)}

#' @rdname n_is
#' @export
n1 <- function(..., na = TRUE, a = TRUE) {uj::nx(..., n = 1, na = na, a = a)}

#' @rdname n_is
#' @export
n2 <- function(..., na = TRUE, a = TRUE) {uj::nx(..., n = 2, na = na, a = a)}

#' @rdname n_is
#' @export
n3 <- function(..., na = TRUE, a = TRUE) {uj::nx(..., n = 3, na = na, a = a)}

#' @rdname n_is
#' @export
n1p <- function(..., na = TRUE, eq = FALSE, a = TRUE) {uj::nx(..., min = 1, na = na, a = a)}

#' @rdname n_is
#' @export
n2p <- function(..., na = TRUE, eq = FALSE, a = TRUE) {uj::nx(..., min = 2, na = na, a = a)}

#' @rdname n_is
#' @export
n3p <- function(..., na = TRUE, eq = FALSE, a = TRUE) {uj::nx(..., min = 3, na = na, a = a)}

#' @rdname n_is
#' @export
nd <- function() {base::eval.parent(base::...length())}

#' @rdname n_is
#' @export
nd0 <- function() {base::eval.parent(base::...length() == 0)}

#' @rdname n_is
#' @export
nd1 <- function() {base::eval.parent(base::...length() == 1)}

#' @rdname n_is
#' @export
nd2 <- function() {base::eval.parent(base::...length() == 2)}

#' @rdname n_is
#' @export
nd3 <- function() {base::eval.parent(base::...length() == 3)}

#' @rdname n_is
#' @export
nd1p <- function() {base::eval.parent(base::...length() >= 1)}

#' @rdname n_is
#' @export
nd2p <- function() {base::eval.parent(base::...length() >= 2)}

#' @rdname n_is
#' @export
nd3p <- function() {base::eval.parent(base::...length() >= 3)}
