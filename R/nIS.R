#' @name nis
#' @encoding UTF-8
#' @family extensions
#' @family counts
#' @family dots
#' @title Dedicated counting functions
#' @description Counting functions can be used with a single unnamed argument, multiple unnamed arguments, and with restrictions in named arguments, in any combination. The functions in this family answer the questions described below.
#' \tabular{ll}{  `nu, nuv`   \tab Number of unique atomic values.                           \cr   \tab  }
#' \tabular{ll}{  `nw,nt`     \tab Number of `TRUE` values.                                  \cr
#'                `nf`        \tab Number of `FALSE` values.                                 \cr   \tab  }
#' \tabular{ll}{  `nna`       \tab Number of atomic `NA` values.                             \cr
#'                `nok`       \tab Number of atomic non-`NA` values.                         \cr   \tab  }
#' \tabular{ll}{  `nch`       \tab Number of characters in each element.                     \cr   \tab  }
#' \tabular{ll}{  `nc`        \tab Number of columns.                                        \cr
#'                `nr`        \tab Number of rows.                                           \cr   \tab  }
#' \tabular{ll}{  `nd`        \tab Gets the number of `...` args.                            \cr
#'                `nd0`       \tab Are there `0 ...` args?                                   \cr
#'                `nd1`       \tab Are there `1 ...` args?                                   \cr
#'                `nd2`       \tab Are there `2 ...` args?                                   \cr
#'                `nd3`       \tab Are there `3 ...` args?                                   \cr
#'                `nd1p`      \tab Are there `1+ ...` args?                                  \cr
#'                `nd2p`      \tab Are there `2+ ...` args?                                  \cr
#'                `nd3p`      \tab Are there `3+ ...` args?                                  \cr   \tab  }
#' \tabular{ll}{  `n0`        \tab Is length `0`?                                            \cr
#'                `n1`        \tab Is length `1`?                                            \cr
#'                `n2`        \tab Is length `2`?                                            \cr
#'                `n3`        \tab Is length `3`?                                            \cr
#'                `n1p`       \tab Is length `1+`?                                           \cr
#'                `n2p`       \tab Is length `2+`?                                           \cr
#'                `n3p`       \tab Is length `3+`?                                           \cr   \tab  }
#' \tabular{ll}{  `nmin`      \tab Min `...` arg length.                                     \cr
#'                `nmax`      \tab Max `...` arg length.                                     \cr   \tab  }
#' \tabular{ll}{  `nsame`     \tab Are `...` arg lengths the same?                           \cr   \tab  }
#' \tabular{ll}{  `nat`       \tab Length after \link[=av]{atomizing}.                       \cr   \tab  }
#' \tabular{ll}{  `nx`        \tab Length(s) of `...` args.                                  \cr   \tab  }
#' \tabular{ll}{  `ns`        \tab Lengths of `...` args.                                    \cr   \tab  }
#' \tabular{ll}{  `nis`       \tab Do counts meet criteria in `c(n, min, max, eq)` if any are specified? }
#' @param x \link[=NNW]{non-negative whole-number} object.
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
#' @examples
#' N <- 0:15
#' nis(N, n = 0:5)
#' nis(N, min = 3, max = 12)
#' nis(N, eq = T)
#' nis(rep(0, 3), eq = T)
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
#'
#' @export
nis <- function(x, n = NULL, min = NULL, max = NULL, eq = FALSE) {
  uj::errs_if_nots(uj::cmpNNW(x)                    , "[x] must contain only non-negative whole numbers."                       ,
                 uj::NLL(n) | uj::cmp_nnw_vec(n)    , "[n] must be NULL or a non-negative whole-number vec (?cmp_nnw_vec)."     ,
                 uj::NLL(min) | uj::cmp_nnw_scl(min), "[min] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl).",
                 uj::NLL(max) | uj::cmp_nnw_scl(max), "[max] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl).",
                 uj::isTF1(eq)                      , "[eq] must be TRUE or FALSE."                                             , PKG = "uj")
  if (!uj::N0(base::c(n, min, max)) | eq) {
    ok.n <- uj::f0(uj::null(n), T, uj::allIN(x, n))
    ok.eq <- uj::f0(eq, uj::NUV1(x), T)
    ok.min <- uj::f0(uj::NLL(min), T, base::all(x >= min))
    ok.max <- uj::f0(uj::NLL(max), T, base::all(x <= max))
    ok.n & ok.min & ok.max & ok.eq
  } else {x}
}

#' @rdname nis
#' @export
nx <- function(..., n = NULL, min = NULL, max = NULL, eq = FALSE, a = FALSE, na = TRUE, vals = NULL, lt = NULL, le = NULL, ge = NULL, gt = NULL) {
  dots <- base::list(...)
  atoms <- uj::av(dots)
  ok.d <- uj::ND1P()
  ok.n <- uj::NLL(n) | uj::cmp_nnw_vec(n)
  ok.min <- uj::NLL(min) | uj::cmp_nnw_scl(min)
  ok.max <- uj::NLL(max) | uj::cmp_nnw_scl(max)
  ok.mm <- uj::f0(!ok.min | !ok.max, T, uj::f0(uj::NLL(min) | uj::NLL(max), T, max >= min))
  ok.lt <- uj::f0(uj::NLL(vals), T, base::all(for (dot in dots) {uj::comparable(lt, vals)}))
  ok.le <- uj::f0(uj::NLL(vals), T, base::all(for (dot in dots) {uj::comparable(le, vals)}))
  ok.ge <- uj::f0(uj::NLL(vals), T, base::all(for (dot in dots) {uj::comparable(ge, vals)}))
  ok.gt <- uj::f0(uj::NLL(vals), T, base::all(for (dot in dots) {uj::comparable(gt, vals)}))
  ok.na <- uj::f0(uj::notF1(na), T, uj::noneNAS(atoms))
  ok.vals <- uj::f0(uj::NLL(vals), T, base::all(for (dot in dots) {uj::compatible(dot, vals)}))
  uj::errs_if_nots(ok.d         , "[...] is empty."                                             ,
                   uj::isTF1(a) , "[a] must be TRUE or FALSE."                                  ,
                   ok.n         , "[n] must be NULL or contain only non-negative whole numbers.",
                   uj::isTF1(eq), "[eq] must be TRUE or FALSE."                                 ,
                   uj::isTF1(na), "[na] must be TRUE or FALSE."                                 ,
                   ok.na        , "[na = FALSE] but arguments in [...] contains NA values."     ,
                   ok.lt        , "[lt] must be NULL or comparable with arguments in [...]."    ,
                   ok.le        , "[le] must be NULL or comparable with arguments in [...]."    ,
                   ok.ge        , "[ge] must be NULL or comparable with arguments in [...]."    ,
                   ok.gt        , "[gt] must be NULL or comparable with arguments in [...]."    ,
                   ok.min       , "[min] must be NULL or a non-negative whole number."          ,
                   ok.max       , "[max] must be NULL or a non-negative whole number."          ,
                   ok.mm        , "[max] must be greater than or equal to [min]."               ,
                   ok.vals      , "[vals] must be NULL or compatible with arguments in [...]."  , PKG = 'uj')
  if (a) {dots <- base::list(atoms)}
  for (i in 1:uj::N(dots)) {
    dot <- dots[[i]]
    if (uj::DEF(vals)) {dot <- dot[dot %in% vals]}
    if (uj::DEF(lt)) {dot <- dot[dot <  lt]}
    if (uj::DEF(le)) {dot <- dot[dot <= le]}
    if (uj::DEF(ge)) {dot <- dot[dot >= ge]}
    if (uj::DEF(gt)) {dot <- dot[dot >  gt]}
    if (uj::NLL(dot)) {dots[[i]] <- dot}
  }
  uj::nis(uj::NS(dots), n = n, min = min, max = max, eq = eq)
}

#' @rdname nis
#' @export
ns <- function(..., n = NULL, min = NULL, max = NULL, na = TRUE, vals = NULL, lt = NULL, le = NULL, ge = NULL, gt = NULL) {uj::nx(..., n = n, min = min, max = max, na = na, a = F, vals = vals, lt = lt, le = le, ge = ge, gt = gt)}

#' @rdname nis
#' @export
nmin <- function(..., na = TRUE, vals = NULL, lt = NULL, le = NULL, ge = NULL, gt = NULL) {base::min(uj::nx(..., na = na, a = F, vals = vals, lt = lt, le = le, ge = ge, gt = gt))}

#' @rdname nis
#' @export
nmax <- function(..., na = TRUE, vals = NULL, lt = NULL, le = NULL, ge = NULL, gt = NULL) {base::max(uj::nx(..., na = na, a = F, vals = vals, lt = lt, le = le, ge = ge, gt = gt))}

#' @rdname nis
#' @export
nsame <- function(..., min = NULL, max = NULL, na = TRUE, vals = NULL, lt = NULL, le = NULL, ge = NULL, gt = NULL) {uj::nx(..., min = min, max = max, eq = T, na = na, a = F, vals = vals, lt = lt, le = le, ge = ge, gt = gt)}

#' @rdname nis
#' @export
nw <- function(..., n = NULL, min = NULL, max = NULL, eq = FALSE, na = FALSE, a = TRUE) {
  dots <- list(...)
  atoms <- uj::av(dots)
  uj::errs_if_nots(base::all(base::sapply(dots, pop_lgl))      , "[...] must contain only non-empty logical objects."                      ,
                 uj::cmp_lgl_scl(a)                            , "[a] must be TRUE or FALSE."                                              ,
                 uj::NLL(n) | uj::cmp_nnw_vec(n)               , "[n] must be NULL or a non-negative whole-number vec (?cmp_nnw_vec)."     ,
                 uj::NLL(min) | uj::cmp_nnw_scl(min)           , "[min] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl).",
                 uj::NLL(max) | uj::cmp_nnw_scl(max)           , "[max] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl).",
                 uj::cmp_lgl_scl(eq)                           , "[eq] must be TRUE or FALSE."                                             ,
                 uj::cmp_lgl_scl(na)                           , "[na] must be TRUE or FALSE."                                             ,
                 uj::f0(uj::notF1(na), T, uj::noneNAS(atoms)), "[na = FALSE] but arguments in [...] contains NA values."                   , PKG = "uj")
  if (uj::isT1(a)) {dots <- base::list(atoms)}
  x <- uj::av(base::sapply(dots, function(x) {uj::NW(x)}))
  uj::nis(x, n = n, min = min, max = max, eq = eq)
}

#' @rdname nis
#' @export
nt <- nw

#' @rdname nis
#' @export
nf <- function(..., n = NULL, min = NULL, max = NULL, eq = FALSE, na = FALSE, a = TRUE) {
  dots <- base::list(...)
  atoms <- uj::av(dots)
  uj::errs_if_nots(base::all(base::sapply(dots, pop_lgl))    , "[...] must contain only non-empty logical objects."                      ,
                 uj::cmp_lgl_scl(a)                          , "[a] must be TRUE or FALSE."                                              ,
                 uj::NLL(n) | uj::cmp_nnw_vec(n)             , "[n] must be NULL or a non-negative whole-number vec (?cmp_nnw_vec)."     ,
                 uj::NLL(min) | uj::cmp_nnw_scl(min)         , "[min] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl).",
                 uj::NLL(max) | uj::cmp_nnw_scl(max)         , "[max] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl).",
                 uj::cmp_lgl_scl(eq)                         , "[eq] must be TRUE or FALSE."                                             ,
                 uj::cmp_lgl_scl(na)                         , "[na] must be TRUE or FALSE."                                             ,
                 uj::f0(uj::notF(na), T, uj::noneNA(atoms)), "[na = FALSE] but arguments in [...] contains NA values."                   , PKG = "uj")
  if (uj::isT1(a)) {dots <- base::list(atoms)}
  x <- uj::av(base::sapply(dots, function(x) uj::NW(!x)))
  uj::nis(x, n = n, min = min, max = max, eq = eq)
}

#' @rdname nis
#' @export
nu <- function(..., n = NULL, min = NULL, max = NULL, eq = FALSE, na = TRUE, a = TRUE) {
  dots <- base::list(...)
  atoms <- uj::av(dots)
  uj::errs_if_nots(base::all(base::sapply(dots, pop_lgl))       , "[...] must contain only non-empty logical objects."                    ,
                  uj::cmp_lgl_scl(a)                           , "[a] must be TRUE or FALSE."                                            ,
                  uj::NLL(n) | uj::cmp_nnw_vec(n)              , "[n] must be NULL or a non-negative whole-number vec (?cmp_nnw_vec)."     ,
                  uj::NLL(min) | uj::cmp_nnw_scl(min)          , "[min] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl).",
                  uj::NLL(max) | uj::cmp_nnw_scl(max)          , "[max] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl).",
                  uj::cmp_lgl_scl(eq)                          , "[eq] must be TRUE or FALSE."                                           ,
                  uj::cmp_lgl_scl(na)                          , "[na] must be TRUE or FALSE."                                           ,
                  uj::f0(uj::notF1(na), T, uj::noneNA(atoms)), "[na = FALSE] but arguments in [...] contains NA values."               , PKG = "uj")
  if (uj::isT1(a)) {dots <- base::list(atoms)}
  x <- uj::av(base::sapply(dots, function(x) uj::NW(x)))
  uj::nis(x, n = n, min = min, max = max, eq = eq)
}

#' @rdname nis
#' @export
nuv <- nu

#' @rdname nis
#' @export
nr <- function(..., n = NULL, min = NULL, max = NULL, eq = FALSE) {
  dots <- base::list(...)
  uj::errs_if_nots(uj::cmp_lgl_scl(a)                 , "[a] must be TRUE or FALSE."                                            ,
                 uj::NLL(n) | uj::cmp_nnw_vec(n)    , "[n] must be NULL or a non-negative whole-number vec (?cmp_nnw_vec)."     ,
                 uj::NLL(min) | uj::cmp_nnw_scl(min), "[min] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl).",
                 uj::NLL(max) | uj::cmp_nnw_scl(max), "[max] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl).",
                 uj::cmp_lgl_scl(eq)                , "[eq] must be TRUE or FALSE."                                           , PKG = "uj")
  x <- uj::av(base::sapply(dots, nrow))
  uj::nis(x, n = n, min = min, max = max, eq = eq)
}

#' @rdname nis
#' @export
nc <- function(..., n = NULL, min = NULL, max = NULL, eq = FALSE) {
  dots <- base::list(...)
  uj::errs_if_nots(uj::cmp_lgl_scl(a)                 , "[a] must be TRUE or FALSE."                                            ,
                 uj::NLL(n) | uj::cmp_nnw_vec(n)    , "[n] must be NULL or a non-negative whole-number vec (?cmp_nnw_vec)."     ,
                 uj::NLL(min) | uj::cmp_nnw_scl(min), "[min] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl).",
                 uj::NLL(max) | uj::cmp_nnw_scl(max), "[max] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl).",
                 uj::cmp_lgl_scl(eq)                , "[eq] must be TRUE or FALSE."                                           , PKG = "uj")
  x <- uj::av(base::sapply(dots, ncol))
  uj::nis(x, n = n, min = min, max = max, eq = eq)
}

#' @rdname nis
#' @export
nch <- function(..., n = NULL, min = NULL, max = NULL, eq = FALSE, na = FALSE, a = TRUE) {
  dots <- base::list(...)
  atoms <- uj::av(dots)
  uj::errs_if_nots(base::all(base::sapply(dots, pop_lgl))        , "[...] must contain only non-empty logical objects."                    ,
                   uj::cmp_lgl_scl(a)                           , "[a] must be TRUE or FALSE."                                            ,
                   uj::NLL(n) | uj::cmp_nnw_vec(n)              , "[n] must be NULL or a non-negative whole-number vec (?cmp_nnw_vec)."     ,
                   uj::NLL(min) | uj::cmp_nnw_scl(min)          , "[min] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl).",
                   uj::NLL(max) | uj::cmp_nnw_scl(max)          , "[max] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl).",
                   uj::cmp_lgl_scl(eq)                          , "[eq] must be TRUE or FALSE."                                           ,
                   uj::cmp_lgl_scl(na)                          , "[na] must be TRUE or FALSE."                                           ,
                   uj::f0(uj::notF1(na), T, uj::noneNA(atoms)), "[na = FALSE] but arguments in [...] contains NA values."               , PKG = "uj")
  if (uj::isT1(a)) {dots <- base::list(atoms)}
  x <- uj::av(base::sapply(dots, base::nchar))
  uj::nis(x, n = n, min = min, max = max, eq = eq)
}

#' @rdname nis
#' @export
nna <- function(..., n = NULL, min = NULL, max = NULL, eq = FALSE, a = TRUE) {
  dots <- base::list(...)
  atoms <- uj::av(dots)
  uj::errs_if_nots(base::all(base::sapply(dots, pop_lgl)), "[...] must contain only non-empty logical objects."                    ,
                   uj::cmp_lgl_scl(a)                   , "[a] must be TRUE or FALSE."                                            ,
                   uj::NLL(n) | uj::cmp_nnw_vec(n)      , "[n] must be NULL or a non-negative whole-number vec (?cmp_nnw_vec)."     ,
                   uj::NLL(min) | uj::cmp_nnw_scl(min)  , "[min] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl).",
                   uj::NLL(max) | uj::cmp_nnw_scl(max)  , "[max] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl).",
                   uj::cmp_lgl_scl(eq)                  , "[eq] must be TRUE or FALSE."                                           , PKG = "uj")
  if (uj::isT1(a)) {dots <- base::list(atoms)}
  x <- uj::av(base::sapply(dots, function(x) uj::NW(uj::na(x))))
  uj::nis(x, n = n, min = min, max = max, eq = eq)
}

#' @rdname nis
#' @export
nok <- function(..., n = NULL, min = NULL, max = NULL, eq = FALSE, a = TRUE) {
  dots <- base::list(...)
  atoms <- uj::av(dots)
  uj::errs_if_nots(base::all(base::sapply(dots, pop_lgl)), "[...] must contain only non-empty logical objects."                    ,
                   uj::cmp_lgl_scl(a)                   , "[a] must be TRUE or FALSE."                                            ,
                   uj::NLL(n) | uj::cmp_nnw_vec(n)      , "[n] must be NULL or a non-negative whole-number vec (?cmp_nnw_vec)."     ,
                   uj::NLL(min) | uj::cmp_nnw_scl(min)  , "[min] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl).",
                   uj::NLL(max) | uj::cmp_nnw_scl(max)  , "[max] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl).",
                   uj::cmp_lgl_scl(eq)                  , "[eq] must be TRUE or FALSE."                                           , PKG = "uj")
  if (uj::isT1(a)) {dots <- base::list(atoms)}
  x <- uj::av(base::sapply(dots, function(x) uj::NW(uj::ok(x))))
  uj::nis(x, n = n, min = min, max = max, eq = eq)
}

#' @rdname nis
#' @export
nat <- function(..., n = NULL, min = NULL, max = NULL, eq = FALSE, a = TRUE) {uj::nx(..., n = n, min = min, max = max, eq = eq, a = T)}

#' @rdname nis
#' @export
n0 <- function(..., na = TRUE, a = TRUE) {uj::nx(..., n = 0, na = na, a = a)}

#' @rdname nis
#' @export
n1 <- function(..., na = TRUE, a = TRUE) {uj::nx(..., n = 1, na = na, a = a)}

#' @rdname nis
#' @export
n2 <- function(..., na = TRUE, a = TRUE) {uj::nx(..., n = 2, na = na, a = a)}

#' @rdname nis
#' @export
n3 <- function(..., na = TRUE, a = TRUE) {uj::nx(..., n = 3, na = na, a = a)}

#' @rdname nis
#' @export
n1p <- function(..., na = TRUE, eq = FALSE, a = TRUE) {uj::nx(..., min = 1, na = na, a = a)}

#' @rdname nis
#' @export
n2p <- function(..., na = TRUE, eq = FALSE, a = TRUE) {uj::nx(..., min = 2, na = na, a = a)}

#' @rdname nis
#' @export
n3p <- function(..., na = TRUE, eq = FALSE, a = TRUE) {uj::nx(..., min = 3, na = na, a = a)}

#' @rdname nis
#' @export
nd <- function() {base::eval.parent(base::...length())}

#' @rdname nis
#' @export
nd0 <- function() {base::eval.parent(base::...length() == 0)}

#' @rdname nis
#' @export
nd1 <- function() {base::eval.parent(base::...length() == 1)}

#' @rdname nis
#' @export
nd2 <- function() {base::eval.parent(base::...length() == 2)}

#' @rdname nis
#' @export
nd3 <- function() {base::eval.parent(base::...length() == 3)}

#' @rdname nis
#' @export
nd1p <- function() {base::eval.parent(base::...length() >= 1)}

#' @rdname nis
#' @export
nd2p <- function() {base::eval.parent(base::...length() >= 2)}

#' @rdname nis
#' @export
nd3p <- function() {base::eval.parent(base::...length() >= 3)}
