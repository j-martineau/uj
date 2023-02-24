#' @encoding UTF-8
#' @family extensions
#' @family counts
#' @family dots
#' @title Dedicated counting functions
#' @details
#' Wrappers for counting functions are as follows:
#' \tabular{ll}{  `N`         \tab `length(x)`
#'                `nc`        \tab `NCOL(x)`                   \cr
#'                `nd`        \tab `...length()`               \cr
#'                `nf`        \tab `length(which(x == FALSE))` \cr
#'                `nr`        \tab `NROW(x)`                   \cr
#'                `ns`        \tab `lengths(x)`                \cr
#'                `nt`        \tab `length(which(x == TRUE))`  \cr
#'                `nu`        \tab `length(unique(x))`         \cr
#'                `nv`        \tab `length(atomize(x))`        \cr
#'                `nw`        \tab `length(which(x))`          \cr
#'                `neq`       \tab `length(x) == length(y)`    \cr
#'                `ndif`      \tab `length(x) != length(y)`    \cr
#'                `ndim`      \tab `length(dim(x))`              }
#' Each of these functions has associated count checking functions, constructed by appending the following suffixes to counting function names:
#' \tabular{ll}{  **suffix**   \tab **check for count** \cr
#'                `0`          \tab `= 0`               \cr
#'                `1`          \tab `= 1`               \cr
#'                `2`          \tab `= 2`               \cr
#'                `3`          \tab `= 3`               \cr
#'                `1p`         \tab `≥ 1`               \cr
#'                `2p`         \tab `≥ 2`               \cr
#'                `3p`         \tab `≥ 3`                 }
#' For example, for the function `N`, the count checking functions are `n0`, `n1`, `n2`, `n3`, `n1p`, `n2p`, and `n3p`.
#' \cr\cr Each count checking function has an associated negation function constructed by prepending `not_` to the name of count checking functions. For example, the negation functions associated with the count-checking functions of `N` are `not_n0`, `not_n1`, `not_n2`, ....
#' \cr\cr Each
#' }
#'                `nrc`       \tab `NROW(x) * NCOL(x)`         \cr
#'                
#'                `nx`        \tab Length(s) of `...` args.                               \cr
#'                `nna`       \tab Number of atomic `NA` values.                          \cr
#'                `nok`       \tab Number of atomic non-`NA` values.                      \cr
#'                `nch`       \tab Number of characters in each element.                  \cr
#'                `nMIN`      \tab Min `...` arg length.                                  \cr
#'                `nMAX`      \tab Max `...` arg length.                                  \cr
#'                `nEQ`       \tab Are `...` arg lengths the same?                        \cr
#'                `nV`        \tab Length after \link[=av]{atomizing}.                    \cr
#'                `nIS`       \tab Do counts meet criteria in `c(n, min, max, eq)` if any are specified? }
#' **Functions evaluating length/count (in)equality**
#' \tabular{ll}{  `NDIF`   \tab `length(x) != length(y)`              \cr
#'                `NEQ`    \tab `length(x) == length(y)` \cr   \tab     }
#' **Functions getting counts/lengths**
#' \tabular{ll}{  `N`                \tab `length(x)`                                  \cr   \tab   \cr
#'                `ND`               \tab `...length()`                                             \cr
#'                `NC`               \tab `NCOL`                                                    \cr
#'                `NR`               \tab `NROW`                                                    \cr
#'                `NS`               \tab `lengths(x)`                                              \cr
#'                `NU`               \tab `N(U(...))`                                               \cr
#'                `NV`               \tab `N(V(...))`                                               \cr
#'                `NW`               \tab `N(W(x))`                                                 \cr   \tab   \cr
#'                `NUV`              \tab `N(UV(...))`                                              \cr
#'                `NWV`              \tab `N(WV(....))`                                             \cr
#'                `NRC`              \tab `NR(x) * NC(x)`                              \cr   \tab   \cr
#'                `NDIM`             \tab `f0(null(x), 0, f0(is_vec(x), 1, N(dim(x))))`                }
#' **Functions checking for specific counts/lengths**
#' \cr\cr The functions described in the previous table also have convenience functions for checking for specific counts/lengths. Those convenience functions are formed by appending the following to function names:
#' \tabular{ll}{  `'0'`              \tab `N = 0`  \cr
#'                `'1'`              \tab `N = 1`  \cr
#'                `'2'`              \tab `N = 2`  \cr
#'                `'1P'`             \tab `N = 1+` \cr
#'                `'2P'`             \tab `N = 2+` \cr
#'                `'3P'`             \tab `N = 3+`   }
#' \cr The following tables gives examples for the functions `N` and `NRC`:
#' \tabular{lllllll}{
#'     **Base**       \tab **`N = 0`**     \tab **`N = 1`**     \tab **`N = 2`**     \tab **`N ≥ 1`**     \tab **`N ≥ 2`**     \tab **`N ≥ 3`**    \cr
#'     `N`            \tab `N0`            \tab `N1`            \tab `N2`            \tab `N1P`           \tab `N2P`           \tab `N3P`          \cr
#'     `ND`           \tab `ND0`           \tab `ND1`           \tab `ND2`           \tab `ND1P`          \tab `ND2P`          \tab `ND3P`         \cr
#'     `NC`           \tab `NC0`           \tab `NC1`           \tab `NC2`           \tab `NC1P`          \tab `NC2P`          \tab `NC3P`         \cr
#'     `NR`           \tab `NR0`           \tab `NR1`           \tab `NR2`           \tab `NR1P`          \tab `NR2P`          \tab `NR3P`         \cr
#'     `NS`           \tab `NS0`           \tab `NS1`           \tab `NS2`           \tab `NS1P`          \tab `NS2P`          \tab `NS3P`         \cr
#'     `NU`           \tab `NU0`           \tab `NU1`           \tab `NU2`           \tab `NU1P`          \tab `NU2P`          \tab `NU3P`         \cr
#'     `NV`           \tab `NV0`           \tab `NV1`           \tab `NV2`           \tab `NV1P`          \tab `NV2P`          \tab `NV3P`         \cr
#'     `NW`           \tab `NW0`           \tab `NW1`           \tab `NW2`           \tab `NW1P`          \tab `NW2P`          \tab `NW3P`         \cr
#'     `NUV`          \tab `NUV0`          \tab `NUV1`          \tab `NUV2`          \tab `NUV1P`         \tab `NUV2P`         \tab `NUV3P`        \cr
#'     `NWV`          \tab `NWV0`          \tab `NMV1`          \tab `NWV2`          \tab `NWV1P`         \tab `NWV2P`         \tab `NWV3P`        \cr
#'     `NRC`          \tab `NRC0`          \tab `NRC1`          \tab `NRC2`          \tab `NRC1P`         \tab `NRC2P`         \tab `NRC3P`        \cr
#'     `NDIM`         \tab `NDIM0`         \tab `NDIM1`         \tab `NDIM2`         \tab `NDIM1P`        \tab `NDIM2P`        \tab `NDIM3P`       \cr
#'                    \tab                 \tab                 \tab                 \tab                 \tab                 \tab                \cr
#'     **Base**       \tab **`N ≠ 0`**     \tab **`N ≠ 1`**     \tab **`N ≠ 2`**     \tab **`N < 1`**     \tab **`N < 2`**     \tab **`N < 3`**    \cr
#'     `N`            \tab `notN0`         \tab `notN1`         \tab `notN2`         \tab `notN1P`        \tab `notN2P`        \tab `notN3P`       \cr
#'     `ND`           \tab `notND0`        \tab `notND1`        \tab `notND2`        \tab `notND1P`       \tab `notND2P`       \tab `notND3P`      \cr
#'     `NC`           \tab `notNC0`        \tab `notNC1`        \tab `notNC2`        \tab `notNC1P`       \tab `notNC2P`       \tab `notNC3P`      \cr
#'     `NR`           \tab `notNR0`        \tab `notNR1`        \tab `notNR2`        \tab `notNR1P`       \tab `notNR2P`       \tab `notNR3P`      \cr
#'     `NS`           \tab `notNS0`        \tab `notNS1`        \tab `notNS2`        \tab `notNS1P`       \tab `notNS2P`       \tab `notNS3P`      \cr
#'     `NU`           \tab `notNU0`        \tab `notNU1`        \tab `notNU2`        \tab `notNU1P`       \tab `notNU2P`       \tab `notNU3P`      \cr
#'     `NV`           \tab `notNV0`        \tab `notNV1`        \tab `notNV2`        \tab `notNV1P`       \tab `notNV2P`       \tab `notNV3P`      \cr
#'     `NW`           \tab `notNW0`        \tab `notNW1`        \tab `notNW2`        \tab `notNW1P`       \tab `notNW2P`       \tab `notNW3P`      \cr
#'     `NUV`          \tab `notNUV0`       \tab `notNUV1`       \tab `notNUV2`       \tab `notNUV1P`      \tab `notNUV2P`      \tab `notNUV3P`     \cr
#'     `NWV`          \tab `notNWV0`       \tab `notNMV1`       \tab `notNWV2`       \tab `notNWV1P`      \tab `notNWV2P`      \tab `notNWV3P`     \cr
#'     `NRC`          \tab `notNRC0`       \tab `notNRC1`       \tab `notNRC2`       \tab `notNRC1P`      \tab `notNRC2P`      \tab `notNRC3P`     \cr
#'     `NDIM`         \tab `notNDIM0`      \tab `notNIM1`       \tab `notNDIM2`      \tab `notNDIM1P`     \tab `notNDIM2P`     \tab `notNDIM3P`      }
#  \cr
#' \cr Finally, the following table gives functions that check for specific *combinations* of numbers of rows and columns and associated negations:
#' \tabular{lllllll}{
#'                    \tab **`NC = 0`**    \tab **`NC = 1`**    \tab **`NC = 2`**    \tab **`NC ≥ 1`**    \tab **`NC ≥ 2`**    \tab **`NC ≥ 3`**   \cr
#'     **`nr = 0`**   \tab `nr0nc0`       \tab `nr0nc1`       \tab `nr0nc2`       \tab `nr0nc1p`      \tab `nr0nc2p`      \tab `nr0nc3p`     \cr
#'     **`nr = 1`**   \tab `nr1nc0`       \tab `nr1nc1`       \tab `nr1nc2`       \tab `nr1nc1p`      \tab `nr1nc2p`      \tab `nr1nc3p`     \cr
#'     **`nr = 2`**   \tab `nr2nc0`       \tab `nr2nc1`       \tab `nr2nc2`       \tab `nr2nc1p`      \tab `nr2nc2p`      \tab `nr2nc3p`     \cr
#'     **`nr ≥ 1`**   \tab `nr1pnc0`      \tab `nr1pnc1`      \tab `nr1pnc2`      \tab `nr1pnc1p`     \tab `nr1pnc2p`     \tab `nr1pnc3p`    \cr
#'     **`nr ≥ 2`**   \tab `nr2pnc0`      \tab `nr2pnc1`      \tab `nr2pnc2`      \tab `nr2pnc1p`     \tab `nr2pnc2p`     \tab `nr2pnc3p`    \cr
#'     **`nr ≥ 3`**   \tab `nr3pnc0`      \tab `nr3pnc1`      \tab `nr3pnc2`      \tab `nr3pnc1p`     \tab `nr3pnc2p`     \tab `nr3pnc3p`    \cr
#'                    \tab                 \tab                 \tab                 \tab                 \tab                 \tab                \cr
#'                    \tab **`NC ≠ 0`**    \tab **`NC ≠ 1`**    \tab **`NC ≠ 2`**    \tab **`NC < 1`**    \tab **`NC < 2`**    \tab **`NC < 3`**   \cr
#'     **`nr ≠ 0`**   \tab `notnr0nc0`    \tab `notnr0nc1`    \tab `notnr0nc2`    \tab `notnr0nc1p`   \tab `notnr0nc2p`   \tab `notnr0nc3p`  \cr
#'     **`nr ≠ 1`**   \tab `notnr1nc0`    \tab `notnr1nc1`    \tab `notnr1nc2`    \tab `notnr1nc1p`   \tab `notnr1nc2p`   \tab `notnr1nc3p`  \cr
#'     **`nr ≠ 2`**   \tab `notnr2nc0`    \tab `notnr2nc1`    \tab `notnr2nc2`    \tab `notnr2nc1p`   \tab `notnr2nc2p`   \tab `notnr2nc3p`  \cr
#'     **`nr < 1`**   \tab `notnr1pnc0`   \tab `notnr1pnc1`   \tab `notnr1pnc2`   \tab `notnr1pnc1p`  \tab `notnr1pnc2p`  \tab `notnr1pnc3p` \cr
#'     **`nr < 2`**   \tab `notnr2pnc0`   \tab `notnr2pnc1`   \tab `notnr2pnc2`   \tab `notnr2pnc1p`  \tab `notnr2pnc2p`  \tab `notnr2pnc3p` \cr
#'     **`nr < 3`**   \tab `notnr3pnc0`   \tab `notnr3pnc1`   \tab `notnr3pnc2`   \tab `notnr3pnc1p`  \tab `notnr3pnc2p`  \tab `notnr3pnc3p`   }#' @param x \link[=Nnw]{non-negative whole-number} object.
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
N <- function(x) {base::length(x)}

#' @rdname N
#' @export
nIS <- function(x, n = NULL, min = NULL, max = NULL, eq = FALSE) {
  uj::errs_if_nots(uj::cmpNnw(x)                    , "[x] must contain only non-negative whole numbers."                       ,
                 base::is.null(n) | uj::cmp_nnw_vec(n)    , "[n] must be NULL or a non-negative whole-number vec (?cmp_nnw_vec)."     ,
                 base::is.null(min) | uj::cmp_nnw_scl(min), "[min] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl).",
                 base::is.null(max) | uj::cmp_nnw_scl(max), "[max] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl).",
                 uj::isTF1(eq)                      , "[eq] must be TRUE or FALSE."                                             , PKG = "uj")
  if (!uj::N0(base::c(n, min, max)) | eq) {
    ok.n <- uj::f0(base::is.null(n), T, uj::allIN(x, n))
    ok.eq <- uj::f0(eq, uj::nuV1(x), T)
    ok.min <- uj::f0(base::is.null(min), T, base::all(x >= min))
    ok.max <- uj::f0(base::is.null(max), T, base::all(x <= max))
    ok.n & ok.min & ok.max & ok.eq
  } else {x}
}

#' @rdname N
#' @export
nX <- function(..., n = NULL, min = NULL, max = NULL, eq = FALSE, a = FALSE, na = TRUE, vals = NULL, lt = NULL, le = NULL, ge = NULL, gt = NULL) {
  dots <- base::list(...)
  atoms <- uj::av(dots)
  ok.d <- uj::ND1p()
  ok.n <- base::is.null(n) | uj::cmp_nnw_vec(n)
  ok.min <- base::is.null(min) | uj::cmp_nnw_scl(min)
  ok.max <- base::is.null(max) | uj::cmp_nnw_scl(max)
  ok.mm <- uj::f0(!ok.min | !ok.max, T, uj::f0(base::is.null(min) | base::is.null(max), T, max >= min))
  ok.lt <- uj::f0(base::is.null(vals), T, base::all(for (dot in dots) {uj::comparable(lt, vals)}))
  ok.le <- uj::f0(base::is.null(vals), T, base::all(for (dot in dots) {uj::comparable(le, vals)}))
  ok.ge <- uj::f0(base::is.null(vals), T, base::all(for (dot in dots) {uj::comparable(ge, vals)}))
  ok.gt <- uj::f0(base::is.null(vals), T, base::all(for (dot in dots) {uj::comparable(gt, vals)}))
  ok.na <- uj::f0(uj::notF1(na), T, uj::noneNAS(atoms))
  ok.vals <- uj::f0(base::is.null(vals), T, base::all(for (dot in dots) {uj::compatible(dot, vals)}))
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
    if (base::is.null(dot)) {dots[[i]] <- dot}
  }
  uj::nIS(uj::ns(dots), n = n, min = min, max = max, eq = eq)
}

#' @rdname N
#' @export
nS <- function(..., n = NULL, min = NULL, max = NULL, na = TRUE, vals = NULL, lt = NULL, le = NULL, ge = NULL, gt = NULL) {uj::nX(..., n = n, min = min, max = max, na = na, a = F, vals = vals, lt = lt, le = le, ge = ge, gt = gt)}

#' @rdname N
#' @export
nMIN <- function(..., na = TRUE, vals = NULL, lt = NULL, le = NULL, ge = NULL, gt = NULL) {base::min(uj::nX(..., na = na, a = F, vals = vals, lt = lt, le = le, ge = ge, gt = gt))}

#' @rdname N
#' @export
nMAX <- function(..., na = TRUE, vals = NULL, lt = NULL, le = NULL, ge = NULL, gt = NULL) {base::max(uj::nX(..., na = na, a = F, vals = vals, lt = lt, le = le, ge = ge, gt = gt))}

#' @rdname N
#' @export
nEQ <- function(..., min = NULL, max = NULL, na = TRUE, vals = NULL, lt = NULL, le = NULL, ge = NULL, gt = NULL) {uj::nX(..., min = min, max = max, eq = T, na = na, a = F, vals = vals, lt = lt, le = le, ge = ge, gt = gt)}

#' @rdname N
#' @export
nW <- function(..., n = NULL, min = NULL, max = NULL, eq = FALSE, na = FALSE, a = TRUE) {
  dots <- list(...)
  atoms <- uj::av(dots)
  uj::errs_if_nots(base::all(base::sapply(dots, pop_lgl))        , "[...] must contain only non-empty logical objects."                      ,
                   uj::cmp_lgl_scl(a)                            , "[a] must be TRUE or FALSE."                                              ,
                   base::is.null(n) | uj::cmp_nnw_vec(n)         , "[n] must be NULL or a non-negative whole-number vec (?cmp_nnw_vec)."     ,
                   base::is.null(min) | uj::cmp_nnw_scl(min)     , "[min] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl).",
                   base::is.null(max) | uj::cmp_nnw_scl(max)     , "[max] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl).",
                   uj::cmp_lgl_scl(eq)                           , "[eq] must be TRUE or FALSE."                                             ,
                   uj::cmp_lgl_scl(na)                           , "[na] must be TRUE or FALSE."                                             ,
                   uj::f0(uj::notF1(na), T, uj::noneNAS(atoms)), "[na = FALSE] but arguments in [...] contains NA values."                   , PKG = "uj")
  if (uj::isT1(a)) {dots <- base::list(atoms)}
  x <- uj::av(base::sapply(dots, function(x) {uj::nw(x)}))
  uj::nIS(x, n = n, min = min, max = max, eq = eq)
}

#' @rdname N
#' @export
nT <- nW

#' @rdname N
#' @export
nF <- function(..., n = NULL, min = NULL, max = NULL, eq = FALSE, na = FALSE, a = TRUE) {
  dots <- base::list(...)
  atoms <- uj::av(dots)
  uj::errs_if_nots(base::all(base::sapply(dots, pop_lgl))    , "[...] must contain only non-empty logical objects."                      ,
                   uj::cmp_lgl_scl(a)                        , "[a] must be TRUE or FALSE."                                              ,
                   base::is.null(n) | uj::cmp_nnw_vec(n)     , "[n] must be NULL or a non-negative whole-number vec (?cmp_nnw_vec)."     ,
                   base::is.null(min) | uj::cmp_nnw_scl(min) , "[min] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl).",
                   base::is.null(max) | uj::cmp_nnw_scl(max) , "[max] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl).",
                   uj::cmp_lgl_scl(eq)                       , "[eq] must be TRUE or FALSE."                                             ,
                   uj::cmp_lgl_scl(na)                       , "[na] must be TRUE or FALSE."                                             ,
                   uj::f0(uj::notF(na), T, uj::noneNA(atoms)), "[na = FALSE] but arguments in [...] contains NA values."                 , PKG = "uj")
  if (uj::isT1(a)) {dots <- base::list(atoms)}
  x <- uj::av(base::sapply(dots, function(x) uj::nw(!x)))
  uj::nIS(x, n = n, min = min, max = max, eq = eq)
}

#' @rdname N
#' @export
nU <- function(..., n = NULL, min = NULL, max = NULL, eq = FALSE, na = TRUE, a = TRUE) {
  dots <- base::list(...)
  atoms <- uj::av(dots)
  uj::errs_if_nots(base::all(base::sapply(dots, pop_lgl))     , "[...] must contain only non-empty logical objects."                      ,
                   uj::cmp_lgl_scl(a)                         , "[a] must be TRUE or FALSE."                                              ,
                   base::is.null(n) | uj::cmp_nnw_vec(n)      , "[n] must be NULL or a non-negative whole-number vec (?cmp_nnw_vec)."     ,
                   base::is.null(min) | uj::cmp_nnw_scl(min)  , "[min] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl).",
                   base::is.null(max) | uj::cmp_nnw_scl(max)  , "[max] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl).",
                   uj::cmp_lgl_scl(eq)                        , "[eq] must be TRUE or FALSE."                                             ,
                   uj::cmp_lgl_scl(na)                        , "[na] must be TRUE or FALSE."                                             ,
                   uj::f0(uj::notF1(na), T, uj::noneNA(atoms)), "[na = FALSE] but arguments in [...] contains NA values."                 , PKG = "uj")
  if (uj::isT1(a)) {dots <- base::list(atoms)}
  x <- uj::av(base::sapply(dots, function(x) uj::nw(x)))
  uj::nis(x, n = n, min = min, max = max, eq = eq)
}

#' @rdname N
#' @export
nR <- function(..., n = NULL, min = NULL, max = NULL, eq = FALSE) {
  dots <- base::list(...)
  uj::errs_if_nots(uj::cmp_lgl_scl(a)                       , "[a] must be TRUE or FALSE."                                              ,
                   base::is.null(n) | uj::cmp_nnw_vec(n)    , "[n] must be NULL or a non-negative whole-number vec (?cmp_nnw_vec)."     ,
                   base::is.null(min) | uj::cmp_nnw_scl(min), "[min] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl).",
                   base::is.null(max) | uj::cmp_nnw_scl(max), "[max] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl).",
                   uj::cmp_lgl_scl(eq)                      , "[eq] must be TRUE or FALSE."                                             , PKG = "uj")
  x <- uj::av(base::sapply(dots, nrow))
  uj::nIS(x, n = n, min = min, max = max, eq = eq)
}

#' @rdname N
#' @export
nC <- function(..., n = NULL, min = NULL, max = NULL, eq = FALSE) {
  dots <- base::list(...)
  uj::errs_if_nots(uj::cmp_lgl_scl(a)                     , "[a] must be TRUE or FALSE."                                                ,
                   base::is.null(n) | uj::cmp_nnw_vec(n)    , "[n] must be NULL or a non-negative whole-number vec (?cmp_nnw_vec)."     ,
                   base::is.null(min) | uj::cmp_nnw_scl(min), "[min] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl).",
                   base::is.null(max) | uj::cmp_nnw_scl(max), "[max] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl).",
                   uj::cmp_lgl_scl(eq)                      , "[eq] must be TRUE or FALSE."                                             , PKG = "uj")
  x <- uj::av(base::sapply(dots, ncol))
  uj::nIS(x, n = n, min = min, max = max, eq = eq)
}

#' @rdname N
#' @export
nCH <- function(..., n = NULL, min = NULL, max = NULL, eq = FALSE, na = FALSE, a = TRUE) {
  dots <- base::list(...)
  atoms <- uj::av(dots)
  uj::errs_if_nots(base::all(base::sapply(dots, pop_lgl))     , "[...] must contain only non-empty logical objects."                      ,
                   uj::cmp_lgl_scl(a)                         , "[a] must be TRUE or FALSE."                                              ,
                   base::is.null(n) | uj::cmp_nnw_vec(n)      , "[n] must be NULL or a non-negative whole-number vec (?cmp_nnw_vec)."     ,
                   base::is.null(min) | uj::cmp_nnw_scl(min)  , "[min] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl).",
                   base::is.null(max) | uj::cmp_nnw_scl(max)  , "[max] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl).",
                   uj::cmp_lgl_scl(eq)                        , "[eq] must be TRUE or FALSE."                                             ,
                   uj::cmp_lgl_scl(na)                        , "[na] must be TRUE or FALSE."                                             ,
                   uj::f0(uj::notF1(na), T, uj::noneNA(atoms)), "[na = FALSE] but arguments in [...] contains NA values."                 , PKG = "uj")
  if (uj::isT1(a)) {dots <- base::list(atoms)}
  x <- uj::av(base::sapply(dots, base::nchar))
  uj::nIS(x, n = n, min = min, max = max, eq = eq)
}

#' @rdname N
#' @export
nNA <- function(..., n = NULL, min = NULL, max = NULL, eq = FALSE, a = TRUE) {
  dots <- base::list(...)
  atoms <- uj::av(dots)
  uj::errs_if_nots(base::all(base::sapply(dots, pop_lgl))   , "[...] must contain only non-empty logical objects."                      ,
                   uj::cmp_lgl_scl(a)                       , "[a] must be TRUE or FALSE."                                              ,
                   base::is.null(n) | uj::cmp_nnw_vec(n)    , "[n] must be NULL or a non-negative whole-number vec (?cmp_nnw_vec)."     ,
                   base::is.null(min) | uj::cmp_nnw_scl(min), "[min] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl).",
                   base::is.null(max) | uj::cmp_nnw_scl(max), "[max] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl).",
                   uj::cmp_lgl_scl(eq)                      , "[eq] must be TRUE or FALSE."                                             , PKG = "uj")
  if (uj::isT1(a)) {dots <- base::list(atoms)}
  x <- uj::av(base::sapply(dots, function(x) uj::nw(uj::na(x))))
  uj::nIS(x, n = n, min = min, max = max, eq = eq)
}

#' @rdname N
#' @export
nOK <- function(..., n = NULL, min = NULL, max = NULL, eq = FALSE, a = TRUE) {
  dots <- base::list(...)
  atoms <- uj::av(dots)
  uj::errs_if_nots(base::all(base::sapply(dots, pop_lgl))   , "[...] must contain only non-empty logical objects."                      ,
                   uj::cmp_lgl_scl(a)                       , "[a] must be TRUE or FALSE."                                              ,
                   base::is.null(n) | uj::cmp_nnw_vec(n)    , "[n] must be NULL or a non-negative whole-number vec (?cmp_nnw_vec)."     ,
                   base::is.null(min) | uj::cmp_nnw_scl(min), "[min] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl).",
                   base::is.null(max) | uj::cmp_nnw_scl(max), "[max] must be NULL or a non-negative whole-number scalar (?cmp_nnw_scl).",
                   uj::cmp_lgl_scl(eq)                      , "[eq] must be TRUE or FALSE."                                             , PKG = "uj")
  if (uj::isT1(a)) {dots <- base::list(atoms)}
  x <- uj::av(base::sapply(dots, function(x) uj::nw(uj::ok(x))))
  uj::nIS(x, n = n, min = min, max = max, eq = eq)
}

#' @rdname N
#' @export
nV <- function(..., n = NULL, min = NULL, max = NULL, eq = FALSE, a = TRUE) {uj::NX(..., n = n, min = min, max = max, eq = eq, a = T)}

# length ####

#' @rdname N
#' @export
n0 <- function(x) {uj::N(x) == 0}

#' @rdname N
#' @export
n1 <- function(x) {uj::N(x) == 1}

#' @rdname N
#' @export
n2 <- function(x) {uj::N(x) == 2}

#' @rdname N
#' @export
n3 <- function(x) {uj::N(x) == 3}

#' @rdname N
#' @export
n1p <- function(x) {uj::N(x) > 0}

#' @rdname N
#' @export
n2p <- function(x) {uj::N(x) > 1}

#' @rdname N
#' @export
n3p <- function(x) {uj::N(x) > 2}

# n-dot-args ####

#' @rdname N
#' @export
nd <- function() {base::eval.parent(base::...length())}

#' @rdname N
#' @export
nd0 <- function() {base::eval.parent(base::...length() == 0)}

#' @rdname N
#' @export
nd1 <- function() {base::eval.parent(base::...length() == 1)}

#' @rdname N
#' @export
nd2 <- function() {base::eval.parent(base::...length() == 2)}

#' @rdname N
#' @export
nd3 <- function() {base::eval.parent(base::...length() == 3)}

#' @rdname N
#' @export
nd1p <- function() {base::eval.parent(base::...length() > 0)}

#' @rdname N
#' @export
nd2p <- function() {base::eval.parent(base::...length() > 1)}

#' @rdname N
#' @export
nd3p <- function() {base::eval.parent(base::...length() > 2)}

#' @rdname basics
#' @export
not_nd0 <- function() {base::eval.parent(base::parse(text = "base::...length()")) != 0}

#' @rdname basics
#' @export
not_nd1 <- function() {base::eval.parent(base::parse(text = "base::...length()")) != 1}

#' @rdname basics
#' @export
not_nd2 <- function() {base::eval.parent(base::parse(text = "base::...length()")) != 2}

#' @rdname basics
#' @export
not_nd3 <- function() {base::eval.parent(base::parse(text = "base::...length()")) != 3}

#' @rdname basics
#' @export
not_nd1p <- function() {!(base::eval.parent(base::parse(text = "base::...length()")) > 0)}

#' @rdname basics
#' @export
not_nd2p <- function() {!(base::eval.parent(base::parse(text = "base::...length()")) > 1)}

#' @rdname basics
#' @export
not_nd3p <- function() {!(base::eval.parent(base::parse(text = "base::...length()")) > 2)}

# n-unique values ####

#' @rdname basics
#' @export
nu <- function(x) {uj::N(uj::uv(x))}

#' @rdname basics
#' @export
nu0 <- function(x) {uj::nu(x) == 0}

#' @rdname basics
#' @export
nu1 <- function(x) {uj::nu(x) == 1}

#' @rdname basics
#' @export
nu2 <- function(x) {uj::nu(x) == 2}

#' @rdname basics
#' @export
nu3 <- function(x) {uj::nu(x) == 3}

#' @rdname basics
#' @export
nu1p <- function(x) {uj::nu(x) > 0}

#' @rdname basics
#' @export
nu2p <- function(x) {uj::nu(x) > 1}

#' @rdname basics
#' @export
nu3p <- function(x) {uj::nu(x) > 2}

#' @rdname basics
#' @export
not_nu0 <- function(x) {!uj::nu0(x)}

#' @rdname basics
#' @export
not_nu1 <- function(x) {!uj::nu1(x)}

#' @rdname basics
#' @export
not_nu2 <- function(x) {!uj::nu2(x)}

#' @rdname basics
#' @export
not_nu3 <- function(x) {!uj::nu3(x)}

#' @rdname basics
#' @export
not_nu1p <- function(x) {!uj::nu1p(x)}

#' @rdname basics
#' @export
not_nu2p <- function(x) {!uj::nu2p(x)}

#' @rdname basics
#' @export
not_nu3p <- function(x) {!uj::nu3p(x)}

# n_values (atomic) ####

#' @rdname basics
#' @export
nv <- function(...) {uj::N(uj::av(...))}

#' @rdname basics
#' @export
nv0 <- function(...) {uj::nv(...) == 0}

#' @rdname basics
#' @export
nv1 <- function(...) {uj::nv(...) == 1}

#' @rdname basics
#' @export
nv2 <- function(...) {uj::nv(...) == 2}

#' @rdname basics
#' @export
nv3 <- function(...) {uj::nv(...) == 3}

#' @rdname basics
#' @export
nv1p <- function(...) {uj::nv(...) > 0}

#' @rdname basics
#' @export
nv2p <- function(...) {uj::nv(...) > 1}

#' @rdname basics
#' @export
nv3p <- function(...) {uj::nv(...) > 2}

#' @rdname basics
#' @export
not_nv0 <- function(...) {!uj::nv0(...)}

#' @rdname basics
#' @export
not_nv1 <- function(...) {!uj::nv1(...)}

#' @rdname basics
#' @export
not_nv2 <- function(...) {!uj::nv2(...)}

#' @rdname basics
#' @export
not_nv3 <- function(...) {!uj::nv3(...)}

#' @rdname basics
#' @export
not_nv1p <- function(...) {!uj::nv1p(...)}

#' @rdname basics
#' @export
not_nv2p <- function(...) {!uj::nv2p(...)}

#' @rdname basics
#' @export
not_nv3p <- function(...) {!uj::nv3p(...)}

# nw ####

#' @rdname basics
#' @export
nw <- function(x) {uj::N(uj::wv(x))}

#' @rdname basics
#' @export
nw0 <- function(x) {uj::nw(x) == 0}

#' @rdname basics
#' @export
nw1 <- function(x) {uj::nw(x) == 1}

#' @rdname basics
#' @export
nw2 <- function(x) {uj::nw(x) == 2}

#' @rdname basics
#' @export
nw3 <- function(x) {uj::nw(x) == 3}

#' @rdname basics
#' @export
nw1p <- function(x) {uj::nw(x) > 0}

#' @rdname basics
#' @export
nw2p <- function(x) {uj::nw(x) > 1}

#' @rdname basics
#' @export
nw3p <- function(x) {uj::nw(x) > 2}

#' @rdname basics
#' @export
not_nw0 <- function(x) {!uj::nw0(x)}

#' @rdname basics
#' @export
not_nw1 <- function(x) {!uj::nw1(x)}

#' @rdname basics
#' @export
not_nw2 <- function(x) {!uj::nw2(x)}

#' @rdname basics
#' @export
not_nw3 <- function(x) {!uj::nw3(x)}

#' @rdname basics
#' @export
not_nw1p <- function(x) {!uj::nw1p(x)}

#' @rdname basics
#' @export
not_nw2p <- function(x) {!uj::nw2p(x)}

#' @rdname basics
#' @export
not_nw3p <- function(x) {!uj::nw3p(x)}

# equality ####

#' @rdname basics
#' @export
neq <- function(x, y) {uj::N(x) == uj::N(y)}

#' @rdname basics
#' @export
ndif <- function(x, y) {!uj::N(x) != N(y)}

# nr (nrows) ####

#' @rdname basics
#' @export
nr <- function(x) {base::NROW(x)}

#' @rdname basics
#' @export
nr0 <- function(x) {uj::nr(x) == 0}

#' @rdname basics
#' @export
nr1 <- function(x) {uj::nr(x) == 1}

#' @rdname basics
#' @export
nr2 <- function(x) {uj::nr(x) == 2}

#' @rdname basics
#' @export
nr3 <- function(x) {uj::nr(x) == 3}

#' @rdname basics
#' @export
nr1p <- function(x) {uj::nr(x) > 0}

#' @rdname basics
#' @export
nr2p <- function(x) {uj::nr(x) > 1}

#' @rdname basics
#' @export
nr3p <- function(x) {uj::nr(x) > 2}

#' @rdname basics
#' @export
not_nr0 <- function(x) {!uj::nr0(x)}

#' @rdname basics
#' @export
not_nr1 <- function(x) {!uj::nr1(x)}

#' @rdname basics
#' @export
not_nr2 <- function(x) {!uj::nr2(x)}

#' @rdname basics
#' @export
not_nr3 <- function(x) {!uj::nr3(x)}

#' @rdname basics
#' @export
not_nr1p <- function(x) {!uj::nr1p(x)}

#' @rdname basics
#' @export
not_nr2p <- function(x) {!uj::nr2p(x)}

#' @rdname basics
#' @export
not_nr3p <- function(x) {!uj::nr3p(x)}

# nc (ncols) ####

#' @rdname basics
#' @export
nc <- function(x) {base::NCOL(x)}

#' @rdname basics
#' @export
nc0 <- function(x) {uj::nc(x) == 0}

#' @rdname basics
#' @export
nc1 <- function(x) {uj::nc(x) == 1}

#' @rdname basics
#' @export
nc2 <- function(x) {uj::nc(x) == 2}

#' @rdname basics
#' @export
nc3 <- function(x) {uj::nc(x) == 3}

#' @rdname basics
#' @export
nc1p <- function(x) {uj::nc(x) > 0}

#' @rdname basics
#' @export
nc2p <- function(x) {uj::nc(x) > 1}

#' @rdname basics
#' @export
nc3p <- function(x) {uj::nc(x) > 2}

#' @rdname basics
#' @export
not_nc0 <- function(x) {!uj::nc0(x)}

#' @rdname basics
#' @export
not_nc1 <- function(x) {!uj::nc1(x)}

#' @rdname basics
#' @export
not_nc2 <- function(x) {!uj::nc2(x)}

#' @rdname basics
#' @export
not_nc3 <- function(x) {!uj::nc3(x)}

#' @rdname basics
#' @export
not_nc1p <- function(x) {!uj::nc1p(x)}

#' @rdname basics
#' @export
not_nc2p <- function(x) {!uj::nc2p(x)}

#' @rdname basics
#' @export
not_nc3p <- function(x) {!uj::nc3p(x)}

# nrc (nrows * ncols) ####

#' @rdname basics
#' @export
nrc <- function(x) {uj::nr(x) & uj::nc(x)}

#' @rdname basics
#' @export
nrc0 <- function(x) {uj::nrc(x) == 0}

#' @rdname basics
#' @export
nrc1 <- function(x) {uj::nrc(x) == 1}

#' @rdname basics
#' @export
nrc2p <- function(x) {uj::nrc(x) > 1}

#' @rdname basics
#' @export
not_nrc0 <- function(x) {uj::nrc(x) != 0}

#' @rdname basics
#' @export
not_nrc1 <- function(x) {uj::nrc(x) != 1}

#' @rdname basics
#' @export
not_nrc2p <- function(x) {uj::nrc(x) < 2}

# nr_nc (nrows, ncols) ####

#' @rdname basics
#' @export
nr0_nc0 <- function(x) {uj::nr0(x) & nc0(x)}

#' @rdname basics
#' @export
nr0_nc1 <- function(x) {uj::nr0(x) & nc1(x)}

#' @rdname basics
#' @export
nr0_nc2 <- function(x) {uj::nr0(x) & nc2(x)}

#' @rdname basics
#' @export
nr0_nc1p <- function(x) {uj::nr0(x) & nc1p(x)}

#' @rdname basics
#' @export
nr0_nc2p <- function(x) {uj::nr0(x) & nc2p(x)}

#' @rdname basics
#' @export
nr0_nc3p <- function(x) {uj::nr0(x) & nc3p(x)}

#' @rdname basics
#' @export
nr1_nc0 <- function(x) {uj::nr1(x) & nc0(x)}

#' @rdname basics
#' @export
nr1_nc1 <- function(x) {uj::nr1(x) & nc1(x)}

#' @rdname basics
#' @export
nr1_nc2 <- function(x) {uj::nr1(x) & nc2(x)}

#' @rdname basics
#' @export
nr1_nc1p <- function(x) {uj::nr1(x) & nc1p(x)}

#' @rdname basics
#' @export
nr1_nc2p <- function(x) {uj::nr1(x) & nc2p(x)}

#' @rdname basics
#' @export
nr1_nc3p <- function(x) {uj::nr1(x) & nc3p(x)}

#' @rdname basics
#' @export
nr2_nc0 <- function(x) {uj::nr2(x) & nc0(x)}

#' @rdname basics
#' @export
nr2_nc1 <- function(x) {uj::nr2(x) & nc1(x)}

#' @rdname basics
#' @export
nr2_nc2 <- function(x) {uj::nr2(x) & nc2(x)}

#' @rdname basics
#' @export
nr2_nc1p <- function(x) {uj::nr2(x) & nc1p(x)}

#' @rdname basics
#' @export
nr2_nc2p <- function(x) {uj::nr2(x) & nc2p(x)}

#' @rdname basics
#' @export
nr2_nc3p <- function(x) {uj::nr2(x) & nc3p(x)}

#' @rdname basics
#' @export
nr1p_nc0 <- function(x) {uj::nr1p(x) & nc0(x)}

#' @rdname basics
#' @export
nr1p_nc1 <- function(x) {uj::nr1p(x) & nc1(x)}

#' @rdname basics
#' @export
nr1p_nc2 <- function(x) {uj::nr1p(x) & nc2(x)}

#' @rdname basics
#' @export
nr1p_nc1p <- function(x) {uj::nr1p(x) & nc1p(x)}

#' @rdname basics
#' @export
nr1p_nc2p <- function(x) {uj::nr1p(x) & nc2p(x)}

#' @rdname basics
#' @export
nr1p_nc3p <- function(x) {uj::nr1p(x) & nc3p(x)}

#' @rdname basics
#' @export
nr2p_nc0 <- function(x) {uj::nr2p(x) & nc0(x)}

#' @rdname basics
#' @export
nr2p_nc1 <- function(x) {uj::nr2p(x) & nc1(x)}

#' @rdname basics
#' @export
nr2p_nc2 <- function(x) {uj::nr2p(x) & nc2(x)}

#' @rdname basics
#' @export
nr2p_nc1p <- function(x) {uj::nr2p(x) & nc1p(x)}

#' @rdname basics
#' @export
nr2p_nc2p <- function(x) {uj::nr2p(x) & nc2p(x)}

#' @rdname basics
#' @export
nr2p_nc3p <- function(x) {uj::nr2p(x) & nc3p(x)}

#' @rdname basics
#' @export
nr3p_nc0 <- function(x) {uj::nr3p(x) & nc0(x)}

#' @rdname basics
#' @export
nr3p_nc1 <- function(x) {uj::nr3p(x) & nc1(x)}

#' @rdname basics
#' @export
nr3p_nc2 <- function(x) {uj::nr3p(x) & nc2(x)}

#' @rdname basics
#' @export
nr3p_nc1p <- function(x) {uj::nr3p(x) & nc1p(x)}

#' @rdname basics
#' @export
nr3p_nc2p <- function(x) {uj::nr3p(x) & nc2p(x)}

#' @rdname basics
#' @export
nr3p_nc3p <- function(x) {uj::nr3p(x) & nc3p(x)}

#' @rdname basics
#' @export
not_nr0_nc0 <- function(x) {!uj::nr0_nc0(x)}

#' @rdname basics
#' @export
not_nr0_nc1 <- function(x) {!uj::nr0_nc1(x)}

#' @rdname basics
#' @export
not_nr0_nc2 <- function(x) {!uj::nr0_nc2(x)}

#' @rdname basics
#' @export
not_nr0_nc1p <- function(x) {!uj::nr0_nc1p(x)}

#' @rdname basics
#' @export
not_nr0_nc2p <- function(x) {!uj::nr0_nc2p(x)}

#' @rdname basics
#' @export
not_nr0_nc3p <- function(x) {!uj::nr0_nc3p(x)}

#' @rdname basics
#' @export
not_nr1_nc0 <- function(x) {!uj::nr1_nc0(x)}

#' @rdname basics
#' @export
not_nr1_nc1 <- function(x) {!uj::nr1_nc1(x)}

#' @rdname basics
#' @export
not_nr1_nc2 <- function(x) {!uj::nr1_nc2(x)}

#' @rdname basics
#' @export
not_nr1_nc1p <- function(x) {!uj::nr1_nc1p(x)}

#' @rdname basics
#' @export
not_nr1_nc2p <- function(x) {!uj::nr1_nc2p(x)}

#' @rdname basics
#' @export
not_nr1_nc3p <- function(x) {!uj::nr1_nc3p(x)}

#' @rdname basics
#' @export
not_nr2_nc0 <- function(x) {!uj::nr2_nc0(x)}

#' @rdname basics
#' @export
not_nr2_nc1 <- function(x) {!uj::nr2_nc1(x)}

#' @rdname basics
#' @export
not_nr2_nc2 <- function(x) {!uj::nr2_nc2(x)}

#' @rdname basics
#' @export
not_nr2_nc1p <- function(x) {!uj::nr2_nc1p(x)}

#' @rdname basics
#' @export
not_nr2_nc2p <- function(x) {!uj::nr2_nc2p(x)}

#' @rdname basics
#' @export
not_nr2_nc3p <- function(x) {!uj::nr2_nc3p(x)}

#' @rdname basics
#' @export
not_nr1p_nc0 <- function(x) {!uj::nr1p_nc0(x)}

#' @rdname basics
#' @export
not_nr1p_nc1 <- function(x) {!uj::nr1p_nc1(x)}

#' @rdname basics
#' @export
not_nr1p_nc2 <- function(x) {!uj::nr1p_nc2(x)}

#' @rdname basics
#' @export
not_nr1p_nc1p <- function(x) {!uj::nr1p_nc1p(x)}

#' @rdname basics
#' @export
not_nr1p_nc2p <- function(x) {!uj::nr1p_nc2p(x)}

#' @rdname basics
#' @export
not_nr1p_nc3p <- function(x) {!uj::nr1p_nc3p(x)}

#' @rdname basics
#' @export
not_nr2p_nc0 <- function(x) {!uj::nr2p_nc0(x)}

#' @rdname basics
#' @export
not_nr2p_nc1 <- function(x) {!uj::nr2p_nc1(x)}

#' @rdname basics
#' @export
not_nr2p_nc2 <- function(x) {!uj::nr2p_nc2(x)}

#' @rdname basics
#' @export
not_nr2p_nc1p <- function(x) {!uj::nr2p_nc1p(x)}

#' @rdname basics
#' @export
not_nr2p_nc2p <- function(x) {!uj::nr2p_nc2p(x)}

#' @rdname basics
#' @export
not_nr2p_nc3p <- function(x) {!uj::nr2p_nc3p(x)}

#' @rdname basics
#' @export
not_nr3p_nc0 <- function(x) {!uj::nr3p_nc0(x)}

#' @rdname basics
#' @export
not_nr3p_nc1 <- function(x) {!uj::nr3p_nc1(x)}

#' @rdname basics
#' @export
not_nr3p_nc2 <- function(x) {!uj::nr3p_nc2(x)}

#' @rdname basics
#' @export
not_nr3p_nc1p <- function(x) {!uj::nr3p_nc1p(x)}

#' @rdname basics
#' @export
not_nr3p_nc2p <- function(x) {!uj::nr3p_nc2p(x)}

#' @rdname basics
#' @export
not_nr3p_nc3p <- function(x) {!uj::nr3p_nc3p(x)}

# ndim ####

#' @rdname basics
#' @export
ndim <- function(x) {
  if (base::is.null(x)) {return(0)}
  D <- base::dim(x)
  if (base::is.null(D)) {1} else {uj::N(D)}
}

#' @rdname basics
#' @export
ndim0 <- function(x) {uj::ndim(x) == 0}

#' @rdname basics
#' @export
ndim1 <- function(x) {uj::ndim(x) == 1}

#' @rdname basics
#' @export
ndim2 <- function(x) {uj::ndim(x) == 2}

#' @rdname basics
#' @export
ndim3 <- function(x) {uj::ndim(x) == 3}

#' @rdname basics
#' @export
ndim1p <- function(x) {uj::ndim(x) > 0}

#' @rdname basics
#' @export
ndim2p <- function(x) {uj::ndim(x) > 1}

#' @rdname basics
#' @export
ndim3p <- function(x) {uj::ndim(x) > 2}

#' @rdname basics
#' @export
not_ndim0 <- function(x) {!uj::ndim0(x)}

#' @rdname basics
#' @export
not_ndim1 <- function(x) {!uj::ndim1(x)}

#' @rdname basics
#' @export
not_ndim2 <- function(x) {!uj::ndim2(x)}

#' @rdname basics
#' @export
not_ndim3 <- function(x) {!uj::ndim3(x)}

#' @rdname basics
#' @export
not_ndim1p <- function(x) {!uj::ndim1p(x)}

#' @rdname basics
#' @export
not_ndim2p <- function(x) {!uj::ndim2p(x)}

#' @rdname basics
#' @export
not_ndim3p <- function(x) {!uj::ndim3p(x)}

# ns (lengths) ####

#' @rdname basics
#' @export
ns <- function(x) {base::lengths(x)}

#' @rdname basics
#' @export
ns0 <- function(x) {uj::ns(x) == 0}

#' @rdname basics
#' @export
ns1 <- function(x) {uj::ns(x) == 1}

#' @rdname basics
#' @export
ns2 <- function(x) {uj::ns(x) == 2}

#' @rdname basics
#' @export
ns3 <- function(x) {uj::ns(x) == 3}

#' @rdname basics
#' @export
ns1p <- function(x) {uj::ns(x) > 0}

#' @rdname basics
#' @export
ns2p <- function(x) {uj::ns(x) > 1}

#' @rdname basics
#' @export
ns3p <- function(x) {uj::ns(x) > 2}

#' @rdname basics
#' @export
not_ns0 <- function(x) {!uj::ns0(x)}

#' @rdname basics
#' @export
not_ns1 <- function(x) {!uj::ns1(x)}

#' @rdname basics
#' @export
not_ns2 <- function(x) {!uj::ns2(x)}

#' @rdname basics
#' @export
not_ns3 <- function(x) {!uj::ns3(x)}

#' @rdname basics
#' @export
not_ns1p <- function(x) {!uj::ns1p(x)}

#' @rdname basics
#' @export
not_ns2p <- function(x) {!uj::ns2p(x)}

#' @rdname basics
#' @export
not_ns3p <- function(x) {!uj::ns3p(x)}
