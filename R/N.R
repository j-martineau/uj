# internals ####

.n_errs <- function(.n = NULL, .min = NULL, .max = NULL, .eq = F, .na = F, .a = F) {
  errs <- NULL
  if (!base::is.null(.n  ) & (!uj::.cmp_nnw_vec(.n  ))) {errs <- base::c(errs, "[.n] must be NULL or a complete non-negative whole-number vec (?cmp_nnw_vec).")}
  if (!base::is.null(.min) & (!uj::.cmp_nnw_scl(.min))) {errs <- base::c(errs, "[.min] must be NULL or a complete non-negative whole-number scalar (?cmp_nnw_scl).")}
  if (!base::is.null(.max) & (!uj::.cmp_nnw_scl(.max))) {errs <- base::c(errs, "[.max] must be NULL or a complete non-negative whole-number scalar (?cmp_nnw_scl).")}
  if (!uj::fs_tf(.eq)) {errs <- base::c(errs, "[.eq] must be TRUE or FALSE.")}
  if (!uj::fs_tf(.na)) {errs <- base::c(errs, "[.na] must be TRUE or FALSE.")}
  if (!uj::fs_tf(.a )) {errs <- base::c(errs, "[.a] must be TRUE or FALSE.")}
  errs
}

# externals ####

#' @encoding UTF-8
#' @family counts
#' @family dots
#' @title Dedicated counting functions
#' @details
#' The following functions are flexible, fully error-checked counting functions with optional count functionality driven by arguments:
#' \tabular{ll}{  `n_check`   \tab Do counts given in `x` meet criteria in `c(N, .min, .max, .eq)`, if any? If none, returns `x`. \cr   \tab   \cr
#'                `...n`      \tab Length(s) of `...` args with optional length validations.                                   \cr   \tab   \cr
#'                `...ns`     \tab Lengths of `...` args                                                                       \cr
#'                `...nv`     \tab Length after \link[=av]{atomizing}.                                                         \cr
#'                `...nch`    \tab Number of characters in each element.                                                       \cr
#'                `...nrc`    \tab `NROW(x) * NCOL(x)`                                                                         \cr
#'                `...nnav`   \tab Number of atomic `NA` values.                                                               \cr
#'                `...nokv`   \tab Number of atomic non-`NA` values.                                                           \cr
#'                `...neq`    \tab Are `...` arg lengths all the same?                                                         \cr
#'                `...ndif`   \tab Are `...` arg lengths *not* all the same?                                                   \cr
#'                `...minn`   \tab .min `...` arg length.                                                                       \cr
#'                `...maxn`   \tab .max `...` arg length.                                                                         }
#' **Functions evaluating length/count (in)equality**
#' \tabular{ll}{  `ndif`   \tab Lengths of `x` and `y` are equal.   \cr
#'                `neq`    \tab Lengths of `x` and `y` are different. }
#' **Functions getting counts/lengths**
#' \tabular{ll}{  `N`             \tab `length(x)`                    \cr   \tab   \cr
#'                `nd, ndots`     \tab `...length()`                               \cr
#'                `...n`          \tab `...length()`                  \cr   \tab   \cr
#'                `nc`            \tab `NCOL(x)`                                   \cr
#'                `nr`            \tab `NROW(x)`                      \cr   \tab   \cr
#'                `ns`            \tab `lengths(x)`                   \cr   \tab   \cr
#'                `nu, nunq`      \tab `length(unique(atoms(...)))`                \cr
#'                `nv, nval`      \tab `length(atoms(...))`                        \cr
#'                `nw`            \tab `length(which(x))`             \cr   \tab   \cr
#'                `nrc`           \tab `NROW(x) * NCOL(x)`            \cr   \tab   \cr
#'                `nnav`          \tab `length(which(is.na(atoms(x))))`            \cr
#'                `nokv`          \tab `length(which(!is.na(atoms(x))))`           \cr
#'                `ndim`          \tab `f0(null(x), 0, f0(is_vec(x), 1, N(dim(x))))` }
#' **Functions checking for specific counts/lengths**
#' \cr\cr The functions described in the previous table also have convenience functions for checking for specific counts/lengths and for negation. Those convenience functions are formed by appending the following to function names:
#' \tabular{ll}{  `'0'`    \tab `N = 0`  \cr
#'                `'1'`    \tab `N = 1`  \cr
#'                `'2'`    \tab `N = 2`  \cr
#'                `'1p'`   \tab `N = 1+` \cr
#'                `'2p'`   \tab `N = 2+` \cr
#'                `'3p'`   \tab `N = 3+`   }
#' The negation functions are formed by prepending `not_`. All convenience functions are given in the following table:
#' \tabular{lllllll}{
#'     **Base**     \tab **`N = 0`**  \tab **`N = 1`**  \tab **`N = 2`**  \tab **`N ≥ 1`**   \tab **`N ≥ 2`**   \tab **`N ≥ 3`** \cr
#'     `N`          \tab `n0`         \tab `n1`         \tab `n2`         \tab `n1p`         \tab `n2p`         \tab `n3p`       \cr
#'     `nd`         \tab `nd0`        \tab `nd1`        \tab `nd2`        \tab `nd1p`        \tab `nd2p`        \tab `nd3p`      \cr
#'     `...n`       \tab `...n0`      \tab `...n1`      \tab `...n2`      \tab `...n1p`      \tab `...n2p`      \tab `...n3p`    \cr
#'     `nc`         \tab `nc0`        \tab `nc1`        \tab `nc2`        \tab `nc1p`        \tab `nc2p`        \tab `nc3p`      \cr
#'     `nr`         \tab `nr0`        \tab `nr1`        \tab `nr2`        \tab `nr1p`        \tab `nr2p`        \tab `nr3p`      \cr
#'     `ns`         \tab `ns0`        \tab `ns1`        \tab `ns2`        \tab `ns1p`        \tab `ns2p`        \tab `ns3p`      \cr
#'     `nu`         \tab `nu0`        \tab `nu1`        \tab `nu2`        \tab `nu1p`        \tab `nu2p`        \tab `nu3p`      \cr
#'     `nv`         \tab `nv0`        \tab `nv1`        \tab `nv2`        \tab `nv1p`        \tab `nv2p`        \tab `nv3p`      \cr
#'     `nw`         \tab `nw0`        \tab `nw1`        \tab `nw2`        \tab `nw1p`        \tab `nw2p`        \tab `nw3p`      \cr
#'     `nrc`        \tab `nrc0`       \tab `nrc1`       \tab  —           \tab  —            \tab `nrc2p`       \tab  —          \cr
#'     `ndim`       \tab `ndim0`      \tab `ndim1`      \tab `ndim2`      \tab `ndim1p`      \tab `ndim2p`      \tab `ndim3p`    \cr
#'                  \tab              \tab              \tab              \tab               \tab               \tab             \cr
#'     **Base**     \tab **`N ≠ 0`**  \tab **`N ≠ 1`**  \tab **`N ≠ 2`**  \tab **`N < 1`**   \tab **`N < 2`**   \tab **`N < 3`** \cr
#'     `not_n`      \tab `not_n0`     \tab `not_n1`     \tab `not_n2`     \tab `not_n1p`     \tab `not_n2p`     \tab `not_n3p`   \cr
#'     `not_nd`     \tab `not_nd0`    \tab `not_nd1`    \tab `not_nd2`    \tab `not_nd1p`    \tab `not_nd2p`    \tab `not_nd3p`  \cr
#'     `...not_n`   \tab `...not_n0`  \tab `...not_n1`  \tab `...not_n2`  \tab `...not_n1p`  \tab `...not_n2p`  \tab `...not_n3p`\cr
#'     `not_nc`     \tab `not_nc0`    \tab `not_nc1`    \tab `not_nc2`    \tab `not_nc1p`    \tab `not_nc2p`    \tab `not_nc3p`  \cr
#'     `not_nr`     \tab `not_nr0`    \tab `not_nr1`    \tab `not_nr2`    \tab `not_nr1p`    \tab `not_nr2p`    \tab `not_nr3p`  \cr
#'     `not_ns`     \tab `not_ns0`    \tab `not_ns1`    \tab `not_ns2`    \tab `not_ns1p`    \tab `not_ns2p`    \tab `not_ns3p`  \cr
#'     `not_nu`     \tab `not_nu0`    \tab `not_nu1`    \tab `not_nu2`    \tab `not_nu1p`    \tab `not_nu2p`    \tab `not_nu3p`  \cr
#'     `not_nv`     \tab `not_nv0`    \tab `not_nv1`    \tab `not_nv2`    \tab `not_nv1p`    \tab `not_nv2p`    \tab `not_nv3p`  \cr
#'     `not_nw`     \tab `not_nw0`    \tab `not_nw1`    \tab `not_nw2`    \tab `not_nw1p`    \tab `not_nw2p`    \tab `not_nw3p`  \cr
#'     `not_nrc`    \tab `not_nrc0`   \tab `not_nrc1`   \tab  —           \tab  —            \tab `not_nrc2p`   \tab  —          \cr
#'     `not_ndim`   \tab `not_ndim0`  \tab `not_ndim1`  \tab `not_ndim2`  \tab `not_ndim1p`  \tab `not_ndim2p`  \tab `not_ndim3p`  }
#  Finally, the following table gives functions that check for specific *combinations* of numbers of rows and columns and associated negations:
#' \tabular{llllllll}{
#'                    \tab **`nc = 0`**    \tab **`nc = 1`**    \tab **`nc = 2`**    \tab **`nc = 3`**    \tab **`nc ≥ 1`**     \tab **`nc ≥ 2`**     \tab **`nc ≥ 3`**    \cr
#'     **`nr = 0`**   \tab `nr0_nc0`       \tab `nr0_nc1`       \tab `nr0_nc2`       \tab `nr0_nc3`       \tab `nr0_nc1p`       \tab `nr0_nc2p`       \tab `nr0_nc3p`      \cr
#'     **`nr = 1`**   \tab `nr1_nc0`       \tab `nr1_nc1`       \tab `nr1_nc2`       \tab `nr1_nc3`       \tab `nr1_nc1p`       \tab `nr1_nc2p`       \tab `nr1_nc3p`      \cr
#'     **`nr = 2`**   \tab `nr2_nc0`       \tab `nr2_nc1`       \tab `nr2_nc2`       \tab `nr2_nc3`       \tab `nr2_nc1p`       \tab `nr2_nc2p`       \tab `nr2_nc3p`      \cr
#'     **`nr = 3`**   \tab `nr3_nc0`       \tab `nr3_nc1`       \tab `nr3_nc2`       \tab `nr3_nc3`       \tab `nr3_nc1p`       \tab `nr3_nc2p`       \tab `nr3_nc3p`      \cr
#'     **`nr ≥ 1`**   \tab `nr1_pnc0`      \tab `nr1p_nc1`      \tab `nr1p_nc2`      \tab `nr1p_nc3`      \tab `nr1p_nc1p`      \tab `nr1p_nc2p`      \tab `nr1p_nc3p`     \cr
#'     **`nr ≥ 2`**   \tab `nr2p_nc0`      \tab `nr2p_nc1`      \tab `nr2p_nc2`      \tab `nr2p_nc3`      \tab `nr2p_nc1p`      \tab `nr2p_nc2p`      \tab `nr2p_nc3p`     \cr
#'     **`nr ≥ 3`**   \tab `nr3p_nc0`      \tab `nr3p_nc1`      \tab `nr3p_nc2`      \tab `nr3p_nc3`      \tab `nr3p_nc1p`      \tab `nr3p_nc2p`      \tab `nr3p_nc3p`     \cr
#'                    \tab                 \tab                 \tab                 \tab                 \tab                  \tab                  \tab                 \cr
#'                    \tab **`nc ≠ 0`**    \tab **`nc ≠ 1`**    \tab **`nc ≠ 3`**    \tab **`nc ≠ 2`**    \tab **`nc < 1`**     \tab **`nc < 2`**     \tab **`nc < 3`**    \cr
#'     **`nr ≠ 0`**   \tab `not_nr0_nc0`   \tab `not_nr0_nc1`   \tab `not_nr0_nc3`   \tab `not_nr0_nc2`   \tab `not_nr0_nc1p`   \tab `not_nr0_nc2p`   \tab `not_nr0_nc3p`  \cr
#'     **`nr ≠ 1`**   \tab `not_nr1_nc0`   \tab `not_nr1_nc1`   \tab `not_nr1_nc3`   \tab `not_nr1_nc2`   \tab `not_nr1_nc1p`   \tab `not_nr1_nc2p`   \tab `not_nr1_nc3p`  \cr
#'     **`nr ≠ 2`**   \tab `not_nr2_nc0`   \tab `not_nr2_nc1`   \tab `not_nr2_nc3`   \tab `not_nr2_nc2`   \tab `not_nr2_nc1p`   \tab `not_nr2_nc2p`   \tab `not_nr2_nc3p`  \cr
#'     **`nr ≠ 3`**   \tab `not_nr2_nc0`   \tab `not_nr2_nc1`   \tab `not_nr2_nc3`   \tab `not_nr2_nc2`   \tab `not_nr2_nc1p`   \tab `not_nr2_nc2p`   \tab `not_nr2_nc3p`  \cr
#'     **`nr < 1`**   \tab `not_nr1p_nc0`  \tab `not_nr1p_nc1`  \tab `not_nr1p_nc3`  \tab `not_nr1p_nc2`  \tab `not_nr1p_nc1p`  \tab `not_nr1p_nc2p`  \tab `not_nr1p_nc3p` \cr
#'     **`nr < 2`**   \tab `not_nr2p_nc0`  \tab `not_nr2p_nc1`  \tab `not_nr2p_nc3`  \tab `not_nr2p_nc2`  \tab `not_nr2p_nc1p`  \tab `not_nr2p_nc2p`  \tab `not_nr2p_nc3p` \cr
#'     **`nr < 3`**   \tab `not_nr3p_nc0`  \tab `not_nr3p_nc1`  \tab `not_nr3p_nc3`  \tab `not_nr3p_nc2`  \tab `not_nr3p_nc1p`  \tab `not_nr3p_nc2p`  \tab `not_nr3p_nc3p`   }
#' @param x For `n_check(x)`, a \link[=NNW]{non-negative whole-number} object giving counts. Otherwise, an arbitrary object.
#' @param ... One or more arguments to be examined for counts.
#' @param .n Optional \link[=cmp_nnw_vec]{complete non-negative whole-number vec} of valid element, row, or column counts.
#' @param .min Optional complete non-negative whole-number scalar giving minimum valid element, row, or column counts.
#' @param .max Optional complete non-negative whole-number scalar giving maximum valid element, row, or column counts.
#' @param .eq Non-`NA` scalar indicating whether all counts must be equal.
#' @param .na Non-`NA` scalar whether `NA` values are allowed.
#' @param .a Non-`NA` scalar indicating whether to \link[=av]{atomize} `...` to create a single atomic vector before processing. If `.a = F`, each argument in `...` is processed separately.
#' @param .vals Optional \link[=atm_vec]{atomic vec} indicating specific values to be counted.
#' @param .lt Optional \link[=cmp_srt_scl]{complete sortable scalar} indicating specific values elements of `...` arguments must be less than in order to be counted.
#' @param .le Optional complete sortable scalar indicating specific values elements of `...` arguments must be less than or equal to in order to be counted
#' @param .ge Optional complete sortable scalar indicating specific values elements of `...` arguments must be greater than or equal to in order to be counted.
#' @param .gt Optional complete sortable scalar indicating specific values elements of `...` arguments must be greater than in order to be counted.
#' @return May be non-negative integer or a logical scalar or vector, depending the properties of primary argument(s) (i.e., `x` and `...`) and optional arguments (i.e., all others).
#' @examples
#'
#' ##
#' ## DEFINE INITIAL VARIABLE SET
#' ##
#'
#' vAa <- c(letters, LETTERS)
#' va <- letters
#' vA <- LETTERS
#'
#' l0s <- c(0, 0, 0)
#' l0 <- NULL
#' l1 <- 0
#' l2 <- 0:1
#' l3 <- 0:2
#' l4 <- 0:3
#' l10 <- 0:9
#' l100 <- 0:99
#' l1000 <- sample(0:10, 1000, replace = T)
#'
#' vList <- list(l0 = l0, l1 = l1, l2 = l2, va = va, vAa = vAa)
#'
#' ##
#' ## DEMO [n_check] FUNCTION
#' ##
#'
#' n_check(length(va), .n = 26)
#' n_check(length(vA), .n = 52)
#' n_check(length(vAa), .n = 26:52)
#'
#' n_check(l10, .min = 5, .max = 10)
#' n_check(l10, .min = 11, .max = 11)
#'
#' n_check(l10, .eq = T)
#' n_check(l0s, .eq = T)
#'
#' ##
#' ## DEMO [nX], [nXp], [not_nX] AND [not_nXp] FUNCTIONS
#' ##
#'
#' c(n0(l0), n0(l0s))
#' c(n1(l1), n1(l0))
#' c(n2(l1), n2(l2))
#' c(n3(l2), n3(l3))
#'
#' c(n1p(l0), n1p(l1), n1p(l2))
#' c(n2p(l1), n2p(l2), n2p(l3))
#' c(n3p(l2), n3p(l3), n3p(l10))
#'
#' c(not_n0(l0), n0(l0s))
#' c(not_n1(l1), n1(l0))
#' c(not_n2(l1), n2(l2))
#' c(not_n3(l2), n3(l3))
#'
#' c(not_n1p(l0), not_n1p(l1), not_n1p(l2))
#' c(not_n2p(l1), not_n2p(l2), not_n2p(l3))
#' c(not_n3p(l2), not_n3p(l3), not_n3p(l10))
#'
#' ## DEMO [ndX], [ndXp], [not_ndX], and [not_ndXp] FUNCTIONS
#'
#' c(nd0(), nd0(l0), nd0(l0, l1))
#' c(nd1(), nd1(l0), nd1(l0, l1))
#' c(nd2(l1), nd2(l1, l2), nd2(l1, l2, l3))
#' c(nd3(l1, l2), nd3(l1, l2, l3), nd3(l1, l2, l3, l4))
#'
#' c(nd1p(), nd1p(l0), nd1p(l0, l1))
#' c(nd2p(l1), nd2p(l1, v2), nd2p(l1, l2, l3))
#' c(nd3p(l1, l2), nd3p(l1, l2, l3), nd3p(l1, l2, l3, l4))
#'
#' c(not_nd0(), not_nd0(l0), not_nd0(l0, l1))
#' c(not_nd1(), not_nd1(l0), not_nd1(l0, l1))
#' c(not_nd2(l1), not_nd2(l1, l2), not_nd2(l1, l2, l3))
#' c(not_nd3(l1, l2), not_nd3(l1, l2, l3), not_nd3(l1, l2, l3, l4))
#'
#' c(not_nd1p(), not_nd1p(l0), not_nd1p(l0, l1))
#' c(not_nd2p(l1), not_nd2p(l1, l2), not_nd2p(l1, l2, l3))
#' c(not_nd3p(l1, l2), not_nd3p(l1, l2, l3), not_nd3p(l1, l2, l3, l4))
#'
#' ##
#' ## DEMO [nu], [nuX], [nuXp], [not_nuX], AND [not_nuXp] FUNCTIONS
#' ##
#'
#' c(nu(va), nu(vA), nu(vAa), nu(l0), nu(l10), nu(l100), nu(l1000))
#'
#' c(nu0(l0), nu0(l1000))
#' c(nu1(l0), nu1(l1), nu2(l2))
#' c(nu2(l1), nu2(l2), nu2(l3))
#' c(nu3(l2), nu3(l3), nu3(l4))
#'
#' c(nu1p(l0), nu1p(l1), nu2p(l2))
#' c(nu2p(l1), nu2p(l2), nu2p(l3))
#' c(nu3p(l2), nu3p(l3), nu3p(l4))
#'
#' c(not_nu0(l0), not_nu0(l1000))
#' c(not_nu1(l0), not_nu1(l1), not_nu2(l2))
#' c(not_nu2(l1), not_nu2(l2), not_nu2(l3))
#' c(not_nu3(l2), not_nu3(l3), not_nu3(l4))
#'
#' c(not_nu1p(l0), not_nu1p(l1), not_nu2p(l2))
#' c(not_nu2p(l1), not_nu2p(l2), not_nu2p(l3))
#' c(not_nu3p(l2), not_nu3p(l3), not_nu3p(l4))
#'
#' ##
#' ## DEMO [nv], [nvX], [nvXp], [not_nvX], AND, [not_nvXp] FUNCTIONS
#' ##
#'
#' c(nv(l1000), nv(va), nv(vList))
#'
#' c(nv0(l0), nv0(l0s))
#' c(nv1(l1), nv1(l0))
#' c(nv2(l1), nv2(l2))
#' c(nv3(l2), nv3(l3))
#'
#' c(nv1p(l0), nv1p(l1), nv1p(l2))
#' c(nv2p(l1), nv2p(l2), nv2p(l3))
#' c(nv3p(l2), nv3p(l3), nv3p(l10))
#'
#' c(not_nv0(l0), nv0(l0s))
#' c(not_nv1(l1), nv1(l0))
#' c(not_nv2(l1), nv2(l2))
#' c(not_nv3(l2), nv3(l3))
#'
#' c(not_nv1p(l0), not_nv1p(l1), not_nv1p(l2))
#' c(not_nv2p(l1), not_nv2p(l2), not_nv2p(l3))
#' c(not_nv3p(l2), not_nv3p(l3), not_nv3p(l10))
#'
#' ##
#' ## DEFINE TRUE/FALSE VARIABLE SET
#' ##
#'
#' vTF0 <- c()
#' vTF1 <- c(T, F)
#' vTF2 <- c(T, T, F, F)
#' vTF3 <- c(T, T, T, F, F, F)
#' vTF4 <- c(T, T, T, T, F, F, F, F)
#' vTFx <- sample(vTF1, 1000, replace = T)
#'
#' ##
#' ## DEMO [nw], [nwX], [nwXp], [not_nwX], AND [not_nwXp] FUNCTIONS
#' ##
#'
#' c(nw(vTF1), nw(vTF2), nv(vTFx))
#'
#' c(nw0(vTF0), nw0(vTF1))
#' c(nw1(vTF0), nw1(vTF1), nw1(vTF2))
#' c(nw2(vTF1), nw2(vTF2), nw2(vTF3))
#' c(nw3(vTF2), nw3(vTF3), nw3(vTF4))
#'
#' c(nw1p(vTF0), nw1p(vTF1), nw1p(vTF2))
#' c(nw2p(vTF1), nw2p(vTF2), nw2p(vTF3))
#' c(nw3p(vTF2), nw3p(vTF3), nw3p(vTF4))
#'
#' c(not_nw0(vTF0), not_nw0(vTF1))
#' c(not_nw1(vTF0), not_nw1(vTF1), not_nw1(vTF2))
#' c(not_nw2(vTF1), not_nw2(vTF2), not_nw2(vTF3))
#' c(not_nw3(vTF2), not_nw3(vTF3), not_nw3(vTF4))
#'
#' c(not_nw1p(vTF0), not_nw1p(vTF1), not_nw1p(vTF2))
#' c(not_nw2p(vTF1), not_nw2p(vTF2), not_nw2p(vTF3))
#' c(not_nw3p(vTF2), not_nw3p(vTF3), not_nw3p(vTF4))
#'
#' c(neq(l3, l3), neq(l3, l0s), neq(l1, l2))
#' c(ndif(l3, l3), ndif(l3, l0s), ndif(l1, l2))
#'
#' ##
#' ## DEFINE MATRIX VARIABLE SET
#' ##
#'
#' m <- function(R, C) {matrix(1, nrow = R, ncol = C)}
#'
#' m00 <- m(0, 0); m01 <- m(0, 1); m02 <- m(0, 2); m03 <- m(0, 3); m04 <- m(0, 4)
#' m10 <- m(1, 0); m11 <- m(1, 1); m12 <- m(1, 2); m13 <- m(1, 3); m14 <- m(1, 4)
#' m20 <- m(2, 0); m21 <- m(2, 1); m22 <- m(2, 2); m23 <- m(2, 3); m24 <- m(2, 4)
#' m30 <- m(3, 0); m31 <- m(3, 1); m32 <- m(3, 2); m33 <- m(3, 3); m34 <- m(3, 4)
#' m40 <- m(4, 0); m41 <- m(4, 1); m42 <- m(4, 2); m43 <- m(4, 3); m44 <- m(4, 4)
#'
#' ##
#' ## DEMO [nr], [nc], AND [nrc] FUNCTIONS
#' ##
#'
#' c(nr(m00), nr(m10), nr(m20), nr(m30), nr(m40))
#' c(nc(m00), nc(m01), nc(m02), nc(m03), nc(m04))
#' c(nrc(m00), nrc(m11), nrc(m22), nrc(m33), nrc(m44))
#'
#' ##
#' ## DEMO [ncX], [ncXp], [not_ncX], AND [not_ncXp] FUNCTIONS
#' ##
#'
#' c(nc0(m00), nc0(m01))
#' c(nc1(m00), nc1(m01), nc1(m02))
#' c(nc2(m01), nc2(m02), nc2(m03))
#' c(nc3(m02), nc3(m03), nc3(m04))
#'
#' c(nc1p(m00), nc1p(m01), nc1p(m02))
#' c(nc2p(m01), nc2p(m02), nc2p(m03))
#' c(nc3p(m02), nc3p(m03), nc3p(m04))
#'
#' c(not_nc0(m00), not_nc0(m01))
#' c(not_nc1(m00), not_nc1(m01), not_nc1(m02))
#' c(not_nc2(m01), not_nc2(m02), not_nc2(m03))
#' c(not_nc3(m02), not_nc3(m03), not_nc3(m04))
#'
#' c(not_nc1p(m00), not_nc1p(m01), not_nc1p(m02))
#' c(not_nc2p(m01), not_nc2p(m02), not_nc2p(m03))
#' c(not_nc3p(m02), not_nc3p(m03), not_nc3p(m04))
#'
#' ##
#' ## DEMO [nrX], [nrXp], [not_nrX], AND [not_nrXp] FUNCTIONS
#' ##
#'
#' c(nr0(m00), nr0(m10))
#' c(nr1(m00), nr1(m10), nr1(m20))
#' c(nr2(m10), nr2(m20), nr2(m30))
#' c(nr3(m20), nr3(m30), nr3(m40))
#'
#' c(nr1p(m00), nr1p(m10), nr1p(m20))
#' c(nr2p(m10), nr2p(m20), nr2p(m30))
#' c(nr3p(m20), nr3p(m30), nr3p(m40))
#'
#' c(not_nr0(m00), not_nr0(m10))
#' c(not_nr1(m00), not_nr1(m10), not_nr1(m20))
#' c(not_nr2(m10), not_nr2(m20), not_nr2(m30))
#' c(not_nr3(m20), not_nr3(m30), not_nr3(m40))
#'
#' c(not_nr1p(m00), not_nr1p(m10), not_nr1p(m20))
#' c(not_nr2p(m10), not_nr2p(m20), not_nr2p(m30))
#' c(not_nr3p(m20), not_nr3p(m30), not_nr3p(m40))
#'
#' ##
#' ## DEMO [nrcX], [nrcXp], [not_nrcX], AND [not_nrcXp] FUNCTIONS
#' ##
#'
#' c(nrc0(m00), nrc0(m01), nrc0(m10), nrc0(m11))
#' c(nrc1(m00), nrc1(m10), nrc1(m11), nrc1(m21))
#' c(nrc2(m11), nrc2(m12), nrc2(m21), nrc2(m22))
#'
#' c(nrc1p(m00), nrc1p(m10), nrc1p(m11), nrc1p(m21))
#' c(nrc2p(m11), nrc2p(m12), nrc2p(m21), nrc2p(m22))
#'
#' c(nr0_nc0(m00), nr0_nc0(m01), nr0_nc0(m02), nr0_nc0(m03), nr0_nc0(m04))
#' c(nr0_nc0(m10), nr0_nc0(m11), nr0_nc0(m12), nr0_nc0(m13), nr0_nc0(m14))
#' c(nr0_nc0(m20), nr0_nc0(m21), nr0_nc0(m22), nr0_nc0(m23), nr0_nc0(m24))
#' c(nr0_nc0(m30), nr0_nc0(m31), nr0_nc0(m32), nr0_nc0(m33), nr0_nc0(m34))
#' c(nr0_nc0(m40), nr0_nc0(m41), nr0_nc0(m42), nr0_nc0(m43), nr0_nc0(m44))
#'
#' c(nr0_nc1(m00), nr0_nc1(m01), nr0_nc1(m02), nr0_nc1(m03), nr0_nc1(m04))
#' c(nr0_nc1(m10), nr0_nc1(m11), nr0_nc1(m12), nr0_nc1(m13), nr0_nc1(m14))
#' c(nr0_nc1(m20), nr0_nc1(m21), nr0_nc1(m22), nr0_nc1(m23), nr0_nc1(m24))
#' c(nr0_nc1(m30), nr0_nc1(m31), nr0_nc1(m32), nr0_nc1(m33), nr0_nc1(m34))
#' c(nr0_nc1(m40), nr0_nc1(m41), nr0_nc1(m42), nr0_nc1(m43), nr0_nc1(m44))
#'
#' c(nr0_nc2(m00), nr0_nc2(m01), nr0_nc2(m02), nr0_nc2(m03), nr0_nc2(m04))
#' c(nr0_nc2(m10), nr0_nc2(m11), nr0_nc2(m12), nr0_nc2(m13), nr0_nc2(m14))
#' c(nr0_nc2(m20), nr0_nc2(m21), nr0_nc2(m22), nr0_nc2(m23), nr0_nc2(m24))
#' c(nr0_nc2(m30), nr0_nc2(m31), nr0_nc2(m32), nr0_nc2(m33), nr0_nc2(m34))
#' c(nr0_nc2(m40), nr0_nc2(m41), nr0_nc2(m42), nr0_nc2(m43), nr0_nc2(m44))
#'
#' c(nr0_nc3(m00), nr0_nc3(m01), nr0_nc3(m02), nr0_nc3(m03), nr0_nc3(m04))
#' c(nr0_nc3(m10), nr0_nc3(m11), nr0_nc3(m12), nr0_nc3(m13), nr0_nc3(m14))
#' c(nr0_nc3(m20), nr0_nc3(m21), nr0_nc3(m22), nr0_nc3(m23), nr0_nc3(m24))
#' c(nr0_nc3(m30), nr0_nc3(m31), nr0_nc3(m32), nr0_nc3(m33), nr0_nc3(m34))
#' c(nr0_nc3(m40), nr0_nc3(m41), nr0_nc3(m42), nr0_nc3(m43), nr0_nc3(m44))
#'
#' c(nr1_nc0(m00), nr1_nc0(m01), nr1_nc0(m02), nr1_nc0(m03), nr1_nc0(m04))
#' c(nr1_nc0(m10), nr1_nc0(m11), nr1_nc0(m12), nr1_nc0(m13), nr1_nc0(m14))
#' c(nr1_nc0(m20), nr1_nc0(m21), nr1_nc0(m22), nr1_nc0(m23), nr1_nc0(m24))
#' c(nr1_nc0(m30), nr1_nc0(m31), nr1_nc0(m32), nr1_nc0(m33), nr1_nc0(m34))
#' c(nr1_nc0(m40), nr1_nc0(m41), nr1_nc0(m42), nr1_nc0(m43), nr1_nc0(m44))
#'
#' c(nr2_nc0(m00), nr2_nc0(m01), nr2_nc0(m02), nr2_nc0(m03), nr2_nc0(m04))
#' c(nr2_nc0(m10), nr2_nc0(m11), nr2_nc0(m12), nr2_nc0(m13), nr2_nc0(m14))
#' c(nr2_nc0(m20), nr2_nc0(m21), nr2_nc0(m22), nr2_nc0(m23), nr2_nc0(m24))
#' c(nr2_nc0(m30), nr2_nc0(m31), nr2_nc0(m32), nr2_nc0(m33), nr2_nc0(m34))
#' c(nr2_nc0(m40), nr2_nc0(m41), nr2_nc0(m42), nr2_nc0(m43), nr2_nc0(m44))
#'
#' c(nr3_nc0(m00), nr3_nc0(m01), nr3_nc0(m02), nr3_nc0(m03), nr3_nc0(m04))
#' c(nr3_nc0(m10), nr3_nc0(m11), nr3_nc0(m12), nr3_nc0(m13), nr3_nc0(m14))
#' c(nr3_nc0(m20), nr3_nc0(m21), nr3_nc0(m22), nr3_nc0(m23), nr3_nc0(m24))
#' c(nr3_nc0(m30), nr3_nc0(m31), nr3_nc0(m32), nr3_nc0(m33), nr3_nc0(m34))
#' c(nr3_nc0(m40), nr3_nc0(m41), nr3_nc0(m42), nr3_nc0(m43), nr3_nc0(m44))
#'
#' c(nr1_nc1(m00), nr1_nc1(m01), nr1_nc1(m02), nr1_nc1(m03), nr1_nc1(m04))
#' c(nr1_nc1(m10), nr1_nc1(m11), nr1_nc1(m12), nr1_nc1(m13), nr1_nc1(m14))
#' c(nr1_nc1(m20), nr1_nc1(m21), nr1_nc1(m22), nr1_nc1(m23), nr1_nc1(m24))
#' c(nr1_nc1(m30), nr1_nc1(m31), nr1_nc1(m32), nr1_nc1(m33), nr1_nc1(m34))
#' c(nr1_nc1(m40), nr1_nc1(m41), nr1_nc1(m42), nr1_nc1(m43), nr1_nc1(m44))
#'
#' c(nr1_nc2(m00), nr1_nc2(m01), nr1_nc2(m02), nr1_nc2(m03), nr1_nc2(m04))
#' c(nr1_nc2(m10), nr1_nc2(m11), nr1_nc2(m12), nr1_nc2(m13), nr1_nc2(m14))
#' c(nr1_nc2(m20), nr1_nc2(m21), nr1_nc2(m22), nr1_nc2(m23), nr1_nc2(m24))
#' c(nr1_nc2(m30), nr1_nc2(m31), nr1_nc2(m32), nr1_nc2(m33), nr1_nc2(m34))
#' c(nr1_nc2(m40), nr1_nc2(m41), nr1_nc2(m42), nr1_nc2(m43), nr1_nc2(m44))
#'
#' c(nr1_nc3(m00), nr1_nc3(m01), nr1_nc3(m02), nr1_nc3(m03), nr1_nc3(m04))
#' c(nr1_nc3(m10), nr1_nc3(m11), nr1_nc3(m12), nr1_nc3(m13), nr1_nc3(m14))
#' c(nr1_nc3(m20), nr1_nc3(m21), nr1_nc3(m22), nr1_nc3(m23), nr1_nc3(m24))
#' c(nr1_nc3(m30), nr1_nc3(m31), nr1_nc3(m32), nr1_nc3(m33), nr1_nc3(m34))
#' c(nr1_nc3(m40), nr1_nc3(m41), nr1_nc3(m42), nr1_nc3(m43), nr1_nc3(m44))
#'
#' c(nr2_nc1(m00), nr2_nc1(m01), nr2_nc1(m02), nr2_nc1(m03), nr2_nc1(m04))
#' c(nr2_nc1(m10), nr2_nc1(m11), nr2_nc1(m12), nr2_nc1(m13), nr2_nc1(m14))
#' c(nr2_nc1(m20), nr2_nc1(m21), nr2_nc1(m22), nr2_nc1(m23), nr2_nc1(m24))
#' c(nr2_nc1(m30), nr2_nc1(m31), nr2_nc1(m32), nr2_nc1(m33), nr2_nc1(m34))
#' c(nr2_nc1(m40), nr2_nc1(m41), nr2_nc1(m42), nr2_nc1(m43), nr2_nc1(m44))
#'
#' c(nr2_nc2(m00), nr2_nc2(m01), nr2_nc2(m02), nr2_nc2(m03), nr2_nc2(m04))
#' c(nr2_nc2(m10), nr2_nc2(m11), nr2_nc2(m12), nr2_nc2(m13), nr2_nc2(m14))
#' c(nr2_nc2(m20), nr2_nc2(m21), nr2_nc2(m22), nr2_nc2(m23), nr2_nc2(m24))
#' c(nr2_nc2(m30), nr2_nc2(m31), nr2_nc2(m32), nr2_nc2(m33), nr2_nc2(m34))
#' c(nr2_nc2(m40), nr2_nc2(m41), nr2_nc2(m42), nr2_nc2(m43), nr2_nc2(m44))
#'
#' c(nr2_nc3(m00), nr2_nc3(m01), nr2_nc3(m02), nr2_nc3(m03), nr2_nc3(m04))
#' c(nr2_nc3(m10), nr2_nc3(m11), nr2_nc3(m12), nr2_nc3(m13), nr2_nc3(m14))
#' c(nr2_nc3(m20), nr2_nc3(m21), nr2_nc3(m22), nr2_nc3(m23), nr2_nc3(m24))
#' c(nr2_nc3(m30), nr2_nc3(m31), nr2_nc3(m32), nr2_nc3(m33), nr2_nc3(m34))
#' c(nr2_nc3(m40), nr2_nc3(m41), nr2_nc3(m42), nr2_nc3(m43), nr2_nc3(m44))
#'
#' c(nr3_nc1(m00), nr3_nc1(m01), nr3_nc1(m02), nr3_nc1(m03), nr3_nc1(m04))
#' c(nr3_nc1(m10), nr3_nc1(m11), nr3_nc1(m12), nr3_nc1(m13), nr3_nc1(m14))
#' c(nr3_nc1(m20), nr3_nc1(m21), nr3_nc1(m22), nr3_nc1(m23), nr3_nc1(m24))
#' c(nr3_nc1(m30), nr3_nc1(m31), nr3_nc1(m32), nr3_nc1(m33), nr3_nc1(m34))
#' c(nr3_nc1(m40), nr3_nc1(m41), nr3_nc1(m42), nr3_nc1(m43), nr3_nc1(m44))
#'
#' c(nr3_nc2(m00), nr3_nc2(m01), nr3_nc2(m02), nr3_nc2(m03), nr3_nc2(m04))
#' c(nr3_nc2(m10), nr3_nc2(m11), nr3_nc2(m12), nr3_nc2(m13), nr3_nc2(m14))
#' c(nr3_nc2(m20), nr3_nc2(m21), nr3_nc2(m22), nr3_nc2(m23), nr3_nc2(m24))
#' c(nr3_nc2(m30), nr3_nc2(m31), nr3_nc2(m32), nr3_nc2(m33), nr3_nc2(m34))
#' c(nr3_nc2(m40), nr3_nc2(m41), nr3_nc2(m42), nr3_nc2(m43), nr3_nc2(m44))
#'
#' c(nr3_nc3(m00), nr3_nc3(m01), nr3_nc3(m02), nr3_nc3(m03), nr3_nc3(m04))
#' c(nr3_nc3(m10), nr3_nc3(m11), nr3_nc3(m12), nr3_nc3(m13), nr3_nc3(m14))
#' c(nr3_nc3(m20), nr3_nc3(m21), nr3_nc3(m22), nr3_nc3(m23), nr3_nc3(m24))
#' c(nr3_nc3(m30), nr3_nc3(m31), nr3_nc3(m32), nr3_nc3(m33), nr3_nc3(m34))
#' c(nr3_nc3(m40), nr3_nc3(m41), nr3_nc3(m42), nr3_nc3(m43), nr3_nc3(m44))
#'
#' c(nr0_nc1p(m00), nr0_nc1p(m01), nr0_nc1p(m02), nr0_nc1p(m03), nr0_nc1p(m04))
#' c(nr0_nc1p(m10), nr0_nc1p(m11), nr0_nc1p(m12), nr0_nc1p(m13), nr0_nc1p(m14))
#' c(nr0_nc1p(m20), nr0_nc1p(m21), nr0_nc1p(m22), nr0_nc1p(m23), nr0_nc1p(m24))
#' c(nr0_nc1p(m30), nr0_nc1p(m31), nr0_nc1p(m32), nr0_nc1p(m33), nr0_nc1p(m34))
#' c(nr0_nc1p(m40), nr0_nc1p(m41), nr0_nc1p(m42), nr0_nc1p(m43), nr0_nc1p(m44))
#'
#' c(nr0_nc2p(m00), nr0_nc2p(m01), nr0_nc2p(m02), nr0_nc2p(m03), nr0_nc2p(m04))
#' c(nr0_nc2p(m10), nr0_nc2p(m11), nr0_nc2p(m12), nr0_nc2p(m13), nr0_nc2p(m14))
#' c(nr0_nc2p(m20), nr0_nc2p(m21), nr0_nc2p(m22), nr0_nc2p(m23), nr0_nc2p(m24))
#' c(nr0_nc2p(m30), nr0_nc2p(m31), nr0_nc2p(m32), nr0_nc2p(m33), nr0_nc2p(m34))
#' c(nr0_nc2p(m40), nr0_nc2p(m41), nr0_nc2p(m42), nr0_nc2p(m43), nr0_nc2p(m44))
#'
#' c(nr0_nc3p(m00), nr0_nc3p(m01), nr0_nc3p(m02), nr0_nc3p(m03), nr0_nc3p(m04))
#' c(nr0_nc3p(m10), nr0_nc3p(m11), nr0_nc3p(m12), nr0_nc3p(m13), nr0_nc3p(m14))
#' c(nr0_nc3p(m20), nr0_nc3p(m21), nr0_nc3p(m22), nr0_nc3p(m23), nr0_nc3p(m24))
#' c(nr0_nc3p(m30), nr0_nc3p(m31), nr0_nc3p(m32), nr0_nc3p(m33), nr0_nc3p(m34))
#' c(nr0_nc3p(m40), nr0_nc3p(m41), nr0_nc3p(m42), nr0_nc3p(m43), nr0_nc3p(m44))
#'
#' c(nr1p_nc0(m00), nr1p_nc0(m01), nr1p_nc0(m02), nr1p_nc0(m03), nr1p_nc0(m04))
#' c(nr1p_nc0(m10), nr1p_nc0(m11), nr1p_nc0(m12), nr1p_nc0(m13), nr1p_nc0(m14))
#' c(nr1p_nc0(m20), nr1p_nc0(m21), nr1p_nc0(m22), nr1p_nc0(m23), nr1p_nc0(m24))
#' c(nr1p_nc0(m30), nr1p_nc0(m31), nr1p_nc0(m32), nr1p_nc0(m33), nr1p_nc0(m34))
#' c(nr1p_nc0(m40), nr1p_nc0(m41), nr1p_nc0(m42), nr1p_nc0(m43), nr1p_nc0(m44))
#'
#' c(nr2p_nc0(m00), nr2p_nc0(m01), nr2p_nc0(m02), nr2p_nc0(m03), nr2p_nc0(m04))
#' c(nr2p_nc0(m10), nr2p_nc0(m11), nr2p_nc0(m12), nr2p_nc0(m13), nr2p_nc0(m14))
#' c(nr2p_nc0(m20), nr2p_nc0(m21), nr2p_nc0(m22), nr2p_nc0(m23), nr2p_nc0(m24))
#' c(nr2p_nc0(m30), nr2p_nc0(m31), nr2p_nc0(m32), nr2p_nc0(m33), nr2p_nc0(m34))
#' c(nr2p_nc0(m40), nr2p_nc0(m41), nr2p_nc0(m42), nr2p_nc0(m43), nr2p_nc0(m44))
#'
#' c(nr3p_nc0(m00), nr3p_nc0(m01), nr3p_nc0(m02), nr3p_nc0(m03), nr3p_nc0(m04))
#' c(nr3p_nc0(m10), nr3p_nc0(m11), nr3p_nc0(m12), nr3p_nc0(m13), nr3p_nc0(m14))
#' c(nr3p_nc0(m20), nr3p_nc0(m21), nr3p_nc0(m22), nr3p_nc0(m23), nr3p_nc0(m24))
#' c(nr3p_nc0(m30), nr3p_nc0(m31), nr3p_nc0(m32), nr3p_nc0(m33), nr3p_nc0(m34))
#' c(nr3p_nc0(m40), nr3p_nc0(m41), nr3p_nc0(m42), nr3p_nc0(m43), nr3p_nc0(m44))
#'
#' c(nr1p_nc1p(m00), nr1p_nc1p(m01), nr1p_nc1p(m02), nr1p_nc1p(m03), nr1p_nc1p(m04))
#' c(nr1p_nc1p(m10), nr1p_nc1p(m11), nr1p_nc1p(m12), nr1p_nc1p(m13), nr1p_nc1p(m14))
#' c(nr1p_nc1p(m20), nr1p_nc1p(m21), nr1p_nc1p(m22), nr1p_nc1p(m23), nr1p_nc1p(m24))
#' c(nr1p_nc1p(m30), nr1p_nc1p(m31), nr1p_nc1p(m32), nr1p_nc1p(m33), nr1p_nc1p(m34))
#' c(nr1p_nc1p(m40), nr1p_nc1p(m41), nr1p_nc1p(m42), nr1p_nc1p(m43), nr1p_nc1p(m44))
#'
#' c(nr1p_nc2p(m00), nr1p_nc2p(m01), nr1p_nc2p(m02), nr1p_nc2p(m03), nr1p_nc2p(m04))
#' c(nr1p_nc2p(m10), nr1p_nc2p(m11), nr1p_nc2p(m12), nr1p_nc2p(m13), nr1p_nc2p(m14))
#' c(nr1p_nc2p(m20), nr1p_nc2p(m21), nr1p_nc2p(m22), nr1p_nc2p(m23), nr1p_nc2p(m24))
#' c(nr1p_nc2p(m30), nr1p_nc2p(m31), nr1p_nc2p(m32), nr1p_nc2p(m33), nr1p_nc2p(m34))
#' c(nr1p_nc2p(m40), nr1p_nc2p(m41), nr1p_nc2p(m42), nr1p_nc2p(m43), nr1p_nc2p(m44))
#'
#' c(nr1p_nc3p(m00), nr1p_nc3p(m01), nr1p_nc3p(m02), nr1p_nc3p(m03), nr1p_nc3p(m04))
#' c(nr1p_nc3p(m10), nr1p_nc3p(m11), nr1p_nc3p(m12), nr1p_nc3p(m13), nr1p_nc3p(m14))
#' c(nr1p_nc3p(m20), nr1p_nc3p(m21), nr1p_nc3p(m22), nr1p_nc3p(m23), nr1p_nc3p(m24))
#' c(nr1p_nc3p(m30), nr1p_nc3p(m31), nr1p_nc3p(m32), nr1p_nc3p(m33), nr1p_nc3p(m34))
#' c(nr1p_nc3p(m40), nr1p_nc3p(m41), nr1p_nc3p(m42), nr1p_nc3p(m43), nr1p_nc3p(m44))
#'
#' c(nr2p_nc1p(m00), nr2p_nc1p(m01), nr2p_nc1p(m02), nr2p_nc1p(m03), nr2p_nc1p(m04))
#' c(nr2p_nc1p(m10), nr2p_nc1p(m11), nr2p_nc1p(m12), nr2p_nc1p(m13), nr2p_nc1p(m14))
#' c(nr2p_nc1p(m20), nr2p_nc1p(m21), nr2p_nc1p(m22), nr2p_nc1p(m23), nr2p_nc1p(m24))
#' c(nr2p_nc1p(m30), nr2p_nc1p(m31), nr2p_nc1p(m32), nr2p_nc1p(m33), nr2p_nc1p(m34))
#' c(nr2p_nc1p(m40), nr2p_nc1p(m41), nr2p_nc1p(m42), nr2p_nc1p(m43), nr2p_nc1p(m44))
#'
#' c(nr2p_nc2p(m00), nr2p_nc2p(m01), nr2p_nc2p(m02), nr2p_nc2p(m03), nr2p_nc2p(m04))
#' c(nr2p_nc2p(m10), nr2p_nc2p(m11), nr2p_nc2p(m12), nr2p_nc2p(m13), nr2p_nc2p(m14))
#' c(nr2p_nc2p(m20), nr2p_nc2p(m21), nr2p_nc2p(m22), nr2p_nc2p(m23), nr2p_nc2p(m24))
#' c(nr2p_nc2p(m30), nr2p_nc2p(m31), nr2p_nc2p(m32), nr2p_nc2p(m33), nr2p_nc2p(m34))
#' c(nr2p_nc2p(m40), nr2p_nc2p(m41), nr2p_nc2p(m42), nr2p_nc2p(m43), nr2p_nc2p(m44))
#'
#' c(nr2p_nc3p(m00), nr2p_nc3p(m01), nr2p_nc3p(m02), nr2p_nc3p(m03), nr2p_nc3p(m04))
#' c(nr2p_nc3p(m10), nr2p_nc3p(m11), nr2p_nc3p(m12), nr2p_nc3p(m13), nr2p_nc3p(m14))
#' c(nr2p_nc3p(m20), nr2p_nc3p(m21), nr2p_nc3p(m22), nr2p_nc3p(m23), nr2p_nc3p(m24))
#' c(nr2p_nc3p(m30), nr2p_nc3p(m31), nr2p_nc3p(m32), nr2p_nc3p(m33), nr2p_nc3p(m34))
#' c(nr2p_nc3p(m40), nr2p_nc3p(m41), nr2p_nc3p(m42), nr2p_nc3p(m43), nr2p_nc3p(m44))
#'
#' c(nr3p_nc1p(m00), nr3p_nc1p(m01), nr3p_nc1p(m02), nr3p_nc1p(m03), nr3p_nc1p(m04))
#' c(nr3p_nc1p(m10), nr3p_nc1p(m11), nr3p_nc1p(m12), nr3p_nc1p(m13), nr3p_nc1p(m14))
#' c(nr3p_nc1p(m20), nr3p_nc1p(m21), nr3p_nc1p(m22), nr3p_nc1p(m23), nr3p_nc1p(m24))
#' c(nr3p_nc1p(m30), nr3p_nc1p(m31), nr3p_nc1p(m32), nr3p_nc1p(m33), nr3p_nc1p(m34))
#' c(nr3p_nc1p(m40), nr3p_nc1p(m41), nr3p_nc1p(m42), nr3p_nc1p(m43), nr3p_nc1p(m44))
#'
#' c(nr3p_nc2p(m00), nr3p_nc2p(m01), nr3p_nc2p(m02), nr3p_nc2p(m03), nr3p_nc2p(m04))
#' c(nr3p_nc2p(m10), nr3p_nc2p(m11), nr3p_nc2p(m12), nr3p_nc2p(m13), nr3p_nc2p(m14))
#' c(nr3p_nc2p(m20), nr3p_nc2p(m21), nr3p_nc2p(m22), nr3p_nc2p(m23), nr3p_nc2p(m24))
#' c(nr3p_nc2p(m30), nr3p_nc2p(m31), nr3p_nc2p(m32), nr3p_nc2p(m33), nr3p_nc2p(m34))
#' c(nr3p_nc2p(m40), nr3p_nc2p(m41), nr3p_nc2p(m42), nr3p_nc2p(m43), nr3p_nc2p(m44))
#'
#' c(nr3p_nc3p(m00), nr3p_nc3p(m01), nr3p_nc3p(m02), nr3p_nc3p(m03), nr3p_nc3p(m04))
#' c(nr3p_nc3p(m10), nr3p_nc3p(m11), nr3p_nc3p(m12), nr3p_nc3p(m13), nr3p_nc3p(m14))
#' c(nr3p_nc3p(m20), nr3p_nc3p(m21), nr3p_nc3p(m22), nr3p_nc3p(m23), nr3p_nc3p(m24))
#' c(nr3p_nc3p(m30), nr3p_nc3p(m31), nr3p_nc3p(m32), nr3p_nc3p(m33), nr3p_nc3p(m34))
#' c(nr3p_nc3p(m40), nr3p_nc3p(m41), nr3p_nc3p(m42), nr3p_nc3p(m43), nr3p_nc3p(m44))
#'
#' c(not_nr0_nc0(m00), not_nr0_nc0(m01), not_nr0_nc0(m02), not_nr0_nc0(m03), not_nr0_nc0(m04))
#' c(not_nr0_nc0(m10), not_nr0_nc0(m11), not_nr0_nc0(m12), not_nr0_nc0(m13), not_nr0_nc0(m14))
#' c(not_nr0_nc0(m20), not_nr0_nc0(m21), not_nr0_nc0(m22), not_nr0_nc0(m23), not_nr0_nc0(m24))
#' c(not_nr0_nc0(m30), not_nr0_nc0(m31), not_nr0_nc0(m32), not_nr0_nc0(m33), not_nr0_nc0(m34))
#' c(not_nr0_nc0(m40), not_nr0_nc0(m41), not_nr0_nc0(m42), not_nr0_nc0(m43), not_nr0_nc0(m44))
#'
#' c(not_nr0_nc1(m00), not_nr0_nc1(m01), not_nr0_nc1(m02), not_nr0_nc1(m03), not_nr0_nc1(m04))
#' c(not_nr0_nc1(m10), not_nr0_nc1(m11), not_nr0_nc1(m12), not_nr0_nc1(m13), not_nr0_nc1(m14))
#' c(not_nr0_nc1(m20), not_nr0_nc1(m21), not_nr0_nc1(m22), not_nr0_nc1(m23), not_nr0_nc1(m24))
#' c(not_nr0_nc1(m30), not_nr0_nc1(m31), not_nr0_nc1(m32), not_nr0_nc1(m33), not_nr0_nc1(m34))
#' c(not_nr0_nc1(m40), not_nr0_nc1(m41), not_nr0_nc1(m42), not_nr0_nc1(m43), not_nr0_nc1(m44))
#'
#' c(not_nr0_nc2(m00), not_nr0_nc2(m01), not_nr0_nc2(m02), not_nr0_nc2(m03), not_nr0_nc2(m04))
#' c(not_nr0_nc2(m10), not_nr0_nc2(m11), not_nr0_nc2(m12), not_nr0_nc2(m13), not_nr0_nc2(m14))
#' c(not_nr0_nc2(m20), not_nr0_nc2(m21), not_nr0_nc2(m22), not_nr0_nc2(m23), not_nr0_nc2(m24))
#' c(not_nr0_nc2(m30), not_nr0_nc2(m31), not_nr0_nc2(m32), not_nr0_nc2(m33), not_nr0_nc2(m34))
#' c(not_nr0_nc2(m40), not_nr0_nc2(m41), not_nr0_nc2(m42), not_nr0_nc2(m43), not_nr0_nc2(m44))
#'
#' c(not_nr0_nc3(m00), not_nr0_nc3(m01), not_nr0_nc3(m02), not_nr0_nc3(m03), not_nr0_nc3(m04))
#' c(not_nr0_nc3(m10), not_nr0_nc3(m11), not_nr0_nc3(m12), not_nr0_nc3(m13), not_nr0_nc3(m14))
#' c(not_nr0_nc3(m20), not_nr0_nc3(m21), not_nr0_nc3(m22), not_nr0_nc3(m23), not_nr0_nc3(m24))
#' c(not_nr0_nc3(m30), not_nr0_nc3(m31), not_nr0_nc3(m32), not_nr0_nc3(m33), not_nr0_nc3(m34))
#' c(not_nr0_nc3(m40), not_nr0_nc3(m41), not_nr0_nc3(m42), not_nr0_nc3(m43), not_nr0_nc3(m44))
#'
#' c(not_nr1_nc0(m00), not_nr1_nc0(m01), not_nr1_nc0(m02), not_nr1_nc0(m03), not_nr1_nc0(m04))
#' c(not_nr1_nc0(m10), not_nr1_nc0(m11), not_nr1_nc0(m12), not_nr1_nc0(m13), not_nr1_nc0(m14))
#' c(not_nr1_nc0(m20), not_nr1_nc0(m21), not_nr1_nc0(m22), not_nr1_nc0(m23), not_nr1_nc0(m24))
#' c(not_nr1_nc0(m30), not_nr1_nc0(m31), not_nr1_nc0(m32), not_nr1_nc0(m33), not_nr1_nc0(m34))
#' c(not_nr1_nc0(m40), not_nr1_nc0(m41), not_nr1_nc0(m42), not_nr1_nc0(m43), not_nr1_nc0(m44))
#'
#' c(not_nr2_nc0(m00), not_nr2_nc0(m01), not_nr2_nc0(m02), not_nr2_nc0(m03), not_nr2_nc0(m04))
#' c(not_nr2_nc0(m10), not_nr2_nc0(m11), not_nr2_nc0(m12), not_nr2_nc0(m13), not_nr2_nc0(m14))
#' c(not_nr2_nc0(m20), not_nr2_nc0(m21), not_nr2_nc0(m22), not_nr2_nc0(m23), not_nr2_nc0(m24))
#' c(not_nr2_nc0(m30), not_nr2_nc0(m31), not_nr2_nc0(m32), not_nr2_nc0(m33), not_nr2_nc0(m34))
#' c(not_nr2_nc0(m40), not_nr2_nc0(m41), not_nr2_nc0(m42), not_nr2_nc0(m43), not_nr2_nc0(m44))
#'
#' c(not_nr3_nc0(m00), not_nr3_nc0(m01), not_nr3_nc0(m02), not_nr3_nc0(m03), not_nr3_nc0(m04))
#' c(not_nr3_nc0(m10), not_nr3_nc0(m11), not_nr3_nc0(m12), not_nr3_nc0(m13), not_nr3_nc0(m14))
#' c(not_nr3_nc0(m20), not_nr3_nc0(m21), not_nr3_nc0(m22), not_nr3_nc0(m23), not_nr3_nc0(m24))
#' c(not_nr3_nc0(m30), not_nr3_nc0(m31), not_nr3_nc0(m32), not_nr3_nc0(m33), not_nr3_nc0(m34))
#' c(not_nr3_nc0(m40), not_nr3_nc0(m41), not_nr3_nc0(m42), not_nr3_nc0(m43), not_nr3_nc0(m44))
#'
#' c(not_nr1_nc1(m00), not_nr1_nc1(m01), not_nr1_nc1(m02), not_nr1_nc1(m03), not_nr1_nc1(m04))
#' c(not_nr1_nc1(m10), not_nr1_nc1(m11), not_nr1_nc1(m12), not_nr1_nc1(m13), not_nr1_nc1(m14))
#' c(not_nr1_nc1(m20), not_nr1_nc1(m21), not_nr1_nc1(m22), not_nr1_nc1(m23), not_nr1_nc1(m24))
#' c(not_nr1_nc1(m30), not_nr1_nc1(m31), not_nr1_nc1(m32), not_nr1_nc1(m33), not_nr1_nc1(m34))
#' c(not_nr1_nc1(m40), not_nr1_nc1(m41), not_nr1_nc1(m42), not_nr1_nc1(m43), not_nr1_nc1(m44))
#'
#' c(not_nr1_nc2(m00), not_nr1_nc2(m01), not_nr1_nc2(m02), not_nr1_nc2(m03), not_nr1_nc2(m04))
#' c(not_nr1_nc2(m10), not_nr1_nc2(m11), not_nr1_nc2(m12), not_nr1_nc2(m13), not_nr1_nc2(m14))
#' c(not_nr1_nc2(m20), not_nr1_nc2(m21), not_nr1_nc2(m22), not_nr1_nc2(m23), not_nr1_nc2(m24))
#' c(not_nr1_nc2(m30), not_nr1_nc2(m31), not_nr1_nc2(m32), not_nr1_nc2(m33), not_nr1_nc2(m34))
#' c(not_nr1_nc2(m40), not_nr1_nc2(m41), not_nr1_nc2(m42), not_nr1_nc2(m43), not_nr1_nc2(m44))
#'
#' c(not_nr1_nc3(m00), not_nr1_nc3(m01), not_nr1_nc3(m02), not_nr1_nc3(m03), not_nr1_nc3(m04))
#' c(not_nr1_nc3(m10), not_nr1_nc3(m11), not_nr1_nc3(m12), not_nr1_nc3(m13), not_nr1_nc3(m14))
#' c(not_nr1_nc3(m20), not_nr1_nc3(m21), not_nr1_nc3(m22), not_nr1_nc3(m23), not_nr1_nc3(m24))
#' c(not_nr1_nc3(m30), not_nr1_nc3(m31), not_nr1_nc3(m32), not_nr1_nc3(m33), not_nr1_nc3(m34))
#' c(not_nr1_nc3(m40), not_nr1_nc3(m41), not_nr1_nc3(m42), not_nr1_nc3(m43), not_nr1_nc3(m44))
#'
#' c(not_nr2_nc1(m00), not_nr2_nc1(m01), not_nr2_nc1(m02), not_nr2_nc1(m03), not_nr2_nc1(m04))
#' c(not_nr2_nc1(m10), not_nr2_nc1(m11), not_nr2_nc1(m12), not_nr2_nc1(m13), not_nr2_nc1(m14))
#' c(not_nr2_nc1(m20), not_nr2_nc1(m21), not_nr2_nc1(m22), not_nr2_nc1(m23), not_nr2_nc1(m24))
#' c(not_nr2_nc1(m30), not_nr2_nc1(m31), not_nr2_nc1(m32), not_nr2_nc1(m33), not_nr2_nc1(m34))
#' c(not_nr2_nc1(m40), not_nr2_nc1(m41), not_nr2_nc1(m42), not_nr2_nc1(m43), not_nr2_nc1(m44))
#'
#' c(not_nr2_nc2(m00), not_nr2_nc2(m01), not_nr2_nc2(m02), not_nr2_nc2(m03), not_nr2_nc2(m04))
#' c(not_nr2_nc2(m10), not_nr2_nc2(m11), not_nr2_nc2(m12), not_nr2_nc2(m13), not_nr2_nc2(m14))
#' c(not_nr2_nc2(m20), not_nr2_nc2(m21), not_nr2_nc2(m22), not_nr2_nc2(m23), not_nr2_nc2(m24))
#' c(not_nr2_nc2(m30), not_nr2_nc2(m31), not_nr2_nc2(m32), not_nr2_nc2(m33), not_nr2_nc2(m34))
#' c(not_nr2_nc2(m40), not_nr2_nc2(m41), not_nr2_nc2(m42), not_nr2_nc2(m43), not_nr2_nc2(m44))
#'
#' c(not_nr2_nc3(m00), not_nr2_nc3(m01), not_nr2_nc3(m02), not_nr2_nc3(m03), not_nr2_nc3(m04))
#' c(not_nr2_nc3(m10), not_nr2_nc3(m11), not_nr2_nc3(m12), not_nr2_nc3(m13), not_nr2_nc3(m14))
#' c(not_nr2_nc3(m20), not_nr2_nc3(m21), not_nr2_nc3(m22), not_nr2_nc3(m23), not_nr2_nc3(m24))
#' c(not_nr2_nc3(m30), not_nr2_nc3(m31), not_nr2_nc3(m32), not_nr2_nc3(m33), not_nr2_nc3(m34))
#' c(not_nr2_nc3(m40), not_nr2_nc3(m41), not_nr2_nc3(m42), not_nr2_nc3(m43), not_nr2_nc3(m44))
#'
#' c(not_nr3_nc1(m00), not_nr3_nc1(m11), not_nr3_nc1(m12), not_nr3_nc1(m13), not_nr3_nc1(m14))
#' c(not_nr3_nc1(m20), not_nr3_nc1(m21), not_nr3_nc1(m22), not_nr3_nc1(m23), not_nr3_nc1(m24))
#' c(not_nr3_nc1(m30), not_nr3_nc1(m31), not_nr3_nc1(m32), not_nr3_nc1(m33), not_nr3_nc1(m34))
#' c(not_nr3_nc1(m40), not_nr3_nc1(m41), not_nr3_nc1(m42), not_nr3_nc1(m43), not_nr3_nc1(m44))
#
#' c(not_nr3_nc2(m00), not_nr3_nc2(m01), not_nr3_nc2(m02), not_nr3_nc2(m03), not_nr3_nc2(m04))
#' c(not_nr3_nc2(m10), not_nr3_nc2(m11), not_nr3_nc2(m12), not_nr3_nc2(m13), not_nr3_nc2(m14))
#' c(not_nr3_nc2(m20), not_nr3_nc2(m21), not_nr3_nc2(m22), not_nr3_nc2(m23), not_nr3_nc2(m24))
#' c(not_nr3_nc2(m30), not_nr3_nc2(m31), not_nr3_nc2(m32), not_nr3_nc2(m33), not_nr3_nc2(m34))
#' c(not_nr3_nc2(m40), not_nr3_nc2(m41), not_nr3_nc2(m42), not_nr3_nc2(m43), not_nr3_nc2(m44))
#'
#' c(not_nr3_nc3(m00), not_nr3_nc3(m01), not_nr3_nc3(m02), not_nr3_nc3(m03), not_nr3_nc3(m04))
#' c(not_nr3_nc3(m10), not_nr3_nc3(m11), not_nr3_nc3(m12), not_nr3_nc3(m13), not_nr3_nc3(m14))
#' c(not_nr3_nc3(m20), not_nr3_nc3(m21), not_nr3_nc3(m22), not_nr3_nc3(m23), not_nr3_nc3(m24))
#' c(not_nr3_nc3(m30), not_nr3_nc3(m31), not_nr3_nc3(m32), not_nr3_nc3(m33), not_nr3_nc3(m34))
#' c(not_nr3_nc3(m40), not_nr3_nc3(m41), not_nr3_nc3(m42), not_nr3_nc3(m43), not_nr3_nc3(m44))
#'
#' c(not_nr0_nc1p(m00), not_nr0_nc1p(m01), not_nr0_nc1p(m02), not_nr0_nc1p(m03), not_nr0_nc1p(m04))
#' c(not_nr0_nc1p(m10), not_nr0_nc1p(m11), not_nr0_nc1p(m12), not_nr0_nc1p(m13), not_nr0_nc1p(m14))
#' c(not_nr0_nc1p(m20), not_nr0_nc1p(m21), not_nr0_nc1p(m22), not_nr0_nc1p(m23), not_nr0_nc1p(m24))
#' c(not_nr0_nc1p(m30), not_nr0_nc1p(m31), not_nr0_nc1p(m32), not_nr0_nc1p(m33), not_nr0_nc1p(m34))
#' c(not_nr0_nc1p(m40), not_nr0_nc1p(m41), not_nr0_nc1p(m42), not_nr0_nc1p(m43), not_nr0_nc1p(m44))
#'
#' c(not_nr0_nc2p(m00), not_nr0_nc2p(m01), not_nr0_nc2p(m02), not_nr0_nc2p(m03), not_nr0_nc2p(m04))
#' c(not_nr0_nc2p(m10), not_nr0_nc2p(m11), not_nr0_nc2p(m12), not_nr0_nc2p(m13), not_nr0_nc2p(m14))
#' c(not_nr0_nc2p(m20), not_nr0_nc2p(m21), not_nr0_nc2p(m22), not_nr0_nc2p(m23), not_nr0_nc2p(m24))
#' c(not_nr0_nc2p(m30), not_nr0_nc2p(m31), not_nr0_nc2p(m32), not_nr0_nc2p(m33), not_nr0_nc2p(m34))
#' c(not_nr0_nc2p(m40), not_nr0_nc2p(m41), not_nr0_nc2p(m42), not_nr0_nc2p(m43), not_nr0_nc2p(m44))
#'
#' c(not_nr0_nc3p(m00), not_nr0_nc3p(m01), not_nr0_nc3p(m02), not_nr0_nc3p(m03), not_nr0_nc3p(m04))
#' c(not_nr0_nc3p(m10), not_nr0_nc3p(m11), not_nr0_nc3p(m12), not_nr0_nc3p(m13), not_nr0_nc3p(m14))
#' c(not_nr0_nc3p(m20), not_nr0_nc3p(m21), not_nr0_nc3p(m22), not_nr0_nc3p(m23), not_nr0_nc3p(m24))
#' c(not_nr0_nc3p(m30), not_nr0_nc3p(m31), not_nr0_nc3p(m32), not_nr0_nc3p(m33), not_nr0_nc3p(m34))
#' c(not_nr0_nc3p(m40), not_nr0_nc3p(m41), not_nr0_nc3p(m42), not_nr0_nc3p(m43), not_nr0_nc3p(m44))
#'
#' c(not_nr1p_nc0(m00), not_nr1p_nc0(m01), not_nr1p_nc0(m02), not_nr1p_nc0(m03), not_nr1p_nc0(m04))
#' c(not_nr1p_nc0(m10), not_nr1p_nc0(m11), not_nr1p_nc0(m12), not_nr1p_nc0(m13), not_nr1p_nc0(m14))
#' c(not_nr1p_nc0(m20), not_nr1p_nc0(m21), not_nr1p_nc0(m22), not_nr1p_nc0(m23), not_nr1p_nc0(m24))
#' c(not_nr1p_nc0(m30), not_nr1p_nc0(m31), not_nr1p_nc0(m32), not_nr1p_nc0(m33), not_nr1p_nc0(m34))
#' c(not_nr1p_nc0(m40), not_nr1p_nc0(m41), not_nr1p_nc0(m42), not_nr1p_nc0(m43), not_nr1p_nc0(m44))
#'
#' c(not_nr2p_nc0(m00), not_nr2p_nc0(m01), not_nr2p_nc0(m02), not_nr2p_nc0(m03), not_nr2p_nc0(m04))
#' c(not_nr2p_nc0(m10), not_nr2p_nc0(m11), not_nr2p_nc0(m12), not_nr2p_nc0(m13), not_nr2p_nc0(m14))
#' c(not_nr2p_nc0(m20), not_nr2p_nc0(m21), not_nr2p_nc0(m22), not_nr2p_nc0(m23), not_nr2p_nc0(m24))
#' c(not_nr2p_nc0(m30), not_nr2p_nc0(m31), not_nr2p_nc0(m32), not_nr2p_nc0(m33), not_nr2p_nc0(m34))
#' c(not_nr2p_nc0(m40), not_nr2p_nc0(m41), not_nr2p_nc0(m42), not_nr2p_nc0(m43), not_nr2p_nc0(m44))
#'
#' c(not_nr3p_nc0(m00), not_nr3p_nc0(m01), not_nr3p_nc0(m02), not_nr3p_nc0(m03), not_nr3p_nc0(m04))
#' c(not_nr3p_nc0(m10), not_nr3p_nc0(m11), not_nr3p_nc0(m12), not_nr3p_nc0(m13), not_nr3p_nc0(m14))
#' c(not_nr3p_nc0(m20), not_nr3p_nc0(m21), not_nr3p_nc0(m22), not_nr3p_nc0(m23), not_nr3p_nc0(m24))
#' c(not_nr3p_nc0(m30), not_nr3p_nc0(m31), not_nr3p_nc0(m32), not_nr3p_nc0(m33), not_nr3p_nc0(m34))
#' c(not_nr3p_nc0(m40), not_nr3p_nc0(m41), not_nr3p_nc0(m42), not_nr3p_nc0(m43), not_nr3p_nc0(m44))
#'
#' c(not_nr1p_nc1p(m00), not_nr1p_nc1p(m01), not_nr1p_nc1p(m02), not_nr1p_nc1p(m03), not_nr1p_nc1p(m04))
#' c(not_nr1p_nc1p(m10), not_nr1p_nc1p(m11), not_nr1p_nc1p(m12), not_nr1p_nc1p(m13), not_nr1p_nc1p(m14))
#' c(not_nr1p_nc1p(m20), not_nr1p_nc1p(m21), not_nr1p_nc1p(m22), not_nr1p_nc1p(m23), not_nr1p_nc1p(m24))
#' c(not_nr1p_nc1p(m30), not_nr1p_nc1p(m31), not_nr1p_nc1p(m32), not_nr1p_nc1p(m33), not_nr1p_nc1p(m34))
#' c(not_nr1p_nc1p(m40), not_nr1p_nc1p(m41), not_nr1p_nc1p(m42), not_nr1p_nc1p(m43), not_nr1p_nc1p(m44))
#'
#' c(not_nr1p_nc2p(m00), not_nr1p_nc2p(m01), not_nr1p_nc2p(m02), not_nr1p_nc2p(m03), not_nr1p_nc2p(m04))
#' c(not_nr1p_nc2p(m10), not_nr1p_nc2p(m11), not_nr1p_nc2p(m12), not_nr1p_nc2p(m13), not_nr1p_nc2p(m14))
#' c(not_nr1p_nc2p(m20), not_nr1p_nc2p(m21), not_nr1p_nc2p(m22), not_nr1p_nc2p(m23), not_nr1p_nc2p(m24))
#' c(not_nr1p_nc2p(m30), not_nr1p_nc2p(m31), not_nr1p_nc2p(m32), not_nr1p_nc2p(m33), not_nr1p_nc2p(m34))
#' c(not_nr1p_nc2p(m40), not_nr1p_nc2p(m41), not_nr1p_nc2p(m42), not_nr1p_nc2p(m43), not_nr1p_nc2p(m44))
#'
#' c(not_nr1p_nc3p(m00), not_nr1p_nc3p(m01), not_nr1p_nc3p(m02), not_nr1p_nc3p(m03), not_nr1p_nc3p(m04))
#' c(not_nr1p_nc3p(m10), not_nr1p_nc3p(m11), not_nr1p_nc3p(m12), not_nr1p_nc3p(m13), not_nr1p_nc3p(m14))
#' c(not_nr1p_nc3p(m20), not_nr1p_nc3p(m21), not_nr1p_nc3p(m22), not_nr1p_nc3p(m23), not_nr1p_nc3p(m24))
#' c(not_nr1p_nc3p(m30), not_nr1p_nc3p(m31), not_nr1p_nc3p(m32), not_nr1p_nc3p(m33), not_nr1p_nc3p(m34))
#' c(not_nr1p_nc3p(m40), not_nr1p_nc3p(m41), not_nr1p_nc3p(m42), not_nr1p_nc3p(m43), not_nr1p_nc3p(m44))
#'
#' c(not_nr2p_nc1p(m00), not_nr2p_nc1p(m01), not_nr2p_nc1p(m02), not_nr2p_nc1p(m03), not_nr2p_nc1p(m04))
#' c(not_nr2p_nc1p(m10), not_nr2p_nc1p(m11), not_nr2p_nc1p(m12), not_nr2p_nc1p(m13), not_nr2p_nc1p(m14))
#' c(not_nr2p_nc1p(m20), not_nr2p_nc1p(m21), not_nr2p_nc1p(m22), not_nr2p_nc1p(m23), not_nr2p_nc1p(m24))
#' c(not_nr2p_nc1p(m30), not_nr2p_nc1p(m31), not_nr2p_nc1p(m32), not_nr2p_nc1p(m33), not_nr2p_nc1p(m34))
#' c(not_nr2p_nc1p(m40), not_nr2p_nc1p(m41), not_nr2p_nc1p(m42), not_nr2p_nc1p(m43), not_nr2p_nc1p(m44))
#'
#' c(not_nr2p_nc2p(m00), not_nr2p_nc2p(m01), not_nr2p_nc2p(m02), not_nr2p_nc2p(m03), not_nr2p_nc2p(m04))
#' c(not_nr2p_nc2p(m10), not_nr2p_nc2p(m11), not_nr2p_nc2p(m12), not_nr2p_nc2p(m13), not_nr2p_nc2p(m14))
#' c(not_nr2p_nc2p(m20), not_nr2p_nc2p(m21), not_nr2p_nc2p(m22), not_nr2p_nc2p(m23), not_nr2p_nc2p(m24))
#' c(not_nr2p_nc2p(m30), not_nr2p_nc2p(m31), not_nr2p_nc2p(m32), not_nr2p_nc2p(m33), not_nr2p_nc2p(m34))
#' c(not_nr2p_nc2p(m40), not_nr2p_nc2p(m41), not_nr2p_nc2p(m42), not_nr2p_nc2p(m43), not_nr2p_nc2p(m44))
#'
#' c(not_nr2p_nc3p(m00), not_nr2p_nc3p(m01), not_nr2p_nc3p(m02), not_nr2p_nc3p(m03), not_nr2p_nc3p(m04))
#' c(not_nr2p_nc3p(m10), not_nr2p_nc3p(m11), not_nr2p_nc3p(m12), not_nr2p_nc3p(m13), not_nr2p_nc3p(m14))
#' c(not_nr2p_nc3p(m20), not_nr2p_nc3p(m21), not_nr2p_nc3p(m22), not_nr2p_nc3p(m23), not_nr2p_nc3p(m24))
#' c(not_nr2p_nc3p(m30), not_nr2p_nc3p(m31), not_nr2p_nc3p(m32), not_nr2p_nc3p(m33), not_nr2p_nc3p(m34))
#' c(not_nr2p_nc3p(m40), not_nr2p_nc3p(m41), not_nr2p_nc3p(m42), not_nr2p_nc3p(m43), not_nr2p_nc3p(m44))
#'
#' c(not_nr3p_nc1p(m00), not_nr3p_nc1p(m01), not_nr3p_nc1p(m02), not_nr3p_nc1p(m03), not_nr3p_nc1p(m04))
#' c(not_nr3p_nc1p(m10), not_nr3p_nc1p(m11), not_nr3p_nc1p(m12), not_nr3p_nc1p(m13), not_nr3p_nc1p(m14))
#' c(not_nr3p_nc1p(m20), not_nr3p_nc1p(m21), not_nr3p_nc1p(m22), not_nr3p_nc1p(m23), not_nr3p_nc1p(m24))
#' c(not_nr3p_nc1p(m30), not_nr3p_nc1p(m31), not_nr3p_nc1p(m32), not_nr3p_nc1p(m33), not_nr3p_nc1p(m34))
#' c(not_nr3p_nc1p(m40), not_nr3p_nc1p(m41), not_nr3p_nc1p(m42), not_nr3p_nc1p(m43), not_nr3p_nc1p(m44))
#'
#' c(not_nr3p_nc2p(m00), not_nr3p_nc2p(m01), not_nr3p_nc2p(m02), not_nr3p_nc2p(m03), not_nr3p_nc2p(m04))
#' c(not_nr3p_nc2p(m10), not_nr3p_nc2p(m11), not_nr3p_nc2p(m12), not_nr3p_nc2p(m13), not_nr3p_nc2p(m14))
#' c(not_nr3p_nc2p(m20), not_nr3p_nc2p(m21), not_nr3p_nc2p(m22), not_nr3p_nc2p(m23), not_nr3p_nc2p(m24))
#' c(not_nr3p_nc2p(m30), not_nr3p_nc2p(m31), not_nr3p_nc2p(m32), not_nr3p_nc2p(m33), not_nr3p_nc2p(m34))
#' c(not_nr3p_nc2p(m40), not_nr3p_nc2p(m41), not_nr3p_nc2p(m42), not_nr3p_nc2p(m43), not_nr3p_nc2p(m44))
#'
#' c(not_nr3p_nc3p(m00), not_nr3p_nc3p(m01), not_nr3p_nc3p(m02), not_nr3p_nc3p(m03), not_nr3p_nc3p(m04))
#' c(not_nr3p_nc3p(m10), not_nr3p_nc3p(m11), not_nr3p_nc3p(m12), not_nr3p_nc3p(m13), not_nr3p_nc3p(m14))
#' c(not_nr3p_nc3p(m20), not_nr3p_nc3p(m21), not_nr3p_nc3p(m22), not_nr3p_nc3p(m23), not_nr3p_nc3p(m24))
#' c(not_nr3p_nc3p(m30), not_nr3p_nc3p(m31), not_nr3p_nc3p(m32), not_nr3p_nc3p(m33), not_nr3p_nc3p(m34))
#' c(not_nr3p_nc3p(m40), not_nr3p_nc3p(m41), not_nr3p_nc3p(m42), not_nr3p_nc3p(m43), not_nr3p_nc3p(m44))
#'
#' ##
#' ## DEFINE ARRAY VARIABLE SET
#' ##
#'
#' a <- function(...) {array(1, dim = unlist(list(...), use.names = F))}
#'
#' a0 <- NULL
#' a1 <- a(1)
#' a11 <- a(1, 1)
#' a111 <- a(1, 1, 1)
#' a1111 <- a(1, 1, 1, 1)
#'
#' ##
#' ## DEMO [ndim], [ndimX], [ndimXp], [not_ndimX], AND [not_ndimXp] FUNCTIONS
#' ##
#'
#' c(ndim(a0), ndim(a1), ndim(a11), ndim(a111), ndim(a1111))
#'
#' c(ndim0(a0), ndim0(a1), ndim0(a11), ndim0(a111), ndim0(a1111))
#' c(ndim1(a0), ndim1(a1), ndim1(a11), ndim1(a111), ndim1(a1111))
#' c(ndim2(a0), ndim2(a1), ndim2(a11), ndim2(a111), ndim2(a1111))
#' c(ndim3(a0), ndim3(a1), ndim3(a11), ndim3(a111), ndim3(a1111))
#'
#' c(ndim1p(a0), ndim1p(a1), ndim1p(a11), ndim1p(a111), ndim1p(a1111))
#' c(ndim2p(a0), ndim2p(a1), ndim2p(a11), ndim2p(a111), ndim2p(a1111))
#' c(ndim3p(a0), ndim3p(a1), ndim3p(a11), ndim3p(a111), ndim3p(a1111))
#'
#' c(not_ndim0(a0), not_ndim0(a1), not_ndim0(a11), not_ndim0(a111), not_ndim0(a1111))
#' c(not_ndim1(a0), not_ndim1(a1), not_ndim1(a11), not_ndim1(a111), not_ndim1(a1111))
#' c(not_ndim2(a0), not_ndim2(a1), not_ndim2(a11), not_ndim2(a111), not_ndim2(a1111))
#' c(not_ndim3(a0), not_ndim3(a1), not_ndim3(a11), not_ndim3(a111), not_ndim3(a1111))
#'
#' c(not_ndim1p(a0), not_ndim1p(a1), not_ndim1p(a11), not_ndim1p(a111), not_ndim1p(a1111))
#' c(not_ndim2p(a0), not_ndim2p(a1), not_ndim2p(a11), not_ndim2p(a111), not_ndim2p(a1111))
#' c(not_ndim3p(a0), not_ndim3p(a1), not_ndim3p(a11), not_ndim3p(a111), not_ndim3p(a1111))
#'
#' ##
#' ## DEFINE [NA] VARIABLE SET
#' ##
#'
#' na0 <- na1 <- na2 <- na3 <- na4 <- letters
#' na1[5] <- NA
#' na2[c(5, 10)] <- NA
#' na3[c(5, 10, 15)] <- NA
#' na4[c(5, 10, 15, 25)] <- NA
#'
#' ##
#' ## DEMO [nnav], [nnavX], [nnavXp], [not_nnavX], AND [not_nnavXp] FUNCTIONS
#' ##
#'
#' c(nnav(na0), nnav(na1), nnav(na2), nnav(na3), nnav(na4))
#'
#' c(nnav0(na0), nnav0(na1), nnav0(na2), nnav0(na3), nnav0(na4))
#' c(nnav1(na0), nnav1(na1), nnav1(na2), nnav1(na3), nnav1(na4))
#' c(nnav2(na0), nnav2(na1), nnav2(na2), nnav2(na3), nnav2(na4))
#' c(nnav3(na0), nnav3(na1), nnav3(na2), nnav3(na3), nnav3(na4))
#'
#' c(nnav1p(na0), nnav1p(na1), nnav1p(na2), nnav1p(na3), nnav1p(na4))
#' c(nnav2p(na0), nnav2p(na1), nnav2p(na2), nnav2p(na3), nnav2p(na4))
#' c(nnav3p(na0), nnav3p(na1), nnav3p(na2), nnav3p(na3), nnav3p(na4))
#'
#' c(not_nnav0(na0), not_nnav0(na1), not_nnav0(na2), not_nnav0(na3), not_nnav0(na4))
#' c(not_nnav1(na0), not_nnav1(na1), not_nnav1(na2), not_nnav1(na3), not_nnav1(na4))
#' c(not_nnav2(na0), not_nnav2(na1), not_nnav2(na2), not_nnav2(na3), not_nnav2(na4))
#' c(not_nnav3(na0), not_nnav3(na1), not_nnav3(na2), not_nnav3(na3), not_nnav3(na4))
#'
#' c(not_nnav1p(na0), not_nnav1p(na1), not_nnav1p(na2), not_nnav1p(na3), not_nnav1p(na4))
#' c(not_nnav2p(na0), not_nnav2p(na1), not_nnav2p(na2), not_nnav2p(na3), not_nnav2p(na4))
#' c(not_nnav3p(na0), not_nnav3p(na1), not_nnav3p(na2), not_nnav3p(na3), not_nnav3p(na4))
#'
#' ##
#' ## DEFINE [OK] VARIABLE SET
#' ##
#'
#' ok0 <- ok1 <- ok2 <- ok3 <- ok4 <- 0:9
#' ok0[1:10] <- NA
#' ok1[1:9] <- NA
#' ok2[1:8] <- NA
#' ok3[1:7] <- NA
#' ok4[1:6] <- NA
#'
#' ##
#' ## DEMO [nokv], [nokvX], [nokvXp], [not_nokvX], AND [not_nokvXp] FUNCTIONS
#' ##
#'
#' c(nokv(ok0), nokv(ok1), nokv(ok2), nokv(ok3), nokv(ok4))
#'
#' c(nokv0(ok0), nokv0(ok1), nokv0(ok2), nokv0(ok3), nokv0(ok4))
#' c(nokv1(ok0), nokv1(ok1), nokv1(ok2), nokv1(ok3), nokv1(ok4))
#' c(nokv2(ok0), nokv2(ok1), nokv2(ok2), nokv2(ok3), nokv2(ok4))
#' c(nokv3(ok0), nokv3(ok1), nokv3(ok2), nokv3(ok3), nokv3(ok4))
#'
#' c(nokv1p(ok0), nokv1p(ok1), nokv1p(ok2), nokv1p(ok3), nokv1p(ok4))
#' c(nokv2p(ok0), nokv2p(ok1), nokv2p(ok2), nokv2p(ok3), nokv2p(ok4))
#' c(nokv3p(ok0), nokv3p(ok1), nokv3p(ok2), nokv3p(ok3), nokv3p(ok4))
#'
#' c(not_nokv0(ok0), not_nokv0(ok1), not_nokv0(ok2), not_nokv0(ok3), not_nokv0(ok4))
#' c(not_nokv1(ok0), not_nokv1(ok1), not_nokv1(ok2), not_nokv1(ok3), not_nokv1(ok4))
#' c(not_nokv2(ok0), not_nokv2(ok1), not_nokv2(ok2), not_nokv2(ok3), not_nokv2(ok4))
#' c(not_nokv3(ok0), not_nokv3(ok1), not_nokv3(ok2), not_nokv3(ok3), not_nokv3(ok4))
#'
#' c(not_nokv1p(ok0), not_nokv1p(ok1), not_nokv1p(ok2), not_nokv1p(ok3), not_nokv1p(ok4))
#' c(not_nokv2p(ok0), not_nokv2p(ok1), not_nokv2p(ok2), not_nokv2p(ok3), not_nokv2p(ok4))
#' c(not_nokv3p(ok0), not_nokv3p(ok1), not_nokv3p(ok2), not_nokv3p(ok3), not_nokv3p(ok4))
#'
#' ##
#' ## DEFINE LIST VARIABLE SET
#' ##
#'
#' NS0 <- list(a = NULL, b = NULL)
#' NS1 <- list(a = 1, b = 2)
#' NS2 <- list(a = 1:2, b = 3:4)
#' NS3 <- list(a = 1:3, b = 4:6)
#' NS4 <- list(a = 1:4, b = 5:8)
#' NSx <- list(a = 1, b = 2:3)
#'
#' ##
#' ## DEMO [ns], [nsX], [nxXp], [not_nsX], AND [not_nsXp] FUNCTIONS
#' ##
#'
#' list(NS0 = ns(NS0), NS1 = ns(NS1), NS2 = ns(NS2), NS3 = ns(NS3), NS4 = ns(NS4), NSx = ns(NSx))
#'
#' c(ns0(NS0), ns0(NS1), ns0(NS2), ns0(NS3), ns0(NS4), ns0(NSx))
#' c(ns1(NS0), ns1(NS1), ns1(NS2), ns1(NS3), ns1(NS4), ns1(NSx))
#' c(ns2(NS0), ns2(NS1), ns2(NS2), ns2(NS3), ns2(NS4), ns2(NSx))
#' c(ns3(NS0), ns3(NS1), ns3(NS2), ns3(NS3), ns3(NS4), ns3(NSx))
#'
#' c(ns1p(NS0), ns1p(NS1), ns1p(NS2), ns1p(NS3), ns1p(NS4), ns1p(NSx))
#' c(ns2p(NS0), ns2p(NS1), ns2p(NS2), ns2p(NS3), ns2p(NS4), ns2p(NSx))
#' c(ns3p(NS0), ns3p(NS1), ns3p(NS2), ns3p(NS3), ns3p(NS4), ns3p(NSx))
#'
#' c(not_ns0(NS0), not_ns0(NS1), not_ns0(NS2), not_ns0(NS3), not_ns0(NS4), not_ns0(NSx))
#' c(not_ns1(NS0), not_ns1(NS1), not_ns1(NS2), not_ns1(NS3), not_ns1(NS4), not_ns1(NSx))
#' c(not_ns2(NS0), not_ns2(NS1), not_ns2(NS2), not_ns2(NS3), not_ns2(NS4), not_ns2(NSx))
#' c(not_ns3(NS0), not_ns3(NS1), not_ns3(NS2), not_ns3(NS3), not_ns3(NS4), not_ns3(NSx))
#'
#' c(not_ns1p(NS0), not_ns1p(NS1), not_ns1p(NS2), not_ns1p(NS3), not_ns1p(NS4), not_ns1p(NSx))
#' c(not_ns2p(NS0), not_ns2p(NS1), not_ns2p(NS2), not_ns2p(NS3), not_ns2p(NS4), not_ns2p(NSx))
#' c(not_ns3p(NS0), not_ns3p(NS1), not_ns3p(NS2), not_ns3p(NS3), not_ns3p(NS4), not_ns3p(NSx))
#'
#' @export
N <- base::length

#' @rdname N
#' @export
n_check <- function(x, .n = NULL, .min = NULL, .max = NULL, .eq = F) {
  errs <- uj:::.n_errs(.n = .n, .min = .min, .max = .max, .eq = .eq, .na = F, .a = F)
  if (!base::is.null(errs)) {uj::stopperr(errs)}
  if (!base::is.null(.n) | !base::is.null(.min) | !base::is.null(.max) | .eq) {
    okN <- uj::f0(base::is.null(.n), T, base::all(x %in% .n))
    okEq <- uj::f0(.eq, base::length(base::unique(x)) == 1, T)
    okMin <- uj::f0(base::is.null(.min), T, base::all(x >= .min))
    okMax <- uj::f0(base::is.null(.max), T, base::all(x <= .max))
    okN & okMin & okMax & okEq
  } else {x}
}

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

#' @rdname N
#' @export
not_n0 <- function(x) {uj::N(x) != 0}

#' @rdname N
#' @export
not_n1 <- function(x) {uj::N(x) != 1}

#' @rdname N
#' @export
not_n2 <- function(x) {uj::N(x) != 2}

#' @rdname N
#' @export
not_n3 <- function(x) {uj::N(x) != 3}

#' @rdname N
#' @export
not_n1p <- function(x) {!(uj::N(x) > 0)}

#' @rdname N
#' @export
not_n2p <- function(x) {!(uj::N(x) > 1)}

#' @rdname N
#' @export
not_n3p <- function(x) {!(uj::N(x) > 2)}

# N-dot-args ####

#' @rdname N
#' @export
nd <- base::...length

#' @rdname N
#' @export
ndots <- nd

#' @rdname N
#' @export
nd0 <- function(...) {base::...length() == 0}

#' @rdname N
#' @export
nd1 <- function(...) {base::...length() == 1}

#' @rdname N
#' @export
nd2 <- function(...) {base::...length() == 2}

#' @rdname N
#' @export
nd3 <- function(...) {base::...length() == 3}

#' @rdname N
#' @export
nd1p <- function(...) {base::...length() > 0}

#' @rdname N
#' @export
nd2p <- function(...) {base::...length() > 1}

#' @rdname N
#' @export
nd3p <- function(...) {base::...length() > 2}

#' @rdname N
#' @export
not_nd0 <- function(...) {base::...length() != 0}

#' @rdname N
#' @export
not_nd1 <- function(...) {base::...length() != 1}

#' @rdname N
#' @export
not_nd2 <- function(...) {base::...length() != 2}

#' @rdname N
#' @export
not_nd3 <- function(...) {base::...length() != 3}

#' @rdname N
#' @export
not_nd1p <- function(...) {!base::...length() > 0}

#' @rdname N
#' @export
not_nd2p <- function(...) {!base::...length() > 1}

#' @rdname N
#' @export
not_nd3p <- function(...) {!base::...length() > 2}

# N-unique values ####

#' @rdname N
#' @export
nu <- function(x) {base::length(base::unique(uj::av(x)))}

#' @rdname N
#' @export
nunq <- nu

#' @rdname N
#' @export
nu0 <- function(x) {uj::nu(x) == 0}

#' @rdname N
#' @export
nu1 <- function(x) {uj::nu(x) == 1}

#' @rdname N
#' @export
nu2 <- function(x) {uj::nu(x) == 2}

#' @rdname N
#' @export
nu3 <- function(x) {uj::nu(x) == 3}

#' @rdname N
#' @export
nu1p <- function(x) {uj::nu(x) > 0}

#' @rdname N
#' @export
nu2p <- function(x) {uj::nu(x) > 1}

#' @rdname N
#' @export
nu3p <- function(x) {uj::nu(x) > 2}

#' @rdname N
#' @export
not_nu0 <- function(x) {!uj::nu0(x)}

#' @rdname N
#' @export
not_nu1 <- function(x) {!uj::nu1(x)}

#' @rdname N
#' @export
not_nu2 <- function(x) {!uj::nu2(x)}

#' @rdname N
#' @export
not_nu3 <- function(x) {!uj::nu3(x)}

#' @rdname N
#' @export
not_nu1p <- function(x) {!uj::nu1p(x)}

#' @rdname N
#' @export
not_nu2p <- function(x) {!uj::nu2p(x)}

#' @rdname N
#' @export
not_nu3p <- function(x) {!uj::nu3p(x)}

# n_values (atomic) ####

#' @rdname N
#' @export
nv <- function(...) {base::length(uj::av(...))}

#' @rdname N
#' @export
nval <- nv

#' @rdname N
#' @export
nv0 <- function(...) {uj::nv(...) == 0}

#' @rdname N
#' @export
nv1 <- function(...) {uj::nv(...) == 1}

#' @rdname N
#' @export
nv2 <- function(...) {uj::nv(...) == 2}

#' @rdname N
#' @export
nv3 <- function(...) {uj::nv(...) == 3}

#' @rdname N
#' @export
nv1p <- function(...) {uj::nv(...) > 0}

#' @rdname N
#' @export
nv2p <- function(...) {uj::nv(...) > 1}

#' @rdname N
#' @export
nv3p <- function(...) {uj::nv(...) > 2}

#' @rdname N
#' @export
not_nv0 <- function(...) {!uj::nv0(...)}

#' @rdname N
#' @export
not_nv1 <- function(...) {!uj::nv1(...)}

#' @rdname N
#' @export
not_nv2 <- function(...) {!uj::nv2(...)}

#' @rdname N
#' @export
not_nv3 <- function(...) {!uj::nv3(...)}

#' @rdname N
#' @export
not_nv1p <- function(...) {!uj::nv1p(...)}

#' @rdname N
#' @export
not_nv2p <- function(...) {!uj::nv2p(...)}

#' @rdname N
#' @export
not_nv3p <- function(...) {!uj::nv3p(...)}

# nw ####

#' @rdname N
#' @export
nw <- function(x) {
  x <- uj::av(x)
  if (!base::is.logical(x)) {return(0)}
  x[base::is.na(x)] <- F
  base::length(base::which(x))
}

#' @rdname N
#' @export
nw0 <- function(x) {uj::nw(x) == 0}

#' @rdname N
#' @export
nw1 <- function(x) {uj::nw(x) == 1}

#' @rdname N
#' @export
nw2 <- function(x) {uj::nw(x) == 2}

#' @rdname N
#' @export
nw3 <- function(x) {uj::nw(x) == 3}

#' @rdname N
#' @export
nw1p <- function(x) {uj::nw(x) > 0}

#' @rdname N
#' @export
nw2p <- function(x) {uj::nw(x) > 1}

#' @rdname N
#' @export
nw3p <- function(x) {uj::nw(x) > 2}

#' @rdname N
#' @export
not_nw0 <- function(x) {!uj::nw0(x)}

#' @rdname N
#' @export
not_nw1 <- function(x) {!uj::nw1(x)}

#' @rdname N
#' @export
not_nw2 <- function(x) {!uj::nw2(x)}

#' @rdname N
#' @export
not_nw3 <- function(x) {!uj::nw3(x)}

#' @rdname N
#' @export
not_nw1p <- function(x) {!uj::nw1p(x)}

#' @rdname N
#' @export
not_nw2p <- function(x) {!uj::nw2p(x)}

#' @rdname N
#' @export
not_nw3p <- function(x) {!uj::nw3p(x)}

# equality ####

#' @rdname N
#' @export
neq <- function(x, y) {base::length(x) == base::length(y)}

#' @rdname N
#' @export
ndif <- function(x, y) {base::length(x) != base::length(y)}

# nr (nrows) ####

#' @rdname N
#' @export
nr <- function(x) {base::NROW(x)}

#' @rdname N
#' @export
nr0 <- function(x) {uj::nr(x) == 0}

#' @rdname N
#' @export
nr1 <- function(x) {uj::nr(x) == 1}

#' @rdname N
#' @export
nr2 <- function(x) {uj::nr(x) == 2}

#' @rdname N
#' @export
nr3 <- function(x) {uj::nr(x) == 3}

#' @rdname N
#' @export
nr1p <- function(x) {uj::nr(x) > 0}

#' @rdname N
#' @export
nr2p <- function(x) {uj::nr(x) > 1}

#' @rdname N
#' @export
nr3p <- function(x) {uj::nr(x) > 2}

#' @rdname N
#' @export
not_nr0 <- function(x) {!uj::nr0(x)}

#' @rdname N
#' @export
not_nr1 <- function(x) {!uj::nr1(x)}

#' @rdname N
#' @export
not_nr2 <- function(x) {!uj::nr2(x)}

#' @rdname N
#' @export
not_nr3 <- function(x) {!uj::nr3(x)}

#' @rdname N
#' @export
not_nr1p <- function(x) {!uj::nr1p(x)}

#' @rdname N
#' @export
not_nr2p <- function(x) {!uj::nr2p(x)}

#' @rdname N
#' @export
not_nr3p <- function(x) {!uj::nr3p(x)}

# nc (ncols) ####

#' @rdname N
#' @export
nc <- function(x) {base::NCOL(x)}

#' @rdname N
#' @export
nc0 <- function(x) {uj::nc(x) == 0}

#' @rdname N
#' @export
nc1 <- function(x) {uj::nc(x) == 1}

#' @rdname N
#' @export
nc2 <- function(x) {uj::nc(x) == 2}

#' @rdname N
#' @export
nc3 <- function(x) {uj::nc(x) == 3}

#' @rdname N
#' @export
nc1p <- function(x) {uj::nc(x) > 0}

#' @rdname N
#' @export
nc2p <- function(x) {uj::nc(x) > 1}

#' @rdname N
#' @export
nc3p <- function(x) {uj::nc(x) > 2}

#' @rdname N
#' @export
not_nc0 <- function(x) {!uj::nc0(x)}

#' @rdname N
#' @export
not_nc1 <- function(x) {!uj::nc1(x)}

#' @rdname N
#' @export
not_nc2 <- function(x) {!uj::nc2(x)}

#' @rdname N
#' @export
not_nc3 <- function(x) {!uj::nc3(x)}

#' @rdname N
#' @export
not_nc1p <- function(x) {!uj::nc1p(x)}

#' @rdname N
#' @export
not_nc2p <- function(x) {!uj::nc2p(x)}

#' @rdname N
#' @export
not_nc3p <- function(x) {!uj::nc3p(x)}

# nrc (nrows * ncols) ####

#' @rdname N
#' @export
nrc <- function(x) {base::NROW(x) & base::NCOL(x)}

#' @rdname N
#' @export
nrc0 <- function(x) {uj::nrc(x) == 0}

#' @rdname N
#' @export
nrc1 <- function(x) {uj::nrc(x) == 1}

#' @rdname N
#' @export
nrc2 <- function(x) {uj::nrc(x) == 2}

#' @rdname N
#' @export
nrc1p <- function(x) {uj::nrc(x) > 0}

#' @rdname N
#' @export
nrc2p <- function(x) {uj::nrc(x) > 1}

#' @rdname N
#' @export
not_nrc0 <- function(x) {uj::nrc(x) != 0}

#' @rdname N
#' @export
not_nrc1 <- function(x) {uj::nrc(x) != 1}

#' @rdname N
#' @export
not_nrc2p <- function(x) {uj::nrc(x) < 2}

# nr_nc (nrows, ncols) ####

#' @rdname N
#' @export
nr0_nc0 <- function(x) {uj::nr0(x) & nc0(x)}

#' @rdname N
#' @export
nr0_nc1 <- function(x) {uj::nr0(x) & nc1(x)}

#' @rdname N
#' @export
nr0_nc2 <- function(x) {uj::nr0(x) & nc2(x)}

#' @rdname N
#' @export
nr0_nc3 <- function(x) {uj::nr0(x) & nc3(x)}

#' @rdname N
#' @export
nr0_nc1p <- function(x) {uj::nr0(x) & nc1p(x)}

#' @rdname N
#' @export
nr0_nc2p <- function(x) {uj::nr0(x) & nc2p(x)}

#' @rdname N
#' @export
nr0_nc3p <- function(x) {uj::nr0(x) & nc3p(x)}

#' @rdname N
#' @export
nr1_nc0 <- function(x) {uj::nr1(x) & nc0(x)}

#' @rdname N
#' @export
nr1_nc1 <- function(x) {uj::nr1(x) & nc1(x)}

#' @rdname N
#' @export
nr1_nc2 <- function(x) {uj::nr1(x) & nc2(x)}

#' @rdname N
#' @export
nr1_nc3 <- function(x) {uj::nr1(x) & nc3(x)}

#' @rdname N
#' @export
nr1_nc1p <- function(x) {uj::nr1(x) & nc1p(x)}

#' @rdname N
#' @export
nr1_nc2p <- function(x) {uj::nr1(x) & nc2p(x)}

#' @rdname N
#' @export
nr1_nc3p <- function(x) {uj::nr1(x) & nc3p(x)}

#' @rdname N
#' @export
nr2_nc0 <- function(x) {uj::nr2(x) & nc0(x)}

#' @rdname N
#' @export
nr2_nc1 <- function(x) {uj::nr2(x) & nc1(x)}

#' @rdname N
#' @export
nr2_nc2 <- function(x) {uj::nr2(x) & nc2(x)}

#' @rdname N
#' @export
nr2_nc3 <- function(x) {uj::nr2(x) & nc3(x)}

#' @rdname N
#' @export
nr2_nc1p <- function(x) {uj::nr2(x) & nc1p(x)}

#' @rdname N
#' @export
nr2_nc2p <- function(x) {uj::nr2(x) & nc2p(x)}

#' @rdname N
#' @export
nr2_nc3p <- function(x) {uj::nr2(x) & nc3p(x)}

#' @rdname N
#' @export
nr3_nc0 <- function(x) {uj::nr3(x) & nc0(x)}

#' @rdname N
#' @export
nr3_nc1 <- function(x) {uj::nr3(x) & nc1(x)}

#' @rdname N
#' @export
nr3_nc2 <- function(x) {uj::nr3(x) & nc2(x)}

#' @rdname N
#' @export
nr3_nc3 <- function(x) {uj::nr3(x) & nc3(x)}

#' @rdname N
#' @export
nr1p_nc0 <- function(x) {uj::nr1p(x) & nc0(x)}

#' @rdname N
#' @export
nr1p_nc1 <- function(x) {uj::nr1p(x) & nc1(x)}

#' @rdname N
#' @export
nr1p_nc2 <- function(x) {uj::nr1p(x) & nc2(x)}

#' @rdname N
#' @export
nr1p_nc3 <- function(x) {uj::nr1p(x) & nc3(x)}

#' @rdname N
#' @export
nr1p_nc1p <- function(x) {uj::nr1p(x) & nc1p(x)}

#' @rdname N
#' @export
nr1p_nc2p <- function(x) {uj::nr1p(x) & nc2p(x)}

#' @rdname N
#' @export
nr1p_nc3p <- function(x) {uj::nr1p(x) & nc3p(x)}

#' @rdname N
#' @export
nr2p_nc0 <- function(x) {uj::nr2p(x) & nc0(x)}

#' @rdname N
#' @export
nr2p_nc1 <- function(x) {uj::nr2p(x) & nc1(x)}

#' @rdname N
#' @export
nr2p_nc2 <- function(x) {uj::nr2p(x) & nc2(x)}

#' @rdname N
#' @export
nr2p_nc3 <- function(x) {uj::nr2p(x) & nc3(x)}

#' @rdname N
#' @export
nr2p_nc1p <- function(x) {uj::nr2p(x) & nc1p(x)}

#' @rdname N
#' @export
nr2p_nc2p <- function(x) {uj::nr2p(x) & nc2p(x)}

#' @rdname N
#' @export
nr2p_nc3p <- function(x) {uj::nr2p(x) & nc3p(x)}

#' @rdname N
#' @export
nr3p_nc0 <- function(x) {uj::nr3p(x) & nc0(x)}

#' @rdname N
#' @export
nr3p_nc1 <- function(x) {uj::nr3p(x) & nc1(x)}

#' @rdname N
#' @export
nr3p_nc2 <- function(x) {uj::nr3p(x) & nc2(x)}

#' @rdname N
#' @export
nr3p_nc3 <- function(x) {uj::nr3p(x) & nc3(x)}

#' @rdname N
#' @export
nr3p_nc1p <- function(x) {uj::nr3p(x) & nc1p(x)}

#' @rdname N
#' @export
nr3p_nc2p <- function(x) {uj::nr3p(x) & nc2p(x)}

#' @rdname N
#' @export
nr3p_nc3p <- function(x) {uj::nr3p(x) & nc3p(x)}

#' @rdname N
#' @export
not_nr0_nc0 <- function(x) {!uj::nr0_nc0(x)}

#' @rdname N
#' @export
not_nr0_nc1 <- function(x) {!uj::nr0_nc1(x)}

#' @rdname N
#' @export
not_nr0_nc2 <- function(x) {!uj::nr0_nc2(x)}

#' @rdname N
#' @export
not_nr0_nc3 <- function(x) {!uj::nr0_nc2(x)}

#' @rdname N
#' @export
not_nr0_nc1p <- function(x) {!uj::nr0_nc1p(x)}

#' @rdname N
#' @export
not_nr0_nc2p <- function(x) {!uj::nr0_nc2p(x)}

#' @rdname N
#' @export
not_nr0_nc3p <- function(x) {!uj::nr0_nc3p(x)}

#' @rdname N
#' @export
not_nr1_nc0 <- function(x) {!uj::nr1_nc0(x)}

#' @rdname N
#' @export
not_nr1_nc1 <- function(x) {!uj::nr1_nc1(x)}

#' @rdname N
#' @export
not_nr1_nc2 <- function(x) {!uj::nr1_nc2(x)}

#' @rdname N
#' @export
not_nr1_nc3 <- function(x) {!uj::nr1_nc3(x)}

#' @rdname N
#' @export
not_nr1_nc1p <- function(x) {!uj::nr1_nc1p(x)}

#' @rdname N
#' @export
not_nr1_nc2p <- function(x) {!uj::nr1_nc2p(x)}

#' @rdname N
#' @export
not_nr1_nc3p <- function(x) {!uj::nr1_nc3p(x)}

#' @rdname N
#' @export
not_nr2_nc0 <- function(x) {!uj::nr2_nc0(x)}

#' @rdname N
#' @export
not_nr2_nc1 <- function(x) {!uj::nr2_nc1(x)}

#' @rdname N
#' @export
not_nr2_nc2 <- function(x) {!uj::nr2_nc2(x)}

#' @rdname N
#' @export
not_nr2_nc3 <- function(x) {!uj::nr2_nc3(x)}

#' @rdname N
#' @export
not_nr2_nc1p <- function(x) {!uj::nr2_nc1p(x)}

#' @rdname N
#' @export
not_nr2_nc2p <- function(x) {!uj::nr2_nc2p(x)}

#' @rdname N
#' @export
not_nr2_nc3p <- function(x) {!uj::nr2_nc3p(x)}

#' @rdname N
#' @export
not_nr3_nc0 <- function(x) {!uj::nr3_nc0(x)}

#' @rdname N
#' @export
not_nr3_nc1 <- function(x) {!uj::nr3_nc1(x)}

#' @rdname N
#' @export
not_nr3_nc2 <- function(x) {!uj::nr3_nc2(x)}

#' @rdname N
#' @export
not_nr3_nc3 <- function(x) {!uj::nr3_nc3(x)}

#' @rdname N
#' @export
not_nr3_nc1p <- function(x) {!uj::nr3_nc1p(x)}

#' @rdname N
#' @export
not_nr3_nc2p <- function(x) {!uj::nr3_nc2p(x)}

#' @rdname N
#' @export
not_nr3_nc3p <- function(x) {!uj::nr3_nc3p(x)}

#' @rdname N
#' @export
not_nr1p_nc0 <- function(x) {!uj::nr1p_nc0(x)}

#' @rdname N
#' @export
not_nr1p_nc1 <- function(x) {!uj::nr1p_nc1(x)}

#' @rdname N
#' @export
not_nr1p_nc2 <- function(x) {!uj::nr1p_nc2(x)}

#' @rdname N
#' @export
not_nr1p_nc1p <- function(x) {!uj::nr1p_nc1p(x)}

#' @rdname N
#' @export
not_nr1p_nc2p <- function(x) {!uj::nr1p_nc2p(x)}

#' @rdname N
#' @export
not_nr1p_nc3p <- function(x) {!uj::nr1p_nc3p(x)}

#' @rdname N
#' @export
not_nr2p_nc0 <- function(x) {!uj::nr2p_nc0(x)}

#' @rdname N
#' @export
not_nr2p_nc1 <- function(x) {!uj::nr2p_nc1(x)}

#' @rdname N
#' @export
not_nr2p_nc2 <- function(x) {!uj::nr2p_nc2(x)}

#' @rdname N
#' @export
not_nr2p_nc1p <- function(x) {!uj::nr2p_nc1p(x)}

#' @rdname N
#' @export
not_nr2p_nc2p <- function(x) {!uj::nr2p_nc2p(x)}

#' @rdname N
#' @export
not_nr2p_nc3p <- function(x) {!uj::nr2p_nc3p(x)}

#' @rdname N
#' @export
not_nr3p_nc0 <- function(x) {!uj::nr3p_nc0(x)}

#' @rdname N
#' @export
not_nr3p_nc1 <- function(x) {!uj::nr3p_nc1(x)}

#' @rdname N
#' @export
not_nr3p_nc2 <- function(x) {!uj::nr3p_nc2(x)}

#' @rdname N
#' @export
not_nr3p_nc1p <- function(x) {!uj::nr3p_nc1p(x)}

#' @rdname N
#' @export
not_nr3p_nc2p <- function(x) {!uj::nr3p_nc2p(x)}

#' @rdname N
#' @export
not_nr3p_nc3p <- function(x) {!uj::nr3p_nc3p(x)}

# ndim ####

#' @rdname N
#' @export
ndim <- function(x) {
  if (base::is.null(x)) {return(0)}
  d <- base::dim(x)
  if (base::is.null(d)) {1} else {base::length(d)}
}

#' @rdname N
#' @export
ndim0 <- function(x) {uj::ndim(x) == 0}

#' @rdname N
#' @export
ndim1 <- function(x) {uj::ndim(x) == 1}

#' @rdname N
#' @export
ndim2 <- function(x) {uj::ndim(x) == 2}

#' @rdname N
#' @export
ndim3 <- function(x) {uj::ndim(x) == 3}

#' @rdname N
#' @export
ndim1p <- function(x) {uj::ndim(x) > 0}

#' @rdname N
#' @export
ndim2p <- function(x) {uj::ndim(x) > 1}

#' @rdname N
#' @export
ndim3p <- function(x) {uj::ndim(x) > 2}

#' @rdname N
#' @export
not_ndim0 <- function(x) {!uj::ndim0(x)}

#' @rdname N
#' @export
not_ndim1 <- function(x) {!uj::ndim1(x)}

#' @rdname N
#' @export
not_ndim2 <- function(x) {!uj::ndim2(x)}

#' @rdname N
#' @export
not_ndim3 <- function(x) {!uj::ndim3(x)}

#' @rdname N
#' @export
not_ndim1p <- function(x) {!uj::ndim1p(x)}

#' @rdname N
#' @export
not_ndim2p <- function(x) {!uj::ndim2p(x)}

#' @rdname N
#' @export
not_ndim3p <- function(x) {!uj::ndim3p(x)}

# nnav ####

#' @rdname N
#' @export
nnav <- function(x) {base::length(base::which(base::is.na(uj::av(x))))}

#' @rdname N
#' @export
nnav0 <- function(x) {uj::nnav(x) == 0}

#' @rdname N
#' @export
nnav1 <- function(x) {uj::nnav(x) == 1}

#' @rdname N
#' @export
nnav2 <- function(x) {uj::nnav(x) == 2}

#' @rdname N
#' @export
nnav3 <- function(x) {uj::nnav(x) == 3}

#' @rdname N
#' @export
nnav1p <- function(x) {uj::nnav(x) > 0}

#' @rdname N
#' @export
nnav2p <- function(x) {uj::nnav(x) > 1}

#' @rdname N
#' @export
nnav3p <- function(x) {uj::nnav(x) > 2}

#' @rdname N
#' @export
not_nnav0 <- function(x) {!uj::nnav0(x)}

#' @rdname N
#' @export
not_nnav1 <- function(x) {!uj::nnav1(x)}

#' @rdname N
#' @export
not_nnav2 <- function(x) {!uj::nnav2(x)}

#' @rdname N
#' @export
not_nnav3 <- function(x) {!uj::nnav3(x)}

#' @rdname N
#' @export
not_nnav1p <- function(x) {!uj::nnav1p(x)}

#' @rdname N
#' @export
not_nnav2p <- function(x) {!uj::nnav2p(x)}

#' @rdname N
#' @export
not_nnav3p <- function(x) {!uj::nnav3p(x)}

# nokv ####

#' @rdname N
#' @export
nokv <- function(x) {base::length(base::which(!base::is.na(uj::av(x))))}

#' @rdname N
#' @export
nokv0 <- function(x) {uj::nokv(x) == 0}

#' @rdname N
#' @export
nokv1 <- function(x) {uj::nokv(x) == 1}

#' @rdname N
#' @export
nokv2 <- function(x) {uj::nokv(x) == 2}

#' @rdname N
#' @export
nokv3 <- function(x) {uj::nokv(x) == 3}

#' @rdname N
#' @export
nokv1p <- function(x) {uj::nokv(x) > 0}

#' @rdname N
#' @export
nokv2p <- function(x) {uj::nokv(x) > 1}

#' @rdname N
#' @export
nokv3p <- function(x) {uj::nokv(x) > 2}

#' @rdname N
#' @export
not_nokv0 <- function(x) {!uj::nokv0(x)}

#' @rdname N
#' @export
not_nokv1 <- function(x) {!uj::nokv1(x)}

#' @rdname N
#' @export
not_nokv2 <- function(x) {!uj::nokv2(x)}

#' @rdname N
#' @export
not_nokv3 <- function(x) {!uj::nokv3(x)}

#' @rdname N
#' @export
not_nokv1p <- function(x) {!uj::nokv1p(x)}

#' @rdname N
#' @export
not_nokv2p <- function(x) {!uj::nokv2p(x)}

#' @rdname N
#' @export
not_nokv3p <- function(x) {!uj::nokv3p(x)}

# ns (lengths) ####

#' @rdname N
#' @export
ns <- function(x) {base::lengths(x)}

#' @rdname N
#' @export
ns0 <- function(x) {base::all(uj::ns(x) == 0)}

#' @rdname N
#' @export
ns1 <- function(x) {base::all(uj::ns(x) == 1)}

#' @rdname N
#' @export
ns2 <- function(x) {base::all(uj::ns(x) == 2)}

#' @rdname N
#' @export
ns3 <- function(x) {base::all(uj::ns(x) == 3)}

#' @rdname N
#' @export
ns1p <- function(x) {base::all(uj::ns(x) > 0)}

#' @rdname N
#' @export
ns2p <- function(x) {base::all(uj::ns(x) > 1)}

#' @rdname N
#' @export
ns3p <- function(x) {base::all(uj::ns(x) > 2)}

#' @rdname N
#' @export
not_ns0 <- function(x) {base::all(!uj::ns0(x))}

#' @rdname N
#' @export
not_ns1 <- function(x) {base::all(!uj::ns1(x))}

#' @rdname N
#' @export
not_ns2 <- function(x) {base::all(!uj::ns2(x))}

#' @rdname N
#' @export
not_ns3 <- function(x) {base::all(!uj::ns3(x))}

#' @rdname N
#' @export
not_ns1p <- function(x) {base::all(!uj::ns1p(x))}

#' @rdname N
#' @export
not_ns2p <- function(x) {base::all(!uj::ns2p(x))}

#' @rdname N
#' @export
not_ns3p <- function(x) {base::all(!uj::ns3p(x))}

# ...n ####

#' @rdname N
#' @export
...n <- function(..., .n = NULL, .min = NULL, .max = NULL, .eq = F, .a = F, .na = T, .vals = NULL, .lt = NULL, .le = NULL, .ge = NULL, .gt = NULL) {
  errs <- uj:::.n_errs(.n = .n, .min = .min, .max = .max, .eq = .eq, .a = .a, .na = .na)
  if (!base::is.null(errs)) {uj::stopperr(errs)}
  dots <- base::list(...)
  atoms <- uj::av(dots)
  if (base::isTRUE(.a)) {dots <- list(atoms)}
  ok.restrict <- function() {
    if (!is.null(.vals) & !is.null(.ge)) {if (!uj::comparable(.vals, .ge)) {return(F)}}
    if (!is.null(.vals) & !is.null(.gt)) {if (!uj::comparable(.vals, .gt)) {return(F)}}
    if (!is.null(.vals) & !is.null(.le)) {if (!uj::comparable(.vals, .le)) {return(F)}}
    if (!is.null(.vals) & !is.null(.lt)) {if (!uj::comparable(.vals, .lt)) {return(F)}}
    if (!is.null(.ge) & !is.null(.gt)) {if (!uj::comparable(.ge, .gt)) {return(F)}}
    if (!is.null(.ge) & !is.null(.le)) {if (!uj::comparable(.ge, .le)) {return(F)}}
    if (!is.null(.ge) & !is.null(.lt)) {if (!uj::comparable(.ge, .lt)) {return(F)}}
    if (!is.null(.gt) & !is.null(.le)) {if (!uj::comparable(.gt, .le)) {return(F)}}
    if (!is.null(.gt) & !is.null(.lt)) {if (!uj::comparable(.gt, .lt)) {return(F)}}
    if (!is.null(.le) & !is.null(.lt)) {if (!uj::comparable(.le, .lt)) {return(F)}}
    T
  }
  ok.dots.vals <- function() {
    if (!is.null(.vals)) {if (!base::all(base::sapply(dots, uj::comparable_xy, y = .vals))) {return(F)}}
    T
  }
  ok.dots.ineq <- function() {
    if (!is.null(.ge)) {if (!base::all(base::sapply(dots, uj::comparable_xy, y = .ge))) {return(F)}}
    if (!is.null(.gt)) {if (!base::all(base::sapply(dots, uj::comparable_xy, y = .gt))) {return(F)}}
    if (!is.null(.le)) {if (!base::all(base::sapply(dots, uj::comparable_xy, y = .le))) {return(F)}}
    if (!is.null(.lt)) {if (!base::all(base::sapply(dots, uj::comparable_xy, y = .lt))) {return(F)}}
    T
  }
  if (base::...length() == 0) {errs <- base::c(errs, "[...] is empty.")}
  if (!is.null(.vals) & (!uj::.cmp_atm(.vals))) {errs <- base::c(errs, "[.vals] must be NULL or a complete atomic object (?cmp_atm).")}
  if (!is.null(.ge) & (!uj::.cmp_srt_scl(.ge))) {errs <- base::c(errs, "[.ge] must be NULL or a complete sortable scalar (?cmp_srt_scl).")}
  if (!is.null(.gt) & (!uj::.cmp_srt_scl(.gt))) {errs <- base::c(errs, "[.gt] must be NULL or a complete sortable scalar (?cmp_srt_scl).")}
  if (!is.null(.le) & (!uj::.cmp_srt_scl(.le))) {errs <- base::c(errs, "[.le] must be NULL or a complete sortable scalar (?cmp_srt_scl).")}
  if (!is.null(.lt) & (!uj::.cmp_srt_scl(.lt))) {errs <- base::c(errs, "[.lt] must be NULL or a complete sortable scalar (?cmp_srt_scl).")}
  if (!base::is.null(errs)) {uj::stopperr(errs)}
  if (!.na & !base::any(base::is.na(atoms))) {errs <- base::c(errs, "[.na = F] but arguments in [...] contains NA values.")}
  if (!ok.restrict()) {errs <- base::c(errs, "when two or more of [.ge], [.gt], [.le], [.lt], and [.vals] are not NULL, they must be comparable (?comparable) with each other.")}
  if (!ok.dots.ineq()) {errs <- base::c(errs, "when [.ge], [.gt], [.le], and/or [.lt] are not NULL, they must be comparable (?comparable) with all [...] args.")}
  if (!ok.dots.vals()) {errs <- base::c(errs, "when [.vals] is not NULL, it must be compatible (?uj::compatible) with all [...] args.")}
  if (!base::is.null(errs)) {uj::stopperr(errs)}
  if (.a) {dots <- base::list(atoms)}
  for (i in 1:uj::N(dots)) {
    dot <- dots[[i]]
    if (!base::is.null(.vals)) {dot <- dot[dot %in% .vals]}
    if (!base::is.null(.lt)) {dot <- dot[dot <  .lt]}
    if (!base::is.null(.le)) {dot <- dot[dot <= .le]}
    if (!base::is.null(.ge)) {dot <- dot[dot >= .ge]}
    if (!base::is.null(.gt)) {dot <- dot[dot >  .gt]}
    dots[[i]] <- dot
  }
  uj::n_check(uj::ns(dots), .n = .n, .min = .min, .max = .max, .eq = .eq)
}

#' @rdname N
#' @export
...n0 <- nd0

#' @rdname N
#' @export
...n1 <- nd1

#' @rdname N
#' @export
...n2 <- nd2

#' @rdname N
#' @export
...n3 <- nd3

#' @rdname N
#' @export
...n1p <- nd1p

#' @rdname N
#' @export
...n2p <- nd2p

#' @rdname N
#' @export
...n3p <- nd3p

#' @rdname N
#' @export
...not_n0 <- not_nd0

#' @rdname N
#' @export
...not_n1 <- not_nd1

#' @rdname N
#' @export
...not_n2 <- not_nd2

#' @rdname N
#' @export
...not_n3 <- not_nd3

#' @rdname N
#' @export
...not_n1p <- not_nd1p

#' @rdname N
#' @export
...not_n2p <- not_nd2p

#' @rdname N
#' @export
...not_n3p <- not_nd3p

#' @rdname N
#' @export
...ns <- function(..., .n = NULL, .min = NULL, .max = NULL, .na = T, .vals = NULL, .lt = NULL, .le = NULL, .ge = NULL, .gt = NULL) {uj::...n(..., .n = .n, .min = .min, .max = .max, .na = .na, .a = F, .vals = .vals, .lt = .lt, .le = .le, .ge = .ge, .gt = .gt)}

#' @rdname N
#' @export
...minn <- function(..., .na = T, .vals = NULL, .lt = NULL, .le = NULL, .ge = NULL, .gt = NULL) {base::min(uj::...n(..., .na = .na, .a = F, .vals = .vals, .lt = .lt, .le = .le, .ge = .ge, .gt = .gt))}

#' @rdname N
#' @export
...maxn <- function(..., .na = T, .vals = NULL, .lt = NULL, .le = NULL, .ge = NULL, .gt = NULL) {base::max(uj::...n(..., .na = .na, .a = F, .vals = .vals, .lt = .lt, .le = .le, .ge = .ge, .gt = .gt))}

#' @rdname N
#' @export
...neq <- function(..., .min = NULL, .max = NULL, .na = T, .vals = NULL, .lt = NULL, .le = NULL, .ge = NULL, .gt = NULL) {uj::...n(..., .min = .min, .max = .max, .eq = T, .na = .na, .a = F, .vals = .vals, .lt = .lt, .le = .le, .ge = .ge, .gt = .gt)}

#' @rdname N
#' @export
...nw <- function(..., .n = NULL, .min = NULL, .max = NULL, .eq = F, .na = F, .a = T) {
  dots <- list(...)
  atoms <- uj::av(dots)
  errs <- uj:::.n_errs(.n = .n, .min = .min, .max = .max, .eq = .eq, .na = .na, .a = .a)
  if (!is.null(errs)) {uj::stopperr(errs)}
  if (base::isTRUE(.na) & base::any(base::is.na(atoms))) {uj::stopperr("[.na = F] but arguments in [...] contains NA values.")}
  if (base::isTRUE(.a)) {dots <- base::list(atoms)}
  x <- uj::av(base::sapply(dots, function(x) {base::length(base::which(x))}))
  uj::n_check(x, .n = .n, .min = .min, .max = .max, .eq = .eq)
}

#' @rdname N
#' @export
...nt <- ...nw

#' @rdname N
#' @export
...nf <- function(..., .n = NULL, .min = NULL, .max = NULL, .eq = F, .na = F, .a = T) {
  dots <- list(...)
  atoms <- uj::av(dots)
  errs <- uj:::.n_errs(.n = .n, .min = .min, .max = .max, .eq = .eq, .na = .na, .a = .a)
  if (!is.null(errs)) {uj::stopperr(errs)}
  if (base::isTRUE(.na) & base::any(base::is.na(atoms))) {uj::stopperr("[.na = F] but arguments in [...] contains NA values.")}
  if (base::isTRUE(.a)) {dots <- base::list(atoms)}
  x <- uj::av(base::sapply(dots, function(x) {base::length(base::which(!x))}))
  uj::n_check(x, .n = .n, .min = .min, .max = .max, .eq = .eq)
}

#' @rdname N
#' @export
...nu <- function(..., .n = NULL, .min = NULL, .max = NULL, .eq = F, .na = T, .a = T) {
  dots <- list(...)
  atoms <- uj::av(dots)
  errs <- uj:::.n_errs(.n = .n, .min = .min, .max = .max, .eq = .eq, .na = .na, .a = .a)
  if (!is.null(errs)) {uj::stopperr(errs)}
  if (base::isTRUE(.na) & base::any(base::is.na(atoms))) {uj::stopperr("[.na = F] but arguments in [...] contains NA values.")}
  if (base::isTRUE(.a)) {dots <- base::list(atoms)}
  x <- uj::av(base::sapply(dots, function(x) {base::length(base::unique(x))}))
  uj::n_check(x, .n = .n, .min = .min, .max = .max, .eq = .eq)
}

#' @rdname N
#' @export
...nr <- function(..., .n = NULL, .min = NULL, .max = NULL, .eq = F) {
  dots <- base::list(...)
  errs <- uj:::.n_errs(.n = .n, .min = .min, .max = .max, .eq = .eq)
  if (!is.null(errs)) {uj::stopperr(errs)}
  x <- uj::av(base::sapply(dots, base::nrow))
  uj::n_check(x, .n = .n, .min = .min, .max = .max, .eq = .eq)
}

#' @rdname N
#' @export
...nc <- function(..., .n = NULL, .min = NULL, .max = NULL, .eq = F) {
  dots <- base::list(...)
  errs <- uj:::.n_errs(.n = .n, .min = .min, .max = .max, .eq = .eq)
  if (!is.null(errs)) {uj::stopperr(errs)}
  x <- uj::av(base::sapply(dots, base::ncol))
  uj::n_check(x, .n = .n, .min = .min, .max = .max, .eq = .eq)
}

#' @rdname N
#' @export
...n_ch <- function(..., .n = NULL, .min = NULL, .max = NULL, .eq = F, .na = F, .a = T) {
  dots <- base::list(...)
  atoms <- uj::av(dots)
  errs <- uj:::.n_errs(.n = .n, .min = .min, .max = .max, .eq = .eq, .na = .na, .a = .a)
  if (!is.null(errs)) {uj::stopperr(errs)}
  if (base::isTRUE(.na) & base::any(base::is.na(atoms))) {uj::stopperr("[.na = F] but arguments in [...] contains NA values.")}
  if (base::isTRUE(.a)) {dots <- base::list(atoms)}
  x <- uj::av(base::sapply(dots, base::nchar))
  uj::n_check(x, .n = .n, .min = .min, .max = .max, .eq = .eq)
}

#' @rdname N
#' @export
...n_nav <- function(..., .n = NULL, .min = NULL, .max = NULL, .eq = F, .a = T) {
  dots <- base::list(...)
  atoms <- uj::av(dots)
  errs <- uj:::.n_errs(.n = .n, .min = .min, .max = .max, .eq = .eq, .a = .a)
  if (!is.null(errs)) {uj::stopperr(errs)}
  if (base::isTRUE(.a)) {dots <- base::list(atoms)}
  x <- uj::av(base::sapply(dots, function(x) {base::length(base::which(base::is.na(x)))}))
  uj::n_check(x, .n = .n, .min = .min, .max = .max, .eq = .eq)
}

#' @rdname N
#' @export
...n_okv <- function(..., .n = NULL, .min = NULL, .max = NULL, .eq = F, .a = T) {
  dots <- base::list(...)
  atoms <- uj::av(dots)
  errs <- uj:::.n_errs(.n = .n, .min = .min, .max = .max, .eq = .eq, .a = .a)
  if (!is.null(errs)) {uj::stopperr(errs)}
  if (base::isTRUE(.a)) {dots <- base::list(atoms)}
  x <- uj::av(base::sapply(dots, function(x) {base::length(base::which(!base::is.na(x)))}))
  uj::n_check(x, .n = .n, .min = .min, .max = .max, .eq = .eq)
}

#' @rdname N
#' @export
...nv <- function(..., .n = NULL, .min = NULL, .max = NULL, .eq = F, .a = T) {
  dots <- base::list(...)
  atoms <- uj::av(dots)
  errs <- uj:::.n_errs(.n = .n, .min = .min, .max = .max, .eq = .eq, .a = .a)
  if (!is.null(errs)) {uj::stopperr(errs)}
  if (.a) {x <- base::length(atoms)} else {x <- base::lengths(dots)}
  uj::n_check(x, .n = .n, .min = .min, .max = .max, .eq = .eq)
}
