#' @encoding UTF-8
#' @family extensions
#' @family counts
#' @family Dots
#' @title Dedicated counting functions
#' @details
#' The following functions are flexible, fully error-checked counting functions with optional count functionality driven by arguments:
#' \tabular{ll}{  `n_check`   \tab Do counts given in `X` meet criteria in `c(N, Min, Max, EQ)`, if any? If none, returns `X`. \cr   \tab   \cr
#'                `...N`      \tab Length(s) of `...` args with optional length validations.                                   \cr   \tab   \cr
#'                `...ns`     \tab Lengths of `...` args                                                                       \cr
#'                `...nv`     \tab Length after \link[=av]{atomizing}.                                                         \cr
#'                `...nch`    \tab Number of characters in each element.                                                       \cr
#'                `...nrc`    \tab `NROW(X) * NCOL(X)`                                                                         \cr
#'                `...nnav`   \tab Number of atomic `NA` values.                                                               \cr
#'                `...nokv`   \tab Number of atomic non-`NA` values.                                                           \cr
#'                `...neq`    \tab Are `...` arg lengths all the same?                                                         \cr
#'                `...ndif`   \tab Are `...` arg lengths *not* all the same?                                                   \cr
#'                `...minn`   \tab Min `...` arg length.                                                                       \cr
#'                `...maxn`   \tab Max `...` arg length.                                                                         }
#' **Functions evaluating length/count (in)equality**
#' \tabular{ll}{  `ndif`   \tab Lengths of `X` and `y` are equal.   \cr
#'                `neq`    \tab Lengths of `X` and `y` are different. }
#' **Functions getting counts/lengths**
#' \tabular{ll}{  `N`             \tab `length(X)`                    \cr   \tab   \cr
#'                `nd, ndots`     \tab `...length()`                               \cr
#'                `nc`            \tab `NCOL(X)`                                   \cr
#'                `nr`            \tab `NROW(X)`                                   \cr
#'                `ns`            \tab `lengths(X)`                                \cr
#'                `nu, nunq`      \tab `length(unique(Atoms(...)))`                \cr
#'                `nv, nval`      \tab `length(Atoms(...))`                        \cr
#'                `nw`            \tab `length(which(X))`             \cr   \tab   \cr
#'                `nrc`           \tab `NROW(X) * NCOL(X)`            \cr   \tab   \cr
#'                `nnav`          \tab `length(which(is.na(Atoms(X))))`            \cr
#'                `nokv`          \tab `length(which(!is.na(Atoms(X))))`           \cr
#'                `ndim`          \tab `f0(null(X), 0, f0(is_vec(X), 1, N(dim(X))))` }
#' **Functions checking for specific counts/lengths**
#' \cr\cr The functions described in the previous table also have convenience functions for checking for specific counts/lengths and for negation. Those convenience functions are formed by appending the following to function names:
#' \tabular{ll}{  `'0'`    \tab `N = 0`  \cr
#'                `'1'`    \tab `N = 1`  \cr
#'                `'2'`    \tab `N = 2`  \cr
#'                `'1P'`   \tab `N = 1+` \cr
#'                `'2P'`   \tab `N = 2+` \cr
#'                `'3P'`   \tab `N = 3+`   }
#' The negation functions are formed by prepending `not_`. All convenience functions are given in the following table:
#' \tabular{lllllll}{
#'     **Base**     \tab **`N = 0`**  \tab **`N = 1`**  \tab **`N = 2`**  \tab **`N ≥ 1`**   \tab **`N ≥ 2`**   \tab **`N ≥ 3`** \cr
#'     `N`          \tab `n0`         \tab `n1`         \tab `n2`         \tab `n1p`         \tab `n2p`         \tab `n3p`       \cr
#'     `nd`         \tab `nd0`        \tab `nd1`        \tab `nd2`        \tab `nd1p`        \tab `nd2p`        \tab `nd3p`      \cr
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
#'     `not_nc`     \tab `not_nc0`    \tab `not_nc1`    \tab `not_nc2`    \tab `not_nc1p`    \tab `not_nc2p`    \tab `not_nc3p`  \cr
#'     `not_nr`     \tab `not_nr0`    \tab `not_nr1`    \tab `not_nr2`    \tab `not_nr1p`    \tab `not_nr2p`    \tab `not_nr3p`  \cr
#'     `not_ns`     \tab `not_ns0`    \tab `not_ns1`    \tab `not_ns2`    \tab `not_ns1p`    \tab `not_ns2p`    \tab `not_ns3p`  \cr
#'     `not_nu`     \tab `not_nu0`    \tab `not_nu1`    \tab `not_nu2`    \tab `not_nu1p`    \tab `not_nu2p`    \tab `not_nu3p`  \cr
#'     `not_nv`     \tab `not_nv0`    \tab `not_nv1`    \tab `not_nv2`    \tab `not_nv1p`    \tab `not_nv2p`    \tab `not_nv3p`  \cr
#'     `not_nw`     \tab `not_nw0`    \tab `not_nw1`    \tab `not_nw2`    \tab `not_nw1p`    \tab `not_nw2p`    \tab `not_nw3p`  \cr
#'     `not_nrc`    \tab `not_nrc0`   \tab `not_nrc1`   \tab  —           \tab  —            \tab `not_nrc2p`   \tab  —          \cr
#'     `not_ndim`   \tab `not_ndim0`  \tab `not_ndim1`  \tab `not_ndim2`  \tab `not_ndim1p`  \tab `not_ndim2p`  \tab `not_ndim3p`  }
#  Finally, the following table gives functions that check for specific *combinations* of numbers of rows and columns and associated negations:
#' \tabular{lllllll}{
#'                    \tab **`nc = 0`**    \tab **`nc = 1`**    \tab **`nc = 2`**    \tab **`nc ≥ 1`**     \tab **`nc ≥ 2`**     \tab **`nc ≥ 3`**    \cr
#'     **`nr = 0`**   \tab `nr0_nc0`       \tab `nr0_nc1`       \tab `nr0_nc2`       \tab `nr0_nc1p`       \tab `nr0_nc2p`       \tab `nr0_nc3p`      \cr
#'     **`nr = 1`**   \tab `nr1_nc0`       \tab `nr1_nc1`       \tab `nr1_nc2`       \tab `nr1_nc1p`       \tab `nr1_nc2p`       \tab `nr1_nc3p`      \cr
#'     **`nr = 2`**   \tab `nr2_nc0`       \tab `nr2_nc1`       \tab `nr2_nc2`       \tab `nr2_nc1p`       \tab `nr2_nc2p`       \tab `nr2_nc3p`      \cr
#'     **`nr ≥ 1`**   \tab `nr1_pnc0`      \tab `nr1p_nc1`      \tab `nr1p_nc2`      \tab `nr1p_nc1p`      \tab `nr1p_nc2p`      \tab `nr1p_nc3p`     \cr
#'     **`nr ≥ 2`**   \tab `nr2p_nc0`      \tab `nr2p_nc1`      \tab `nr2p_nc2`      \tab `nr2p_nc1p`      \tab `nr2p_nc2p`      \tab `nr2p_nc3p`     \cr
#'     **`nr ≥ 3`**   \tab `nr3p_nc0`      \tab `nr3p_nc1`      \tab `nr3p_nc2`      \tab `nr3p_nc1p`      \tab `nr3p_nc2p`      \tab `nr3p_nc3p`     \cr
#'                    \tab                 \tab                 \tab                 \tab                  \tab                  \tab                 \cr
#'                    \tab **`nc ≠ 0`**    \tab **`nc ≠ 1`**    \tab **`nc ≠ 2`**    \tab **`nc < 1`**     \tab **`nc < 2`**     \tab **`nc < 3`**    \cr
#'     **`nr ≠ 0`**   \tab `not_nr0_nc0`   \tab `not_nr0_nc1`   \tab `not_nr0_nc2`   \tab `not_nr0_nc1p`   \tab `not_nr0_nc2p`   \tab `not_nr0_nc3p`  \cr
#'     **`nr ≠ 1`**   \tab `not_nr1_nc0`   \tab `not_nr1_nc1`   \tab `not_nr1_nc2`   \tab `not_nr1_nc1p`   \tab `not_nr1_nc2p`   \tab `not_nr1_nc3p`  \cr
#'     **`nr ≠ 2`**   \tab `not_nr2_nc0`   \tab `not_nr2_nc1`   \tab `not_nr2_nc2`   \tab `not_nr2_nc1p`   \tab `not_nr2_nc2p`   \tab `not_nr2_nc3p`  \cr
#'     **`nr < 1`**   \tab `not_nr1p_nc0`  \tab `not_nr1p_nc1`  \tab `not_nr1p_nc2`  \tab `not_nr1p_nc1p`  \tab `not_nr1p_nc2p`  \tab `not_nr1p_nc3p` \cr
#'     **`nr < 2`**   \tab `not_nr2p_nc0`  \tab `not_nr2p_nc1`  \tab `not_nr2p_nc2`  \tab `not_nr2p_nc1p`  \tab `not_nr2p_nc2p`  \tab `not_nr2p_nc3p` \cr
#'     **`nr < 3`**   \tab `not_nr3p_nc0`  \tab `not_nr3p_nc1`  \tab `not_nr3p_nc2`  \tab `not_nr3p_nc1p`  \tab `not_nr3p_nc2p`  \tab `not_nr3p_nc3p`   }
#' @param X \link[=Nnw]{non-negative whole-number} object.
#' @param ... One or more arguments to be examined for counts.
#' @param N Optional \link[=cmp_nnw_vec]{complete non-negative whole-number vec} of valid element, row, or column counts.
#' @param Min Optional complete non-negative whole-number scalar giving minimum valid element, row, or column counts.
#' @param Max Optional complete non-negative whole-number scalar giving maximum valid element, row, or column counts.
#' @param EQ Non-`NA` scalar indicating whether all counts must be equal.
#' @param Na Non-`NA` scalar whether `NA` values are allowed.
#' @param A Non-`NA` scalar indicating whether to \link[=av]{atomize} `...` to create a single atomic vector before processing. If `A = FALSE`, each argument in `...` is processed separately.
#' @param Vals Optional \link[=atm_vec]{atomic vec} indicating specific values to be counted.
#' @param LT Optional \link[=cmp_srt_scl]{complete sortable scalar} indicating specific values elements of `...` arguments must be less than in order to be counted.
#' @param LE Optional complete sortable scalar indicating specific values elements of `...` arguments must be less than or equal to in order to be counted
#' @param GE Optional complete sortable scalar indicating specific values elements of `...` arguments must be greater than or equal to in order to be counted.
#' @param GT Optional complete sortable scalar indicating specific values elements of `...` arguments must be greater than in order to be counted.
#' @return May be non-negative integer or a logical scalar or vector, depending the properties of primary argument(s) (i.e., `X` and `...`) and optional arguments (i.e., all others).
#' @examples
#' N <- 0:15
#' nis(N, N = 0:5)
#' nis(N, Nin = 3, Max = 12)
#' nis(N, EQ = T)
#' nis(rep(0, 3), EQ = T)
#' nx(letters, LETTERS, 0:9, NULL)
#' nx(letters, LETTERS, Vals = letters)
#' nx(letters, LETTERS, LE = "M", GE = "m")
#' nx(letters, LETTERS, LT = "M", GT = "m")
#' n0(letters, LETTERS, TRUE, 0:1, 0:2, 0:9)
#' n1(letters, LETTERS, TRUE, 0:1, 0:2, 0:9)
#' n1p(letters, LETTERS, TRUE, 0:1, 0:2, 0:9)
#' n2p(letters, LETTERS, TRUE, 0:1, 0:2, 0:9)
#' nmin(letters, LETTERS, 0:9)
#' nmax(letters, LETTERS, 0:9)
#' nsame(letters, LETTERS, 0:9)
#' nsame(letters, LETTERS)
#' nsame(letters, LETTERS, LT = "M", GT = "m")
#' nmin(letters, LETTERS, 0:9, Min = 11)
#' nmax(letters, LETTERS, 0:9, Max = 11)
#' nsame(letters, LETTERS, 0:9, Min = 11, Max = 25)
#' nw(0:99 %in% 50:59, 0:99 %in% 41:49)
#' nt(0:99 %in% 50:59, 0:99 %in% 41:49)
#' nw(0:99 %in% 50:59, 0:99 %in% 41:49, N = 0:9)
#' nw(0:99 %in% 50:59, 0:99 %in% 41:49, Min = 9, Max = 19, EQ = T)
#' nf(0:99 %in% 50:59, 0:99 %in% 41:49)
#' nf(0:99 %in% 50:59, 0:99 %in% 41:49, N = 0:9)
#' nf(0:99 %in% 50:59, 0:99 %in% 41:49, Min = 9, Max = 19)
#' nf(0:99 %in% 50:59, 0:99 %in% 41:49, Min = 9, Max = 19, EQ = T)
#' nr(tibble(letters, LETTERS), matrix(c(letters, LETTERS), nrow = 2))
#' nc(tibble(letters, LETTERS), matrix(c(letters, LETTERS), nrow = 2))
#' nch(letters, EQ = T)
#' nch(letters, "a string")
#' nch(letters, "a string", A = T)
#' @export
N <- base::length

#' @rdname N
#' @export
n_check <- function(X, N = NULL, Min = NULL, Max = NULL, EQ = FALSE) {
  errs <- uj:::.n_errs(N = N, Min = Min, Max = Max, EQ = EQ)
  if (!base::is.null(errs)) {uj::stopperr(errs, PKG = "uj")}
  if (!base::is.null(N) | !base::is.null(Min) | !base::is.null(Max) | EQ) {
    OkN <- uj::f0(base::is.null(N), T, base::all(X %in% N))
    OkEQ <- uj::f0(EQ, base::length(base::unique(X)) == 1, T)
    OkMin <- uj::f0(base::is.null(Min), T, base::all(X >= Min))
    OkMax <- uj::f0(base::is.null(Max), T, base::all(X <= Max))
    OkN & OkMin & OkMax & OkEQ
  } else {X}
}

# length ####

#' @rdname N
#' @export
n0 <- function(X) {uj::N(X) == 0}

#' @rdname N
#' @export
n1 <- function(X) {uj::N(X) == 1}

#' @rdname N
#' @export
n2 <- function(X) {uj::N(X) == 2}

#' @rdname N
#' @export
n3 <- function(X) {uj::N(X) == 3}

#' @rdname N
#' @export
n1p <- function(X) {uj::N(X) > 0}

#' @rdname N
#' @export
n2p <- function(X) {uj::N(X) > 1}

#' @rdname N
#' @export
n3p <- function(X) {uj::N(X) > 2}

# N-Dot-args ####

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
nu <- function(X) {base::length(base::unique(uj::av(X)))}

#' @rdname N
#' @export
nunq <- nu

#' @rdname N
#' @export
nu0 <- function(X) {uj::nu(X) == 0}

#' @rdname N
#' @export
nu1 <- function(X) {uj::nu(X) == 1}

#' @rdname N
#' @export
nu2 <- function(X) {uj::nu(X) == 2}

#' @rdname N
#' @export
nu3 <- function(X) {uj::nu(X) == 3}

#' @rdname N
#' @export
nu1p <- function(X) {uj::nu(X) > 0}

#' @rdname N
#' @export
nu2p <- function(X) {uj::nu(X) > 1}

#' @rdname N
#' @export
nu3p <- function(X) {uj::nu(X) > 2}

#' @rdname N
#' @export
not_nu0 <- function(X) {!uj::nu0(X)}

#' @rdname N
#' @export
not_nu1 <- function(X) {!uj::nu1(X)}

#' @rdname N
#' @export
not_nu2 <- function(X) {!uj::nu2(X)}

#' @rdname N
#' @export
not_nu3 <- function(X) {!uj::nu3(X)}

#' @rdname N
#' @export
not_nu1p <- function(X) {!uj::nu1p(X)}

#' @rdname N
#' @export
not_nu2p <- function(X) {!uj::nu2p(X)}

#' @rdname N
#' @export
not_nu3p <- function(X) {!uj::nu3p(X)}

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
nw <- function(X) {
  X <- uj::av(X)
  if (!base::is.logical(X)) {return(0)}
  X[base::is.na(X)] <- F
  base::length(base::which(X))
}

#' @rdname N
#' @export
nw0 <- function(X) {uj::nw(X) == 0}

#' @rdname N
#' @export
nw1 <- function(X) {uj::nw(X) == 1}

#' @rdname N
#' @export
nw2 <- function(X) {uj::nw(X) == 2}

#' @rdname N
#' @export
nw3 <- function(X) {uj::nw(X) == 3}

#' @rdname N
#' @export
nw1p <- function(X) {uj::nw(X) > 0}

#' @rdname N
#' @export
nw2p <- function(X) {uj::nw(X) > 1}

#' @rdname N
#' @export
nw3p <- function(X) {uj::nw(X) > 2}

#' @rdname N
#' @export
not_nw0 <- function(X) {!uj::nw0(X)}

#' @rdname N
#' @export
not_nw1 <- function(X) {!uj::nw1(X)}

#' @rdname N
#' @export
not_nw2 <- function(X) {!uj::nw2(X)}

#' @rdname N
#' @export
not_nw3 <- function(X) {!uj::nw3(X)}

#' @rdname N
#' @export
not_nw1p <- function(X) {!uj::nw1p(X)}

#' @rdname N
#' @export
not_nw2p <- function(X) {!uj::nw2p(X)}

#' @rdname N
#' @export
not_nw3p <- function(X) {!uj::nw3p(X)}

# equality ####

#' @rdname N
#' @export
neq <- function(X, y) {base::length(X) == base::length(y)}

#' @rdname N
#' @export
ndif <- function(X, y) {base::length(X) != base::length(y)}

# nr (nrows) ####

#' @rdname N
#' @export
nr <- function(X) {base::NROW(X)}

#' @rdname N
#' @export
nr0 <- function(X) {uj::nr(X) == 0}

#' @rdname N
#' @export
nr1 <- function(X) {uj::nr(X) == 1}

#' @rdname N
#' @export
nr2 <- function(X) {uj::nr(X) == 2}

#' @rdname N
#' @export
nr3 <- function(X) {uj::nr(X) == 3}

#' @rdname N
#' @export
nr1p <- function(X) {uj::nr(X) > 0}

#' @rdname N
#' @export
nr2p <- function(X) {uj::nr(X) > 1}

#' @rdname N
#' @export
nr3p <- function(X) {uj::nr(X) > 2}

#' @rdname N
#' @export
not_nr0 <- function(X) {!uj::nr0(X)}

#' @rdname N
#' @export
not_nr1 <- function(X) {!uj::nr1(X)}

#' @rdname N
#' @export
not_nr2 <- function(X) {!uj::nr2(X)}

#' @rdname N
#' @export
not_nr3 <- function(X) {!uj::nr3(X)}

#' @rdname N
#' @export
not_nr1p <- function(X) {!uj::nr1p(X)}

#' @rdname N
#' @export
not_nr2p <- function(X) {!uj::nr2p(X)}

#' @rdname N
#' @export
not_nr3p <- function(X) {!uj::nr3p(X)}

# nc (ncols) ####

#' @rdname N
#' @export
nc <- function(X) {base::NCOL(X)}

#' @rdname N
#' @export
nc0 <- function(X) {uj::nc(X) == 0}

#' @rdname N
#' @export
nc1 <- function(X) {uj::nc(X) == 1}

#' @rdname N
#' @export
nc2 <- function(X) {uj::nc(X) == 2}

#' @rdname N
#' @export
nc3 <- function(X) {uj::nc(X) == 3}

#' @rdname N
#' @export
nc1p <- function(X) {uj::nc(X) > 0}

#' @rdname N
#' @export
nc2p <- function(X) {uj::nc(X) > 1}

#' @rdname N
#' @export
nc3p <- function(X) {uj::nc(X) > 2}

#' @rdname N
#' @export
not_nc0 <- function(X) {!uj::nc0(X)}

#' @rdname N
#' @export
not_nc1 <- function(X) {!uj::nc1(X)}

#' @rdname N
#' @export
not_nc2 <- function(X) {!uj::nc2(X)}

#' @rdname N
#' @export
not_nc3 <- function(X) {!uj::nc3(X)}

#' @rdname N
#' @export
not_nc1p <- function(X) {!uj::nc1p(X)}

#' @rdname N
#' @export
not_nc2p <- function(X) {!uj::nc2p(X)}

#' @rdname N
#' @export
not_nc3p <- function(X) {!uj::nc3p(X)}

# nrc (nrows * ncols) ####

#' @rdname N
#' @export
nrc <- function(X) {base::NROW(X) & base::NCOL(X)}

#' @rdname N
#' @export
nrc0 <- function(X) {uj::nrc(X) == 0}

#' @rdname N
#' @export
nrc1 <- function(X) {uj::nrc(X) == 1}

#' @rdname N
#' @export
nrc2p <- function(X) {uj::nrc(X) > 1}

#' @rdname N
#' @export
not_nrc0 <- function(X) {uj::nrc(X) != 0}

#' @rdname N
#' @export
not_nrc1 <- function(X) {uj::nrc(X) != 1}

#' @rdname N
#' @export
not_nrc2p <- function(X) {uj::nrc(X) < 2}

# nr_nc (nrows, ncols) ####

#' @rdname N
#' @export
nr0_nc0 <- function(X) {uj::nr0(X) & nc0(X)}

#' @rdname N
#' @export
nr0_nc1 <- function(X) {uj::nr0(X) & nc1(X)}

#' @rdname N
#' @export
nr0_nc2 <- function(X) {uj::nr0(X) & nc2(X)}

#' @rdname N
#' @export
nr0_nc1p <- function(X) {uj::nr0(X) & nc1p(X)}

#' @rdname N
#' @export
nr0_nc2p <- function(X) {uj::nr0(X) & nc2p(X)}

#' @rdname N
#' @export
nr0_nc3p <- function(X) {uj::nr0(X) & nc3p(X)}

#' @rdname N
#' @export
nr1_nc0 <- function(X) {uj::nr1(X) & nc0(X)}

#' @rdname N
#' @export
nr1_nc1 <- function(X) {uj::nr1(X) & nc1(X)}

#' @rdname N
#' @export
nr1_nc2 <- function(X) {uj::nr1(X) & nc2(X)}

#' @rdname N
#' @export
nr1_nc1p <- function(X) {uj::nr1(X) & nc1p(X)}

#' @rdname N
#' @export
nr1_nc2p <- function(X) {uj::nr1(X) & nc2p(X)}

#' @rdname N
#' @export
nr1_nc3p <- function(X) {uj::nr1(X) & nc3p(X)}

#' @rdname N
#' @export
nr2_nc0 <- function(X) {uj::nr2(X) & nc0(X)}

#' @rdname N
#' @export
nr2_nc1 <- function(X) {uj::nr2(X) & nc1(X)}

#' @rdname N
#' @export
nr2_nc2 <- function(X) {uj::nr2(X) & nc2(X)}

#' @rdname N
#' @export
nr2_nc1p <- function(X) {uj::nr2(X) & nc1p(X)}

#' @rdname N
#' @export
nr2_nc2p <- function(X) {uj::nr2(X) & nc2p(X)}

#' @rdname N
#' @export
nr2_nc3p <- function(X) {uj::nr2(X) & nc3p(X)}

#' @rdname N
#' @export
nr1p_nc0 <- function(X) {uj::nr1p(X) & nc0(X)}

#' @rdname N
#' @export
nr1p_nc1 <- function(X) {uj::nr1p(X) & nc1(X)}

#' @rdname N
#' @export
nr1p_nc2 <- function(X) {uj::nr1p(X) & nc2(X)}

#' @rdname N
#' @export
nr1p_nc1p <- function(X) {uj::nr1p(X) & nc1p(X)}

#' @rdname N
#' @export
nr1p_nc2p <- function(X) {uj::nr1p(X) & nc2p(X)}

#' @rdname N
#' @export
nr1p_nc3p <- function(X) {uj::nr1p(X) & nc3p(X)}

#' @rdname N
#' @export
nr2p_nc0 <- function(X) {uj::nr2p(X) & nc0(X)}

#' @rdname N
#' @export
nr2p_nc1 <- function(X) {uj::nr2p(X) & nc1(X)}

#' @rdname N
#' @export
nr2p_nc2 <- function(X) {uj::nr2p(X) & nc2(X)}

#' @rdname N
#' @export
nr2p_nc1p <- function(X) {uj::nr2p(X) & nc1p(X)}

#' @rdname N
#' @export
nr2p_nc2p <- function(X) {uj::nr2p(X) & nc2p(X)}

#' @rdname N
#' @export
nr2p_nc3p <- function(X) {uj::nr2p(X) & nc3p(X)}

#' @rdname N
#' @export
nr3p_nc0 <- function(X) {uj::nr3p(X) & nc0(X)}

#' @rdname N
#' @export
nr3p_nc1 <- function(X) {uj::nr3p(X) & nc1(X)}

#' @rdname N
#' @export
nr3p_nc2 <- function(X) {uj::nr3p(X) & nc2(X)}

#' @rdname N
#' @export
nr3p_nc1p <- function(X) {uj::nr3p(X) & nc1p(X)}

#' @rdname N
#' @export
nr3p_nc2p <- function(X) {uj::nr3p(X) & nc2p(X)}

#' @rdname N
#' @export
nr3p_nc3p <- function(X) {uj::nr3p(X) & nc3p(X)}

#' @rdname N
#' @export
not_nr0_nc0 <- function(X) {!uj::nr0_nc0(X)}

#' @rdname N
#' @export
not_nr0_nc1 <- function(X) {!uj::nr0_nc1(X)}

#' @rdname N
#' @export
not_nr0_nc2 <- function(X) {!uj::nr0_nc2(X)}

#' @rdname N
#' @export
not_nr0_nc1p <- function(X) {!uj::nr0_nc1p(X)}

#' @rdname N
#' @export
not_nr0_nc2p <- function(X) {!uj::nr0_nc2p(X)}

#' @rdname N
#' @export
not_nr0_nc3p <- function(X) {!uj::nr0_nc3p(X)}

#' @rdname N
#' @export
not_nr1_nc0 <- function(X) {!uj::nr1_nc0(X)}

#' @rdname N
#' @export
not_nr1_nc1 <- function(X) {!uj::nr1_nc1(X)}

#' @rdname N
#' @export
not_nr1_nc2 <- function(X) {!uj::nr1_nc2(X)}

#' @rdname N
#' @export
not_nr1_nc1p <- function(X) {!uj::nr1_nc1p(X)}

#' @rdname N
#' @export
not_nr1_nc2p <- function(X) {!uj::nr1_nc2p(X)}

#' @rdname N
#' @export
not_nr1_nc3p <- function(X) {!uj::nr1_nc3p(X)}

#' @rdname N
#' @export
not_nr2_nc0 <- function(X) {!uj::nr2_nc0(X)}

#' @rdname N
#' @export
not_nr2_nc1 <- function(X) {!uj::nr2_nc1(X)}

#' @rdname N
#' @export
not_nr2_nc2 <- function(X) {!uj::nr2_nc2(X)}

#' @rdname N
#' @export
not_nr2_nc1p <- function(X) {!uj::nr2_nc1p(X)}

#' @rdname N
#' @export
not_nr2_nc2p <- function(X) {!uj::nr2_nc2p(X)}

#' @rdname N
#' @export
not_nr2_nc3p <- function(X) {!uj::nr2_nc3p(X)}

#' @rdname N
#' @export
not_nr1p_nc0 <- function(X) {!uj::nr1p_nc0(X)}

#' @rdname N
#' @export
not_nr1p_nc1 <- function(X) {!uj::nr1p_nc1(X)}

#' @rdname N
#' @export
not_nr1p_nc2 <- function(X) {!uj::nr1p_nc2(X)}

#' @rdname N
#' @export
not_nr1p_nc1p <- function(X) {!uj::nr1p_nc1p(X)}

#' @rdname N
#' @export
not_nr1p_nc2p <- function(X) {!uj::nr1p_nc2p(X)}

#' @rdname N
#' @export
not_nr1p_nc3p <- function(X) {!uj::nr1p_nc3p(X)}

#' @rdname N
#' @export
not_nr2p_nc0 <- function(X) {!uj::nr2p_nc0(X)}

#' @rdname N
#' @export
not_nr2p_nc1 <- function(X) {!uj::nr2p_nc1(X)}

#' @rdname N
#' @export
not_nr2p_nc2 <- function(X) {!uj::nr2p_nc2(X)}

#' @rdname N
#' @export
not_nr2p_nc1p <- function(X) {!uj::nr2p_nc1p(X)}

#' @rdname N
#' @export
not_nr2p_nc2p <- function(X) {!uj::nr2p_nc2p(X)}

#' @rdname N
#' @export
not_nr2p_nc3p <- function(X) {!uj::nr2p_nc3p(X)}

#' @rdname N
#' @export
not_nr3p_nc0 <- function(X) {!uj::nr3p_nc0(X)}

#' @rdname N
#' @export
not_nr3p_nc1 <- function(X) {!uj::nr3p_nc1(X)}

#' @rdname N
#' @export
not_nr3p_nc2 <- function(X) {!uj::nr3p_nc2(X)}

#' @rdname N
#' @export
not_nr3p_nc1p <- function(X) {!uj::nr3p_nc1p(X)}

#' @rdname N
#' @export
not_nr3p_nc2p <- function(X) {!uj::nr3p_nc2p(X)}

#' @rdname N
#' @export
not_nr3p_nc3p <- function(X) {!uj::nr3p_nc3p(X)}

# ndim ####

#' @rdname N
#' @export
ndim <- function(X) {
  if (base::is.null(X)) {return(0)}
  D <- base::dim(X)
  if (base::is.null(D)) {1} else {base::length(D)}
}

#' @rdname N
#' @export
ndim0 <- function(X) {uj::ndim(X) == 0}

#' @rdname N
#' @export
ndim1 <- function(X) {uj::ndim(X) == 1}

#' @rdname N
#' @export
ndim2 <- function(X) {uj::ndim(X) == 2}

#' @rdname N
#' @export
ndim3 <- function(X) {uj::ndim(X) == 3}

#' @rdname N
#' @export
ndim1p <- function(X) {uj::ndim(X) > 0}

#' @rdname N
#' @export
ndim2p <- function(X) {uj::ndim(X) > 1}

#' @rdname N
#' @export
ndim3p <- function(X) {uj::ndim(X) > 2}

#' @rdname N
#' @export
not_ndim0 <- function(X) {!uj::ndim0(X)}

#' @rdname N
#' @export
not_ndim1 <- function(X) {!uj::ndim1(X)}

#' @rdname N
#' @export
not_ndim2 <- function(X) {!uj::ndim2(X)}

#' @rdname N
#' @export
not_ndim3 <- function(X) {!uj::ndim3(X)}

#' @rdname N
#' @export
not_ndim1p <- function(X) {!uj::ndim1p(X)}

#' @rdname N
#' @export
not_ndim2p <- function(X) {!uj::ndim2p(X)}

#' @rdname N
#' @export
not_ndim3p <- function(X) {!uj::ndim3p(X)}

# nnav ####

#' @rdname N
#' @export
nnav <- function(X) {base::length(base::which(base::is.na(uj::av(X))))}

#' @rdname N
#' @export
nnav0 <- function(X) {uj::nnav(X) == 0}

#' @rdname N
#' @export
nnav1 <- function(X) {uj::nnav(X) == 1}

#' @rdname N
#' @export
nnav2 <- function(X) {uj::nnav(X) == 2}

#' @rdname N
#' @export
nnav3 <- function(X) {uj::nnav(X) == 3}

#' @rdname N
#' @export
nnav1p <- function(X) {uj::nnav(X) > 0}

#' @rdname N
#' @export
nnav2p <- function(X) {uj::nnav(X) > 1}

#' @rdname N
#' @export
nnav3p <- function(X) {uj::nnav(X) > 2}

#' @rdname N
#' @export
not_nnav0 <- function(X) {!uj::nnav0(X)}

#' @rdname N
#' @export
not_nnav1 <- function(X) {!uj::nnav1(X)}

#' @rdname N
#' @export
not_nnav2 <- function(X) {!uj::nnav2(X)}

#' @rdname N
#' @export
not_nnav3 <- function(X) {!uj::nnav3(X)}

#' @rdname N
#' @export
not_nnav1p <- function(X) {!uj::nnav1p(X)}

#' @rdname N
#' @export
not_nnav2p <- function(X) {!uj::nnav2p(X)}

#' @rdname N
#' @export
not_nnav3p <- function(X) {!uj::nnav3p(X)}

# nokv ####

#' @rdname N
#' @export
nokv <- function(X) {base::length(base::which(!base::is.na(uj::av(X))))}

#' @rdname N
#' @export
nokv0 <- function(X) {uj::nokv(X) == 0}

#' @rdname N
#' @export
nokv1 <- function(X) {uj::nokv(X) == 1}

#' @rdname N
#' @export
nokv2 <- function(X) {uj::nokv(X) == 2}

#' @rdname N
#' @export
nokv3 <- function(X) {uj::nokv(X) == 3}

#' @rdname N
#' @export
nokv1p <- function(X) {uj::nokv(X) > 0}

#' @rdname N
#' @export
nokv2p <- function(X) {uj::nokv(X) > 1}

#' @rdname N
#' @export
nokv3p <- function(X) {uj::nokv(X) > 2}

#' @rdname N
#' @export
not_nokv0 <- function(X) {!uj::nokv0(X)}

#' @rdname N
#' @export
not_nokv1 <- function(X) {!uj::nokv1(X)}

#' @rdname N
#' @export
not_nokv2 <- function(X) {!uj::nokv2(X)}

#' @rdname N
#' @export
not_nokv3 <- function(X) {!uj::nokv3(X)}

#' @rdname N
#' @export
not_nokv1p <- function(X) {!uj::nokv1p(X)}

#' @rdname N
#' @export
not_nokv2p <- function(X) {!uj::nokv2p(X)}

#' @rdname N
#' @export
not_nokv3p <- function(X) {!uj::nokv3p(X)}

# ns (lengths) ####

#' @rdname N
#' @export
ns <- function(X) {base::lengths(X)}

#' @rdname N
#' @export
ns0 <- function(X) {uj::ns(X) == 0}

#' @rdname N
#' @export
ns1 <- function(X) {uj::ns(X) == 1}

#' @rdname N
#' @export
ns2 <- function(X) {uj::ns(X) == 2}

#' @rdname N
#' @export
ns3 <- function(X) {uj::ns(X) == 3}

#' @rdname N
#' @export
ns1p <- function(X) {uj::ns(X) > 0}

#' @rdname N
#' @export
ns2p <- function(X) {uj::ns(X) > 1}

#' @rdname N
#' @export
ns3p <- function(X) {uj::ns(X) > 2}

#' @rdname N
#' @export
not_ns0 <- function(X) {!uj::ns0(X)}

#' @rdname N
#' @export
not_ns1 <- function(X) {!uj::ns1(X)}

#' @rdname N
#' @export
not_ns2 <- function(X) {!uj::ns2(X)}

#' @rdname N
#' @export
not_ns3 <- function(X) {!uj::ns3(X)}

#' @rdname N
#' @export
not_ns1p <- function(X) {!uj::ns1p(X)}

#' @rdname N
#' @export
not_ns2p <- function(X) {!uj::ns2p(X)}

#' @rdname N
#' @export
not_ns3p <- function(X) {!uj::ns3p(X)}

# ...N ####

#' @rdname N
#' @export
...N <- function(..., N = NULL, Min = NULL, max = NULL, EQ = FALSE, A = FALSE, Na = TRUE, Vals = NULL, LT = NULL, LE = NULL, GE = NULL, GT = NULL) {
  Errors <- uj:::.n_errs(N = N, Min = Min, max = max, EQ = EQ, A = A, Na = Na)
  if (!base::is.null(Errors)) {stopperr(Errors, PKG = "uj")}
  Dots <- base::list(...)
  Atoms <- uj::av(Dots)
  if (base::isTRUE(A)) {Dots <- list(Atoms)}
  ok.restrict <- function() {
    if (!is.null(Vals) & !is.null(GE)) {if (!uj::comparable(Vals, GE)) {return(F)}}
    if (!is.null(Vals) & !is.null(GT)) {if (!uj::comparable(Vals, GT)) {return(F)}}
    if (!is.null(Vals) & !is.null(LE)) {if (!uj::comparable(Vals, LE)) {return(F)}}
    if (!is.null(Vals) & !is.null(LT)) {if (!uj::comparable(Vals, LT)) {return(F)}}
    if (!is.null(GE) & !is.null(GT)) {if (!uj::comparable(GE, GT)) {return(F)}}
    if (!is.null(GE) & !is.null(LE)) {if (!uj::comparable(GE, LE)) {return(F)}}
    if (!is.null(GE) & !is.null(LT)) {if (!uj::comparable(GE, LT)) {return(F)}}
    if (!is.null(GT) & !is.null(LE)) {if (!uj::comparable(GT, LE)) {return(F)}}
    if (!is.null(GT) & !is.null(LT)) {if (!uj::comparable(GT, LT)) {return(F)}}
    if (!is.null(LE) & !is.null(LT)) {if (!uj::comparable(LE, LT)) {return(F)}}
    T
  }
  ok.dots.vals <- function() {
    if (!is.null(Vals)) {if (!base::all(base::sapply(Dots, uj::comparable_xy, y = Vals))) {return(F)}}
    T
  }
  ok.dots.ineq <- function() {
    if (!is.null(GE)) {if (!base::all(base::sapply(Dots, uj::comparable_xy, y = GE))) {return(F)}}
    if (!is.null(GT)) {if (!base::all(base::sapply(Dots, uj::comparable_xy, y = GT))) {return(F)}}
    if (!is.null(LE)) {if (!base::all(base::sapply(Dots, uj::comparable_xy, y = LE))) {return(F)}}
    if (!is.null(LT)) {if (!base::all(base::sapply(Dots, uj::comparable_xy, y = LT))) {return(F)}}
    T
  }
  if (base::...length() == 0) {Errors <- base::c(Errors, "[...] is empty.")}
  if (!is.null(Vals) & (!uj:::.cmp_atm(Vals))) {Errors <- base::c(Errors, "[Vals] must be NULL or a complete atomic object (?cmp_atm).")}
  if (!is.null(GE) & (!uj:::.cmp_srt_scl(GE))) {Errors <- base::c(Errors, "[GE] must be NULL or a complete sortable scalar (?cmp_srt_scl).")}
  if (!is.null(GT) & (!uj:::.cmp_srt_scl(GT))) {Errors <- base::c(Errors, "[GT] must be NULL or a complete sortable scalar (?cmp_srt_scl).")}
  if (!is.null(LE) & (!uj:::.cmp_srt_scl(LE))) {Errors <- base::c(Errors, "[LE] must be NULL or a complete sortable scalar (?cmp_srt_scl).")}
  if (!is.null(LT) & (!uj:::.cmp_srt_scl(LT))) {Errors <- base::c(Errors, "[LT] must be NULL or a complete sortable scalar (?cmp_srt_scl).")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  if (!Na & !base::any(base::is.na(Atoms))) {Errors <- base::c(Errors, "[Na = FALSE] but arguments in [...] contains NA values.")}
  if (!ok.restrict()) {Errors <- base::c(Errors, "when two or more of [GE], [GT], [LE], [LT], and [Vals] are not NULL, they must be comparable (?comparable) with each other.")}
  if (!ok.dots.ineq()) {Errors <- base::c(Errors, "when [GE], [GT], [LE], and/or [LT] are not NULL, they must be comparable (?comparable) with all [...] args.")}
  if (!ok.dots.vals()) {Errors <- base::c(Errors, "when [Vals] is not NULL, it must be compatible (?uj::compatible) with all [...] args.")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  if (A) {Dots <- base::list(Atoms)}
  for (i in 1:uj::N(Dots)) {
    Dot <- Dots[[i]]
    if (!base::is.null(Vals)) {Dot <- Dot[Dot %in% Vals]}
    if (!base::is.null(LT)) {Dot <- Dot[Dot <  LT]}
    if (!base::is.null(LE)) {Dot <- Dot[Dot <= LE]}
    if (!base::is.null(GE)) {Dot <- Dot[Dot >= GE]}
    if (!base::is.null(GT)) {Dot <- Dot[Dot >  GT]}
    Dots[[i]] <- Dot
  }
  uj::n_check(uj::ns(Dots), N = N, Min = Min, Max = Max, EQ = EQ)
}

#' @rdname N
#' @export
...ns <- function(..., N = NULL, Min = NULL, Max = NULL, Na = TRUE, Vals = NULL, LT = NULL, LE = NULL, GE = NULL, GT = NULL) {uj::...N(..., N = N, Min = Min, Max = Max, Na = Na, A = F, Vals = Vals, LT = LT, LE = LE, GE = GE, GT = GT)}

#' @rdname N
#' @export
...minn <- function(..., Na = TRUE, Vals = NULL, LT = NULL, LE = NULL, GE = NULL, GT = NULL) {base::min(uj::...N(..., Na = Na, A = F, Vals = Vals, LT = LT, LE = LE, GE = GE, GT = GT))}

#' @rdname N
#' @export
...maxn <- function(..., Na = TRUE, Vals = NULL, LT = NULL, LE = NULL, GE = NULL, GT = NULL) {base::max(uj::...N(..., Na = Na, A = F, Vals = Vals, LT = LT, LE = LE, GE = GE, GT = GT))}

#' @rdname N
#' @export
...neq <- function(..., Min = NULL, Max = NULL, Na = TRUE, Vals = NULL, LT = NULL, LE = NULL, GE = NULL, GT = NULL) {uj::...N(..., Min = Min, Max = Max, EQ = T, Na = Na, A = F, Vals = Vals, LT = LT, LE = LE, GE = GE, GT = GT)}

#' @rdname N
#' @export
...nw <- function(..., N = NULL, Min = NULL, Max = NULL, EQ = FALSE, Na = FALSE, A = TRUE) {
  Dots <- list(...)
  Atoms <- uj::av(Dots)
  Errors <- uj:::.n_errs(N = N, Min = Min, Max = Max, EQ = EQ, Na = Na, A = A)
  if (!is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  if (base::isTRUE(Na) & base::any(base::is.na(Atoms))) {uj::stopperr("[Na = FALSE] but arguments in [...] contains NA values.", PKG = "uj")}
  if (base::isTRUE(A)) {Dots <- base::list(Atoms)}
  X <- uj::av(base::sapply(Dots, function(X) {base::length(base::which(X))}))
  uj::n_check(X, N = N, Min = Min, Max = Max, EQ = EQ)
}

#' @rdname N
#' @export
...nt <- ...nw

#' @rdname N
#' @export
...nf <- function(..., N = NULL, Min = NULL, Max = NULL, EQ = FALSE, Na = FALSE, A = TRUE) {
  Dots <- list(...)
  Atoms <- uj::av(Dots)
  Errors <- uj:::.n_errs(N = N, Min = Min, Max = Max, EQ = EQ, Na = Na, A = A)
  if (!is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  if (base::isTRUE(Na) & base::any(base::is.na(Atoms))) {uj::stopperr("[Na = FALSE] but arguments in [...] contains NA values.", PKG = "uj")}
  if (base::isTRUE(A)) {Dots <- base::list(Atoms)}
  X <- uj::av(base::sapply(Dots, function(X) {base::length(base::which(!X))}))
  uj::n_check(X, N = N, Min = Min, Max = Max, EQ = EQ)
}

#' @rdname N
#' @export
...nu <- function(..., N = NULL, Min = NULL, Max = NULL, EQ = FALSE, Na = TRUE, A = TRUE) {
  Dots <- list(...)
  Atoms <- uj::av(Dots)
  Errors <- uj:::.n_errs(N = N, Min = Min, Max = Max, EQ = EQ, Na = Na, A = A)
  if (!is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  if (base::isTRUE(Na) & base::any(base::is.na(Atoms))) {uj::stopperr("[Na = FALSE] but arguments in [...] contains NA values.", PKG = "uj")}
  if (base::isTRUE(A)) {Dots <- base::list(Atoms)}
  X <- uj::av(base::sapply(Dots, function(X) {base::length(base::unique(X))}))
  uj::n_check(X, N = N, Min = Min, Max = Max, EQ = EQ)
}

#' @rdname N
#' @export
...nr <- function(..., N = NULL, Min = NULL, Max = NULL, EQ = FALSE) {
  Dots <- base::list(...)
  Errors <- uj:::.n_errs(N = N, Min = Min, Max = Max, EQ = EQ)
  if (!is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  X <- uj::av(base::sapply(Dots, base::nrow))
  uj::n_check(X, N = N, Min = Min, Max = Max, EQ = EQ)
}

#' @rdname N
#' @export
...nc <- function(..., N = NULL, Min = NULL, Max = NULL, EQ = FALSE) {
  Dots <- base::list(...)
  Errors <- uj:::.n_errs(N = N, Min = Min, Max = Max, EQ = EQ)
  if (!is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  X <- uj::av(base::sapply(Dots, base::ncol))
  uj::n_check(X, N = N, min = min, Max = Max, EQ = EQ)
}

#' @rdname N
#' @export
...n_ch <- function(..., N = NULL, Min = NULL, Max = NULL, EQ = FALSE, Na = FALSE, a = TRUE) {
  Dots <- base::list(...)
  Atoms <- uj::av(Dots)
  Errors <- uj:::.n_errs(N = N, Min = Min, Max = Max, EQ = EQ, Na = Na, a = a)
  if (!is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  if (base::isTRUE(Na) & base::any(base::is.na(Atoms))) {uj::stopperr("[Na = FALSE] but arguments in [...] contains NA values.", PKG = "uj")}
  if (base::isTRUE(a)) {Dots <- base::list(Atoms)}
  X <- uj::av(base::sapply(Dots, base::nchar))
  uj::n_check(X, N = N, Min = Min, Max = Max, EQ = EQ)
}

#' @rdname N
#' @export
...n_nav <- function(..., N = NULL, Min = NULL, Max = NULL, EQ = FALSE, a = TRUE) {
  Dots <- base::list(...)
  Atoms <- uj::av(Dots)
  Errors <- uj:::.n_errs(N = N, Min = Min, Max = Max, EQ = EQ, a = a)
  if (!is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  if (base::isTRUE(A)) {Dots <- base::list(Atoms)}
  X <- uj::av(base::sapply(Dots, function(X) {base::length(base::which(base::is.na(X)))}))
  uj::n_check(X, N = N, Min = Min, Max = Max, EQ = EQ)
}

#' @rdname N
#' @export
...n_okv <- function(..., N = NULL, Min = NULL, Max = NULL, EQ = FALSE, A = TRUE) {
  Dots <- base::list(...)
  Atoms <- uj::av(Dots)
  Errors <- uj:::.n_errs(N = N, Min = Min, Max = Max, EQ = EQ, A = A)
  if (!is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  if (base::isTRUE(A)) {Dots <- base::list(Atoms)}
  X <- uj::av(base::sapply(Dots, function(X) {base::length(base::which(!base::is.na(X)))}))
  uj::n_check(X, N = N, Min = Min, Max = Max, EQ = EQ)
}

#' @rdname N
#' @export
...nv <- function(..., N = NULL, Min = NULL, Max = NULL, EQ = FALSE, a = TRUE) {
  Dots <- base::list(...)
  Atoms <- uj::av(Dots)
  Errors <- uj:::.n_errs(N = N, Min = Min, Max = Max, EQ = EQ, A = A)
  if (!is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  if (A) {X <- base::length(Atoms)} else {N <- base::lengths(Dots)}
  uj::n_check(X, N = N, Min = Min, Max = Max, EQ = EQ)
}
