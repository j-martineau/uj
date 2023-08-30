#' @encoding UTF-8
#' @family extensions
#' @family counts
#' @family Dots
#' @title Dedicated counting functions
#' @details
#' The following functions are flexible, fully error-checked counting functions with optional count functionality driven by arguments:
#' \tabular{ll}{  `n_check`   \tab Do counts given in `x` meet criteria in `c(N, .MIN, .MAX, .EQ)`, if any? If none, returns `x`. \cr   \tab   \cr
#'                `...N`      \tab Length(s) of `...` args with optional length validations.                                   \cr   \tab   \cr
#'                `...ns`     \tab Lengths of `...` args                                                                       \cr
#'                `...nv`     \tab Length after \link[=av]{atomizing}.                                                         \cr
#'                `...nch`    \tab Number of characters in each element.                                                       \cr
#'                `...nrc`    \tab `NROW(x) * NCOL(x)`                                                                         \cr
#'                `...nnav`   \tab Number of atomic `NA` values.                                                               \cr
#'                `...nokv`   \tab Number of atomic non-`NA` values.                                                           \cr
#'                `...neq`    \tab Are `...` arg lengths all the same?                                                         \cr
#'                `...ndif`   \tab Are `...` arg lengths *not* all the same?                                                   \cr
#'                `...minn`   \tab .MIN `...` arg length.                                                                       \cr
#'                `...maxn`   \tab .MAX `...` arg length.                                                                         }
#' **Functions evaluating length/count (in)equality**
#' \tabular{ll}{  `ndif`   \tab Lengths of `x` and `y` are equal.   \cr
#'                `neq`    \tab Lengths of `x` and `y` are different. }
#' **Functions getting counts/lengths**
#' \tabular{ll}{  `N`             \tab `length(x)`                    \cr   \tab   \cr
#'                `nd, ndots`     \tab `...length()`                               \cr
#'                `nc`            \tab `NCOL(x)`                                   \cr
#'                `nr`            \tab `NROW(x)`                                   \cr
#'                `ns`            \tab `lengths(x)`                                \cr
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
#' @param x \link[=Nnw]{non-negative whole-number} object.
#' @param ... One or more arguments to be examined for counts.
#' @param .N Optional \link[=cmp_nnw_vec]{complete non-negative whole-number vec} of valid element, row, or column counts.
#' @param .MIN Optional complete non-negative whole-number scalar giving minimum valid element, row, or column counts.
#' @param .MAX Optional complete non-negative whole-number scalar giving maximum valid element, row, or column counts.
#' @param .EQ Non-`NA` scalar indicating whether all counts must be equal.
#' @param .NA Non-`NA` scalar whether `NA` values are allowed.
#' @param .A Non-`NA` scalar indicating whether to \link[=av]{atomize} `...` to create a single atomic vector before processing. If `.A = FALSE`, each argument in `...` is processed separately.
#' @param .VALS Optional \link[=atm_vec]{atomic vec} indicating specific values to be counted.
#' @param .LT Optional \link[=cmp_srt_scl]{complete sortable scalar} indicating specific values elements of `...` arguments must be less than in order to be counted.
#' @param .LE Optional complete sortable scalar indicating specific values elements of `...` arguments must be less than or equal to in order to be counted
#' @param .GE Optional complete sortable scalar indicating specific values elements of `...` arguments must be greater than or equal to in order to be counted.
#' @param .GT Optional complete sortable scalar indicating specific values elements of `...` arguments must be greater than in order to be counted.
#' @return May be non-negative integer or a logical scalar or vector, depending the properties of primary argument(s) (i.e., `x` and `...`) and optional arguments (i.e., all others).
#' @examples
#' N <- 0:15
#' nis(N, .N = 0:5)
#' nis(N, .MIN = 3, .MAX = 12)
#' nis(N, .EQ = T)
#' nis(rep(0, 3), .EQ = T)
#' nx(letters, LETTERS, 0:9, NULL)
#' nx(letters, LETTERS, .VALS = letters)
#' nx(letters, LETTERS, .LE = "M", .GE = "m")
#' nx(letters, LETTERS, .LT = "M", .GT = "m")
#' n0(letters, LETTERS, TRUE, 0:1, 0:2, 0:9)
#' n1(letters, LETTERS, TRUE, 0:1, 0:2, 0:9)
#' n1p(letters, LETTERS, TRUE, 0:1, 0:2, 0:9)
#' n2p(letters, LETTERS, TRUE, 0:1, 0:2, 0:9)
#' nmin(letters, LETTERS, 0:9)
#' nmax(letters, LETTERS, 0:9)
#' nsame(letters, LETTERS, 0:9)
#' nsame(letters, LETTERS)
#' nsame(letters, LETTERS, .LT = "M", .GT = "m")
#' nmin(letters, LETTERS, 0:9, .MIN = 11)
#' nmax(letters, LETTERS, 0:9, .MAX = 11)
#' nsame(letters, LETTERS, 0:9, .MIN = 11, .MAX = 25)
#' nw(0:99 %in% 50:59, 0:99 %in% 41:49)
#' nt(0:99 %in% 50:59, 0:99 %in% 41:49)
#' nw(0:99 %in% 50:59, 0:99 %in% 41:49, .N = 0:9)
#' nw(0:99 %in% 50:59, 0:99 %in% 41:49, .MIN = 9, .MAX = 19, .EQ = T)
#' nf(0:99 %in% 50:59, 0:99 %in% 41:49)
#' nf(0:99 %in% 50:59, 0:99 %in% 41:49, .N = 0:9)
#' nf(0:99 %in% 50:59, 0:99 %in% 41:49, .MIN = 9, .MAX = 19)
#' nf(0:99 %in% 50:59, 0:99 %in% 41:49, .MIN = 9, .MAX = 19, .EQ = T)
#' nr(tibble(letters, LETTERS), matrix(c(letters, LETTERS), nrow = 2))
#' nc(tibble(letters, LETTERS), matrix(c(letters, LETTERS), nrow = 2))
#' nch(letters, .EQ = T)
#' nch(letters, "a string")
#' nch(letters, "a string", .A = T)
#' @export
N <- base::length

#' @rdname N
#' @export
n_check <- function(x, .N = NULL, .MIN = NULL, .MAX = NULL, .EQ = FALSE) {
  errs <- uj:::.n_errs(.N = .N, .MIN = .MIN, .MAX = .MAX, .EQ = .EQ)
  if (!base::is.null(errs)) {uj::stopperr(errs, .PKG = "uj")}
  if (!base::is.null(.N) | !base::is.null(.MIN) | !base::is.null(.MAX) | .EQ) {
    OkN <- uj::f0(base::is.null(N), T, base::all(x %in% N))
    OkEQ <- uj::f0(.EQ, base::length(base::unique(x)) == 1, T)
    OkMin <- uj::f0(base::is.null(.MIN), T, base::all(x >= .MIN))
    OkMax <- uj::f0(base::is.null(.MAX), T, base::all(x <= .MAX))
    OkN & OkMin & OkMax & OkEQ
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
nr2_nc1p <- function(x) {uj::nr2(x) & nc1p(x)}

#' @rdname N
#' @export
nr2_nc2p <- function(x) {uj::nr2(x) & nc2p(x)}

#' @rdname N
#' @export
nr2_nc3p <- function(x) {uj::nr2(x) & nc3p(x)}

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
not_nr2_nc1p <- function(x) {!uj::nr2_nc1p(x)}

#' @rdname N
#' @export
not_nr2_nc2p <- function(x) {!uj::nr2_nc2p(x)}

#' @rdname N
#' @export
not_nr2_nc3p <- function(x) {!uj::nr2_nc3p(x)}

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
  D <- base::dim(x)
  if (base::is.null(D)) {1} else {base::length(D)}
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
ns0 <- function(x) {uj::ns(x) == 0}

#' @rdname N
#' @export
ns1 <- function(x) {uj::ns(x) == 1}

#' @rdname N
#' @export
ns2 <- function(x) {uj::ns(x) == 2}

#' @rdname N
#' @export
ns3 <- function(x) {uj::ns(x) == 3}

#' @rdname N
#' @export
ns1p <- function(x) {uj::ns(x) > 0}

#' @rdname N
#' @export
ns2p <- function(x) {uj::ns(x) > 1}

#' @rdname N
#' @export
ns3p <- function(x) {uj::ns(x) > 2}

#' @rdname N
#' @export
not_ns0 <- function(x) {!uj::ns0(x)}

#' @rdname N
#' @export
not_ns1 <- function(x) {!uj::ns1(x)}

#' @rdname N
#' @export
not_ns2 <- function(x) {!uj::ns2(x)}

#' @rdname N
#' @export
not_ns3 <- function(x) {!uj::ns3(x)}

#' @rdname N
#' @export
not_ns1p <- function(x) {!uj::ns1p(x)}

#' @rdname N
#' @export
not_ns2p <- function(x) {!uj::ns2p(x)}

#' @rdname N
#' @export
not_ns3p <- function(x) {!uj::ns3p(x)}

# ...N ####

#' @rdname N
#' @export
...N <- function(..., .N = NULL, .MIN = NULL, .MAX = NULL, .EQ = FALSE, .A = FALSE, .NA = TRUE, .VALS = NULL, .LT = NULL, .LE = NULL, .GE = NULL, .GT = NULL) {
  Errors <- uj:::.n_errs(.N = .N, .MIN = .MIN, .MAX = .MAX, .EQ = .EQ, .A = .A, .NA = .NA)
  if (!base::is.null(Errors)) {stopperr(Errors, .PKG = "uj")}
  Dots <- base::list(...)
  Atoms <- uj::av(Dots)
  if (base::isTRUE(.A)) {Dots <- list(Atoms)}
  ok.restrict <- function() {
    if (!is.null(.VALS) & !is.null(.GE)) {if (!uj::comparable(.VALS, .GE)) {return(F)}}
    if (!is.null(.VALS) & !is.null(.GT)) {if (!uj::comparable(.VALS, .GT)) {return(F)}}
    if (!is.null(.VALS) & !is.null(.LE)) {if (!uj::comparable(.VALS, .LE)) {return(F)}}
    if (!is.null(.VALS) & !is.null(.LT)) {if (!uj::comparable(.VALS, .LT)) {return(F)}}
    if (!is.null(.GE) & !is.null(.GT)) {if (!uj::comparable(.GE, .GT)) {return(F)}}
    if (!is.null(.GE) & !is.null(.LE)) {if (!uj::comparable(.GE, .LE)) {return(F)}}
    if (!is.null(.GE) & !is.null(.LT)) {if (!uj::comparable(.GE, .LT)) {return(F)}}
    if (!is.null(.GT) & !is.null(.LE)) {if (!uj::comparable(.GT, .LE)) {return(F)}}
    if (!is.null(.GT) & !is.null(.LT)) {if (!uj::comparable(.GT, .LT)) {return(F)}}
    if (!is.null(.LE) & !is.null(.LT)) {if (!uj::comparable(.LE, .LT)) {return(F)}}
    T
  }
  ok.dots.vals <- function() {
    if (!is.null(.VALS)) {if (!base::all(base::sapply(Dots, uj::comparable_xy, y = .VALS))) {return(F)}}
    T
  }
  ok.dots.ineq <- function() {
    if (!is.null(.GE)) {if (!base::all(base::sapply(Dots, uj::comparable_xy, y = .GE))) {return(F)}}
    if (!is.null(.GT)) {if (!base::all(base::sapply(Dots, uj::comparable_xy, y = .GT))) {return(F)}}
    if (!is.null(.LE)) {if (!base::all(base::sapply(Dots, uj::comparable_xy, y = .LE))) {return(F)}}
    if (!is.null(.LT)) {if (!base::all(base::sapply(Dots, uj::comparable_xy, y = .LT))) {return(F)}}
    T
  }
  if (base::...length() == 0) {Errors <- base::c(Errors, "[...] is empty.")}
  if (!is.null(.VALS) & (!uj:::.cmp_atm(.VALS))) {Errors <- base::c(Errors, "[.VALS] must be NULL or a complete atomic object (?cmp_atm).")}
  if (!is.null(.GE) & (!uj:::.cmp_srt_scl(.GE))) {Errors <- base::c(Errors, "[.GE] must be NULL or a complete sortable scalar (?cmp_srt_scl).")}
  if (!is.null(.GT) & (!uj:::.cmp_srt_scl(.GT))) {Errors <- base::c(Errors, "[.GT] must be NULL or a complete sortable scalar (?cmp_srt_scl).")}
  if (!is.null(.LE) & (!uj:::.cmp_srt_scl(.LE))) {Errors <- base::c(Errors, "[.LE] must be NULL or a complete sortable scalar (?cmp_srt_scl).")}
  if (!is.null(.LT) & (!uj:::.cmp_srt_scl(.LT))) {Errors <- base::c(Errors, "[.LT] must be NULL or a complete sortable scalar (?cmp_srt_scl).")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, .PKG = "uj")}
  if (!.NA & !base::any(base::is.na(Atoms))) {Errors <- base::c(Errors, "[.NA = FALSE] but arguments in [...] contains NA values.")}
  if (!ok.restrict()) {Errors <- base::c(Errors, "when two or more of [.GE], [.GT], [.LE], [.LT], and [.VALS] are not NULL, they must be comparable (?comparable) with each other.")}
  if (!ok.dots.ineq()) {Errors <- base::c(Errors, "when [.GE], [.GT], [.LE], and/or [.LT] are not NULL, they must be comparable (?comparable) with all [...] args.")}
  if (!ok.dots.vals()) {Errors <- base::c(Errors, "when [.VALS] is not NULL, it must be compatible (?uj::compatible) with all [...] args.")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, .PKG = "uj")}
  if (.A) {Dots <- base::list(Atoms)}
  for (i in 1:uj::N(Dots)) {
    Dot <- Dots[[i]]
    if (!base::is.null(.VALS)) {Dot <- Dot[Dot %in% .VALS]}
    if (!base::is.null(.LT)) {Dot <- Dot[Dot <  .LT]}
    if (!base::is.null(.LE)) {Dot <- Dot[Dot <= .LE]}
    if (!base::is.null(.GE)) {Dot <- Dot[Dot >= .GE]}
    if (!base::is.null(.GT)) {Dot <- Dot[Dot >  .GT]}
    Dots[[i]] <- Dot
  }
  uj::n_check(uj::ns(Dots), .N = .N, .MIN = .MIN, .MAX = .MAX, .EQ = .EQ)
}

#' @rdname N
#' @export
...ns <- function(..., .N = NULL, .MIN = NULL, .MAX = NULL, .NA = TRUE, .VALS = NULL, .LT = NULL, .LE = NULL, .GE = NULL, .GT = NULL) {uj::...N(..., .N = .N, .MIN = .MIN, .MAX = .MAX, .NA = .NA, .A = F, .VALS = .VALS, .LT = .LT, .LE = .LE, .GE = .GE, .GT = .GT)}

#' @rdname N
#' @export
...minn <- function(..., .NA = TRUE, .VALS = NULL, .LT = NULL, .LE = NULL, .GE = NULL, .GT = NULL) {base::min(uj::...N(..., .NA = .NA, .A = F, .VALS = .VALS, .LT = .LT, .LE = .LE, .GE = .GE, .GT = .GT))}

#' @rdname N
#' @export
...maxn <- function(..., .NA = TRUE, .VALS = NULL, .LT = NULL, .LE = NULL, .GE = NULL, .GT = NULL) {base::max(uj::...N(..., .NA = .NA, .A = F, .VALS = .VALS, .LT = .LT, .LE = .LE, .GE = .GE, .GT = .GT))}

#' @rdname N
#' @export
...neq <- function(..., .MIN = NULL, .MAX = NULL, .NA = TRUE, .VALS = NULL, .LT = NULL, .LE = NULL, .GE = NULL, .GT = NULL) {uj::...N(..., .MIN = .MIN, .MAX = .MAX, .EQ = T, .NA = .NA, .A = F, .VALS = .VALS, .LT = .LT, .LE = .LE, .GE = .GE, .GT = .GT)}

#' @rdname N
#' @export
...nw <- function(..., .N = NULL, .MIN = NULL, .MAX = NULL, .EQ = FALSE, .NA = FALSE, .A = TRUE) {
  Dots <- list(...)
  Atoms <- uj::av(Dots)
  Errors <- uj:::.n_errs(.N = .N, .MIN = .MIN, .MAX = .MAX, .EQ = .EQ, .NA = .NA, .A = .A)
  if (!is.null(Errors)) {uj::stopperr(Errors, .PKG = "uj")}
  if (base::isTRUE(.NA) & base::any(base::is.na(Atoms))) {uj::stopperr("[.NA = FALSE] but arguments in [...] contains NA values.", .PKG = "uj")}
  if (base::isTRUE(.A)) {Dots <- base::list(Atoms)}
  x <- uj::av(base::sapply(Dots, function(x) {base::length(base::which(x))}))
  uj::n_check(x, .N = .N, .MIN = .MIN, .MAX = .MAX, .EQ = .EQ)
}

#' @rdname N
#' @export
...nt <- ...nw

#' @rdname N
#' @export
...nf <- function(..., .N = NULL, .MIN = NULL, .MAX = NULL, .EQ = FALSE, .NA = FALSE, .A = TRUE) {
  Dots <- list(...)
  Atoms <- uj::av(Dots)
  Errors <- uj:::.n_errs(.N = .N, .MIN = .MIN, .MAX = .MAX, .EQ = .EQ, .NA = .NA, .A = .A)
  if (!is.null(Errors)) {uj::stopperr(Errors, .PKG = "uj")}
  if (base::isTRUE(.NA) & base::any(base::is.na(Atoms))) {uj::stopperr("[.NA = FALSE] but arguments in [...] contains NA values.", .PKG = "uj")}
  if (base::isTRUE(.A)) {Dots <- base::list(Atoms)}
  x <- uj::av(base::sapply(Dots, function(x) {base::length(base::which(!x))}))
  uj::n_check(x, .N = .N, .MIN = .MIN, .MAX = .MAX, .EQ = .EQ)
}

#' @rdname N
#' @export
...nu <- function(..., .N = NULL, .MIN = NULL, .MAX = NULL, .EQ = FALSE, .NA = TRUE, .A = TRUE) {
  Dots <- list(...)
  Atoms <- uj::av(Dots)
  Errors <- uj:::.n_errs(.N = .N, .MIN = .MIN, .MAX = .MAX, .EQ = .EQ, .NA = .NA, .A = .A)
  if (!is.null(Errors)) {uj::stopperr(Errors, .PKG = "uj")}
  if (base::isTRUE(.NA) & base::any(base::is.na(Atoms))) {uj::stopperr("[.NA = FALSE] but arguments in [...] contains NA values.", .PKG = "uj")}
  if (base::isTRUE(.A)) {Dots <- base::list(Atoms)}
  x <- uj::av(base::sapply(Dots, function(x) {base::length(base::unique(x))}))
  uj::n_check(x, .N = .N, .MIN = .MIN, .MAX = .MAX, .EQ = .EQ)
}

#' @rdname N
#' @export
...nr <- function(..., .N = NULL, .MIN = NULL, .MAX = NULL, .EQ = FALSE) {
  Dots <- base::list(...)
  Errors <- uj:::.n_errs(.N = .N, .MIN = .MIN, .MAX = .MAX, .EQ = .EQ)
  if (!is.null(Errors)) {uj::stopperr(Errors, .PKG = "uj")}
  x <- uj::av(base::sapply(Dots, base::nrow))
  uj::n_check(x, .N = .N, .MIN = .MIN, .MAX = .MAX, .EQ = .EQ)
}

#' @rdname N
#' @export
...nc <- function(..., .N = NULL, .MIN = NULL, .MAX = NULL, .EQ = FALSE) {
  Dots <- base::list(...)
  Errors <- uj:::.n_errs(.N = .N, .MIN = .MIN, .MAX = .MAX, .EQ = .EQ)
  if (!is.null(Errors)) {uj::stopperr(Errors, .PKG = "uj")}
  x <- uj::av(base::sapply(Dots, base::ncol))
  uj::n_check(x, .N = .N, .MIN = .MIN, .MAX = .MAX, .EQ = .EQ)
}

#' @rdname N
#' @export
...n_ch <- function(..., .N = NULL, .MIN = NULL, .MAX = NULL, .EQ = FALSE, .NA = FALSE, .A = TRUE) {
  Dots <- base::list(...)
  Atoms <- uj::av(Dots)
  Errors <- uj:::.n_errs(.N = .N, .MIN = .MIN, .MAX = .MAX, .EQ = .EQ, .NA = .NA, .A = .A)
  if (!is.null(Errors)) {uj::stopperr(Errors, .PKG = "uj")}
  if (base::isTRUE(.NA) & base::any(base::is.na(Atoms))) {uj::stopperr("[.NA = FALSE] but arguments in [...] contains NA values.", .PKG = "uj")}
  if (base::isTRUE(.A)) {Dots <- base::list(Atoms)}
  x <- uj::av(base::sapply(Dots, base::nchar))
  uj::n_check(x, .N = .N, .MIN = .MIN, .MAX = .MAX, .EQ = .EQ)
}

#' @rdname N
#' @export
...n_nav <- function(..., .N = NULL, .MIN = NULL, .MAX = NULL, .EQ = FALSE, .A = TRUE) {
  Dots <- base::list(...)
  Atoms <- uj::av(Dots)
  Errors <- uj:::.n_errs(.N = .N, .MIN = .MIN, .MAX = .MAX, .EQ = .EQ, .A = .A)
  if (!is.null(Errors)) {uj::stopperr(Errors, .PKG = "uj")}
  if (base::isTRUE(.A)) {Dots <- base::list(Atoms)}
  x <- uj::av(base::sapply(Dots, function(x) {base::length(base::which(base::is.na(x)))}))
  uj::n_check(x, .N = .N, .MIN = .MIN, .MAX = .MAX, .EQ = .EQ)
}

#' @rdname N
#' @export
...n_okv <- function(..., .N = NULL, .MIN = NULL, .MAX = NULL, .EQ = FALSE, .A = TRUE) {
  Dots <- base::list(...)
  Atoms <- uj::av(Dots)
  Errors <- uj:::.n_errs(.N = .N, .MIN = .MIN, .MAX = .MAX, .EQ = .EQ, .A = .A)
  if (!is.null(Errors)) {uj::stopperr(Errors, .PKG = "uj")}
  if (base::isTRUE(.A)) {Dots <- base::list(Atoms)}
  x <- uj::av(base::sapply(Dots, function(x) {base::length(base::which(!base::is.na(x)))}))
  uj::n_check(x, .N = .N, .MIN = .MIN, .MAX = .MAX, .EQ = .EQ)
}

#' @rdname N
#' @export
...nv <- function(..., .N = NULL, .MIN = NULL, .MAX = NULL, .EQ = FALSE, .A = TRUE) {
  Dots <- base::list(...)
  Atoms <- uj::av(Dots)
  Errors <- uj:::.n_errs(.N = .N, .MIN = .MIN, .MAX = .MAX, .EQ = .EQ, .A = .A)
  if (!is.null(Errors)) {uj::stopperr(Errors, .PKG = "uj")}
  if (.A) {x <- base::length(Atoms)} else {x <- base::lengths(Dots)}
  uj::n_check(x, .N = .N, .MIN = .MIN, .MAX = .MAX, .EQ = .EQ)
}
