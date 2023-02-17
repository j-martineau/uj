#' @encoding UTF-8
#' @name basics
#' @family wraps
#' @title Wrappers for `base` functions
#' @description Wrappers for frequently used `base` package functions.
#' @details **Basic function wrappers not related to counts/lengths**
#' \tabular{ll}{  `g`                \tab `paste0(V(...), collapse = g)` (glues `...`) \cr
#'                `p`                \tab `paste(..., sep = p)`                        \cr   \tab     }
#' \tabular{ll}{  `g0`               \tab `paste0(V(...), collapse = "")`              \cr
#'                `p0`               \tab `paste(..., sep = "")`                       \cr
#'                `g1`               \tab `paste0(V(...), collapse = " ")`             \cr
#'                `p1`               \tab `paste(..., sep = " ")`                      \cr   \tab   \cr
#'                `na`               \tab `is.na(x)`                                   \cr
#'                `ok`               \tab `!is.na(x)`                                  \cr   \tab     }
#' \tabular{ll}{  `wna`              \tab `which(na(x))`                               \cr
#'                `wok`              \tab `which(ok(x))`                               \cr   \tab     }
#' \tabular{ll}{  `none`             \tab `!any(av(...))`                              \cr   \tab     }
#' \tabular{ll}{  `AV`               \tab `atomize(...)`                               \cr
#'                `UV`               \tab `unique(atomize(x))`                         \cr
#'                `WV`               \tab `which(atomize(x))`                          \cr   \tab     }
#' \tabular{ll}{  `CN`               \tab `colnames(x)`                                \cr
#'                `DN`               \tab `...names(x)`                                \cr
#'                `EN`               \tab `names(x)`                                   \cr
#'                `RN`               \tab `rownames`                                   \cr   \tab   \cr
#'                `HI`               \tab `ceiling(x)`                                 \cr
#'                `LO`               \tab `floor(x)`                                   \cr   \tab   \cr
#'                `UV`               \tab `U(V(...))`                                  \cr
#'                `WV`               \tab `W(V(...))`                                  \cr   \tab     }
#' \tabular{ll}{  `xIN`              \tab `x[isIN(x, ...)]`                            \cr
#'                `xMF`              \tab `x[isMF(x, ...)]`                            \cr
#'                `LEN`              \tab `nchar(x)`                                   \cr   \tab   \cr
#'                `MID`              \tab `substr(.)`                                  \cr   \tab   \cr
#'                `SPF`              \tab `sprintf(.)`                                 \cr   \tab     }
#' \tabular{ll}{  `LEVS`             \tab `levels(x)`                                  \cr   \tab     }
#' \tabular{ll}{  `ERR, notERR`      \tab `(!)assertthat::is.error(x)`                 \cr   \tab   \cr
#'                `UNQ, notUNQ`      \tab `(!)(length(x) == length(U(x)))`             \cr   \tab     }
#' \tabular{ll}{  `isARR, notARR`    \tab `(!)is.array(x)`                             \cr
#'                `isDTF, notDTF`    \tab `(!)is.data.frame(x)`                        \cr
#'                `isFUN, notFUN`    \tab `(!)is.function(x)`                          \cr
#'                `isLST, notLST`    \tab `(!)is.list(x)`                              \cr
#'                `isMAT, notMAT`    \tab `(!)is.matrix(x)`                            \cr
#'                `isVEC, notVEC`    \tab `(!)is.vector(x)`                            \cr   \tab   \cr
#'                `isATM, notATM`    \tab `(!)is.atomic(x)`                            \cr
#'                `isCHR, notCHR`    \tab `(!)is.character(x)`                         \cr
#'                `isFAC, notFAC`    \tab `(!)is.factor(x)`                            \cr
#'                `isINT, notINT`    \tab `(!)is.itneger(x)`                           \cr
#'                `isLGL, notLGL`    \tab `(!)is.logical(x)`                           \cr
#'                `isNUM, notNUM`    \tab `(!)is.numeric(x)`                           \cr
#'                `isORD, notORD`    \tab `(!)is.ordered(x)`                           \cr
#'                `isUNO, notUNO`    \tab `(!)(isFAC(x) & !isORD(x))`                  \cr   \tab     }
#' **Functions evaluating length/count (in)equality**
#' \tabular{ll}{  `NDIF`             \tab `length(x) != length(y)`                     \cr
#'                `NEQ`              \tab `length(x) == length(y)`                     \cr   \tab     }
#' **Functions getting counts/lengths**
#' \tabular{ll}{  `N`                \tab `length(x)`                                  \cr   \tab     }
#' \tabular{ll}{  `ND`               \tab `...length()`                                \cr
#'                `NC`               \tab `NCOL`                                       \cr
#'                `NR`               \tab `NROW`                                       \cr
#'                `NS`               \tab `lengths(x)`                                 \cr
#'                `NU`               \tab `N(U(...))`                                  \cr
#'                `NV`               \tab `N(V(...))`                                  \cr
#'                `NW`               \tab `N(W(x))`                                    \cr   \tab     }
#' \tabular{ll}{  `NUV`              \tab `N(UV(...))`                                 \cr
#'                `NWV`              \tab `N(WV(....))`                                \cr
#'                `NRC`              \tab `NR(x) * NC(x)`                              \cr   \tab     }
#' \tabular{ll}{  `NDIM`             \tab `f0(null(x), 0, f0(isVEC(x), 1, N(dim(x))))` \cr   \tab     }
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
#'     **`nr < 3`**   \tab `notnr3pnc0`   \tab `notnr3pnc1`   \tab `notnr3pnc2`   \tab `notnr3pnc1p`  \tab `notnr3pnc2p`  \tab `notnr3pnc3p`   }
#' @examples
#' vals <- c(1:3, 2:4) / 3
#' vars <- c("a", "bb", "ccc", "dddd", "ccc", "bb")
#' text <- "%s = %0.2f and %s = %0.0f"
#' vals
#' vars
#' text
#' u(vals)
#' u(vars)
#' u(data.frame(var = vars, val = vals))
#' dn(vals)
#' up(vals)
#' len(vars)
#' mid(vars, 1, 3)
#' spf(text, vars[1:3], vals[1:3], vars[4:6], vals[4:6])
#' ### more examples ###
#' @export
basics <- function() {utils::help("basics", package = "uj")}

#' @rdname basics
#' @export
none <- function(...) {!base::any(uj::av(...))}

# g (glue within) ####

#' @rdname basics
#' @export
g <- function(g, ...) {base::paste(uj::av(...), collapse = g)}

#' @rdname basics
#' @export
g0 <- function(...) {uj::g("", ...)}

#' @rdname basics
#' @export
g1 <- function(...) {uj::g(" ", ...)}

# p (paste across) ####

#' @rdname basics
#' @inherit base::paste
#' @export
p <- function(p, ...) {base::paste(..., sep = p)}

#' @rdname basics
#' @export
p0 <- function(...) {uj::p("", ...)}

#' @rdname basics
#' @export
p1 <- function(...) {uj::p(" ", ...)}

# special values ####

#' @rdname basics
#' @export
na <- function(x) {base::is.na(x)}

#' @rdname basics
#' @export
ok <- function(x) {!base::is.na(x)}

#' @rdname basics
#' @export
wna <- function(x) {base::which(base::is.na(x))}

#' @rdname basics
#' @export
wok <- function(x) {base::which(!base::is.na(x))}

#' @rdname basics
#' @export
null <- function(x) {
  x <- tryCatch(base::identity(x), error = function(e) e, finally = NULL)
  if (assertthat::is.error(x)) {T} else {base::is.null(x)}
}

#' @rdname basics
#' @export
def <- function(x) {
  x <- tryCatch(base::identity(x), error = function(e) e, finally = NULL)
  if (assertthat::is.error(x)) {F} else {!base::is.null(x)}
}

#' @rdname basics
#' @export
LO <- function(x) {base::floor(x)}

#' @rdname basics
#' @export
HI <- function(x) {base::ceiling(x)}

#' @rdname basics
#' @export
LEN <- function(x) {base::nchar(x)}

#' @rdname basics
#' @export
MID <- function(x, start, stop) {base::substr(x, start, stop)}

#' @rdname basics
#' @export
SPF <- function(fmt, ...) {base::sprintf(fmt, ...)}

#' @rdname basics
#' @export
UNQ <- function(...) {
  x <- uj::av(...)
  n <- uj::N(x)
  n > 0 & n == uj::NU(x)
}

#' @rdname basics
#' @export
notUNQ <- function(...) {!uj::UNQ(...)}

#' @rdname basics
#' @export
LEVS <- function(x) {base::levels(x)}

#' @rdname basics
#' @export
rounded <- function(x) {base::all(x == base::round(x))}

# AV WV UV

#' @rdname basics
#' @export
AV <- function(...) {
  x <- uj::failsafe(base::list(...))
  if (uj::isERR(x)) {return(base::vector(0))}
  x <- base::unlist(x, T, F)
  base::attributes(x) <- NULL
  x
}

#' @rdname basics
#' @export
WV <- function(x) {base::which(uj::AV(x))}

#' @rdname basics
#' @export
UV <- function(...) {base::unique(uj::AV(...))}

# ND (...length) ####

#' @rdname basics
#' @export
ND <- function() {base::eval.parent(base::parse(text = "base::...length()"))}

#' @rdname basics
#' @export
ND0 <- function() {base::eval.parent(base::parse(text = "base::...length()")) == 0}

#' @rdname basics
#' @export
ND1 <- function() {base::eval.parent(base::parse(text = "base::...length()")) == 1}

#' @rdname basics
#' @export
ND2 <- function() {base::eval.parent(base::parse(text = "base::...length()")) == 2}

#' @rdname basics
#' @export
ND3 <- function() {base::eval.parent(base::parse(text = "base::...length()")) == 3}

#' @rdname basics
#' @export
ND1P <- function() {base::eval.parent(base::parse(text = "base::...length()")) >= 1}

#' @rdname basics
#' @export
ND2P <- function() {base::eval.parent(base::parse(text = "base::...length()")) >= 2}

#' @rdname basics
#' @export
ND3P <- function() {base::eval.parent(base::parse(text = "base::...length()")) >= 3}

#' @rdname basics
#' @export
notND0 <- function() {base::eval.parent(base::parse(text = "base::...length()")) != 0}

#' @rdname basics
#' @export
notND1 <- function() {base::eval.parent(base::parse(text = "base::...length()")) != 1}

#' @rdname basics
#' @export
notND2 <- function() {base::eval.parent(base::parse(text = "base::...length()")) != 2}

#' @rdname basics
#' @export
notND1P <- function() {!(base::eval.parent(base::parse(text = "base::...length()")) >= 1)}

#' @rdname basics
#' @export
notND2P <- function() {!(base::eval.parent(base::parse(text = "base::...length()")) >= 2)}

#' @rdname basics
#' @export
notND3P <- function() {!(base::eval.parent(base::parse(text = "base::...length()")) >= 3)}

# N (length) ####

#' @rdname basics
#' @export
N <- function(x) {base::length(x)}

#' @rdname basics
#' @export
N0 <- function(x) {uj::N(x) == 0}

#' @rdname basics
#' @export
N1 <- function(x) {uj::N(x) == 1}

#' @rdname basics
#' @export
N2 <- function(x) {uj::N(x) == 2}

#' @rdname basics
#' @export
N1P <- function(x) {uj::N(x) >= 1}

#' @rdname basics
#' @export
N2P <- function(x) {uj::N(x) >= 2}

#' @rdname basics
#' @export
N3P <- function(x) {uj::N(x) >= 3}

#' @rdname basics
#' @export
notN0 <- function(x) {!uj::N0(x)}

#' @rdname basics
#' @export
notN1 <- function(x) {!uj::N1(x)}

#' @rdname basics
#' @export
notN2 <- function(x) {!uj::N2(x)}

#' @rdname basics
#' @export
notN1P <- function(x) {!uj::N1P(x)}

#' @rdname basics
#' @export
notN2P <- function(x) {!uj::N2P(x)}

#' @rdname basics
#' @export
notN3P <- function(x) {!uj::N3P(x)}

# NC (ncols) ####

#' @rdname basics
#' @export
NC <- function(x) {base::NCOL(x)}

#' @rdname basics
#' @export
NC0 <- function(x) {uj::NC(x) == 0}

#' @rdname basics
#' @export
NC1 <- function(x) {uj::NC(x) == 1}

#' @rdname basics
#' @export
NC2 <- function(x) {uj::NC(x) == 2}

#' @rdname basics
#' @export
NC1P <- function(x) {uj::NC(x) >= 1}

#' @rdname basics
#' @export
NC2P <- function(x) {uj::NC(x) >= 2}

#' @rdname basics
#' @export
NC3P <- function(x) {uj::NC(x) >= 3}

#' @rdname basics
#' @export
notNC0 <- function(x) {!uj::NC0(x)}

#' @rdname basics
#' @export
notNC1 <- function(x) {!uj::NC1(x)}

#' @rdname basics
#' @export
notNC2 <- function(x) {!uj::NC2(x)}

#' @rdname basics
#' @export
notNC1P <- function(x) {!uj::N1CP(x)}

#' @rdname basics
#' @export
notNC2P <- function(x) {!uj::N2CP(x)}

#' @rdname basics
#' @export
notNC3P <- function(x) {!uj::N3CP(x)}

# NR (nrows) ####

#' @rdname basics
#' @export
NR <- function(x) {base::NROW(x)}

#' @rdname basics
#' @export
NR0 <- function(x) {uj::NR(x) == 0}

#' @rdname basics
#' @export
NR1 <- function(x) {uj::NR(x) == 1}

#' @rdname basics
#' @export
NR2 <- function(x) {uj::NR(x) == 2}

#' @rdname basics
#' @export
NR1P <- function(x) {uj::NR(x) >= 1}

#' @rdname basics
#' @export
NR2P <- function(x) {uj::NR(x) >= 2}

#' @rdname basics
#' @export
NR3P <- function(x) {uj::NR(x) >= 3}

#' @rdname basics
#' @export
notNR0 <- function(x) {!uj::NR0(x)}

#' @rdname basics
#' @export
notNR1 <- function(x) {!uj::NR1(x)}

#' @rdname basics
#' @export
notNR2 <- function(x) {!uj::NR2(x)}

#' @rdname basics
#' @export
notNR1P <- function(x) {!uj::NR1P(x)}

#' @rdname basics
#' @export
notNR2P <- function(x) {!uj::NR2P(x)}

#' @rdname basics
#' @export
notNR3P <- function(x) {!uj::NR3P(x)}

# NRC (nrows * ncols) ####

#' @rdname basics
#' @export
NRC <- function(x) {uj::NR(x) & uj::NC(x)}

#' @rdname basics
#' @export
NRC0 <- function(x) {uj::NRC(x) == 0}

#' @rdname basics
#' @export
NRC1 <- function(x) {uj::NRC(x) == 1}

#' @rdname basics
#' @export
NRC2 <- function(x) {uj::NRC(x) == 2}

#' @rdname basics
#' @export
NRC1P <- function(x) {uj::NRC(x) >= 1}

#' @rdname basics
#' @export
NRC2P <- function(x) {uj::NRC(x) >= 2}

#' @rdname basics
#' @export
NRC3P <- function(x) {uj::NRC(x) >= 3}

#' @rdname basics
#' @export
notNRC0 <- function(x) {!uj::NRC0(x)}

#' @rdname basics
#' @export
notNRC1 <- function(x) {!uj::NRC1(x)}

#' @rdname basics
#' @export
notNRC2 <- function(x) {!uj::NRC2(x)}

#' @rdname basics
#' @export
notNRC1P <- function(x) {!uj::NRC1P(x)}

#' @rdname basics
#' @export
notNRC2P <- function(x) {!uj::NRC2P(x)}

#' @rdname basics
#' @export
notNRC3P <- function(x) {!uj::NRC3P(x)}

# NRNC (nrows, ncols) ####

#' @rdname basics
#' @export
nr0nc0 <- function(x) {uj::NR0(x) & NC0(x)}

#' @rdname basics
#' @export
nr0nc1 <- function(x) {uj::NR0(x) & NC1(x)}

#' @rdname basics
#' @export
nr0nc2 <- function(x) {uj::NR0(x) & NC2(x)}

#' @rdname basics
#' @export
nr0nc1P <- function(x) {uj::NR0(x) & NC1P(x)}

#' @rdname basics
#' @export
nr0nc2P <- function(x) {uj::NR0(x) & NC2P(x)}

#' @rdname basics
#' @export
nr0nc3P <- function(x) {uj::NR0(x) & NC3P(x)}

#' @rdname basics
#' @export
nr1nc0 <- function(x) {uj::NR1(x) & NC0(x)}

#' @rdname basics
#' @export
nr1nc1 <- function(x) {uj::NR1(x) & NC1(x)}

#' @rdname basics
#' @export
nr1nc2 <- function(x) {uj::NR1(x) & NC2(x)}

#' @rdname basics
#' @export
nr1nc1P <- function(x) {uj::NR1(x) & NC1P(x)}

#' @rdname basics
#' @export
nr1nc2P <- function(x) {uj::NR1(x) & NC2P(x)}

#' @rdname basics
#' @export
nr1nc3P <- function(x) {uj::NR1(x) & NC3P(x)}

#' @rdname basics
#' @export
nr2nc0 <- function(x) {uj::NR2(x) & NC0(x)}

#' @rdname basics
#' @export
nr2nc1 <- function(x) {uj::NR2(x) & NC1(x)}

#' @rdname basics
#' @export
nr2nc2 <- function(x) {uj::NR2(x) & NC2(x)}

#' @rdname basics
#' @export
nr2nc1P <- function(x) {uj::NR2(x) & NC1P(x)}

#' @rdname basics
#' @export
nr2nc2P <- function(x) {uj::NR2(x) & NC2P(x)}

#' @rdname basics
#' @export
nr2nc3P <- function(x) {uj::NR2(x) & NC3P(x)}

#' @rdname basics
#' @export
nr1Pnc0 <- function(x) {uj::NR1P(x) & NC0(x)}

#' @rdname basics
#' @export
nr1Pnc1 <- function(x) {uj::NR1P(x) & NC1(x)}

#' @rdname basics
#' @export
nr1Pnc2 <- function(x) {uj::NR1P(x) & NC2(x)}

#' @rdname basics
#' @export
nr1Pnc1P <- function(x) {uj::NR1P(x) & NC1P(x)}

#' @rdname basics
#' @export
nr1Pnc2P <- function(x) {uj::NR1P(x) & NC2P(x)}

#' @rdname basics
#' @export
nr1Pnc3P <- function(x) {uj::NR1P(x) & NC3P(x)}

#' @rdname basics
#' @export
nr2Pnc0 <- function(x) {uj::nr2P(x) & NC0(x)}

#' @rdname basics
#' @export
nr2Pnc1 <- function(x) {uj::nr2P(x) & NC1(x)}

#' @rdname basics
#' @export
nr2Pnc2 <- function(x) {uj::nr2P(x) & NC2(x)}

#' @rdname basics
#' @export
nr2Pnc1P <- function(x) {uj::nr2P(x) & NC1P(x)}

#' @rdname basics
#' @export
nr2Pnc2P <- function(x) {uj::nr2P(x) & NC2P(x)}

#' @rdname basics
#' @export
nr2Pnc3P <- function(x) {uj::nr2P(x) & NC3P(x)}

#' @rdname basics
#' @export
nr3Pnc0 <- function(x) {uj::nr3P(x) & NC0(x)}

#' @rdname basics
#' @export
nr3Pnc1 <- function(x) {uj::nr3P(x) & NC1(x)}

#' @rdname basics
#' @export
nr3Pnc2 <- function(x) {uj::nr3P(x) & NC2(x)}

#' @rdname basics
#' @export
nr3Pnc1P <- function(x) {uj::nr3P(x) & NC1P(x)}

#' @rdname basics
#' @export
nr3Pnc2P <- function(x) {uj::nr3P(x) & NC2P(x)}

#' @rdname basics
#' @export
nr3Pnc3P <- function(x) {uj::nr3P(x) & NC3P(x)}

#' @rdname basics
#' @export
notnr0nc0 <- function(x) {!uj::nr0nc0(x)}

#' @rdname basics
#' @export
notnr0nc1 <- function(x) {!uj::nr0nc1(x)}

#' @rdname basics
#' @export
notnr0nc2 <- function(x) {!uj::nr0nc2(x)}

#' @rdname basics
#' @export
notnr0nc1P <- function(x) {!uj::nr0nc1P(x)}

#' @rdname basics
#' @export
notnr0nc2P <- function(x) {!uj::nr0nc2P(x)}

#' @rdname basics
#' @export
notnr0nc3P <- function(x) {!uj::nr0nc3P(x)}

#' @rdname basics
#' @export
notnr1nc0 <- function(x) {!uj::nr1nc0(x)}

#' @rdname basics
#' @export
notnr1nc1 <- function(x) {!uj::nr1nc1(x)}

#' @rdname basics
#' @export
notnr1nc2 <- function(x) {!uj::nr1nc2(x)}

#' @rdname basics
#' @export
notnr1nc1P <- function(x) {!uj::nr1nc1P(x)}

#' @rdname basics
#' @export
notnr1nc2P <- function(x) {!uj::nr1nc2P(x)}

#' @rdname basics
#' @export
notnr1nc3P <- function(x) {!uj::nr1nc3P(x)}

#' @rdname basics
#' @export
notnr2nc0 <- function(x) {!uj::nr2nc0(x)}

#' @rdname basics
#' @export
notnr2nc1 <- function(x) {!uj::nr2nc1(x)}

#' @rdname basics
#' @export
notnr2nc2 <- function(x) {!uj::nr2nc2(x)}

#' @rdname basics
#' @export
notnr2nc1P <- function(x) {!uj::nr2nc1P(x)}

#' @rdname basics
#' @export
notnr2nc2P <- function(x) {!uj::nr2nc2P(x)}

#' @rdname basics
#' @export
notnr2nc3P <- function(x) {!uj::nr2nc3P(x)}

#' @rdname basics
#' @export
notnr1Pnc0 <- function(x) {!uj::nr1Pnc0(x)}

#' @rdname basics
#' @export
notnr1Pnc1 <- function(x) {!uj::nr1Pnc1(x)}

#' @rdname basics
#' @export
notnr1Pnc2 <- function(x) {!uj::nr1Pnc2(x)}

#' @rdname basics
#' @export
notnr1Pnc1P <- function(x) {!uj::nr1Pnc1P(x)}

#' @rdname basics
#' @export
notnr1Pnc2P <- function(x) {!uj::nr1Pnc2P(x)}

#' @rdname basics
#' @export
notnr1Pnc3P <- function(x) {!uj::nr1Pnc3P(x)}

#' @rdname basics
#' @export
notnr2Pnc0 <- function(x) {!uj::nr2Pnc0(x)}

#' @rdname basics
#' @export
notnr2Pnc1 <- function(x) {!uj::nr2Pnc1(x)}

#' @rdname basics
#' @export
notnr2Pnc2 <- function(x) {!uj::nr2Pnc2(x)}

#' @rdname basics
#' @export
notnr2Pnc1P <- function(x) {!uj::nr2Pnc1P(x)}

#' @rdname basics
#' @export
notnr2Pnc2P <- function(x) {!uj::nr2Pnc2P(x)}

#' @rdname basics
#' @export
notnr2Pnc3P <- function(x) {!uj::nr2Pnc3P(x)}

#' @rdname basics
#' @export
notnr3Pnc0 <- function(x) {!uj::nr3Pnc0(x)}

#' @rdname basics
#' @export
notnr3Pnc1 <- function(x) {!uj::nr3Pnc1(x)}

#' @rdname basics
#' @export
notnr3Pnc2 <- function(x) {!uj::nr3Pnc2(x)}

#' @rdname basics
#' @export
notnr3Pnc1P <- function(x) {!uj::nr3Pnc1P(x)}

#' @rdname basics
#' @export
notnr3Pnc2P <- function(x) {!uj::nr3Pnc2P(x)}

#' @rdname basics
#' @export
notnr3Pnc3P <- function(x) {!uj::nr3Pnc3P(x)}

# NDIM ####

#' @rdname basics
#' @export
NDIM <- function(x) {
  if (uj::NLL(x)) {return(0)}
  D <- base::dim(x)
  if (uj::NLL(D)) {1} else {uj::N(D)}
}

#' @rdname basics
#' @export
NDIM0 <- function(x) {uj::NDIM(x) == 0}

#' @rdname basics
#' @export
NDIM1 <- function(x) {uj::NDIM(x) == 1}

#' @rdname basics
#' @export
NDIM2 <- function(x) {uj::NDIM(x) == 2}

#' @rdname basics
#' @export
NDIM1P <- function(x) {uj::NDIM(x) >= 1}

#' @rdname basics
#' @export
NDIM2P <- function(x) {uj::NDIM(x) >= 2}

#' @rdname basics
#' @export
NDIM3P <- function(x) {uj::NDIM(x) >= 3}

#' @rdname basics
#' @export
notNDIM0 <- function(x) {!uj::NDIM0(x)}

#' @rdname basics
#' @export
notNDIM1 <- function(x) {!uj::NDIM1(x)}

#' @rdname basics
#' @export
notNDIM2 <- function(x) {!uj::NDIM2(x)}

#' @rdname basics
#' @export
notNDIM1P <- function(x) {!uj::NDIM1P(x)}

#' @rdname basics
#' @export
notNDIM2P <- function(x) {!uj::NDIM2P(x)}

#' @rdname basics
#' @export
notNDIM3P <- function(x) {!uj::NDIM3P(x)}

# NS (lengths) ####

#' @rdname basics
#' @export
NS <- function(x) {base::lengths(x)}

#' @rdname basics
#' @export
NS0 <- function(x) {uj::NS(x) == 0}

#' @rdname basics
#' @export
NS1 <- function(x) {uj::NS(x) == 1}

#' @rdname basics
#' @export
NS2 <- function(x) {uj::NS(x) == 2}

#' @rdname basics
#' @export
NS1P <- function(x) {uj::NS(x) >= 1}

#' @rdname basics
#' @export
NS2P <- function(x) {uj::NS(x) >= 2}

#' @rdname basics
#' @export
NS3P <- function(x) {uj::NS(x) >= 3}

#' @rdname basics
#' @export
notNS0 <- function(x) {!uj::NS0(x)}

#' @rdname basics
#' @export
notNS1 <- function(x) {!uj::NS1(x)}

#' @rdname basics
#' @export
notNS2 <- function(x) {!uj::NS2(x)}

#' @rdname basics
#' @export
notNS1P <- function(x) {!uj::NS1P(x)}

#' @rdname basics
#' @export
notNS2P <- function(x) {!uj::NS2P(x)}

#' @rdname basics
#' @export
notNS3P <- function(x) {!uj::NS3P(x)}

# U (unique) ####

#' @rdname basics
#' @export
NU <- function(x) {uj::N(uj::UV(x))}

#' @rdname basics
#' @export
NU0 <- function(x) {uj::NU(x) == 0}

#' @rdname basics
#' @export
NU1 <- function(x) {uj::NU(x) == 1}

#' @rdname basics
#' @export
NU2 <- function(x) {uj::NU(x) == 2}

#' @rdname basics
#' @export
NU1P <- function(x) {uj::NU(x) >= 1}

#' @rdname basics
#' @export
NU2P <- function(x) {uj::NU(x) >= 2}

#' @rdname basics
#' @export
NU3P <- function(x) {uj::NU(x) >= 3}

#' @rdname basics
#' @export
notNU0 <- function(x) {!uj::NU0(x)}

#' @rdname basics
#' @export
notNU1 <- function(x) {!uj::NU1(x)}

#' @rdname basics
#' @export
notNU2 <- function(x) {!uj::NU2(x)}

#' @rdname basics
#' @export
notNU1P <- function(x) {!uj::NU1P(x)}

#' @rdname basics
#' @export
notNU2P <- function(x) {!uj::NU2P(x)}

#' @rdname basics
#' @export
notNU3P <- function(x) {!uj::NU3P(x)}

# V (atomic values) ####

#' @rdname basics
#' @export
NV <- function(...) {uj::N(uj::AV(...))}

#' @rdname basics
#' @export
NV0 <- function(...) {uj::NV(...) == 0}

#' @rdname basics
#' @export
NV1 <- function(...) {uj::NV(...) == 1}

#' @rdname basics
#' @export
NV2 <- function(...) {uj::NV(...) == 2}

#' @rdname basics
#' @export
NV1P <- function(...) {uj::NV(...) >= 1}

#' @rdname basics
#' @export
NV2P <- function(...) {uj::NV(...) >= 2}

#' @rdname basics
#' @export
NV3P <- function(...) {uj::NV(...) >= 3}

#' @rdname basics
#' @export
notNV0 <- function(...) {!uj::NV0(...)}

#' @rdname basics
#' @export
notNV1 <- function(...) {!uj::NV1(...)}

#' @rdname basics
#' @export
notNV2 <- function(...) {!uj::NV2(...)}

#' @rdname basics
#' @export
notNV1P <- function(...) {!uj::NV1P(...)}

#' @rdname basics
#' @export
notNV2P <- function(...) {!uj::NV2P(...)}

#' @rdname basics
#' @export
notNV3P <- function(...) {!uj::NV3P(...)}

# W (which) ####

#' @rdname basics
#' @export
NW <- function(x) {uj::N(uj::WV(x))}

#' @rdname basics
#' @export
NW0 <- function(x) {uj::NW(x) == 0}

#' @rdname basics
#' @export
NW1 <- function(x) {uj::NW(x) == 1}

#' @rdname basics
#' @export
NW2 <- function(x) {uj::NW(x) == 2}

#' @rdname basics
#' @export
NW1P <- function(x) {uj::NW(x) >= 1}

#' @rdname basics
#' @export
NW2P <- function(x) {uj::NW(x) >= 2}

#' @rdname basics
#' @export
NW3P <- function(x) {uj::NW(x) >= 3}

#' @rdname basics
#' @export
notN0 <- function(x) {!uj::NW0(x)}

#' @rdname basics
#' @export
notNW1 <- function(x) {!uj::NW1(x)}

#' @rdname basics
#' @export
notNW2 <- function(x) {!uj::NW2(x)}

#' @rdname basics
#' @export
notNW1P <- function(x) {!uj::NW1P(x)}

#' @rdname basics
#' @export
notNW2P <- function(x) {!uj::NW2P(x)}

#' @rdname basics
#' @export
notNW3P <- function(x) {!uj::NW3P(x)}

# names ####

#' @rdname basics
#' @export
CN <- function(x) {base::colnames(x)}

#' @rdname basics
#' @export
DN <- function() {base::eval.parent(base::...names())}

#' @rdname basics
#' @export
EN <- function(x) {base::names(x)}

#' @rdname basics
#' @export
RN <- function(x) {base::rownames(x)}

# N (in)equality ####

#' @rdname basics
#' @export
NEQ <- function(x, y) {uj::N(x) == uj::N(y)}

#' @rdname basics
#' @export
NDIF <- function(x, y) {!uj::NEQ(x, y)}

# is(nt) class ####

#' @rdname basics
#' @export
isATM <- function(x) {base::is.atomic(x)}

#' @rdname basics
#' @export
isCHR <- function(x) {base::is.character(x)}

#' @rdname basics
#' @export
ERR <- function(x) {assertthat::is.error(x)}

#' @rdname basics
#' @export
notERR <- function(x) {!assertthat::is.error(x)}

#' @rdname basics
#' @export
isFAC <- function(x) {base::is.factor(x)}

#' @rdname basics
#' @export
isINT <- function(x) {base::is.integer(x)}

#' @rdname basics
#' @export
isLGL <- function(x) {base::is.logical(x)}

#' @rdname basics
#' @export
isNUM <- function(x) {base::is.numeric(x)}

#' @rdname basics
#' @export
isORD <- function(x) {base::is.ordered(x)}

#' @rdname basics
#' @export
isUNO <- function(x) {uj::isFAC(x) & !uj::isORD(x)}

#' @rdname basics
#' @export
notATM <- function(x) {!uj::isATM(x)}

#' @rdname basics
#' @export
notCHR <- function(x) {!uj::isCHR(x)}

#' @rdname basics
#' @export
notFAC <- function(x) {!uj::isFAC(x)}

#' @rdname basics
#' @export
notINT <- function(x) {!uj::isINT(x)}

#' @rdname basics
#' @export
notLGL <- function(x) {!uj::isLGL(x)}

#' @rdname basics
#' @export
notNUM <- function(x) {!uj::isNUM(x)}

#' @rdname basics
#' @export
notORD <- function(x) {!uj::isORD(x)}

#' @rdname basics
#' @export
notUNO <- function(x) {!uj::isUNO(x)}

# is(n't) class ####

#' @rdname basics
#' @export
isARR <- function(x) {base::is.array(x)}

#' @rdname basics
#' @export
isDTF <- function(x) {base::is.data.frame(x)}

#' @rdname basics
#' @export
isFUN <- function(x) {base::is.function(x)}

#' @rdname basics
#' @export
isLST <- function(x) {base::is.list(x)}

#' @rdname basics
#' @export
isMAT <- function(x) {base::is.matrix(x)}

#' @rdname basics
#' @export
isVEC <- function(x) {base::is.vector(x)}

#' @rdname basics
#' @export
notARR <- function(x) {!uj::isARR(x)}

#' @rdname basics
#' @export
notDTF <- function(x) {!uj::isDTF(x)}

#' @rdname basics
#' @export
notFUN <- function(x) {!uj::isFUN(x)}

#' @rdname basics
#' @export
notLST <- function(x) {!uj::isLST(x)}

#' @rdname basics
#' @export
notMAT <- function(x) {!uj::isMAT(x)}

#' @rdname basics
#' @export
notVEC <- function(x) {!uj::isVEC(x)}

#' @rdname basics
#' @export
xIN <- function(x, ...) {x[uj::isIN(x, ...)]}

#' @rdname basics
#' @export
xMF <- function(x, ...) {x[uj::isMF(x, ...)]}

