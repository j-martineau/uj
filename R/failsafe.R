# are x and y of compatible (sortable or unsortable) modes
.compat <- function(x, y) {
  if      (uj::isCHR(x) & uj::isCHR(y)) {T}
  else if (uj::isLGL(x) & uj::isLGL(y)) {T}
  else if (uj::isNUM(x) & uj::isNUM(y)) {T}
  else if (uj::isORD(x) & uj::isORD(y)) {uj::isVEQ(uj::LEVS(x), uj::LEVS(y))}
  else if (uj::isFAC(x) & uj::isFAC(y)) {uj::isSEQ(uj::LEVS(x), uj::LEVS(y))}
  else {F}
}

# are x and y of comparable (sortable) modes
.compar <- function(x, y) {
  if      (uj::isCHR(x) & uj::isCHR(y)) {T}
  else if (uj::isLGL(x) & uj::isLGL(y)) {T}
  else if (uj::isNUM(x) & uj::isNUM(y)) {T}
  else if (uj::isORD(x) & uj::isORD(y)) {uj::isVEQ(uj::LEVS(x), uj::LEVS(y))}
  else {F}
}

# are x and y of recyclable lengths (1, max(c(length(x), length(y))))
.mismatchN <- function(x, y) {
  nx <- uj::N(x)
  ny <- uj::N(y)
  nxy <- base::c(nx, ny)
  nvalid <- base::c(1, nxy)
  nx == 0 | ny == 0 | !base::all(nxy %in% nvalid)
}

#' @encoding UTF-8
#' @family extensions
#' @family logicals
#' @family errs
#' @title Failsafe functions that **always** return a valid object.
#' @description Evaluate objects, test if an object has certain properties, and conduct binary logical operations safely. These functions never stop execution; they always produce a valid result, even if that result is an error object.
#' @section Failsafe error management functions: These functions evaluate generate, check for, and manage error objects.
#' \tabular{ll}{  `makeERR`      \tab Generate object of class `'simpleError'`.         \cr
#'                `msgERR`       \tab Get error message, if any; otherwise, `NULL`.     \cr
#'                `notERR`       \tab Does evaluating `x` not produce an error?.        \cr
#'                `isERR`        \tab Does evaluating `x` produce an error?               }
#' @section Failsafe check for classes of objects with values:
#' \tabular{ll}{  `failsafe`     \tab Returns `x` if identity evaluation does not cause
#'                                  an error. Return an error object otherwise.         \cr   \tab  }
#' \tabular{ll}{  `fsNULL`       \tab `x` is `NULL`.                                    \cr
#'                `fsDEF`        \tab `x` is defined (not `NULL`).                      \cr   \tab  }
#' \tabular{ll}{  `fsSCL`        \tab `x` is scalar of a specific atomic value.         \cr
#'                `sclF`         \tab `x` is scalar `FALSE`                             \cr
#'                `sclT`         \tab `x` is scalar `TRUE`                              \cr
#'                `sclTF`        \tab `x` is scalar `TRUE` or `FALSE`.                  \cr
#'                `sclLG`        \tab `x` is scalar `TRUE`, `FALSE`, or `NA`.           \cr
#'                `sclBL`        \tab `x` is blank string scalar (`""`).                \cr
#'                `sclNA`        \tab `x` is scalar `NA`                                \cr   \tab  }
#' \tabular{ll}{  `fsVEC`        \tab Elements of `x` are of a specific atomic value.   \cr
#'                `vecF`         \tab Elements of `x` are `FALSE`.                      \cr
#'                `vecT`         \tab Elements of `x` are `TRUE`.                       \cr
#'                `vecTF`        \tab Elements of `x` are `TRUE` or `FALSE`.            \cr
#'                `vecLG`        \tab Elements of `x` are `TRUE`, `FALSE`, or `NA`.     \cr
#'                `vecBL`        \tab Elements of `x` are blank strings.                \cr
#'                `vecNA`        \tab Elements of `x` are `NA`.                           }
#' @section Failsafe forced-evaluation functions with conditional return values: These functions \link[base:force]{force} evaluation of `x` returning values as shown in the following table with the first value returned when forcing evaluation does not produce an error and the second when it does:
#' \tabular{ll}{  `fsOR`         \tab `x` or something else if evaluating `x` produces an error \cr
#'                `orF`          \tab `x` or `FALSE`                                    \cr
#'                `orT`          \tab `x` or `TRUE`                                     \cr
#'                `orC0`         \tab `x` or `character(0)`                             \cr
#'                `orI0`         \tab `x` or `integer(0)`                               \cr
#'                `orL0`         \tab `x` or `logical(0)`                               \cr
#'                `orN0`         \tab `x` or `numeric(0)`                               \cr
#'                `orBL`         \tab `x` or `""`                                       \cr
#'                `orNA`         \tab `x` or `NA`                                       \cr
#'                `orNAC`        \tab `x` or `NA_character_`                            \cr
#'                `orNAI`        \tab `x` or `NA_integer_`                              \cr
#'                `orNAL`        \tab `x` or `NA` (logical)                             \cr
#'                `orNAR`        \tab `x` or `NA_real_`                                   }
#' @details Failsafe binary comparison functions: These are binary functions that always produce either scalar `TRUE` or scalar `FALSE`. They return `TRUE` when two variables meet the conditions shown in the following table and `FALSE` in every other circumstance (where `fx` and `fy`)
#' \tabular{ll}{  `%.is.%`       \tab `failsafe(identical(x, y))`                                                        \cr
#'                `%.isnt.%`     \tab `!(x %.is.% y)`                                                                    \cr   \tab  }
#' \tabular{ll}{  `%.in.%`       \tab `sapply(failsafe(x %in% y), isTRUE)`                                               \cr
#'                `%.mf.%`       \tab `!(x %.in.% y)` (missing from)                                                     \cr
#'                `%.has.%`      \tab `y %.in.% x` (`x` has each `y` value)                                              \cr
#'                `%.lacks.%`    \tab `y %.mf.% x` (`x` lacks each `y` value)                                            \cr   \tab  }
#' \tabular{ll}{  `%.in1.%`      \tab if `x` is atomic scalar, `x %.in.% y`, otherwise `FALSE`                           \cr
#'                `%.mf1.%`      \tab `x` is atomic scalar and `x %.mf.% y`                                              \cr
#'                `%.has1.%`     \tab `y` is atomic scalar and `x %.has.% y`                                             \cr
#'                `%.lacks1.%`   \tab `y` is atomic scalar and `x %.lacks.% y`                                           \cr   \tab  }
#' \tabular{ll}{  `%.eq.%`       \tab If `x` and `y` match\eqn{^{(1)}}, `x = y`, otherwise `FALSE`                       \cr
#'                `%.ge.%`       \tab If `x` and `y` match\eqn{^{(1)}}, `x ≥ y`, otherwise `FALSE`                       \cr
#'                `%.gt.%`       \tab If `x` and `y` match\eqn{^{(1)}}, `x > y`, otherwise `FALSE`                       \cr
#'                `%.le.%`       \tab If `x` and `y` match\eqn{^{(1)}}, `x ≤ y`, otherwise `FALSE`                       \cr
#'                `%.lt.%`       \tab If `x` and `y` match\eqn{^{(1)}}, `x < y`, otherwise `FALSE`                       \cr
#'                `%.dif.%`      \tab If `x` and `y` match\eqn{^{(1)}}, `x ≠ y`, otherwise `TRUE`                        \cr
#'                               \tab \eqn{^{(1)}} Of equal length and \link[=compatible]{compatible modes}.             \cr   \tab  }
#' \tabular{ll}{  `%.eq1.%`      \tab `x` and `y` are atomic scalar and `x = y`  \eqn{^{(2)}}                            \cr
#'                `%.ge1.%`      \tab `x` and `y` are atomic scalar and `x ≥ y`  \eqn{^{(2)}}                            \cr
#'                `%.gt1.%`      \tab `x` and `y` are atomic scalar and `x > y`  \eqn{^{(2)}}                            \cr
#'                `%.le1.%`      \tab `x` and `y` are atomic scalar and `x ≤ y`  \eqn{^{(2)}}                            \cr
#'                `%.lt1.%`      \tab `x` and `y` are atomic scalar and `x ≥ y`  \eqn{^{(2)}}                            \cr
#'                `%.dif1.%`     \tab `x` and `y` are *not* atomic scalar equal  \eqn{^{(3)}}                            \cr
#'                               \tab \eqn{^{(2)}} Of \link[=compatible]{compatible modes} and meeting the (in)equality. \cr
#'                               \tab \eqn{^{(3)}} Not meeting the requirements of `%.eq1.%`.                            \cr   \tab  }
#' \tabular{ll}{  `%.seq.%`      \tab `x` and `y` are \code{\link[base]{setequal}}\eqn{^{(4)}}                           \cr
#'                `%.veq.%`      \tab `x` and `y` are vector equal\eqn{^{(5)}}                                           \cr
#'                `%.sdif.%`     \tab `x` and `y` are set different\eqn{^{(6)}}                                          \cr
#'                `%.vdif.%`     \tab `x` and `y` are vector different\eqn{^{(7)}}                                       \cr
#'                               \tab \eqn{^{(4)}} Atomic, of the same length, with the same values in the same order.   \cr
#'                               \tab \eqn{^{(5)}} Atomic, possibly of different lengths, and containing the same unique
#'                                                values regardless of order or duplicates.                              \cr
#'                               \tab \eqn{^{(6)}} Not meeting the requirements of `%.seq.%`.                            \cr
#'                               \tab \eqn{^{(7)}} Not meeting the requirements of `%.veq.%`.                            \cr   \tab  }
#' \tabular{ll}{  `%.or.%`       \tab values of `x` *and/or* `y` are `TRUE`          \cr
#'                `%.and.%`      \tab values of `x` *and* `y` are `TRUE`             \cr
#'                `%.xor.%`      \tab values of *either* `x` *or* `y` are `TRUE`     \cr
#'                `%.nor.%`      \tab values of *neither* `x` *nor* `y` are `TRUE`   \cr   \tab  }
#' \tabular{ll}{  `%.or1.%`      \tab `x` *and/or* `y` are scalar `TRUE`             \cr
#'                `%.and1.%`     \tab `x` *and* `y` are scalar `TRUE`                \cr
#'                `%.xor1.%`     \tab *either* `x` *or* `y` is scalar `TRUE`         \cr
#'                `%.nor1.%`     \tab *neither* `x` *nor* `y` is scalar `TRUE`         }
#' @section Failsafe scalar value, membership, and equality/inequality checking functions: These functions check for class, mode, and/or value and/or count the number of checks passed. They *always* produce `TRUE`, `FALSE`, or an integer scalar.
#' \cr\cr Function names are constructed of root words, prefixes, and/or suffixes. Root word specify the type of check conducted. Prefixes and suffixes specify how to modify the results of a check or how to apply the check to each `...` argument and check whether a certain number of `...` args passed the checks.
#' \cr\cr \strong{*Root words for atomic scalar value checking functions*}
#' \tabular{ll}{  `nas`     \tab `x` is scalar `NA` and atomic.                      \cr
#'                `oks`     \tab `x` is scalar, non-`NA` and atomic.                 \cr
#'                `bl`      \tab `x` is a scalar blank string (`""`).                \cr
#'                `lg`      \tab `x` is a logical scalar (`TRUE`, `FALSE`, or `NA`). \cr
#'                `tf`      \tab `x` is scalar `TRUE` or scalar `FALSE`.             \cr
#'                `f`       \tab `x` is scalar `TRUE`.                               \cr
#'                `t`       \tab `x` is scalar `FALSE`.                                }
#' \cr \strong{*Root words for atomic value membership checking functions*}
#' \tabular{ll}{  `lacks`   \tab `x %.lacks.% y`                      \cr
#'                `has`     \tab `x %.has.% y`                        \cr
#'                `in`      \tab `x %.in.% y`                         \cr
#'                `mf`      \tab `x %.mf.% y`                           }
#' \cr \strong{*Root words for equality/inequality checking functions*}
#' \tabular{ll}{  `is`      \tab `x %.is.% y`                         \cr
#'                `isnt`    \tab `x %.isnt.% y`                         }
#' \tabular{ll}{  `eq`      \tab `x %.eq.% y`                         \cr
#'                `eq1`     \tab `x %.eq1.% y`                        \cr
#'                `seq`     \tab `x %.seq.% y`                        \cr
#'                `veq`     \tab `x %.veq.% y`                        \cr
#'                `dif`     \tab `x %.dif.% y`                        \cr
#'                `dif1`    \tab `x %.dif1.% y`                       \cr
#'                `sdif`    \tab `x %.sdif.% y`                       \cr
#'                `vdif`    \tab `x %.sdif.% y`                         }
#' \tabular{ll}{  `ge`      \tab `x` is greater than or equal to `y`. \cr
#'                `gt`      \tab `x` is greater than `y`.             \cr
#'                `le`      \tab `x` is less than or equal to `y`.    \cr
#'                `lt`      \tab `x` is less than `y`.                  }
#' \cr \strong{*Modifier prefixes/suffixes*}
#' \tabular{ll}{  `n`       \tab Count `...` args passing the check.  \cr
#'                `is`      \tab Keep the result of a check as is.    \cr
#'                `not`     \tab Negate the result of a check.          }
#' \cr \strong{*Apply-and-sweep prefixes/suffixes evaluating whether a certain number of checks were passed*}
#' \cr\cr The following table contains prefixes in the first column, and in the second, the number of checks that must be passed to return `TRUE`.
#' \tabular{ll}{  `none`    \tab `0` values passed the check.         \cr
#'                `any`     \tab `> 0` values passed the check.       \cr
#'                `one`     \tab `1` value passed the check.          \cr
#'                `some`    \tab `> 1` values passed the check.       \cr
#'                `two`     \tab `2` values passed the check.         \cr
#'                `many`    \tab `> 2` values passed the check.       \cr
#'                `all`     \tab All values passed the check.           }
#' \cr \strong{*Identity-equality, set-equality and vector-equality checking functions*}
#' \tabular{lll}{              \tab **IS**     \tab **NOT**           \cr
#'                 **IS**      \tab `IS`       \tab  —                \cr
#'                 **ISNT**    \tab `ISNT`     \tab  —                \cr
#'                 **SEQ**     \tab `isSEQ`    \tab `notSEQ`          \cr
#'                 **VEQ**     \tab `isVEQ`    \tab `notVEQ`          \cr
#'                 **SDIF**    \tab `isSDIF`   \tab `notSDIF`         \cr
#'                 **VDIF**    \tab `isVDIF`   \tab `notVDIF`           }
#' \\cr \strong{*Atomic scalar value checking functions*}
#' \tabular{lll}{              \tab **IS1**    \tab **NOT1**          \cr
#'                 **T**       \tab `isT1`     \tab `notT1`           \cr
#'                 **F**       \tab `isF1`     \tab `notF1`           \cr
#'                 **TF**      \tab `isTF1`    \tab `notTF1`          \cr
#'                 **LG**      \tab `isLG1`    \tab `notLG1`          \cr
#'                 **BL**      \tab `isBL1`    \tab `notBL1`          \cr
#'                 **EQ**      \tab `isEQ1`    \tab `notEQ1`          \cr
#'                 **GE**      \tab `isGE1`    \tab `notGE1`          \cr
#'                 **GT**      \tab `isGT1`    \tab `notGT1`          \cr
#'                 **LE**      \tab `isLE1`    \tab `notLE1`          \cr
#'                 **LT**      \tab `isLT1`    \tab `notLT1`          \cr
#'                 **NAS**     \tab `isNAS`    \tab `notNAS`          \cr
#'                 **OKS**     \tab `isOKS`    \tab `notOKS`          \cr
#'                 **DIF**     \tab `isDIF1`   \tab `notDIF1`           }
#' \cr\cr \strong{*Atomic scalar membership checking functions*}
#' \tabular{lll}{              \tab **IS1**    \tab **NOT1**          \cr
#'                 **IN**      \tab `IN1`      \tab `notIN1`          \cr
#'                 **MF**      \tab `MF1`      \tab `notMF1`          \cr
#'                 **HAS**     \tab `HAS1`     \tab `notHAS1`         \cr
#'                 **LACKS**   \tab `LACKS1`   \tab `notLACKS1`         }
#' \cr\cr \strong{*Atomic value checking functions*}
#' \tabular{lllllllll}{              \tab **IS**    \tab **N**      \tab **NOT**      \tab **NONE**      \tab **ONE**      \tab **TWO**      \tab **ANY**      \tab  **ALL**   \cr
#'                       **T**       \tab `isT`     \tab `nT`       \tab `notT`       \tab `noneT`       \tab `oneT`       \tab `twoT`       \tab `anyT`       \tab `allT`     \cr
#'                       **F**       \tab `isF`     \tab `nF`       \tab `notF`       \tab `noneF`       \tab `oneF`       \tab `twoF`       \tab `anyF`       \tab `allF`     \cr
#'                       **TF**      \tab `isTF`    \tab `nTF`      \tab `notTF`      \tab `noneTF`      \tab `oneTF`      \tab `twoTF`      \tab `anyTF`      \tab `allTF`    \cr
#'                       **BL**      \tab `isBL`    \tab `nBL`      \tab `notBL`      \tab `noneBL`      \tab `oneBL`      \tab `twoBL`      \tab `anyBL`      \tab `allBL`    \cr
#'                       **EQ**      \tab `isEQ`    \tab `nEQ`      \tab `notEQ`      \tab `noneEQ`      \tab `oneEQ`      \tab `twoEQ`      \tab `anyEQ`      \tab `allEQ`    \cr
#'                       **GE**      \tab `isGE`    \tab `nGE`      \tab `notGE`      \tab `noneGE`      \tab `oneGE`      \tab `twoGE`      \tab `anyGE`      \tab `allGE`    \cr
#'                       **GT**      \tab `isGT`    \tab `nGT`      \tab `notGT`      \tab `noneGT`      \tab `oneGT`      \tab `twoGT`      \tab `anyGT`      \tab `allGT`    \cr
#'                       **LE**      \tab `isLE`    \tab `nLE`      \tab `notLE`      \tab `noneLE`      \tab `oneLE`      \tab `twoLE`      \tab `anyLE`      \tab `allLE`    \cr
#'                       **LT**      \tab `isLT`    \tab `nLT`      \tab `notLT`      \tab `noneLT`      \tab `oneLT`      \tab `twoLT`      \tab `anyLT`      \tab `allLT`    \cr
#'                       **DIF**     \tab `isDIF`   \tab `nDIF`     \tab `notDIF`     \tab `noneDIF`     \tab `oneDIF`     \tab `twoDIF`     \tab `anyDIF`     \tab `allDIF`   \cr
#'                       **NAS**     \tab `isNA`    \tab `nNAS`     \tab `notNAS`     \tab `noneNAS`     \tab `oneNAS`     \tab `twoNAS`     \tab `anyNAS`     \tab `allNAS`   \cr
#'                       **OKS**     \tab `isOKS  ` \tab `nOKS`     \tab `notOKS`     \tab `noneOKS`     \tab `oneOKS`     \tab `twoOKS`     \tab `anyOKS`     \tab `allOKS`     }
#' \cr\cr \strong{*Atomic value membership checking functions*}
#' \tabular{lllllllll}{              \tab **IS**     \tab **N**     \tab **NOT**      \tab **NONE**      \tab **ONE**      \tab **TWO**      \tab **ANY**      \tab **ALL**    \cr
#'                       **IN**      \tab `isIN`    \tab `nIN`      \tab `notIN`      \tab `inNONE`      \tab `inONE`      \tab `inTWO`      \tab `inANY`      \tab `inALL`    \cr
#'                       **MF**      \tab `isMF`    \tab `nMF`      \tab `notMF`      \tab `mfNONE`      \tab `mfONE`      \tab `mfTWO`      \tab `mfANY`      \tab `mfALL`    \cr
#'                       **HAS**     \tab `HAS`     \tab `nHAS`     \tab `notHAS`     \tab `hasNONE`     \tab `hasONE`     \tab `hasTWO`     \tab `hasANY`     \tab `hasALL`   \cr
#'                       **ALCKS**   \tab `LACKS`   \tab `nLACKS`   \tab `notLACKS`   \tab `lacksNONE`   \tab `lacksONE`   \tab `lacksTWO`   \tab `lacksANY`   \tab `lacksALL`   }
#' @return A logical scalar.
#' @param x,y Any objects for failsafe binary functions and miscellaneous failsafe functions.
#' @param x An object or a call to evaluate in the environment of a parent function where the initial call was made.
#' @param val An atomic scalar.
#' @param or An object to return if evaluating `x` produces an error.
#' @examples
#' egFailsafe <- function() {
#'   abc <- c("a", "b", "c")
#'   ABC <- abc
#'   Blank. <- ""
#'   MssScl. <- NA
#'   LglScl. <- FALSE
#'   FacScl. <- factor("q", levels = c("x", "q"))
#'   FacVec. <- factor(c("x", "q"), levels = c("x", "q"))
#'   ChrMat. <- matrix(c("a", "b", "c", "NA"), nrow = 2)
#'   ChrDtf. <- data.frame(abc = letters[1:3], def = letters[4:6])
#'   NumVls. <- list(first3 = c(1:3, NA), next3 = c(4:6, NA))
#'   Combo. <- list(MssScl., LglScl., FacVec., ChrMat., ChrDtf., NumVls.)
#'   attr(ABC, "custom") <- "custom"
#'   list(`"A" %.and1.% FALSE`                   = "A" %.and.% FALSE                   ,
#'        `"a" %.in1.% abc`                      = "a" %.in.% abc                      ,
#'        `"A" %.nor1.% FALSE`                   = "A" %.or.% FALSE                    ,
#'        `"A" %.xor1.% FALSE`                   = "A" %.cor.% FALSE                   ,
#'        `"A" %.or1.% FALSE`                    = "A" %.or.% FALSE                    ,
#'        `"a" %.mf1.T% abc`                     = "a" %.mf.% abc                      ,
#'        `"A" %.and.% FALSE`                    = "A" %.and.% FALSE                   ,
#'        `abs %.in.% abc`                       = "a" %.in.% abc                      ,
#'        `"A" %.nor.% FALSE`                    = "A" %.or.% FALSE                    ,
#'        `"A" %.xor.% FALSE`                    = "A" %.cor.% FALSE                   ,
#'        `"A" %.or.% FALSE`                     = "A" %.or.% FALSE                    ,
#'        `"a" %.mf.% abc`                       = "a" %.mf.% abc                      ,
#'        `1 %.in.% abc`                         = 1 %.in.% abc                        ,
#'        `1 %.mf.% abc`                         = 1 %.mf.% abc                        ,
#'        `abc %.eq1.% ABC`                      = abc %.eq1.% ABC                     ,
#'        `abc %.eq1.% letters[1:3]`             = abc %.eq1.% letters[1:3]            ,
#'        `abc %.eq1.% NULL`                     = abc %.eq1.% NULL                    ,
#'        `abc %.has1.% "a"`                     = abc %.has.% "a"                     ,
#'        `abc %.has1.% 1`                       = abc %.has.% 1                       ,
#'        `abc %.has1.% NULL`                    = abc %.has.% NULL                    ,
#'        `abc %.has.% letters[1:6]`             = abc %.has.% "a"                     ,
#'        `abc %.has.% 1:3`                      = abc %.has.% 1                       ,
#'        `abc %.has.% NULL`                     = abc %.has.% NULL                    ,
#'        `abc %.is.% NULL`                      = abc %.is.% NULL                     ,
#'        `abc %.lacks1.% "a"`                   = abc %.lacks.% "a"                   ,
#'        `abc %.lacks1.% 1`                     = abc %.lacks.% 1                     ,
#'        `abc %.lacks1.% NULL`                  = abc %.lacks.% NULL                  ,
#'        `abc %.is.% ABC`                       = abc %.is.% ABC                      ,
#'        `abc %.is.% letters[1:3]`              = abc %.is.% letters[1:3]             ,
#'        `abc %.isnt.% ABC`                     = abc %.isnt.% ABC                    ,
#'        `abc %.isnt.% letters[4:6]`            = abc %.isnt.% letters[4:6]           ,
#'        `allBL(c(Blank., letters))`            = allBL(c(Blank., letters))           ,
#'        `allDIF("a", c(Blank., NA, letters))`  = allDIF("a", c(Blank., NA, letters)) ,
#'        `allEQ(NA, c(Blank., NA, letters))`    = allEQ(NA, c(Blank., NA, letters))   ,
#'        `allF(c(list(Blank.), Combo.))`        = allF(c(list(Blank.), Combo.))       ,
#'        `allGE("a", c(Blank., NA, letters))`   = allGE("a", c(Blank., NA, letters))  ,
#'        `allGT("a", c(Blank., NA, letters))`   = allGT("a", c(Blank., NA, letters))  ,
#'        `allIN(NULL, Blank., Combo.)`          = allIN(NULL, Blank., Combo.)         ,
#'        `allLE("a", c(Blank., NA, letters))`   = allLE("a", c(Blank., NA, letters))  ,
#'        `allLG(c(NA, T, F))`                   = allLG(c(NA, T, F))                  ,
#'        `allLT("a", c(Blank., NA, letters))`   = allLT("a", c(Blank., NA, letters))  ,
#'        `allMF(NULL, Blank., Combo.)`          = allMF(NULL, Blank., Combo.)         ,
#'        `allNAS(c(Blank., NA, letters))`       = allNAS(c(Blank., NA, letters))      ,
#'        `allNAS(ChrMat.)`                      = allNAS(ChrMat.)                     ,
#'        `allOKS(c(Blank., NA, letters))`       = allOKS(c(Blank., NA, letters))      ,
#'        `allOKS(ChrMat.)`                      = allOKS(ChrMat.)                     ,
#'        `allT(c(list(Blank.), Combo.))`        = allT(c(list(Blank.), Combo.))       ,
#'        `allTF(c(list(Blank.), Combo.))`       = allTF(c(list(Blank.), Combo.))      ,
#'        `anyBL(c(Blank., letters))`            = anyBL(c(Blank., letters))           ,
#'        `anyDIF("a", c(Blank., NA, letters))`  = anyDIF("a", c(Blank., NA, letters)) ,
#'        `anyEQ(NA, c(Blank., NA, letters))`    = anyEQ(NA, c(Blank., NA, letters))   ,
#'        `anyF(c(list(Blank.), Combo.))`        = anyF(c(list(Blank.), Combo.))       ,
#'        `anyGE("a", c(Blank., NA, letters))`   = anyGE("a", c(Blank., NA, letters))  ,
#'        `anyGT("a", c(Blank., NA, letters))`   = anyGT("a", c(Blank., NA, letters))  ,
#'        `anyIN(NULL, Blank., Combo.)`          = anyIN(NULL, Blank., Combo.)         ,
#'        `anyLE("a", c(Blank., NA, letters))`   = anyLE("a", c(Blank., NA, letters))  ,
#'        `anyLG(c(NA, T, F))`                   = anyLG(c(NA, T, F))                  ,
#'        `anyLT("a", c(Blank., NA, letters))`   = anyLT("a", c(Blank., NA, letters))  ,
#'        `anyMF(NULL, Blank., Combo.)`          = anyMF(NULL, Blank., Combo.)         ,
#'        `anyNAS(c(Blank., NA, letters))`       = anyNAS(c(Blank., NA, letters))      ,
#'        `anyNAS(ChrMat.)`                      = anyNAS(ChrMat.)                     ,
#'        `anyOKS(c(Blank., NA, letters))`       = anyOKS(c(Blank., NA, letters))      ,
#'        `anyOKS(ChrMat.)`                      = anyOKS(ChrMat.)                     ,
#'        `anyT(c(list(Blank.), Combo.))`        = anyT(c(list(Blank.), Combo.))       ,
#'        `anyTF(c(list(Blank.), Combo.))`       = anyTF(c(list(Blank.), Combo.))      ,
#'        `failsafe(non.existent.variable)`      = failsafe(non.existent.variable)     ,
#'        `failsafe(pi)`                         = failsafe(pi)                        ,
#'        `FALSE %AND% FALSE`                    = FALSE %AND% FALSE                   ,
#'        `FALSE %NOR% FALSE`                    = FALSE %NOR% FALSE                   ,
#'        `FALSE %ONE% FALSE`                    = FALSE %ONE% FALSE                   ,
#'        `FALSE %OR% FALSE`                     = FALSE %OR% FALSE                    ,
#'        `hasALL(Blank., Combo.)`               = hasALL(Blank., Combo.)              ,
#'        `hasANY(Blank., Combo.)`               = hasANY(Blank., Combo.)              ,
#'        `hasMANY(Blank., Combo.)`              = hasMANY(Blank., Combo.)             ,
#'        `hasNONE(Blank., Combo.)`              = hasNONE(Blank., Combo.)             ,
#'        `hasONE(Blank., Combo.)`               = hasONE(Blank., Combo.)              ,
#'        `hasSOME(Blank., Combo.)`              = hasSOME(Blank., Combo.)             ,
#'        `hasTWO(Blank., Combo.)`               = hasTWO(Blank., Combo.)              ,
#'        `isBL("")`                             = isBL("")                            ,
#'        `isBL("a")`                            = isBL("a")                           ,
#'        `isBL(c("", ""))`                      = isBL(c("", ""))                     ,
#'        `isBL(NA)`                             = isBL(NA)                            ,
#'        `isBL1("")`                            = isBL1("")                           ,
#'        `isBL1("a")`                           = isBL1("a")                          ,
#'        `isBL1(c("", ""))`                     = isBL1(c("", ""))                    ,
#'        `isBL1(NA)`                            = isBL1(NA)                           ,
#'        `isDIF("", "")`                        = isDIF("", "")                       ,
#'        `isDIF("word", 7)`                     = isDIF("word", 7)                    ,
#'        `isDIF(ChrMat., ChrMat.)`              = isDIF(ChrMat., ChrMat.)             ,
#'        `isDIF(NA, NA)`                        = isDIF(NA, NA)                       ,
#'        `isDIF1("", "")`                       = isDIF1("", "")                      ,
#'        `isDIF1("word", 7)`                    = isDIF1("word", 7)                   ,
#'        `isDIF1(ChrMat., ChrMat.)`             = isDIF1(ChrMat., ChrMat.)            ,
#'        `isDIF1(NA, NA)`                       = isDIF1(NA, NA)                      ,
#'        `isEQ("", "")`                         = isEQ("", "")                        ,
#'        `isEQ("word", 7)`                      = isEQ("word", 7)                     ,
#'        `isEQ(ChrMat., ChrMat.)`               = isEQ(ChrMat., ChrMat.)              ,
#'        `isEQ(NA, NA)`                         = isEQ(NA, NA)                        ,
#'        `isEQ1("", "")`                        = isEQ1("", "")                       ,
#'        `isEQ1("word", 7)`                     = isEQ1("word", 7)                    ,
#'        `isEQ1(ChrMat., ChrMat.)`              = isEQ1(ChrMat., ChrMat.)             ,
#'        `isEQ1(NA, NA)`                        = isEQ1(NA, NA)                       ,
#'        `isERR(non.existent.variable)`         = isERR(non.existent.variable)        ,
#'        `isERR(pi)`                            = isERR(pi)                           ,
#'        `isF(c(T, F))`                         = isF(c(T, F))                        ,
#'        `isF(ChrMat.)`                         = isF(ChrMat.)                        ,
#'        `isF(NA)`                              = isF(NA)                             ,
#'        `isF(T)`                               = isF(T)                              ,
#'        `isF(T)`                               = isF(T)                              ,
#'        `isF1(c(T, F))`                        = isF1(c(T, F))                       ,
#'        `isF1(ChrMat.)`                        = isF1(ChrMat.)                       ,
#'        `isF1(F)`                              = isF1(F)                             ,
#'        `isF1(NA)`                             = isF1(NA)                            ,
#'        `isF1(T)`                              = isF1(T)                             ,
#'        `isGE("a", "b")`                       = isGE("a", "b")                      ,
#'        `isGE("b", "a")`                       = isGE("b", "a")                      ,
#'        `isGE(1, 1)`                           = isGE(1, 1)                          ,
#'        `isGE(ChrMat., ChrMat.)`               = isGE(ChrMat., ChrMat.)              ,
#'        `isGE1("a", "b")`                      = isGE1("a", "b")                     ,
#'        `isGE1("b", "a")`                      = isGE1("b", "a")                     ,
#'        `isGE1(1, 1)`                          = isGE1(1, 1)                         ,
#'        `isGE1(ChrMat., ChrMat.)`              = isGE1(ChrMat., ChrMat.)             ,
#'        `isGT("a", "b")`                       = isGT("a", "b")                      ,
#'        `isGT("b", "a")`                       = isGT("b", "a")                      ,
#'        `isGT(1, 1)`                           = isGT(1, 1)                          ,
#'        `isGT(ChrMat., ChrMat.)`               = isGT(ChrMat., ChrMat.)              ,
#'        `isGT1("a", "b")`                      = isGT1("a", "b")                     ,
#'        `isGT1("b", "a")`                      = isGT1("b", "a")                     ,
#'        `isGT1(1, 1)`                          = isGT1(1, 1)                         ,
#'        `isGT1(ChrMat., ChrMat.)`              = isGT1(ChrMat., ChrMat.)             ,
#'        `isIN(1, "", "a", 1:2)`                = isIN(1, "", "a", 1:2)               ,
#'        `isIN(1, "a", letters)`                = isIN(1, "a", letters)               ,
#'        `isIN(NULL, Blank., Combo.)`           = isIN(NULL, Blank., Combo.)          ,
#'        `isIN1(1, "", "a", 1:2)`               = isIN1(1, "", "a", 1:2)              ,
#'        `isIN1(1, "a", letters)`               = isIN1(1, "a", letters)              ,
#'        `isIN1(NULL, Blank., Combo.)`          = isIN1(NULL, Blank., Combo.)         ,
#'        `isLB1(ChrMat.)`                       = isLB1(ChrMat.)                      ,
#'        `isLE("a", "b")`                       = isLE("a", "b")                      ,
#'        `isLE("b", "a")`                       = isLE("b", "a")                      ,
#'        `isLE(1, 1)`                           = isLE(1, 1)                          ,
#'        `isLE(ChrMat., ChrMat.)`               = isLE(ChrMat., ChrMat.)              ,
#'        `isLE1("a", "b")`                      = isLE1("a", "b")                     ,
#'        `isLE1("b", "a")`                      = isLE1("b", "a")                     ,
#'        `isLE1(1, 1)`                          = isLE1(1, 1)                         ,
#'        `isLE1(ChrMat., ChrMat.)`              = isLE1(ChrMat., ChrMat.)             ,
#'        `isLG1(c(T, F))`                       = isLG1(c(T, F))                      ,
#'        `isLG1(ChrMat.)`                       = isLG1(ChrMat.)                      ,
#'        `isLG1(F)`                             = isLG1(F)                            ,
#'        `isLG1(NA)`                            = isLG1(NA)                           ,
#'        `isLG1(T)`                             = isLG1(T)                            ,
#'        `isLT("a", "b")`                       = isLT("a", "b")                      ,
#'        `isLT("b", "a")`                       = isLT("b", "a")                      ,
#'        `isLT(1, 1)`                           = isLT(1, 1)                          ,
#'        `isLT(ChrMat., ChrMat.)`               = isLT(ChrMat., ChrMat.)              ,
#'        `isLT1("a", "b")`                      = isLT1("a", "b")                     ,
#'        `isLT1("b", "a")`                      = isLT1("b", "a")                     ,
#'        `isLT1(1, 1)`                          = isLT1(1, 1)                         ,
#'        `isLT1(ChrMat., ChrMat.)`              = isLT1(ChrMat., ChrMat.)             ,
#'        `isMF(1, "", "a", 1:2)`                = isMF(1, "", "a", 1:2)               ,
#'        `isMF(1, "a", letters)`                = isMF(1, "a", letters)               ,
#'        `isMF(NULL, Blank., Combo.)`           = isMF(NULL, Blank., Combo.)          ,
#'        `isMF1(1, "", "a", 1:2)`               = isMF1(1, "", "a", 1:2)              ,
#'        `isMF1(1, "a", letters)`               = isMF1(1, "a", letters)              ,
#'        `isMF1(NULL, Blank., Combo.)`          = isMF1(NULL, Blank., Combo.)         ,
#'        `isNA(1)`                              = isNA(1)                             ,
#'        `isNA(c(NA, NA))`                      = isNA(c(NA, NA))                     ,
#'        `isNA(ChrMat.)`                        = isNA(ChrMat.)                       ,
#'        `isNA(NA)`                             = isNA(NA)                            ,
#'        `isNA1(1)`                             = isNA1(1)                            ,
#'        `isNA1(c(NA, NA))`                     = isNA1(c(NA, NA))                    ,
#'        `isNA1(ChrMat.)`                       = isNA1(ChrMat.)                      ,
#'        `isNA1(NA)`                            = isNA1(NA)                           ,
#'        `isOK(1)`                              = isOK(1)                             ,
#'        `isOK(c(NA, NA))`                      = isOK(c(NA, NA))                     ,
#'        `isOK(ChrMat.)`                        = isOK(ChrMat.)                       ,
#'        `isOK(NA)`                             = isOK(NA)                            ,
#'        `isOK1(1)`                             = isOK1(1)                            ,
#'        `isOK1(c(NA, NA))`                     = isOK1(c(NA, NA))                    ,
#'        `isOK1(ChrMat.)`                       = isOK1(ChrMat.)                      ,
#'        `isOK1(NA)`                            = isOK1(NA)                           ,
#'        `isSEQ(1, letters)`                    = isSEQ(1, letters)                   ,
#'        `isSEQ(1, letters)`                    = isSEQ(1, letters)                   ,
#'        `isSEQ(1:3, c(1, 2, 3, 2, 1))`         = isSEQ(1:3, c(1, 2, 3, 2, 1))        ,
#'        `isSEQ(1:3, c(1, 2, 3, 2, 1))`         = isSEQ(1:3, c(1, 2, 3, 2, 1))        ,
#'        `isSEQ(1:3, c(1, 2, 3))`               = isSEQ(1:3, c(1, 2, 3))              ,
#'        `isSEQ(1:3, c(1, 2, 3))`               = isSEQ(1:3, c(1, 2, 3))              ,
#'        `isT(c(T, F))`                         = isT(c(T, F))                        ,
#'        `isT(ChrMat.)`                         = isT(ChrMat.)                        ,
#'        `isT(NA)`                              = isT(NA)                             ,
#'        `isT(T)`                               = isT(T)                              ,
#'        `isT(T)`                               = isT(T)                              ,
#'        `isT1(c(T, F))`                        = isT1(c(T, F))                       ,
#'        `isT1(ChrMat.)`                        = isT1(ChrMat.)                       ,
#'        `isT1(F)`                              = isT1(F)                             ,
#'        `isT1(NA)`                             = isT1(NA)                            ,
#'        `isT1(T)`                              = isT1(T)                             ,
#'        `isTF(c(T, F))`                        = isTF(c(T, F))                       ,
#'        `isTF(ChrMat.)`                        = isTF(ChrMat.)                       ,
#'        `isTF(ChrMat.)`                        = isTF(ChrMat.)                       ,
#'        `isTF(NA)`                             = isTF(NA)                            ,
#'        `isTF(T)`                              = isTF(T)                             ,
#'        `isTF(T)`                              = isTF(T)                             ,
#'        `isTF1(c(T, F))`                       = isTF1(c(T, F))                      ,
#'        `isTF1(ChrMat.)`                       = isTF1(ChrMat.)                      ,
#'        `isTF1(F)`                             = isTF1(F)                            ,
#'        `isTF1(NA)`                            = isTF1(NA)                           ,
#'        `isTF1(T)`                             = isTF1(T)                            ,
#'        `isVEC(1, letters)`                    = isVEC(1, letters)                   ,
#'        `isVEQ(1:3, c(1, 2, 3, 2, 1))`         = isVEQ(1:3, c(1, 2, 3, 2, 1))        ,
#'        `isVEQ(1:3, c(1, 2, 3, 2, 1))`         = isVEQ(1:3, c(1, 2, 3, 2, 1))        ,
#'        `isVEQ(1:3, letters)`                  = isVEQ(1:3, letters)                 ,
#'        `isVEQ(1:3, c(1, 2, 3))`               = isVEQ(1:3, c(1, 2, 3))              ,
#'        `isVEQ(1:3, c(1, 2, 3))`               = isVEQ(1:3, c(1, 2, 3))              ,
#'        `lacksALL(Blank., Combo.)`             = lacksALL(Blank., Combo.)            ,
#'        `lacksANY(Blank., Combo.)`             = lacksANY(Blank., Combo.)            ,
#'        `lacksMANY(Blank., Combo.)`            = lacksMANY(Blank., Combo.)           ,
#'        `lacksNONE(Blank., Combo.)`            = lacksNONE(Blank., Combo.)           ,
#'        `lacksONE(Blank., Combo.)`             = lacksONE(Blank., Combo.)            ,
#'        `lacksSOME(Blank., Combo.)`            = lacksSOME(Blank., Combo.)           ,
#'        `lacksTWO(Blank., Combo.)`             = lacksTWO(Blank., Combo.)            ,
#'        `manyBL(c(Blank., letters))`           = manyBL(c(Blank., letters))          ,
#'        `manyDIF("a", c(Blank., NA, letters))` = manyDIF("a", c(Blank., NA, letters)),
#'        `manyEQ(NA, c(Blank., NA, letters))`   = manyEQ(NA, c(Blank., NA, letters))  ,
#'        `manyF(c(list(Blank.), Combo.))`       = manyF(c(list(Blank.), Combo.))      ,
#'        `manyGE("a", c(Blank., NA, letters))`  = manyGE("a", c(Blank., NA, letters)) ,
#'        `manyGT("a", c(Blank., NA, letters))`  = manyGT("a", c(Blank., NA, letters)) ,
#'        `manyIN(NULL, Blank., Combo.)`         = manyIN(NULL, Blank., Combo.)        ,
#'        `manyLE("a", c(Blank., NA, letters))`  = manyLE("a", c(Blank., NA, letters)) ,
#'        `manyLG(c(NA, T, F))`                  = manyLG(c(NA, T, F))                 ,
#'        `manyLT("a", c(Blank., NA, letters))`  = manyLT("a", c(Blank., NA, letters)) ,
#'        `manyMF(NULL, Blank., Combo.)`         = manyMF(NULL, Blank., Combo.)        ,
#'        `manyNAS(c(Blank., NA, letters))`      = manyNAS(c(Blank., NA, letters))     ,
#'        `manyNAS(ChrMat.)`                     = manyNAS(ChrMat.)                    ,
#'        `manyOKS(c(Blank., NA, letters))`      = manyOKS(c(Blank., NA, letters))     ,
#'        `manyOKS(ChrMat.)`                     = manyOKS(ChrMat.)                    ,
#'        `manyT(c(list(Blank.), Combo.))`       = manyT(c(list(Blank.), Combo.))      ,
#'        `manyTF(c(list(Blank.), Combo.))`      = manyTF(c(list(Blank.), Combo.))     ,
#'        `msgERR(non.existent.variable)`        = msgERR(non.existent.variable)       ,
#'        `msgERR(pi)`                           = msgERR(pi)                          ,
#'        `nBL(c(list(Blank.), Combo.))`         = nBL(c(list(Blank.), Combo.))        ,
#'        `nF(c(list(Blank.), Combo.))`          = nF(c(list(Blank.), Combo.))         ,
#'        `nHAS(Blank., Combo.)`                 = nHAS(Blank., Combo.)                ,
#'        `nIN(NULL, Blank., Combo.)`            = nIN(NULL, Blank., Combo.)           ,
#'        `nLACKS(Blank., Combo.)`               = nLACKS(Blank., Combo.)              ,
#'        `nMF(NULL, Blank., Combo.)`            = nMF(NULL, Blank., Combo.)           ,
#'        `nNAS(ChrMat.)`                        = nNAS(ChrMat.)                       ,
#'        `nOKS(ChrMat.)`                        = nOKS(ChrMat.)                       ,
#'        `noneBL(c(Blank., letters))`           = noneBL(c(Blank., letters))          ,
#'        `noneDIF("a", c(Blank., NA, letters))` = noneDIF("a", c(Blank., NA, letters)),
#'        `noneEQ(NA, c(Blank., NA, letters))`   = noneEQ(NA, c(Blank., NA, letters))  ,
#'        `noneF(c(list(Blank.), Combo.))`       = noneF(c(list(Blank.), Combo.))      ,
#'        `noneGE("a", c(Blank., NA, letters))`  = noneGE("a", c(Blank., NA, letters)) ,
#'        `noneGT("a", c(Blank., NA, letters))`  = noneGT("a", c(Blank., NA, letters)) ,
#'        `noneIN(NULL, Blank., Combo.)`         = noneIN(NULL, Blank., Combo.)        ,
#'        `noneLE("a", c(Blank., NA, letters))`  = noneLE("a", c(Blank., NA, letters)) ,
#'        `noneLG(c(NA, T, F))`                  = noneLG(c(NA, T, F))                 ,
#'        `noneLT("a", c(Blank., NA, letters))`  = noneLT("a", c(Blank., NA, letters)) ,
#'        `noneMF(NULL, Blank., Combo.)`         = noneMF(NULL, Blank., Combo.)        ,
#'        `noneNAS(c(Blank., NA, letters))`      = noneNAS(c(Blank., NA, letters))     ,
#'        `noneNAS(ChrMat.)`                     = noneNAS(ChrMat.)                    ,
#'        `noneOKS(c(Blank., NA, letters))`      = noneOKS(c(Blank., NA, letters))     ,
#'        `noneOKS(ChrMat.)`                     = noneOKS(ChrMat.)                    ,
#'        `noneT(c(list(Blank.), Combo.))`       = noneT(c(list(Blank.), Combo.))      ,
#'        `noneTF(c(list(Blank.), Combo.))`      = noneTF(c(list(Blank.), Combo.))     ,
#'        `notBL("")`                            = notBL("")                           ,
#'        `notBL("a")`                           = notBL("a")                          ,
#'        `notBL(c("", ""))`                     = notBL(c("", ""))                    ,
#'        `notBL(ChrMat.)`                       = notBL(ChrMat.)                      ,
#'        `notBL(NA)`                            = notBL(NA)                           ,
#'        `notBL1("")`                           = notBL1("")                          ,
#'        `notBL1("a")`                          = notBL1("a")                         ,
#'        `notBL1(c("", ""))`                    = notBL1(c("", ""))                   ,
#'        `notBL1(ChrMat.)`                      = notBL1(ChrMat.)                     ,
#'        `notBL1(NA)`                           = notBL1(NA)                          ,
#'        `notDIF("", "")`                       = notDIF("", "")                      ,
#'        `notDIF("word", 7)`                    = notDIF("word", 7)                   ,
#'        `notDIF(ChrMat., ChrMat.)`             = notDIF(ChrMat., ChrMat.)            ,
#'        `notDIF(NA, NA)`                       = notDIF(NA, NA)                      ,
#'        `notDIF1("", "")`                      = notDIF1("", "")                     ,
#'        `notDIF1("word", 7)`                   = notDIF1("word", 7)                  ,
#'        `notDIF1(ChrMat., ChrMat.)`            = notDIF1(ChrMat., ChrMat.)           ,
#'        `notDIF1(NA, NA)`                      = notDIF1(NA, NA)                     ,
#'        `notEQ("", "")`                        = notEQ("", "")                       ,
#'        `notEQ("word", 7)`                     = notEQ("word", 7)                    ,
#'        `notEQ(ChrMat., ChrMat.)`              = notEQ(ChrMat., ChrMat.)             ,
#'        `notEQ(NA, NA)`                        = notEQ(NA, NA)                       ,
#'        `notEQ1("", "")`                       = notEQ1("", "")                      ,
#'        `notEQ1("word", 7)`                    = notEQ1("word", 7)                   ,
#'        `notEQ1(ChrMat., ChrMat.)`             = notEQ1(ChrMat., ChrMat.)            ,
#'        `notEQ1(NA, NA)`                       = notEQ1(NA, NA)                      ,
#'        `notERR(non.existent.variable)`        = notERR(non.existent.variable)       ,
#'        `notERR(pi)`                           = notERR(pi)                          ,
#'        `notF(c(T, F))`                        = notF(c(T, F))                       ,
#'        `notF(ChrMat.)`                        = notF(ChrMat.)                       ,
#'        `notF(NA)`                             = notF(NA)                            ,
#'        `notF(T)`                              = notF(T)                             ,
#'        `notF(T)`                              = notF(T)                             ,
#'        `notF1(c(T, F))`                       = notF1(c(T, F))                      ,
#'        `notF1(ChrMat.)`                       = notF1(ChrMat.)                      ,
#'        `notF1(F)`                             = notF1(F)                            ,
#'        `notF1(NA)`                            = notF1(NA)                           ,
#'        `notF1(T)`                             = notF1(T)                            ,
#'        `notGE("a", "b")`                      = notGE("a", "b")                     ,
#'        `notGE("b", "a")`                      = notGE("b", "a")                     ,
#'        `notGE(1, 1)`                          = notGE(1, 1)                         ,
#'        `notGE(ChrMat., ChrMat.)`              = notGE(ChrMat., ChrMat.)             ,
#'        `notGE1("a", "b")`                     = notGE1("a", "b")                    ,
#'        `notGE1("b", "a")`                     = notGE1("b", "a")                    ,
#'        `notGE1(1, 1)`                         = notGE1(1, 1)                        ,
#'        `notGE1(ChrMat., ChrMat.)`             = notGE1(ChrMat., ChrMat.)            ,
#'        `notGT("a", "b")`                      = notGT("a", "b")                     ,
#'        `notGT("b", "a")`                      = notGT("b", "a")                     ,
#'        `notGT(1, 1)`                          = notGT(1, 1)                         ,
#'        `notGT(ChrMat., ChrMat.)`              = notGT(ChrMat., ChrMat.)             ,
#'        `notGT1("a", "b")`                     = notGT1("a", "b")                    ,
#'        `notGT1("b", "a")`                     = notGT1("b", "a")                    ,
#'        `notGT1(1, 1)`                         = notGT1(1, 1)                        ,
#'        `notGT1(ChrMat., ChrMat.)`             = notGT1(ChrMat., ChrMat.)            ,
#'        `notIN(1, "", "a", 1:2)`               = notIN(1, "", "a", 1:2)              ,
#'        `notIN(1, "a", letters)`               = notIN(1, "a", letters)              ,
#'        `notIN(NULL, Blank., Combo.)`          = notIN(NULL, Blank., Combo.)         ,
#'        `notIN1(1, "", "a", 1:2)`              = notIN1(1, "", "a", 1:2)             ,
#'        `notIN1(1, "a", letters)`              = notIN1(1, "a", letters)             ,
#'        `notIN1(NULL, Blank., Combo.)`         = notIN1(NULL, Blank., Combo.)        ,
#'        `notLE("a", "b")`                      = notLE("a", "b")                     ,
#'        `notLE("b", "a")`                      = notLE("b", "a")                     ,
#'        `notLE(1, 1)`                          = notLE(1, 1)                         ,
#'        `notLE(ChrMat., ChrMat.)`              = notLE(ChrMat., ChrMat.)             ,
#'        `notLE1("a", "b")`                     = notLE1("a", "b")                    ,
#'        `notLE1("b", "a")`                     = notLE1("b", "a")                    ,
#'        `notLE1(1, 1)`                         = notLE1(1, 1)                        ,
#'        `notLE1(ChrMat., ChrMat.)`             = notLE1(ChrMat., ChrMat.)            ,
#'        `notLG1(c(T, F))`                      = notLG1(c(T, F))                     ,
#'        `notLG1(ChrMat.)`                      = notLG1(ChrMat.)                     ,
#'        `notLG1(F)`                            = notLG1(F)                           ,
#'        `notLG1(NA)`                           = notLG1(NA)                          ,
#'        `notLG1(T)`                            = notLG1(T)                           ,
#'        `notLT("a", "b")`                      = notLT("a", "b")                     ,
#'        `notLT("b", "a")`                      = notLT("b", "a")                     ,
#'        `notLT(1, 1)`                          = notLT(1, 1)                         ,
#'        `notLT(ChrMat., ChrMat.)`              = notLT(ChrMat., ChrMat.)             ,
#'        `notLT1("a", "b")`                     = notLT1("a", "b")                    ,
#'        `notLT1("b", "a")`                     = notLT1("b", "a")                    ,
#'        `notLT1(1, 1)`                         = notLT1(1, 1)                        ,
#'        `notLT1(ChrMat., ChrMat.)`             = notLT1(ChrMat., ChrMat.)            ,
#'        `notMF(1, "", "a", 1:2)`               = notMF(1, "", "a", 1:2)              ,
#'        `notMF(1, "a", letters)`               = notMF(1, "a", letters)              ,
#'        `notMF(NULL, Blank., Combo.)`          = notMF(NULL, Blank., Combo.)         ,
#'        `notMF1(1, "", "a", 1:2)`              = notMF1(1, "", "a", 1:2)             ,
#'        `notMF1(1, "a", letters)`              = notMF1(1, "a", letters)             ,
#'        `notMF1(NULL, Blank., Combo.)`         = notMF1(NULL, Blank., Combo.)        ,
#'        `notNA1(1)`                            = notNA1(1)                           ,
#'        `notNA1(c(NA, NA))`                    = notNA1(c(NA, NA))                   ,
#'        `notNA1(ChrMat.)`                      = notNA1(ChrMat.)                     ,
#'        `notNA1(NA)`                           = notNA1(NA)                          ,
#'        `notNAS(c(NA, NA)), notNAS(ChrMat.)`   = notNAS(c(NA, NA)), notNAS(ChrMat.)  ,
#'        `notNAS(NA), notNAS(1)`                = notNAS(NA), notNAS(1)               ,
#'        `notOK1(1)`                            = notOK1(1)                           ,
#'        `notOK1(c(NA, NA))`                    = notOK1(c(NA, NA))                   ,
#'        `notOK1(ChrMat.)`                      = notOK1(ChrMat.)                     ,
#'        `notOK1(NA)`                           = notOK1(NA)                          ,
#'        `notOKS(c(NA, NA)), notOKS(ChrMat.)`   = notOKS(c(NA, NA)), notOKS(ChrMat.)  ,
#'        `notOKS(NA), notOKS(1)`                = notOKS(NA), notOKS(1)               ,
#'        `notSEQ(1, letters)`                   = notSEQ(1, letters)                  ,
#'        `notSEQ(1, letters)`                   = notSEQ(1, letters)                  ,
#'        `notSEQ(1:3, c(1, 2, 3, 2, 1))`        = notSEQ(1:3, c(1, 2, 3, 2, 1))       ,
#'        `notSEQ(1:3, c(1, 2, 3, 2, 1))`        = notSEQ(1:3, c(1, 2, 3, 2, 1))       ,
#'        `notSEQ(1:3, c(1, 2, 3))`              = notSEQ(1:3, c(1, 2, 3))             ,
#'        `notSEQ(1:3, c(1, 2, 3))`              = notSEQ(1:3, c(1, 2, 3))             ,
#'        `notT(c(T, F))`                        = notT(c(T, F))                       ,
#'        `notT(ChrMat.)`                        = notT(ChrMat.)                       ,
#'        `notT(NA)`                             = notT(NA)                            ,
#'        `notT(T)`                              = notT(T)                             ,
#'        `notT(T)`                              = notT(T)                             ,
#'        `notT1(c(T, F))`                       = notT1(c(T, F))                      ,
#'        `notT1(ChrMat.)`                       = notT1(ChrMat.)                      ,
#'        `notT1(F)`                             = notT1(F)                            ,
#'        `notT1(NA)`                            = notT1(NA)                           ,
#'        `notT1(T)`                             = notT1(T)                            ,
#'        `notTF(c(T, F))`                       = notTF(c(T, F))                      ,
#'        `notTF(ChrMat.)`                       = notTF(ChrMat.)                       ,
#'        `notTF(NA)`                            = notTF(NA)                           ,
#'        `notTF(T)`                             = notTF(T)                            ,
#'        `notTF(T)`                             = notTF(T)                            ,
#'        `notTF1(c(T, F))`                      = notTF1(c(T, F))                     ,
#'        `notTF1(ChrMat.)`                      = notTF1(ChrMat.)                     ,
#'        `notTF1(F)`                            = notTF1(F)                           ,
#'        `notTF1(NA)`                           = notTF1(NA)                          ,
#'        `notTF1(T)`                            = notTF1(T)                           ,
#'        `notVEC(1, letters)`                   = notVEC(1, letters)                  ,
#'        `notVEC(1, letters)`                   = notVEC(1, letters)                  ,
#'        `notVEQ(1:3, c(1, 2, 3, 2, 1))`        = notVEQ(1:3, c(1, 2, 3, 2, 1))       ,
#'        `notVEQ(1:3, c(1, 2, 3, 2, 1))`        = notVEQ(1:3, c(1, 2, 3, 2, 1))       ,
#'        `notVEQ(1:3, c(1, 2, 3))`              = notVEQ(1:3, c(1, 2, 3))             ,
#'        `notVEQ(1:3, c(1, 2, 3))`              = notVEQ(1:3, c(1, 2, 3))             ,
#'        `nT(c(list(Blank.), Combo.))`          = nT(c(list(Blank.), Combo.))         ,
#'        `nTF(c(list(Blank.), Combo.))`         = nTF(c(list(Blank.), Combo.))        ,
#'        `NULL %IN% abc`                        = NULL %IN% abc                       ,
#'        `NULL %OUT% abc`                       = NULL %OUT% abc                      ,
#'        `oneBL(c(Blank., letters))`            = oneBL(c(Blank., letters))           ,
#'        `oneDIF("a", c(Blank., NA, letters))`  = oneDIF("a", c(Blank., NA, letters)) ,
#'        `oneEQ(NA, c(Blank., NA, letters))`    = oneEQ(NA, c(Blank., NA, letters))   ,
#'        `oneF(c(list(Blank.), Combo.))`        = oneF(c(list(Blank.), Combo.))       ,
#'        `oneGE("a", c(Blank., NA, letters))`   = oneGE("a", c(Blank., NA, letters))  ,
#'        `oneGT("a", c(Blank., NA, letters))`   = oneGT("a", c(Blank., NA, letters))  ,
#'        `oneIN(NULL, Blank., Combo.)`          = oneIN(NULL, Blank., Combo.)         ,
#'        `oneLE("a", c(Blank., NA, letters))`   = oneLE("a", c(Blank., NA, letters))  ,
#'        `oneLG(c(NA, T, F))`                   = oneLG(c(NA, T, F))                  ,
#'        `oneLT("a", c(Blank., NA, letters))`   = oneLT("a", c(Blank., NA, letters))  ,
#'        `oneMF(NULL, Blank., Combo.)`          = oneMF(NULL, Blank., Combo.)         ,
#'        `oneNAS(c(Blank., NA, letters))`       = oneNAS(c(Blank., NA, letters))      ,
#'        `oneNAS(ChrMat.)`                      = oneNAS(ChrMat.)                     ,
#'        `oneOKS(c(Blank., NA, letters))`       = oneOKS(c(Blank., NA, letters))      ,
#'        `oneOKS(ChrMat.)`                      = oneOKS(ChrMat.)                     ,
#'        `oneT(c(list(Blank.), Combo.))`        = oneT(c(list(Blank.), Combo.))       ,
#'        `oneTF(c(list(Blank.), Combo.))`       = oneTF(c(list(Blank.), Combo.))      ,
#'        `someBL(c(Blank., letters))`           = someBL(c(Blank., letters))          ,
#'        `someDIF("a", c(Blank., NA, letters))` = someDIF("a", c(Blank., NA, letters)),
#'        `someEQ(NA, c(Blank., NA, letters))`   = someEQ(NA, c(Blank., NA, letters))  ,
#'        `someF(c(list(Blank.), Combo.))`       = someF(c(list(Blank.), Combo.))      ,
#'        `someGE("a", c(Blank., NA, letters))`  = someGE("a", c(Blank., NA, letters)) ,
#'        `someGT("a", c(Blank., NA, letters))`  = someGT("a", c(Blank., NA, letters)) ,
#'        `someIN(NULL, Blank., Combo.)`         = someIN(NULL, Blank., Combo.)        ,
#'        `someLE("a", c(Blank., NA, letters))`  = someLE("a", c(Blank., NA, letters)) ,
#'        `someLG(c(NA, T, F))`                  = someLG(c(NA, T, F))                 ,
#'        `someLT("a", c(Blank., NA, letters))`  = someLT("a", c(Blank., NA, letters)) ,
#'        `someMF(NULL, Blank., Combo.)`         = someMF(NULL, Blank., Combo.)        ,
#'        `someNAS(c(Blank., NA, letters))`      = someNAS(c(Blank., NA, letters))     ,
#'        `someNAS(ChrMat.)`                     = someNAS(ChrMat.)                    ,
#'        `someOKS(c(Blank., NA, letters))`      = someOKS(c(Blank., NA, letters))     ,
#'        `someOKS(ChrMat.)`                     = someOKS(ChrMat.)                    ,
#'        `someT(c(list(Blank.), Combo.))`       = someT(c(list(Blank.), Combo.))      ,
#'        `someTF(c(list(Blank.), Combo.))`      = someTF(c(list(Blank.), Combo.))     ,
#'        `TRUE %.and.% 42`                      = TRUE %.and.% 42                     ,
#'        `TRUE %.and.% TRUE`                    = TRUE %.and.% TRUE                   ,
#'        `TRUE %.nor.% 42`                      = TRUE %.nor.% 42                     ,
#'        `TRUE %.nor.% TRUE`                    = TRUE %.nor.% TRUE                   ,
#'        `TRUE %.xor.% 42`                      = TRUE %.xor.% 42                     ,
#'        `TRUE %.xor.% TRUE`                    = TRUE %.xor.% TRUE                   ,
#'        `TRUE %.or.% 42`                       = TRUE %.or.% 42                      ,
#'        `TRUE %.or.% TRUE`                     = TRUE %.or.% TRUE                    ,
#'        `twoBL(c(Blank., letters))`            = twoBL(c(Blank., letters))           ,
#'        `twoDIF("a", c(Blank., NA, letters))`  = twoDIF("a", c(Blank., NA, letters)) ,
#'        `twoEQ(NA, c(Blank., NA, letters))`    = twoEQ(NA, c(Blank., NA, letters))   ,
#'        `twoF(c(list(Blank.), Combo.))`        = twoF(c(list(Blank.), Combo.))       ,
#'        `twoGE("a", c(Blank., NA, letters))`   = twoGE("a", c(Blank., NA, letters))  ,
#'        `twoGT("a", c(Blank., NA, letters))`   = twoGT("a", c(Blank., NA, letters))  ,
#'        `twoIN(NULL, Blank., Combo.)`          = twoIN(NULL, Blank., Combo.)         ,
#'        `twoLE("a", c(Blank., NA, letters))`   = twoLE("a", c(Blank., NA, letters))  ,
#'        `twoLG(c(NA, T, F))`                   = twoLG(c(NA, T, F))                  ,
#'        `twoLT("a", c(Blank., NA, letters))`   = twoLT("a", c(Blank., NA, letters))  ,
#'        `twoMF(NULL, Blank., Combo.)`          = twoMF(NULL, Blank., Combo.)         ,
#'        `twoNAS(c(Blank., NA, letters))`       = twoNAS(c(Blank., NA, letters))      ,
#'        `twoNAS(ChrMat.)`                      = twoNAS(ChrMat.)                     ,
#'        `twoOKS(c(Blank., NA, letters))`       = twoOKS(c(Blank., NA, letters))      ,
#'        `twoOKS(ChrMat.)`                      = twoOKS(ChrMat.)                     ,
#'        `twoT(c(list(Blank.), Combo.))`        = twoT(c(list(Blank.), Combo.))       ,
#'        `twoTF(c(list(Blank.), Combo.))`       = twoTF(c(list(Blank.), Combo.))      )
#' }
#' egFailsafe()
#' @export
failsafe <- function(x, def = ".err") {
  x <- tryCatch(base::identity(x), error = function(e) e, finally = NULL)
  if (assertthat::is.error(x)) {return(x)}
  def <- tryCatch(base::identity(def), error = function(e) e, finally = NULL)
  if (assertthat::is.error(def)) {def <- ".err"}
  def
}

#' @rdname failsafe
#' @export
isERR <- function(x) {assertthat::is.error(tryCatch(base::identity(x), error = function(e) e, finally = NULL))}

#' @rdname failsafe
#' @export
notERR <- function(x) {!assertthat::is.error(tryCatch(base::identity(x), error = function(e) e, finally = NULL))}

#' @rdname failsafe
#' @export
msgERR <- function(x) {
  x <- uj::failsafe(x)
  if (assertthat::is.error(x)) {x$message} else {NULL}
}

#' @rdname failsafe
#' @export
makeERR <- function() {base::simpleError("Intentionally-generated error.")}

#' @rdname failsafe
#' @export
fsDEF <- function(x) {uj::f0(uj::isERR(x), F, uj::DEF(x))}

#' @rdname failsafe
#' @export
fsSCL <- function(x, val) {uj::f0(uj::isERR(x) | uj::isERR(val), F,
                                  uj::f0(uj::notN1(x) | uj::notN1(val), F,
                                         uj::f0(uj::notATM(x) | uj::notATM(val), F,
                                                uj::f0(uj:::.compat(x, val), F,
                                                       uj::f0(uj::na(x) & uj::na(val), T,
                                                              uj::f0(uj::na(x) | uj::na(val), F, x == val))))))}

#' @rdname failsafe
#' @export
fsVEC <- function(x, val) {uj::f0(uj::isERR(x) | uj::isERR(val), F,
                                  uj::f0(uj:::.mismatchN(x, val), F,
                                         uj::f0(uj::notATM(x) | uj::notATM(val), F,
                                                uj::f0(uj:::.compat(x, val), F,
                                                       uj::f0(uj::na(x) & uj::na(val), T,
                                                              uj::f0(uj::na(x) | uj::na(val) , F, x == val))))))}

#' @rdname failsafe
#' @export
fsNULL <- function(x) {uj::f0(uj::isERR(x), T, uj::null(x))}

#' @rdname failsafe
#' @export
sclBL <- function(x) {uj::fsSCL(x, "")}

#' @rdname failsafe
#' @export
sclT <- function(x)  {uj::fsSCL(x, T)}

#' @rdname failsafe
#' @export
sclF <- function(x)  {uj::fsSCL(x, T) | uj::fsSCL(x, F)}

#' @rdname failsafe
#' @export
sclTF <- function(x)  {uj::fsSCL(x, T) | uj::fsSCL(x, F)}

#' @rdname failsafe
#' @export
sclLG <- function(x)  {uj::fsSCL(x, T) | uj::fsSCL(x, F) | fsSCL(x, NA)}

#' @rdname failsafe
#' @export
sclNA <- function(x)  {uj::fsSCL(x, NA)}

#' @rdname failsafe
#' @export
vecBL <- function(x) {uj::fsVEC(x, "")}

#' @rdname failsafe
#' @export
vecT <- function(x)  {uj::fsVEC(x, T)}

#' @rdname failsafe
#' @export
vecF <- function(x)  {uj::fsVEC(x, F)}

#' @rdname failsafe
#' @export
vecTF <- function(x)  {uj::fsVEC(x, T) | uj::fsVEC(x, F)}

#' @rdname failsafe
#' @export
vecNA <- function(x)  {uj::fsVEC(x, NA)}

#' @rdname failsafe
#' @export
fsOR <- function(x, or = NULL) {uj::f0(isERR(x), or, x)}

#' @rdname failsafe
#' @export
orF <- function(x) {uj::fsOR(x, FALSE)}

#' @rdname failsafe
#' @export
orT <- function(x) {uj::fsOR(x, TRUE)}

#' @rdname failsafe
#' @export
orBL <- function(x) {uj::fsOR(x, "")}

#' @rdname failsafe
#' @export
orC0 <- function(x) {uj::fsOR(x, base::character(0))}

#' @rdname failsafe
#' @export
orI0 <- function(x) {uhj::fsOR(x, base::integer(0))}

#' @rdname failsafe
#' @export
orL0 <- function(x) {uhj::fsOR(x, base::logical(0))}

#' @rdname failsafe
#' @export
orN0 <- function(x) {uhj::fsOR(x, base::numeric(0))}

#' @rdname failsafe
#' @export
orNA <- function(x) {uj::fsOR(x, NA)}

#' @rdname failsafe
#' @export
orNAC <- function(x) {uj::fsOR(x, NA_character_)}

#' @rdname failsafe
#' @export
orNAI <- function(x) {uj::fsOR(x, NA_integer_)}

#' @rdname failsafe
#' @export
orNAL <- function(x) {uj::fsOR(x, NA)}

#' @rdname failsafe
#' @export
orNAR <- function(x) {uj::fsOR(x, NA_real_)}

#' @rdname failsafe
#' @export
orNULL <- function(x) {uj::fsOR(x)}

#' @rdname failsafe
#' @export
IS <- function(x, y) {
  if (!assertthat::is.error(base::identical(x))) {if (!assertthat::is.error(base::identical(y))) {return(base::identical(x, y))}}
  F
}

#' @rdname failsafe
#' @export
ISNT <- function(x, y) {!uj::IS(x, y)}

#' @rdname failsafe
#' @export
isIN1 <- function(x, ...) {
  if (!assertthat::is.error(base::identity(x))) {
    if (uj::ND1P()) {
      if (uj::N1(x)) {
        if (uj::isATM(x)) {
          for (i in 1:uj::ND()) {
            dot <- uj::failsafe(base::identity(base::...elt(i)))
            if (!assertthat::is.error(dot)) {
              if (uj::isATM(dot)) {
                if (uj:::.compat(x, dot)) {
                  if (base::`%in%`(x, base::...elt(i))) {return(T)}
                }
              }
            }
          }
        }
      }
    }
  }
  F
}

#' @rdname failsafe
#' @export
isMF1 <- function(x, ...) {!uj::isIN1(x, ...)}

#' @rdname failsafe
#' @export
HAS1 <- function(x, y) {uj::isIN1(y, x)}

#' @rdname failsafe
#' @export
LACKS1 <- function(x, y) {!uj::isIN1(y, x)}

#' @rdname failsafe
#' @export
isEQ1 <- function(x, y) {
  if (!assertthat::is.error(base::identity(x))) {
    if (!assertthat::is.error(base::identity(y))) {
      if (uj::isATM(x) & uj::isATM(y)) {
        if (uj::N1(x) & uj::N1(y)) {
          if (uj:::.compat(x, y)) {
            if (uj::na(x) | uj::na(y)) {return(F)}
            else {return(uj::av(x == y))}
          }
        }
      }
    }
  }
  F
}

#' @rdname failsafe
#' @export
isGE1 <- function(x, y) {
  if (!assertthat::is.error(base::identity(x))) {
    if (!assertthat::is.error(base::identity(y))) {
      if (uj::isATM(x) & uj::isATM(y)) {
        if (uj::N1(x) & uj::N1(y)) {
          if (uj:::.compar(x, y)) {return(uj::av(x >= y))}
        }
      }
    }
  }
  F
}

#' @rdname failsafe
#' @export
isGT1 <- function(x, y) {
  if (!assertthat::is.error(base::identity(x))) {
    if (!assertthat::is.error(base::identity(y))) {
      if (uj::isATM(x) & uj::isATM(y)) {
        if (uj::N1(x) & uj::N1(y)) {
          if (uj:::.compar(x, y)) {return(uj::av(x > y))}
        }
      }
    }
  }
  F
}

#' @rdname failsafe
#' @export
isLE1 <- function(x, y) {
  if (!assertthat::is.error(base::identity(x))) {
    if (!assertthat::is.error(base::identity(y))) {
      if (uj::isATM(x) & uj::isATM(y)) {
        if (uj::N1(x) & uj::N1(y)) {
          if (uj:::.compar(x, y)) {return(uj::av(x <= y))}
        }
      }
    }
  }
  F
}

#' @rdname failsafe
#' @export
isLT1 <- function(x, y) {
  if (!assertthat::is.error(base::identity(x))) {
    if (!assertthat::is.error(base::identity(y))) {
      if (uj::isATM(x) & uj::isATM(y)) {
        if (uj::N1(x) & uj::N1(y)) {
          if (uj:::.compar(x, y)) {return(uj::av(x < y))}
        }
      }
    }
  }
  F
}

#' @rdname failsafe
#' @export
isDIF1 <- function(x, y) {!uj::isEQ1(x, y)}

#' @rdname failsafe
#' @export
isF1 <- function(x) {uj::isEQ1(x, F)}

#' @rdname failsafe
#' @export
isT1 <- function(x) {uj::isEQ1(x, T)}

#' @rdname failsafe
#' @export
isBL1 <- function(x) {uj::isEQ1(x, "")}

#' @rdname failsafe
#' @export
isTF1 <- function(x) {uj::isEQ1(x, F) | uj::isEQ1(x, T)}

#' @rdname failsafe
#' @export
isNA1 <- function(x) {
  if (!assertthat::is.error(x)) {if (uj::isATM(x)) {if (uj::N1(x)) {return(base::is.na(x))}}}
  F
}

#' @rdname failsafe
#' @export
isOK1 <- function(x) {
  if (!assertthat::is.error(x)) {if (uj::isATM(x)) {if (uj::N1(x)) {return(!base::is.na(x))}}}
  F
}

#' @rdname failsafe
#' @export
isLG1 <- function(x) {uj::isT1(x) | uj::isF1(x) | uj::isNA1(x)}





isIN <- function(x, ...) {
  if (!assertthat::is.error(base::identity(x))) {if (uj::isATM(x) & uj::N1P(x)) {return(base::sapply(x, uj::isIN1, ...))}}
  F
}

#' @rdname failsafe
#' @export
isMF <- function(x, ...) {!uj::isIN(x, ...)}

#' @rdname failsafe
#' @export
HAS <- function(x, y) {uj::isIN(y, x)}

#' @rdname failsafe
#' @export
LACKS <- function(x, y) {!uj::isIN(y, x)}

#' @rdname failsafe
#' @export
isEQ <- function(x, y) {
  if (!assertthat::is.error(base::identity(x))) {
    if (!assertthat::is.error(base::identity(y))) {
      if (uj::isATM(x) & uj::isATM(y)) {
        nx <- uj::N(x)
        ny <- uj::N(y)
        if (nx %in% base::c(1, ny)) {
          if (ny %in% base::c(1, nx)) {
            if (uj:::.compat(x, y)) {return(x == y)}
          }
        }
      }
    }
  }
  F
}

#' @rdname failsafe
#' @export
isGE <- function(x, y) {
  if (!assertthat::is.error(base::identity(x))) {
    if (!assertthat::is.error(base::identity(y))) {
      if (uj::isATM(x) & uj::isATM(y)) {
        nx <- uj::N(x)
        ny <- uj::N(y)
        if (nx %in% base::c(1, ny)) {
          if (ny %in% base::c(1, nx)) {
            if (uj:::.compar(x, y)) {return(x >= y)}
          }
        }
      }
    }
  }
  F
}

#' @rdname failsafe
#' @export
isGT <- function(x, y) {
  if (!assertthat::is.error(base::identity(x))) {
    if (!assertthat::is.error(base::identity(y))) {
      if (uj::isATM(x) & uj::isATM(y)) {
        nx <- uj::N(x)
        ny <- uj::N(y)
        if (nx %in% base::c(1, ny)) {
          if (ny %in% base::c(1, nx)) {
            if (uj:::.compar(x, y)) {return(x > y)}
          }
        }
      }
    }
  }
  F
}

#' @rdname failsafe
#' @export
isLE <- function(x, y) {
  if (!assertthat::is.error(base::identity(x))) {
    if (!assertthat::is.error(base::identity(y))) {
      if (uj::isATM(x) & uj::isATM(y)) {
        nx <- uj::N(x)
        ny <- uj::N(y)
        if (nx %in% base::c(1, ny)) {
          if (ny %in% base::c(1, nx)) {
            if (uj:::.compar(x, y)) {return(x <= y)}
          }
        }
      }
    }
  }
  F
}

#' @rdname failsafe
#' @export
isLT <- function(x, y) {
  if (!assertthat::is.error(base::identity(x))) {
    if (!assertthat::is.error(base::identity(y))) {
      if (uj::isATM(x) & uj::isATM(y)) {
        nx <- uj::N(x)
        ny <- uj::N(y)
        if (nx %in% base::c(1, ny)) {
          if (ny %in% base::c(1, nx)) {
            if (uj:::.compar(x, y)) {return(x < y)}
          }
        }
      }
    }
  }
  F
}

#' @rdname failsafe
#' @export
isDIF <- function(x, y) {!uj::isEQ(x, y)}

#' @rdname failsafe
#' @export
isF <- function(x) {uj::isEQ(x, F)}

#' @rdname failsafe
#' @export
isT <- function(x) {uj::isEQ(x, T)}

#' @rdname failsafe
#' @export
isBL <- function(x) {uj::isEQ(x, "")}

#' @rdname failsafe
#' @export
isTF <- function(x) {uj::isEQ(x, F) | uj::isEQ(x, T)}

#' @rdname failsafe
#' @export
isNAS <- function(x) {
  if (!assertthat::is.error(base::identity(x))) {if (uj::isATM(x)) {return(base::is.na(x))}}
  F
}

#' @rdname failsafe
#' @export
isOKS <- function(x) {
  if (!assertthat::is.error(base::identity(x))) {if (uj::isATM(x)) {return(!base::is.na(x))}}
  F
}


#' @rdname failsafe
#' @export
isSEQ <- function(x, y) {
  if (!assertthat::is.error(base::identity(x))) {
    if (!assertthat::is.error(base::identity(y))) {
      if (uj::isATM(x)) {
        if (uj::isATM(y)) {
          if (uj:::.compat(x, y)) {return(base::setequal(uj::av(x), uj::av(y)))}
        }
      }
    }
  }
  F
}

#' @rdname failsafe
#' @export
isVEQ <- function(x, y) {
  if (!assertthat::is.error(base::identity(x))) {
    if (!assertthat::is.error(base::identity(y))) {
      if (uj::isATM(x)) {
        if (uj::isATM(y)) {
          if (uj:::.compat(x, y)) {
            if (uj::N(x) == uj::N(y)) {
              if (!base::any(base::is.na(x))) {
                if (!base::any(base::is.na(y))) {
                  base::all(x == y)
                }
              }
            }
          }
        }
      }
    }
  }
  F
}

#' @rdname failsafe
#' @export
isSDIF <- function(x, y) {!uj::isSEQ(x, y)}

#' @rdname failsafe
#' @export
isVDIF <- function(x, y) {!uj::isVEQ(x, y)}

#' @rdname failsafe
#' @export
or1 <- function(x, y) {uj::isT1(x) | uj::isT1(y)}

#' @rdname failsafe
#' @export
and1 <- function(x, y) {uj::isT1(x) & uj::isT1(y)}

#' @rdname failsafe
#' @export
xor1 <- function(x, y) {base::xor(uj::isT1(x), uj::isT1(y))}

#' @rdname failsafe
#' @export
nor1 <- function(x, y) {!uj::isT1(x) & !uj::isT1(y)}

#' @rdname failsafe
#' @export
OR <- function(x, y) {
  if (!assertthat::is.error(base::identity(x))) {
    if (!assertthat::is.error(base::identity(y))) {
      if (uj::isLGL(x) & uj::isLGL(y)) {
        nx <- uj::N(x)
        ny <- uj::N(y)
        if (nx %in% base::c(1, ny)) {
          if (ny %in% base::c(1, nx)) {return(x | y)}
        }
      }
    }
  }
  F
}

#' @rdname failsafe
#' @export
AND <- function(x, y) {
  if (!assertthat::is.error(base::identity(x))) {
    if (!assertthat::is.error(base::identity(y))) {
      if (uj::isLGL(x) & uj::isLGL(y)) {
        nx <- uj::N(x)
        ny <- uj::N(y)
        if (nx %in% base::c(1, ny)) {
          if (ny %in% base::c(1, nx)) {return(x & y)}
        }
      }
    }
  }
  F
}

#' @rdname failsafe
#' @export
XOR <- function(x, y) {
  if (!assertthat::is.error(base::identity(x))) {
    if (!assertthat::is.error(base::identity(y))) {
      if (uj::isLGL(x) & uj::isLGL(y)) {
        nx <- uj::N(x)
        ny <- uj::N(y)
        if (nx %in% base::c(1, ny)) {
          if (ny %in% base::c(1, nx)) {return(base::xor(x, y))}
        }
      }
    }
  }
  F
}

#' @rdname failsafe
#' @export
NOR <- function(x, y) {
  if (!assertthat::is.error(base::identity(x))) {
    if (!assertthat::is.error(base::identity(y))) {
      if (uj::isLGL(x) & uj::isLGL(y)) {
        nx <- uj::N(x)
        ny <- uj::N(y)
        if (nx %in% base::c(1, ny)) {
          if (ny %in% base::c(1, nx)) {return(!x & !y)}
        }
      }
    }
  }
  F
}

#' @rdname failsafe
#' @export
notIN1 <- function(x, ...) {!uj::isIN1(x, ...)}

#' @rdname failsafe
#' @export
notMF1 <- function(x, ...) {!uj::isMF1(x, ...)}

#' @rdname failsafe
#' @export
notHAS1 <- function(x, y) {!uj::HAS1(x, y)}

#' @rdname failsafe
#' @export
notLACKS1 <- function(x, y) {!uj::LACKS1(x, y)}

#' @rdname failsafe
#' @export
notEQ1 <- function(x, y) {!uj::isEQ1(x, y)}

#' @rdname failsafe
#' @export
notGE1 <- function(x, y) {!uj::isGE1(x, y)}

#' @rdname failsafe
#' @export
notGT1 <- function(x, y) {!uj::isGT1(x, y)}

#' @rdname failsafe
#' @export
notLE1 <- function(x, y) {!uj::isLE1(x, y)}

#' @rdname failsafe
#' @export
notLT1 <- function(x, y) {!uj::isLT1(x, y)}

#' @rdname failsafe
#' @export
notDIF1 <- function(x, y) {!uj::isDIF1(x, y)}

#' @rdname failsafe
#' @export
notF1 <- function(x) {!uj::isF1(x)}

#' @rdname failsafe
#' @export
notT1 <- function(x) {!uj::isT1(x)}

#' @rdname failsafe
#' @export
notTF1 <- function(x) {!uj::isTF1(x)}

#' @rdname failsafe
#' @export
notBL1 <- function(x) {!uj::isBL1(x)}

#' @rdname failsafe
#' @export
notNA1 <- function(x) {!uj::isNA1(x)}

#' @rdname failsafe
#' @export
notOK1 <- function(x) {!uj::isOK1(x)}

#' @rdname failsafe
#' @export
notLG1 <- function(x) {!uj::isLG1(x)}

#' @rdname failsafe
#' @export
notIN <- function(x, ...) {!uj::isIN(x, ...)}

#' @rdname failsafe
#' @export
notMF <- function(x, ...) {!uj::isMF(x, ...)}

#' @rdname failsafe
#' @export
notHAS <- function(x, y) {!uj::HAS(x, y)}

#' @rdname failsafe
#' @export
notLACKS <- function(x, y) {!uj::LACKS(x, y)}

#' @rdname failsafe
#' @export
notEQ <- function(x, y) {!uj::isEQ(x, y)}

#' @rdname failsafe
#' @export
notGE <- function(x, y) {!uj::isGE(x, y)}

#' @rdname failsafe
#' @export
notGT <- function(x, y) {!uj::isGT(x, y)}

#' @rdname failsafe
#' @export
notLE <- function(x, y) {!uj::isLE(x, y)}

#' @rdname failsafe
#' @export
notLT <- function(x, y) {!uj::isLT(x, y)}

#' @rdname failsafe
#' @export
notDIF <- function(x, y) {!uj::isDIF(x, y)}

#' @rdname failsafe
#' @export
notF <- function(x) {!uj::isF(x)}

#' @rdname failsafe
#' @export
notT <- function(x) {!uj::isT(x)}

#' @rdname failsafe
#' @export
notTF <- function(x) {!uj::isTF(x)}

#' @rdname failsafe
#' @export
notBL <- function(x) {!uj::isBL(x)}

#' @rdname failsafe
#' @export
notNAS <- function(x) {!uj::isNAS(x)}

#' @rdname failsafe
#' @export
notOKS <- function(x) {!uj::isOKS(x)}

#' @rdname failsafe
#' @export
notSEQ <- function(x, y) {!uj::isSEQ(x, y)}

#' @rdname failsafe
#' @export
notVEQ <- function(x, y) {!uj::isVEQ(x, y)}

#' @rdname failsafe
#' @export
notSDIF <- function(x, y) {!uj::isSDIF(x, y)}

#' @rdname failsafe
#' @export
notVDIF <- function(x, y) {!uj::isVDIF(x, y)}

#' @rdname failsafe
#' @export
nIN <- function(x, ...) {uj::NW(uj::isIN(x, ...))}

#' @rdname failsafe
#' @export
nMF <- function(x, ...) {uj::NW(uj::isMF(x, ...))}

#' @rdname failsafe
#' @export
nHAS <- function(x, y) {uj::NW(uj::HAS(x, y))}

#' @rdname failsafe
#' @export
nLACKS <- function(x, y) {uj::NW(uj::LACKS(x, y))}

#' @rdname failsafe
#' @export
nEQ <- function(x, y) {uj::NW(uj::isEQ(x, y))}

#' @rdname failsafe
#' @export
nGE <- function(x, y) {uj::NW(uj::isGE(x, y))}

#' @rdname failsafe
#' @export
nGT <- function(x, y) {uj::NW(uj::isGT(x, y))}

#' @rdname failsafe
#' @export
nLE <- function(x, y) {uj::NW(uj::isLE(x, y))}

#' @rdname failsafe
#' @export
nLT <- function(x, y) {uj::NW(uj::isLT(x, y))}

#' @rdname failsafe
#' @export
nDIF <- function(x, y) {uj::NW(uj::isDIF(x, y))}

#' @rdname failsafe
#' @export
nF <- function(x) {uj::NW(uj::isF(x))}

#' @rdname failsafe
#' @export
nT <- function(x) {uj::NW(uj::isT(x))}

#' @rdname failsafe
#' @export
nTF <- function(x) {uj::NW(uj::isTF(x))}

#' @rdname failsafe
#' @export
nBL <- function(x) {uj::NW(uj::isBL(x))}

#' @rdname failsafe
#' @export
nNAS <- function(x) {uj::NW(uj::isNAS(x))}

#' @rdname failsafe
#' @export
nOKS <- function(x) {uj::NW(uj::isOKS(x))}

#' @rdname failsafe
#' @export
noneF <- function(x) {uj::nF(x) == 0}

#' @rdname failsafe
#' @export
noneT <- function(x) {uj::nT(x) == 0}

#' @rdname failsafe
#' @export
noneTF <- function(x) {uj::nTF(x) == 0}

#' @rdname failsafe
#' @export
noneBL <- function(x) {uj::nBL(x) == 0}

#' @rdname failsafe
#' @export
noneEQ <- function(x, y) {uj::nEQ(x, y) == 0}

#' @rdname failsafe
#' @export
noneGE <- function(x, y) {uj::nGE(x, y) == 0}

#' @rdname failsafe
#' @export
noneGT <- function(x, y) {uj::nGT(x, y) == 0}

#' @rdname failsafe
#' @export
noneLE <- function(x, y) {uj::nLE(x, y) == 0}

#' @rdname failsafe
#' @export
noneLT <- function(x, y) {uj::nLT(x, y) == 0}

#' @rdname failsafe
#' @export
noneIN <- function(x, ...) {uj::nIN(x, ...) == 0}

#' @rdname failsafe
#' @export
noneMF <- function(x, ...) {uj::nMF(x, ...) == 0}

#' @rdname failsafe
#' @export
noneDIF <- function(x, y) {uj::nDIF(x, y) == 0}

#' @rdname failsafe
#' @export
noneNAS <- function(x) {uj::nNAS(x) == 0}

#' @rdname failsafe
#' @export
noneOKS <- function(x) {uj::nOKS(x) == 0}

#' @rdname failsafe
#' @export
hasNONE <- function(x, y) {uj::nHAS(x, y) == 0}

#' @rdname failsafe
#' @export
lacksNONE <- function(x, y) {uj::nLACKS(x, y) == 0}

#' @rdname failsafe
#' @export
anyF <- function(x) {uj::nF(x) > 0}

#' @rdname failsafe
#' @export
anyT <- function(x) {uj::nT(x) > 0}

#' @rdname failsafe
#' @export
anyBL <- function(x) {uj::nBL(x) > 0}

#' @rdname failsafe
#' @export
anyEQ <- function(x, y) {uj::nEQ(x, y) > 0}

#' @rdname failsafe
#' @export
anyGE <- function(x, y) {uj::nGE(x, y) > 0}

#' @rdname failsafe
#' @export
anyGT <- function(x, y) {uj::nGT(x, y) > 0}

#' @rdname failsafe
#' @export
anyIN <- function(x, ...) {uj::nIN(x, ...) > 0}

#' @rdname failsafe
#' @export
anyLE <- function(x, y) {uj::nLE(x, y) > 0}

#' @rdname failsafe
#' @export
anyLT <- function(x, y) {uj::nLT(x, y) > 0}

#' @rdname failsafe
#' @export
anyMF <- function(x, ...) {uj::nMF(x, ...) > 0}

#' @rdname failsafe
#' @export
anyTF <- function(x) {uj::nTF(x) > 0}

#' @rdname failsafe
#' @export
anyDIF <- function(x, y) {uj::nDIF(x, y) > 0}

#' @rdname failsafe
#' @export
anyNAS <- function(x) {uj::nNAS(x) > 0}

#' @rdname failsafe
#' @export
anyOKS <- function(x) {uj::nOKS(x) > 0}

#' @rdname failsafe
#' @export
hasANY <- function(x, y) {uj::nHAS(x, y) > 0}

#' @rdname failsafe
#' @export
lacksANY <- function(x, y) {uj::nLACKS(x, y) > 0}

#' @rdname failsafe
#' @export
oneF <- function(x) {uj::nF(x) == 1}

#' @rdname failsafe
#' @export
oneT <- function(x) {uj::nT(x) == 1}

#' @rdname failsafe
#' @export
oneBL <- function(x) {uj::nBL(x) == 1}

#' @rdname failsafe
#' @export
oneEQ <- function(x, y) {uj::nEQ(x, y) == 1}

#' @rdname failsafe
#' @export
oneGE <- function(x, y) {uj::nGE(x, y) == 1}

#' @rdname failsafe
#' @export
oneGT <- function(x, y) {uj::nGT(x, y) == 1}

#' @rdname failsafe
#' @export
oneIN <- function(x, ...) {uj::nIN(x, ...) == 1}

#' @rdname failsafe
#' @export
oneLE <- function(x, y) {uj::nLE(x, y) == 1}

#' @rdname failsafe
#' @export
oneLT <- function(x, y) {uj::nLT(x, y) == 1}

#' @rdname failsafe
#' @export
oneMF <- function(x, ...) {uj::nMF(x, ...) == 1}

#' @rdname failsafe
#' @export
oneTF <- function(x) {uj::nTF(x) == 1}

#' @rdname failsafe
#' @export
oneDIF <- function(x, y) {uj::nDIF(x, y) == 1}

#' @rdname failsafe
#' @export
oneNAS <- function(x) {uj::nNAS(x) == 1}

#' @rdname failsafe
#' @export
oneOKS <- function(x) {uj::nOKS(x) == 1}

#' @rdname failsafe
#' @export
hasONE <- function(x, y) {uj::nHAS(x, y) == 1}

#' @rdname failsafe
#' @export
lacksONE <- function(x, y) {uj::nLACKS(x, y) == 1}

#' @rdname failsafe
#' @export
allF <- function(x) {base::all(uj::isF(x))}

#' @rdname failsafe
#' @export
allT <- function(x) {base::all(uj::isT(x))}

#' @rdname failsafe
#' @export
allBL <- function(x) {base::all(uj::isBL(x))}

#' @rdname failsafe
#' @export
allEQ <- function(x, y) {base::all(uj::isEQ(x, y))}

#' @rdname failsafe
#' @export
allGE <- function(x, y) {base::all(uj::isGE(x, y))}

#' @rdname failsafe
#' @export
allGT <- function(x, y) {base::all(uj::isGT(x, y))}

#' @rdname failsafe
#' @export
allIN <- function(x, ...) {base::all(uj::isIN(x, ...))}

#' @rdname failsafe
#' @export
allLE <- function(x, y) {base::all(uj::isLE(x, y))}

#' @rdname failsafe
#' @export
allLT <- function(x, y) {base::all(uj::isLT(x, y))}

#' @rdname failsafe
#' @export
allMF <- function(x, ...) {base::all(uj::isMF(x, ...))}

#' @rdname failsafe
#' @export
allTF <- function(x) {base::all(uj::isTF(x))}

#' @rdname failsafe
#' @export
allDIF <- function(x, y) {base::all(uj::isDIF(x, y))}

#' @rdname failsafe
#' @export
allNAS <- function(x) {base::all(uj::isNA(x))}

#' @rdname failsafe
#' @export
allOKS <- function(x) {base::all(uj::isOK(x))}

#' @rdname failsafe
#' @export
hasALL <- function(x, y) {base::all(uj::HAS(x, y))}

#' @rdname failsafe
#' @export
lacksALL <- function(x, y) {base::all(uj::LACKS(x, y))}

#' @rdname failsafe
#' @export
someF <- function(x) {uj::nF(x) > 1}

#' @rdname failsafe
#' @export
someT <- function(x) {uj::nT(x) > 1}

#' @rdname failsafe
#' @export
someBL <- function(x) {uj::nBL(x) > 1}

#' @rdname failsafe
#' @export
someEQ <- function(x, y) {uj::nEQ(x, y) > 1}

#' @rdname failsafe
#' @export
someLE <- function(x, y) {uj::nLE(x, y) > 1}

#' @rdname failsafe
#' @export
someGE <- function(x, y) {uj::nGE(x, y) > 1}

#' @rdname failsafe
#' @export
someGT <- function(x, y) {uj::nGT(x, y) > 1}

#' @rdname failsafe
#' @export
someIN <- function(x, ...) {uj::nIN(x, ...) > 1}

#' @rdname failsafe
#' @export
someLT <- function(x, y) {uj::nLT(x, y) > 1}

#' @rdname failsafe
#' @export
someMF <- function(x, ...) {uj::nMF(x, ...) > 1}

#' @rdname failsafe
#' @export
someTF <- function(x) {uj::nTF(x) > 1}

#' @rdname failsafe
#' @export
someDIF <- function(x, y) {uj::nDIF(x, y) > 1}

#' @rdname failsafe
#' @export
someNAS <- function(x) {uj::nNAS(x) > 1}

#' @rdname failsafe
#' @export
someOKS <- function(x) {uj::nOKS(x) > 1}

#' @rdname failsafe
#' @export
hasSOME <- function(x, y) {uj::nHAS(x, y) > 1}

#' @rdname failsafe
#' @export
lacksSOME <- function(x, y) {uj::nLACKS(x, y) > 1}

#' @rdname failsafe
#' @export
twoF <- function(x) {uj::nF(x) == 2}

#' @rdname failsafe
#' @export
twoT <- function(x) {uj::nT(x) == 2}

#' @rdname failsafe
#' @export
twoBL <- function(x) {uj::nBL(x) == 2}

#' @rdname failsafe
#' @export
twoEQ <- function(x, y) {uj::nEQ(x, y) == 2}

#' @rdname failsafe
#' @export
twoGE <- function(x, y) {uj::nGE(x, y) == 2}

#' @rdname failsafe
#' @export
twoGT <- function(x, y) {uj::nGT(x, y) == 2}

#' @rdname failsafe
#' @export
twoIN <- function(x, ...) {uj::nIN(x, ...) == 2}

#' @rdname failsafe
#' @export
twoLE <- function(x, y) {uj::nLE(x, y) == 2}

#' @rdname failsafe
#' @export
twoLT <- function(x, y) {uj::nLT(x, y) == 2}

#' @rdname failsafe
#' @export
twoMF <- function(x, ...) {uj::nMF(x, ...) == 2}

#' @rdname failsafe
#' @export
twoTF <- function(x) {uj::nTF(x) == 2}

#' @rdname failsafe
#' @export
twoDIF <- function(x, y) {uj::nDIF(x, y) == 2}

#' @rdname failsafe
#' @export
twoNAS <- function(x) {uj::nNAS(x) == 2}

#' @rdname failsafe
#' @export
twoOKS <- function(x) {uj::nOKS(x) == 2}

#' @rdname failsafe
#' @export
hasTWO <- function(x, y) {uj::nHAS(x, y) == 2}

#' @rdname failsafe
#' @export
lacksTWO <- function(x, y) {uj::nLACKS(x, y) == 2}

#' @rdname failsafe
#' @export
manyF <- function(x) {uj::nF(x) > 2}

#' @rdname failsafe
#' @export
manyT <- function(x) {uj::nT(x) > 2}

#' @rdname failsafe
#' @export
manyBL <- function(x) {uj::nBL(x) > 2}

#' @rdname failsafe
#' @export
manyEQ <- function(x, y) {uj::nEQ(x, y) > 2}

#' @rdname failsafe
#' @export
manyGE <- function(x, y) {uj::nGE(x, y) > 2}

#' @rdname failsafe
#' @export
manyGT <- function(x, y) {uj::nGT(x, y) > 2}

#' @rdname failsafe
#' @export
manyIN <- function(x, ...) {uj::nIN(x, ...) > 2}

#' @rdname failsafe
#' @export
manyLE <- function(x, y) {uj::nLE(x, y) > 2}

#' @rdname failsafe
#' @export
manyLT <- function(x, y) {uj::nLT(x, y) > 2}

#' @rdname failsafe
#' @export
manyMF <- function(x, ...) {uj::nMF(x, ...) > 2}

#' @rdname failsafe
#' @export
manyTF <- function(x) {uj::nTF(x) > 2}

#' @rdname failsafe
#' @export
manyDIF <- function(x, y) {uj::nDIF(x, y) > 2}

#' @rdname failsafe
#' @export
manyNAS <- function(x) {uj::nNAS(x) > 2}

#' @rdname failsafe
#' @export
manyOKS <- function(x) {uj::nOKS(x) > 2}

#' @rdname failsafe
#' @export
hasMANY <- function(x, y) {uj::nHAS(x, y) > 2}

#' @rdname failsafe
#' @export
lacksMANY <- function(x, y) {uj::nLACKS(x, y) > 2}

#' @rdname failsafe
#' @export
`%.in1.%` <- function(x, y) {uj::isIN1(x, y)}

#' @rdname failsafe
#' @export
`%.mf1.%` <- function(x, y) {uj::isMF1(x, y)}

#' @rdname failsafe
#' @export
`%.eq1.%` <- function(x, y) {uj::isEQ1(x, y)}

#' @rdname failsafe
#' @export
`%.ge1.%` <- function(x, y) {uj::isGE1(x, y)}

#' @rdname failsafe
#' @export
`%.gt1.%` <- function(x, y) {uj::isGT1(x, y)}

#' @rdname failsafe
#' @export
`%.le1.%` <- function(x, y) {uj::isLE1(x, y)}

#' @rdname failsafe
#' @export
`%.lt1.%` <- function(x, y) {uj::isLT1(x, y)}

#' @rdname failsafe
#' @export
`%.has1.%` <- function(x, y) {uj::HAS1(x, y)}

#' @rdname failsafe
#' @export
`%.dif1.%` <- function(x, y) {uj::isDIF1(x, y)}

#' @rdname failsafe
#' @export
`%.lacks1.%` <- function(x, y) {uj::LACKS1(x, y)}

#' @rdname failsafe
#' @export
`%.in.%` <- function(x, y) {uj::isIN(x, y)}

#' @rdname failsafe
#' @export
`%.mf.%` <- function(x, y) {uj::isMF(x, y)}

#' @rdname failsafe
#' @export
`%.eq.%` <- function(x, y) {uj::isEQ(x, y)}

#' @rdname failsafe
#' @export
`%.ge.%` <- function(x, y) {uj::isGE(x, y)}

#' @rdname failsafe
#' @export
`%.gt.%` <- function(x, y) {uj::isGT(x, y)}

#' @rdname failsafe
#' @export
`%.le.%` <- function(x, y) {uj::isLE(x, y)}

#' @rdname failsafe
#' @export
`%.lt.%` <- function(x, y) {uj::isLT(x, y)}

#' @rdname failsafe
#' @export
`%.has.%` <- function(x, y) {uj::HAS(x, y)}

#' @rdname failsafe
#' @export
`%.dif.%` <- function(x, y) {uj::isDIF(x, y)}

#' @rdname failsafe
#' @export
`%.lacks.%` <- function(x, y) {uj::LACKS(x, y)}

#' @rdname failsafe
#' @export
`%.seq.%` <- function(x, y) {uj::isSEQ(x, y)}

#' @rdname failsafe
#' @export
`%.veq.%` <- function(x, y) {uj::isVEQ(x, y)}

#' @rdname failsafe
#' @export
`%.sdif.%` <- function(x, y) {uj::isSDIF(x, y)}

#' @rdname failsafe
#' @export
`%.vdif.%` <- function(x, y) {uj::isVDIF(x, y)}

#' @rdname failsafe
#' @export
`%.or1.%` <- function(x, y) {uj::or1(x, y)}

#' @rdname failsafe
#' @export
`%.and1.%` <- function(x, y) {uj::and1(x, y)}

#' @rdname failsafe
#' @export
`%.xor1.%` <- function(x, y) {uj::xor1(x, y)}

#' @rdname failsafe
#' @export
`%.nor1.%` <- function(x, y) {uj::nor1(x, y)}

#' @rdname failsafe
#' @export
`%.or.%` <- function(x, y) {uj::OR(x, y)}

#' @rdname failsafe
#' @export
`%.and.%` <- function(x, y) {uj::AND(x, y)}

#' @rdname failsafe
#' @export
`%.xor.%` <- function(x, y) {uj::XOR(x, y)}

#' @rdname failsafe
#' @export
`%.nor.%` <- function(x, y) {uj::NOR(x, y)}
