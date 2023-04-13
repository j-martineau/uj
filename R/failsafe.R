#' @encoding UTF-8
#' @family extensions
#' @family logicals
#' @family errs
#' @title failsafe functions that **always** return a valid object.
#' @description Evaluate objects, test if an object has certain properties, and conduct binary logical operations safely. these functions never stop execution; they always produce a valid result, even if that result is an error object.
#' @details
#' **Error management functions**
#' \cr\cr These functions evaluate generate, check for, and manage error objects.
#' \tabular{ll}{  `make_err`   \tab generate object of class `'simpleError'`.     \cr
#'                `msg_err`    \tab get error message, if any; otherwise, `NULL`. \cr
#'                `not_err`    \tab Does evaluating `X` not produce an error?.    \cr
#'                `is_err`     \tab Does evaluating `X` produce an error?           }
#' \cr **failsafe check for classes of objects with values**
#' \tabular{ll}{  `failsafe`   \tab Returns `X` if identity evaluation does not_ cause an error. Returns
#'                                  an error object otherwise with the attribute `stack = uj::callers()`. \cr   \tab   \cr
#'                `fs_null`    \tab `X` is `NULL`.                                                        \cr
#'                `fs_def`     \tab `X` is defined (not `NULL`).                                          \cr   \tab   \cr
#'                `fs_scl`     \tab `X` is scalar of a specific atomic value.                             \cr
#'                `fs_t`       \tab `X` is scalar `TRUE`.                                                 \cr
#'                `fs_f`       \tab `X` is scalar `FALSE`.                                                \cr
#'                `fs_na`      \tab `X` is scalar `NA`.                                                   \cr
#'                `fs_tf`      \tab `X` is scalar `TRUE` or scalar `FALSE`.                               \cr
#'                `scl_f`      \tab `X` is scalar `FALSE`                                                 \cr
#'                `scl_t`      \tab `X` is scalar `TRUE`                                                  \cr
#'                `scl_tf`     \tab `X` is scalar `TRUE` or `FALSE`.                                      \cr
#'                `scl_lg`     \tab `X` is scalar `TRUE`, `FALSE`, or `NA`.                               \cr
#'                `scl_bl`     \tab `X` is blank string scalar (`""`).                                    \cr
#'                `scl_na`     \tab `X` is scalar `NA`                                                    \cr   \tab   \cr
#'                `fs_vec`     \tab Elements of `X` are of a specific atomic value.                       \cr
#'                `vec_f`      \tab Elements of `X` are `FALSE`.                                          \cr
#'                `vec_t`      \tab Elements of `X` are `TRUE`.                                           \cr
#'                `vec_tf`     \tab Elements of `X` are `TRUE` or `FALSE`.                                \cr
#'                `vec_lg`     \tab Elements of `X` are `TRUE`, `FALSE`, or `NA`.                         \cr
#'                `vec_bl`     \tab Elements of `X` are blank strings.                                    \cr
#'                `vec_na`     \tab Elements of `X` are `NA`.                                               }
#' **Forced-evaluation functions with conditional return values**
#' \cr\cr These functions \link[base:force]{force} evaluation of `X` returning values as shown in the following table with the first value returned when forcing evaluation does not produce an error and the second when it does:
#' \tabular{ll}{  `fs_or`    \tab `X` or something else if evaluating `X` produces an error \cr   \tab   \cr
#'                `or_f`     \tab `X` or `FALSE`                                            \cr
#'                `or_t`     \tab `X` or `TRUE`                                             \cr   \tab   \cr
#'                `or_c0`    \tab `X` or `character(0)`                                     \cr
#'                `or_i0`    \tab `X` or `integer(0)`                                       \cr
#'                `or_l0`    \tab `X` or `logical(0)`                                       \cr
#'                `or_n0`    \tab `X` or `numeric(0)`                                       \cr   \tab   \cr
#'                `or_bl`    \tab `X` or `""`                                               \cr   \tab   \cr
#'                `or_na`    \tab `X` or `NA`                                               \cr
#'                `or_nac`   \tab `X` or `NA_character_`                                    \cr
#'                `or_nai`   \tab `X` or `NA_integer_`                                      \cr
#'                `or_nal`   \tab `X` or `NA` (logical)                                     \cr
#'                `or_nar`   \tab `X` or `NA_real_`                                           }
#' **Binary comparison functions**
#' \cr\cr These are binary functions that always produce either scalar `TRUE` or scalar `FALSE`. They return `TRUE` when two variables meet the conditions shown in the following table and `FALSE` in every other circumstance (where `fx` and `fy`)
#' \tabular{ll}{  `%.is.%`      \tab `failsafe(identical(X, Y))`                                  \cr
#'                `%.isnt.%`    \tab `!(X %.is.% Y)`                                              \cr   \tab   \cr
#'                `%.in.%`      \tab `sapply(failsafe(X %in% Y), isTRUE)`                         \cr
#'                `%.mf.%`      \tab `!(X %.in.% Y)` (missing from)                               \cr
#'                `%.has.%`     \tab `Y %.in.% X` (`X` has each `Y` value)                        \cr
#'                `%.lacks.%`   \tab `Y %.mf.% X` (`X` lacks each `Y` value)                      \cr   \tab   \cr
#'                `%.eq.%`      \tab If `X` and `Y` match\eqn{^{(1)}}, `X = Y`, otherwise `FALSE` \cr
#'                `%.ge.%`      \tab If `X` and `Y` match\eqn{^{(1)}}, `X ≥ Y`, otherwise `FALSE` \cr
#'                `%.gt.%`      \tab If `X` and `Y` match\eqn{^{(1)}}, `X > Y`, otherwise `FALSE` \cr
#'                `%.le.%`      \tab If `X` and `Y` match\eqn{^{(1)}}, `X ≤ Y`, otherwise `FALSE` \cr
#'                `%.lt.%`      \tab If `X` and `Y` match\eqn{^{(1)}}, `X < Y`, otherwise `FALSE` \cr
#'                `%.dif.%`     \tab If `X` and `Y` match\eqn{^{(1)}}, `X ≠ Y`, otherwise `TRUE`  \cr   \tab   \cr
#'                `%.seq.%`     \tab `X` and `Y` are \code{\link[base]{setequal}}\eqn{^{(4)}}     \cr
#'                `%.veq.%`     \tab `X` and `Y` are vector equal\eqn{^{(5)}}                     \cr
#'                `%.sdif.%`    \tab `X` and `Y` are set different\eqn{^{(6)}}                    \cr
#'                `%.vdif.%`    \tab `X` and `Y` are vector different\eqn{^{(7)}}                 \cr   \tab   \cr
#'                `%.or.%`      \tab values of `X` *and/or* `Y` are `TRUE`                        \cr
#'                `%.and.%`     \tab values of `X` *and* `Y` are `TRUE`                           \cr
#'                `%.xor.%`     \tab values of *either* `X` *or* `Y` are `TRUE`                   \cr
#'                `%.nor.%`     \tab values of *neither* `X` *nor* `Y` are `TRUE`                 \cr   \tab   \cr
#'                `%.IN.%`      \tab if `X` is atomic scalar, `X %.in.% Y`, otherwise `FALSE`     \cr
#'                `%.MF.%`      \tab `X` is atomic scalar and `X %.mf.% Y`                        \cr
#'                `%.HAS.%`     \tab `Y` is atomic scalar and `X %.has.% Y`                       \cr
#'                `%.LACKS.%`   \tab `Y` is atomic scalar and `X %.lacks.% Y`                     \cr   \tab   \cr
#'                `%.EQ.%`      \tab `X` and `Y` are atomic scalar and `X = Y`  \eqn{^{(2)}}      \cr
#'                `%.GE.%`      \tab `X` and `Y` are atomic scalar and `X ≥ Y`  \eqn{^{(2)}}      \cr
#'                `%.GT.%`      \tab `X` and `Y` are atomic scalar and `X > Y`  \eqn{^{(2)}}      \cr
#'                `%.LE.%`      \tab `X` and `Y` are atomic scalar and `X ≤ Y`  \eqn{^{(2)}}      \cr
#'                `%.LT.%`      \tab `X` and `Y` are atomic scalar and `X ≥ Y`  \eqn{^{(2)}}      \cr
#'                `%.DIF.%`     \tab `X` and `Y` are *not* atomic scalar equal  \eqn{^{(3)}}      \cr   \tab   \cr
#'                `%.OR.%`      \tab `X` *and/or* `Y` are scalar `TRUE`                           \cr
#'                `%.AND.%`     \tab `X` *and* `Y` are scalar `TRUE`                              \cr
#'                `%.XOR.%`     \tab *either* `X` *or* `Y` is scalar `TRUE`                       \cr
#'                `%.NOR.%`     \tab *neither* `X` *nor* `Y` is scalar `TRUE`                     \cr   \tab     }
#'  \tabular{l}{  \eqn{^{(1)}} Of equal length and \link[=compatible]{compatible modes}.                                                       \cr
#'                \eqn{^{(2)}} Of \link[=compatible]{compatible modes} and meeting the (in)equality.                                           \cr
#'                \eqn{^{(3)}} not meeting the requirements of `%.eq0.%`.                                                                      \cr
#'                \eqn{^{(4)}} atomic, of the same length, with the same values in the same order.                                             \cr
#'                \eqn{^{(5)}} atomic, possibly of different lengths, and containing the same unique values regardless of order or duplicates. \cr
#'                \eqn{^{(6)}} not meeting the requirements of `%.seq.%`.                                                                      \cr
#'                \eqn{^{(7)}} not meeting the requirements of `%.veq.%`.                                                                        }
#' **Scalar value, membership, and equality/inequality checking functions**
#' \cr\cr These functions check for class, mode, and/or value and/or count the number of checks passed. they *always* produce `TRUE`, `FALSE`, or an integer scalar.
#' \cr\cr Function names are constructed of root words, prefixes, and/or suffixes. Root word specify the type of check conducted. Prefixes and suffixes specify how to modify the results of a check or how to apply the check to each `...` argument and check whether a certain number of `...` args passed the checks.
#' \cr\cr Uppercase root words indicate one or both arguments must be scalar. Lowercase root words indicate arguments may be vectors.
#' \cr\cr *Root words for atomic scalar value checking functions*
#' \tabular{ll}{  `nav`   \tab `X` is atomic and values are `NA`.                               \cr
#'                `okv`   \tab `X` is atomic and values are *not* `NA`.                         \cr
#'                `bl`    \tab `X` is atomic and values are blanks (`""`).                      \cr
#'                `lg`    \tab `X` is atomic and values are logical (`TRUE`, `FALSE`, or `NA`). \cr
#'                `tf`    \tab `X` is atomic and values are `TRUE` or `FALSE`.                  \cr
#'                `f`     \tab `X` is atomic and values are `TRUE`.                             \cr
#'                `t`     \tab `X` is atomic and valules are `FALSE`.                             }
#' \cr *Root words for atomic value membership checking functions*
#' \tabular{ll}{  `lacks`   \tab `X` lacks values of `Y`.      \cr
#'                `has`     \tab `X` has values of `Y`.        \cr
#'                `in`      \tab `X` in values of `Y`.         \cr
#'                `mf`      \tab `X` missing from values of `Y`. }
#' \cr *Root words for equality/inequality checking functions*
#' \tabular{ll}{  `id`     \tab objects are identical                \cr
#'                `ge`     \tab `X` is greater than or equal to `Y`. \cr
#'                `gt`     \tab `X` is greater than `Y`.             \cr
#'                `le`     \tab `X` is less than or equal to `Y`.    \cr
#'                `lt`     \tab `X` is less than `Y`.                \cr
#'                `eq`     \tab values are equal                     \cr
#'                `dif`    \tab values are different                 \cr
#'                `seq`    \tab values are set equal                 \cr
#'                `sdif`   \tab values are set different             \cr
#'                `veq`    \tab values are vector equal              \cr
#'                `vdif`   \tab values are vector different            }
#' \cr *Modifier prefixes/suffixes*
#' \tabular{ll}{  `n`     \tab count `...` args passing the check. \cr
#'                `is`    \tab Keep the result of a check as is.   \cr
#'                `not`   \tab Negate the result of a check.         }
#' \cr *Apply-and-sweep prefixes/suffixes evaluating whether a certain number of checks were passed*
#' \cr\cr the following table contains prefixes in the first column, and in the second, the number of checks that must be passed to return `TRUE`.
#' \tabular{ll}{  `none`   \tab `0` values passed the check.   \cr
#'                `any`    \tab `> 0` values passed the check. \cr
#'                `one`    \tab `1` value passed the check.    \cr
#'                `some`   \tab `> 1` values passed the check. \cr
#'                `two`    \tab `2` values passed the check.   \cr
#'                `many`   \tab `> 2` values passed the check. \cr
#'                `all`    \tab all values passed the check.    }
#' \cr **Identity-equality, set-equality and vector-equality checking functions**
#' \tabular{lll}{             \tab **is**      \tab **not**    \cr
#'                 **id**     \tab `is_id`     \tab `not_id`   \cr
#'                 **seq**    \tab `is_seq`    \tab `not_deq`  \cr
#'                 **veq**    \tab `is_veq`    \tab `not_veq`  \cr
#'                 **sdif**   \tab `is_sdif`   \tab `not_sdif` \cr
#'                 **vdif**   \tab `is_vdif`   \tab `not_vdif`   }
#' \cr **atomic scalar value checking functions**
#' \tabular{lll}{            \tab **is**     \tab **not**   \cr
#'                 **t**     \tab `is_T`     \tab `not_T`   \cr
#'                 **f**     \tab `is_F`     \tab `not_F`   \cr
#'                 **tf**    \tab `is_TF`    \tab `not_TF`  \cr
#'                 **lg**    \tab `is_LG`    \tab `not_LG`  \cr
#'                 **bl**    \tab `is_BL`    \tab `not_BL`  \cr
#'                 **eq**    \tab `is_EQ`    \tab `not_EQ`  \cr
#'                 **ge**    \tab `is_GE`    \tab `not_GE`  \cr
#'                 **gt**    \tab `is_GT`    \tab `not_GT`  \cr
#'                 **le**    \tab `is_LE`    \tab `not_LE`  \cr
#'                 **lt**    \tab `is_LT`    \tab `not_LT`  \cr
#'                 **nav**   \tab `is_NAV`   \tab `not_NAV` \cr
#'                 **okv**   \tab `is_OKV`   \tab `not_OKV` \cr
#'                 **dif**   \tab `is_DIF`   \tab `not_DIF`   }
#' \cr **atomic scalar value membership checking functions**
#' \tabular{lll}{              \tab **is**     \tab **not**   \cr
#'                 **in**      \tab `is_IN`    \tab `not_IN`  \cr
#'                 **mf**      \tab `is_MF`    \tab `not_MF`  \cr
#'                 **has**     \tab `HAS`      \tab `not_HAS` \cr
#'                 **lacks**   \tab `LACKS`    \tab `not_LACKS` }
#' \cr **atomic value checking functions**
#' \tabular{lllllllllll}{              \tab **is**     \tab **n**     \tab **not**        \tab **none**       \tab **one**       \tab **two**       \tab **any**       \tab  **some**      \tab  **many**      \tab  **all_** \cr
#'                         **t**       \tab `is_t`     \tab `n_t`     \tab `not_t`        \tab `none_t`       \tab `one_t`       \tab `two_t`       \tab `any_t`       \tab `some_t`       \tab `many_t`       \tab `all_t`   \cr
#'                         **f**       \tab `is_f`     \tab `n_f`     \tab `not_f`        \tab `none_f`       \tab `one_f`       \tab `two_f`       \tab `any_f`       \tab `some_f`       \tab `many_f`       \tab `all_f`   \cr
#'                         **tf**      \tab `is_tf`    \tab `n_tf`    \tab `not_tf`       \tab `none_tf`      \tab `one_tf`      \tab `two_tf`      \tab `any_tf`      \tab `some_tf`      \tab `many_tf`      \tab `all_tf`  \cr
#'                         **bl**      \tab `is_bl`    \tab `n_bl`    \tab `not_bl`       \tab `none_bl`      \tab `one_bl`      \tab `two_bl`      \tab `any_bl`      \tab `some_bl`      \tab `many_bl`      \tab `all_bl`  \cr
#'                         **eq**      \tab `is_eq`    \tab `n_eq`    \tab `not_eq`       \tab `none_eq`      \tab `one_eq`      \tab `two_eq`      \tab `any_eq`      \tab `some_eq`      \tab `many_eq`      \tab `all_eq`  \cr
#'                         **ge**      \tab `is_ge`    \tab `n_ge`    \tab `not_ge`       \tab `none_ge`      \tab `one_ge`      \tab `two_ge`      \tab `any_ge`      \tab `some_ge`      \tab `many_ge`      \tab `all_ge`  \cr
#'                         **gt**      \tab `is_gt`    \tab `n_gt`    \tab `not_gt`       \tab `none_gt`      \tab `one_gt`      \tab `two_gt`      \tab `any_gt`      \tab `some_gt`      \tab `many_gt`      \tab `all_gt`  \cr
#'                         **le**      \tab `is_le`    \tab `n_le`    \tab `not_le`       \tab `none_le`      \tab `one_le`      \tab `two_le`      \tab `any_le`      \tab `some_le`      \tab `many_le`      \tab `all_le`  \cr
#'                         **lt**      \tab `is_lt`    \tab `n_lt`    \tab `not_lt`       \tab `none_lt`      \tab `one_lt`      \tab `two_lt`      \tab `any_lt`      \tab `some_lt`      \tab `many_lt`      \tab `all_lt`  \cr
#'                         **dif**     \tab `is_dif`   \tab `n_dif`   \tab `not_dif`      \tab `none_dif`     \tab `one_dif`     \tab `two_dif`     \tab `any_dif`     \tab `some_dif`     \tab `many_dif`     \tab `all_dif` \cr
#'                         **nav**     \tab `is_nav`   \tab `n_nav`   \tab `not_nav`      \tab `none_nav`     \tab `one_nav`     \tab `two_nav`     \tab `any_nav`     \tab `some_nav`     \tab `many_nav`     \tab `all_nav` \cr
#'                         **okv**     \tab `is_okv  ` \tab `n_okv`   \tab `not_okv`      \tab `none_okv`     \tab `one_okv`     \tab `two_okv`     \tab `any_okv`     \tab `some_okv`     \tab `many_okv`     \tab `all_okv`   }
#' \cr **atomic value membership checking functions**
#' \tabular{lllllllllll}{              \tab **is**    \tab **n**       \tab **not**       \tab **none**       \tab **one**       \tab **two**       \tab **any**       \tab **some**       \tab **many**       \tab **all**   \cr
#'                         **in**      \tab `is_in`   \tab `n_in`      \tab `not_in`      \tab `in_none`      \tab `in_one`      \tab `in_two`      \tab `in_any`      \tab `in_some`      \tab `in_many`      \tab `in_all`  \cr
#'                         **mf**      \tab `is_mf`   \tab `n_mf`      \tab `not_mf`      \tab `mf_none`      \tab `mf_one`      \tab `mf_two`      \tab `mf_any`      \tab `mf_some`      \tab `mf_many`      \tab `mf_all`  \cr
#'                         **has**     \tab `has`     \tab `n_has`     \tab `not_has`     \tab `has_none`     \tab `has_one`     \tab `has_two`     \tab `has_any`     \tab `has_some`     \tab `has_many`     \tab `has_all` \cr
#'                         **lacks**   \tab `locks`   \tab `n_lacks`   \tab `not_lacks`   \tab `lacks_none`   \tab `lacks_one`   \tab `lacks_two`   \tab `lacks_any`   \tab `lacks_some`   \tab `lacks_many`   \tab `lacks_all` }
#' @return A logical scalar or vector.
#' @param ... Any objects/expressions to be evaluated, whether or not doing so produces an error.
#' @param Val,Or,X,Y Any object/expression to be evaluated, whether or not doing so produces an error.
#' @param Def A character scalar default error message if forcing evaluation produces an error. If not a character scalar, it is replaced with the default.
#' @examples
#' egfailsafe <- function() {
#'   abc <- c("a", "b", "c")
#'   abc <- abc
#'   blank <- ""
#'   mss.scl <- Na
#'   lgl.scl <- FALSE
#'   fac.scl <- factor("q", levels = c("X", "q"))
#'   fac.vec <- factor(c("X", "q"), levels = c("X", "q"))
#'   chr.mat <- matrix(c("a", "b", "c", "Na"), nrow = 2)
#'   chr.dtf <- data.frame(abc = letters[1:3], def = letters[4:6])
#'   num.vls <- list(first3 = c(1:3, NA), next3 = c(4:6, NA))
#'   combo <- list(mss.scl, lgl.scl, fac.vec, chr.mat, chr.dtf, numvls)
#'   attr(abc, "custom") <- "custom"
#'   list(`"a" %.AND.% FALSE`                    = "a" %.AND.% FALSE                   ,
#'        `"a" %.IN.% abc`                       = "a" %.IN.% abc                      ,
#'        `"a" %.NOR.% FALSE`                    = "a" %.OR.% FALSE                    ,
#'        `"a" %.XOR.% FALSE`                    = "a" %.XOR.% FALSE                   ,
#'        `"a" %.OR.% FALSE`                     = "a" %.OR.% FALSE                    ,
#'        `"a" %.MF.% abc`                       = "a" %.MF.% abc                      ,
#'        `"a" %.and.% FALSE`                    = "a" %.and.% FALSE                   ,
#'        `"a" %.in.% abc`                       = "a" %.in.% abc                      ,
#'        `"a" %.nor.% FALSE`                    = "a" %.or.% FALSE                    ,
#'        `"a" %.xor.% FALSE`                    = "a" %.xor.% FALSE                   ,
#'        `"a" %.or.% FALSE`                     = "a" %.or.% FALSE                    ,
#'        `"a" %.mf.% abc`                       = "a" %.mf.% abc                      ,
#'        `1 %.in.% abc`                         = 1 %.in.% abc                        ,
#'        `1 %.mf.% abc`                         = 1 %.mf.% abc                        ,
#'        `abc %.EQ.% abc`                       = abc %.EQ.% abc                      ,
#'        `abc %.EQ.% letters[1:3]`              = abc %.EQ.% letters[1:3]             ,
#'        `abc %.EQ.% NULL`                      = abc %.EQ.% NULL                     ,
#'        `abc %.HAS.% "a"`                      = abc %.HAS.% "a"                     ,
#'        `abc %.HAS.% 1`                        = abc %.HAS.% 1                       ,
#'        `abc %.HAS.% NULL`                     = abc %.HAS.% NULL                    ,
#'        `abc %.has.% letters[1:6]`             = abc %.has.% "a"                     ,
#'        `abc %.has.% 1:3`                      = abc %.has.% 1                       ,
#'        `abc %.has.% NULL`                     = abc %.has.% NULL                    ,
#'        `abc %.is.% NULL`                      = abc %.is.% NULL                     ,
#'        `abc %.LACKS.% "a"`                    = abc %.LACKS.% "a"                   ,
#'        `abc %.LACKS.% 1`                      = abc %.LACKS.% 1                     ,
#'        `abc %.LACKS.% NULL`                   = abc %.LACKS.% NULL                  ,
#'        `abc %.is.% abc`                       = abc %.is.% abc                      ,
#'        `abc %.is.% letters[1:3]`              = abc %.is.% letters[1:3]             ,
#'        `abc %.isnt.% abc`                     = abc %.isnt.% abc                    ,
#'        `abc %.isnt.% letters[4:6]`            = abc %.isnt.% letters[4:6]           ,
#'        `all_bl(c(blank, letters))`            = all_bl(c(blank, letters))           ,
#'        `all_dif("a", c(blank, NA, letters))`  = all_dif("a", c(blank, NA, letters)) ,
#'        `all_eq(NA, c(blank, NA, letters))`    = all_eq(NA, c(blank, NA, letters))   ,
#'        `all_f(c(list(blank), combo))`         = all_f(c(list(blank), combo))        ,
#'        `all_ge("a", c(blank, NA, letters))`   = all_ge("a", c(blank, NA, letters))  ,
#'        `all_gt("a", c(blank, NA, letters))`   = all_gt("a", c(blank, NA, letters))  ,
#'        `all_in(NULL, blank, combo)`           = all_in(NULL, blank, combo)          ,
#'        `all_le("a", c(blank, NA, letters))`   = all_le("a", c(blank, NA, letters))  ,
#'        `all_lg(c(NA, TRUE, FALSE))`           = all_lg(c(NA, TRUE, FALSE))          ,
#'        `all_lt("a", c(blank, NA, letters))`   = all_lt("a", c(blank, NA, letters))  ,
#'        `all_mf(NULL, blank, combo)`           = all_mf(NULL, blank, combo)          ,
#'        `all_nav(c(blank, NA, letters))`       = all_nav(c(blank, NA, letters))      ,
#'        `all_nav(chr.mat)`                     = all_nav(chr.mat)                    ,
#'        `all_okv(c(blank, NA, letters))`       = all_okv(c(blank, NA, letters))      ,
#'        `all_okv(chr.mat)`                     = all_okv(chr.mat)                    ,
#'        `all_t(c(list(blank), combo))`         = all_t(c(list(blank), combo))        ,
#'        `all_tf(c(list(blank), combo))`        = all_tf(c(list(blank), combo))       ,
#'        `any_bl(c(blank, letters))`            = any_bl(c(blank, letters))           ,
#'        `any_dif("a", c(blank, NA, letters))`  = any_dif("a", c(blank, NA, letters)) ,
#'        `any_eq(NA, c(blank, NA, letters))`    = any_eq(NA, c(blank, NA, letters))   ,
#'        `any_f(c(list(blank), combo))`         = any_f(c(list(blank), combo))        ,
#'        `any_ge("a", c(blank, NA, letters))`   = any_ge("a", c(blank, NA, letters))  ,
#'        `any_gt("a", c(blank, NA, letters))`   = any_gt("a", c(blank, NA, letters))  ,
#'        `any_in(NULL, blank, combo)`           = any_in(NULL, blank, combo)          ,
#'        `any_le("a", c(blank, NA, letters))`   = any_le("a", c(blank, NA, letters))  ,
#'        `any_lg(c(NA, TRUE, FALSE))`           = any_lg(c(NA, TRUE, FALSE))          ,
#'        `any_lt("a", c(blank, NA, letters))`   = any_lt("a", c(blank, NA, letters))  ,
#'        `any_mf(NULL, blank, combo)`           = any_mf(NULL, blank, combo)          ,
#'        `any_nav(c(blank, NA, letters))`       = any_nav(c(blank, NA, letters))      ,
#'        `any_nav(chr.mat)`                     = any_nav(chr.mat)                    ,
#'        `any_okv(c(blank, NA, letters))`       = any_okv(c(blank, NA, letters))      ,
#'        `any_okv(chr.mat)`                     = any_okv(chr.mat)                    ,
#'        `any_t(c(list(blank), combo))`         = any_t(c(list(blank), combo))        ,
#'        `any_tf(c(list(blank), combo))`        = any_tf(c(list(blank), combo))       ,
#'        `failsafe(non.existent.variable)`      = failsafe(non.existent.variable)     ,
#'        `failsafe(pi)`                         = failsafe(pi)                        ,
#'        `FALSE %.and.% FALSE`                  = FALSE %.and.% FALSE                 ,
#'        `FALSE %.nor.% FALSE`                  = FALSE %.nor.% FALSE                 ,
#'        `FALSE %.xor.% FALSE`                  = FALSE %.xor.% FALSE                 ,
#'        `FALSE %.or.% FALSE`                   = FALSE %.or.% FALSE                  ,
#'        `has_all(blank, combo)`                = has_all(blank, combo)               ,
#'        `has_any(blank, combo)`                = has_any(blank, combo)               ,
#'        `has_many(blank, combo)`               = has_many(blank, combo)              ,
#'        `has_none(blank, combo)`               = has_none(blank, combo)              ,
#'        `has_one(blank, combo)`                = has_one(blank, combo)               ,
#'        `has_some(blank, combo)`               = has_some(blank, combo)              ,
#'        `has_two(blank, combo)`                = has_two(blank, combo)               ,
#'        `is_bl("")`                            = is_bl("")                           ,
#'        `is_bl("a")`                           = is_bl("a")                          ,
#'        `is_bl(c("", ""))`                     = is_bl(c("", ""))                    ,
#'        `is_bl(NA)`                            = is_bl(NA)                           ,
#'        `is_BL("")`                            = is_BL("")                           ,
#'        `is_BL("a")`                           = is_BL("a")                          ,
#'        `is_BL(c("", ""))`                     = is_BL(c("", ""))                    ,
#'        `is_BL(NA)`                            = is_BL(NA)                           ,
#'        `is_dif("", "")`                       = is_dif("", "")                      ,
#'        `is_dif("word", 7)`                    = is_dif("word", 7)                   ,
#'        `is_dif(chr.mat, chr.mat)`             = is_dif(chr.mat, chr.mat)            ,
#'        `is_dif(NA, NA)`                       = is_dif(NA, NA)                      ,
#'        `is_DIF("", "")`                       = is_DIF("", "")                      ,
#'        `is_DIF("word", 7)`                    = is_DIF("word", 7)                   ,
#'        `is_DIF(chr.mat, chr.mat)`             = is_DIF(chr.mat, chr.mat)            ,
#'        `is_DIF(NA, NA)`                       = is_DIF(NA, NA)                      ,
#'        `is_eq("", "")`                        = is_eq("", "")                       ,
#'        `is_eq("word", 7)`                     = is_eq("word", 7)                    ,
#'        `is_eq(chr.mat, chr.mat)`              = is_eq(chr.mat, chr.mat)             ,
#'        `is_eq(NA, NA)`                        = is_eq(NA, NA)                       ,
#'        `is_EQ("", "")`                        = is_EQ("", "")                       ,
#'        `is_EQ("word", 7)`                     = is_EQ("word", 7)                    ,
#'        `is_EQ(chr.mat, chr.mat)`              = is_EQ(chr.mat, chr.mat)             ,
#'        `is_EQ(NA, NA)`                        = is_EQ(NA, NA)                       ,
#'        `is_err(non.existent.variable)`        = is_err(non.existent.variable)       ,
#'        `is_err(pi)`                           = is_err(pi)                          ,
#'        `is_f(c(TRUE, FALSE))`                 = is_f(c(TRUE, FALSE))                ,
#'        `is_f(chr.mat)`                        = is_f(chr.mat)                       ,
#'        `is_f(NA)`                             = is_f(NA)                            ,
#'        `is_f(TRUE)`                           = is_f(TRUE)                          ,
#'        `is_f(TRUE)`                           = is_f(TRUE)                          ,
#'        `is_F(c(TRUE, FALSE))`                 = is_F(c(TRUE, FALSE))                ,
#'        `is_F(chr.mat)`                        = is_F(chr.mat)                       ,
#'        `is_F(FALSE)`                          = is_F(FALSE)                         ,
#'        `is_F(NA)`                             = is_F(NA)                            ,
#'        `is_F(TRUE)`                           = is_F(TRUE)                          ,
#'        `is_ge("a", "b")`                      = is_ge("a", "b")                     ,
#'        `is_ge("b", "a")`                      = is_ge("b", "a")                     ,
#'        `is_ge(1, 1)`                          = is_ge(1, 1)                         ,
#'        `is_ge(chr.mat, chr.mat)`              = is_ge(chr.mat, chr.mat)             ,
#'        `is_GE("a", "b")`                      = is_GE("a", "b")                     ,
#'        `is_GE("b", "a")`                      = is_GE("b", "a")                     ,
#'        `is_GE(1, 1)`                          = is_GE(1, 1)                         ,
#'        `is_GE(chr.mat, chr.mat)`              = is_GE(chr.mat, chr.mat)             ,
#'        `is_gt("a", "b")`                      = is_gt("a", "b")                     ,
#'        `is_gt("b", "a")`                      = is_gt("b", "a")                     ,
#'        `is_gt(1, 1)`                          = is_gt(1, 1)                         ,
#'        `is_gt(chr.mat, chr.mat)`              = is_gt(chr.mat, chr.mat)             ,
#'        `is_GT("a", "b")`                      = is_GT("a", "b")                     ,
#'        `is_GT("b", "a")`                      = is_GT("b", "a")                     ,
#'        `is_GT(1, 1)`                          = is_GT(1, 1)                         ,
#'        `is_GT(chr.mat, chr.mat)`              = is_GT(chr.mat, chr.mat)             ,
#'        `is_in(1, "", "a", 1:2)`               = is_in(1, "", "a", 1:2)              ,
#'        `is_in(1, "a", letters)`               = is_in(1, "a", letters)              ,
#'        `is_in(NULL, blank, combo)`            = is_in(NULL, blank, combo)           ,
#'        `is_IN(1, "", "a", 1:2)`               = is_IN(1, "", "a", 1:2)              ,
#'        `is_IN(1, "a", letters)`               = is_IN(1, "a", letters)              ,
#'        `is_IN(NULL, blank, combo)`            = is_IN(NULL, blank, combo)           ,
#'        `is_BL(chr.mat)`                       = is_BL(chr.mat)                      ,
#'        `is_le("a", "b")`                      = is_le("a", "b")                     ,
#'        `is_le("b", "a")`                      = is_le("b", "a")                     ,
#'        `is_le(1, 1)`                          = is_le(1, 1)                         ,
#'        `is_le(chr.mat, chr.mat)`              = is_le(chr.mat, chr.mat)             ,
#'        `is_LE("a", "b")`                      = is_LE("a", "b")                     ,
#'        `is_LE("b", "a")`                      = is_LE("b", "a")                     ,
#'        `is_LE(1, 1)`                          = is_LE(1, 1)                         ,
#'        `is_LE(chr.mat, chr.mat)`              = is_LE(chr.mat, chr.mat)             ,
#'        `is_LG(c(TRUE, FALSE))`                = is_LG(c(TRUE, FALSE))               ,
#'        `is_LG(chr.mat)`                       = is_LG(chr.mat)                      ,
#'        `is_LG(FALSE)`                         = is_LG(FALSE)                        ,
#'        `is_LG(NA)`                            = is_LG(NA)                           ,
#'        `is_LG(TRUE)`                          = is_LG(TRUE)                         ,
#'        `is_lt("a", "b")`                      = is_lt("a", "b")                     ,
#'        `is_lt("b", "a")`                      = is_lt("b", "a")                     ,
#'        `is_lt(1, 1)`                          = is_lt(1, 1)                         ,
#'        `is_lt(chr.mat, chr.mat)`              = is_lt(chr.mat, chr.mat)             ,
#'        `is_LT("a", "b")`                      = is_LT("a", "b")                     ,
#'        `is_LT("b", "a")`                      = is_LT("b", "a")                     ,
#'        `is_LT(1, 1)`                          = is_LT(1, 1)                         ,
#'        `is_LT(chr.mat, chr.mat)`              = is_LT(chr.mat, chr.mat)             ,
#'        `is_mf(1, "", "a", 1:2)`               = is_mf(1, "", "a", 1:2)              ,
#'        `is_mf(1, "a", letters)`               = is_mf(1, "a", letters)              ,
#'        `is_mf(NULL, blank, combo)`            = is_mf(NULL, blank, combo)           ,
#'        `is_MF(1, "", "a", 1:2)`               = is_MF(1, "", "a", 1:2)              ,
#'        `is_MF(1, "a", letters)`               = is_MF(1, "a", letters)              ,
#'        `is_MF(NULL, blank, combo)`            = is_MF(NULL, blank, combo)           ,
#'        `is_nav(1)`                            = is_nav(1)                           ,
#'        `is_nav(c(NA, NA))`                    = is_nav(c(NA, NA))                   ,
#'        `is_nav(chr.mat)`                      = is_nav(chr.mat)                     ,
#'        `is_nav(NA)`                           = is_nav(NA)                          ,
#'        `is_NAV(1)`                            = is_NAV(1)                           ,
#'        `is_NAV(c(NA, NA))`                    = is_NAV(c(NA, NA))                   ,
#'        `is_NAV(chr.mat)`                      = is_NAV(chr.mat)                     ,
#'        `is_NAV(NA)`                           = is_NAV(NA)                          ,
#'        `is_okv(1)`                            = is_okv(1)                           ,
#'        `is_okv(c(NA, NA))`                    = is_okv(c(NA, NA))                   ,
#'        `is_okv(chr.mat)`                      = is_ok(chr.mat)                      ,
#'        `is_okv(NA)`                           = is_okv(NA)                          ,
#'        `is_OKV(1)`                            = is_OKV(1)                           ,
#'        `is_OKV(c(NA, NA))`                    = is_OKV(c(NA, NA))                   ,
#'        `is_OKV(chr.mat)`                      = is_OKV(chr.mat)                     ,
#'        `is_OKV(NA)`                           = is_OKV(NA)                          ,
#'        `is_seq(1, letters)`                   = is_seq(1, letters)                  ,
#'        `is_seq(1, letters)`                   = is_seq(1, letters)                  ,
#'        `is_seq(1:3, c(1, 2, 3, 2, 1))`        = is_seq(1:3, c(1, 2, 3, 2, 1))       ,
#'        `is_seq(1:3, c(1, 2, 3, 2, 1))`        = is_seq(1:3, c(1, 2, 3, 2, 1))       ,
#'        `is_seq(1:3, c(1, 2, 3))`              = is_seq(1:3, c(1, 2, 3))             ,
#'        `is_seq(1:3, c(1, 2, 3))`              = is_seq(1:3, c(1, 2, 3))             ,
#'        `is_t(c(TRUE, FALSE))`                 = is_t(c(TRUE, FALSE))                ,
#'        `is_t(chr.mat)`                        = is_t(chr.mat)                       ,
#'        `is_t(NA)`                             = is_t(NA)                            ,
#'        `is_t(TRUE)`                           = is_t(TRUE)                          ,
#'        `is_t(TRUE)`                           = is_t(TRUE)                          ,
#'        `is_T(c(TRUE, FALSE))`                 = is_T(c(TRUE, FALSE))                ,
#'        `is_T(chr.mat)`                        = is_T(chr.mat)                       ,
#'        `is_T(FALSE)`                          = is_T(FALSE)                         ,
#'        `is_T(NA)`                             = is_T(NA)                            ,
#'        `is_T(TRUE)`                           = is_T(TRUE)                          ,
#'        `is_tf(c(TRUE, FALSE))`                = is_tf(c(TRUE, FALSE))               ,
#'        `is_tf(chr.mat)`                       = is_tf(chr.mat)                      ,
#'        `is_tf(chr.mat)`                       = is_tf(chr.mat)                      ,
#'        `is_tf(NA)`                            = is_tf(NA)                           ,
#'        `is_tf(TRUE)`                          = is_tf(TRUE)                         ,
#'        `is_tf(TRUE)`                          = is_tf(TRUE)                         ,
#'        `is_TF(c(TRUE, FALSE))`                = is_TF(c(TRUE, FALSE))               ,
#'        `is_TF(chr.mat)`                       = is_TF(chr.mat)                      ,
#'        `is_TF(FALSE)`                         = is_TF(FALSE)                        ,
#'        `is_TF(NA)`                            = is_TF(NA)                           ,
#'        `is_TF(TRUE)`                          = is_TF(TRUE)                         ,
#'        `is_veq(1, letters)`                   = is_veq(1, letters)                  ,
#'        `is_veq(1:3, c(1, 2, 3, 2, 1))`        = is_veq(1:3, c(1, 2, 3, 2, 1))       ,
#'        `is_veq(1:3, c(1, 2, 3, 2, 1))`        = is_veq(1:3, c(1, 2, 3, 2, 1))       ,
#'        `is_veq(1:3, letters)`                 = is_veq(1:3, letters)                ,
#'        `is_veq(1:3, c(1, 2, 3))`              = is_veq(1:3, c(1, 2, 3))             ,
#'        `is_veq(1:3, c(1, 2, 3))`              = is_veq(1:3, c(1, 2, 3))             ,
#'        `lacks_all(blank, combo)`              = lacks_all(blank, combo)             ,
#'        `lacks_any(blank, combo)`              = lacks_any(blank, combo)             ,
#'        `lacks_many(blank, combo)`             = lacks_many(blank, combo)            ,
#'        `lacks_none(blank, combo)`             = lacks_none(blank, combo)            ,
#'        `lacks_one(blank, combo)`              = lacks_one(blank, combo)             ,
#'        `lacks_some(blank, combo)`             = lacks_some(blank, combo)            ,
#'        `lacks_two(blank, combo)`              = lacks_two(blank, combo)             ,
#'        `many_bl(c(blank, letters))`           = many_bl(c(blank, letters))          ,
#'        `many_dif("a", c(blank, NA, letters))` = many_dif("a", c(blank, NA, letters)),
#'        `many_eq(NA, c(blank, NA, letters))`   = many_eq(NA, c(blank, NA, letters))  ,
#'        `many_f(c(list(blank), combo))`        = many_f(c(list(blank), combo))       ,
#'        `many_ge("a", c(blank, NA, letters))`  = many_ge("a", c(blank, NA, letters)) ,
#'        `many_gt("a", c(blank, NA, letters))`  = many_gt("a", c(blank, NA, letters)) ,
#'        `many_in(NULL, blank, combo)`          = many_in(NULL, blank, combo)         ,
#'        `many_le("a", c(blank, NA, letters))`  = many_le("a", c(blank, NA, letters)) ,
#'        `many_lg(c(NA, TRUE, FALSE))`          = many_lg(c(NA, TRUE, FALSE))         ,
#'        `many_lt("a", c(blank, NA, letters))`  = many_lt("a", c(blank, NA, letters)) ,
#'        `many_mf(NULL, blank, combo)`          = many_mf(NULL, blank, combo)         ,
#'        `many_nav(c(blank, NA, letters))`      = many_nav(c(blank, NA, letters))     ,
#'        `many_nav(chr.mat)`                    = many_nav(chr.mat)                   ,
#'        `many_okv(c(blank, NA, letters))`      = many_okv(c(blank, NA, letters))     ,
#'        `many_okv(chr.mat)`                    = many_okv(chr.mat)                   ,
#'        `many_t(c(list(blank), combo))`        = many_t(c(list(blank), combo))       ,
#'        `many_tf(c(list(blank), combo))`       = many_tf(c(list(blank), combo))      ,
#'        `msg_err(non.existent.variable)`       = msg_err(non.existent.variable)      ,
#'        `msg_err(pi)`                          = msg_err(pi)                         ,
#'        `n_bl(c(list(blank), combo))`          = n_bl(c(list(blank), combo))         ,
#'        `n_f(c(list(blank), combo))`           = n_f(c(list(blank), combo))          ,
#'        `n_has(blank, combo)`                  = n_has(blank, combo)                 ,
#'        `n_in(NULL, blank, combo)`             = n_in(NULL, blank, combo)            ,
#'        `n_lacks(blank, combo)`                = n_lacks(blank, combo)               ,
#'        `n_mf(NULL, blank, combo)`             = n_mf(NULL, blank, combo)            ,
#'        `n_nav(chr.mat)`                       = n_nav(chr.mat)                      ,
#'        `n_okv(chr.mat)`                       = n_okv(chr.mat)                      ,
#'        `none_bl(c(blank, letters))`           = none_bl(c(blank, letters))          ,
#'        `none_dif("a", c(blank, NA, letters))` = none_dif("a", c(blank, NA, letters)),
#'        `none_eq(NA, c(blank, NA, letters))`   = none_eq(NA, c(blank, NA, letters))  ,
#'        `none_f(c(list(blank), combo))`        = none_f(c(list(blank), combo))       ,
#'        `none_ge("a", c(blank, NA, letters))`  = none_ge("a", c(blank, NA, letters)) ,
#'        `none_gt("a", c(blank, NA, letters))`  = none_gt("a", c(blank, NA, letters)) ,
#'        `none_in(NULL, blank, combo)`          = none_in(NULL, blank, combo)         ,
#'        `none_le("a", c(blank, NA, letters))`  = none_le("a", c(blank, NA, letters)) ,
#'        `none_lg(c(NA, TRUE, FALSE))`          = none_lg(c(NA, TRUE, FALSE))         ,
#'        `none_lt("a", c(blank, NA, letters))`  = none_lt("a", c(blank, NA, letters)) ,
#'        `none_mf(NULL, blank, combo)`          = none_mf(NULL, blank, combo)         ,
#'        `none_nav(c(blank, NA, letters))`      = none_nav(c(blank, NA, letters))     ,
#'        `none_nav(chr.mat)`                    = none_nav(chr.mat)                   ,
#'        `none_okv(c(blank, NA, letters))`      = none_okv(c(blank, NA, letters))     ,
#'        `none_okv(chr.mat)`                    = none_okv(chr.mat)                   ,
#'        `none_t(c(list(blank), combo))`        = none_t(c(list(blank), combo))       ,
#'        `none_tf(c(list(blank), combo))`       = none_tf(c(list(blank), combo))      ,
#'        `not_bl("")`                           = not_bl("")                          ,
#'        `not_bl("a")`                          = not_bl("a")                         ,
#'        `not_bl(c("", ""))`                    = not_bl(c("", ""))                   ,
#'        `not_bl(chr.mat)`                      = not_bl(chr.mat)                     ,
#'        `not_bl(NA)`                           = not_bl(NA)                          ,
#'        `not_BL("")`                           = not_BL("")                          ,
#'        `not_BL("a")`                          = not_BL("a")                         ,
#'        `not_BL(c("", ""))`                    = not_BL(c("", ""))                   ,
#'        `not_BL(chr.mat)`                      = not_BL(chr.mat)                     ,
#'        `not_BL(NA)`                           = not_BL(NA)                          ,
#'        `not_dif("", "")`                      = not_dif("", "")                     ,
#'        `not_dif("word", 7)`                   = not_dif("word", 7)                  ,
#'        `not_dif(chr.mat, chr.mat)`            = not_dif(chr.mat, chr.mat)           ,
#'        `not_dif(NA, NA)`                      = not_dif(NA, NA)                     ,
#'        `not_DIF("", "")`                      = not_DIF("", "")                     ,
#'        `not_DIF("word", 7)`                   = not_DIF("word", 7)                  ,
#'        `not_DIF(chr.mat, chr.mat)`            = not_DIF(chr.mat, chr.mat)           ,
#'        `not_DIF(NA, NA)`                      = not_DIF(NA, NA)                     ,
#'        `not_eq("", "")`                       = not_eq("", "")                      ,
#'        `not_eq("word", 7)`                    = not_eq("word", 7)                   ,
#'        `not_eq(chr.mat, chr.mat)`             = not_eq(chr.mat, chr.mat)            ,
#'        `not_eq(NA, NA)`                       = not_eq(NA, NA)                      ,
#'        `not_EQ("", "")`                       = not_EQ("", "")                      ,
#'        `not_EQ("word", 7)`                    = not_EQ("word", 7)                   ,
#'        `not_EQ(chr.mat, chr.mat)`             = not_EQ(chr.mat, chr.mat)            ,
#'        `not_EQ(NA, NA)`                       = not_EQ(NA, NA)                      ,
#'        `not_err(non.existent.variable)`       = not_err(non.existent.variable)      ,
#'        `not_err(pi)`                          = not_err(pi)                         ,
#'        `not_f(c(TRUE, FALSE))`                = not_f(c(TRUE, FALSE))               ,
#'        `not_f(chr.mat)`                       = not_f(chr.mat)                      ,
#'        `not_f(NA)`                            = not_f(NA)                           ,
#'        `not_f(TRUE)`                          = not_f(TRUE)                         ,
#'        `not_f(TRUE)`                          = not_f(TRUE)                         ,
#'        `not_F(c(TRUE, FALSE))`                = not_F(c(TRUE, FALSE))               ,
#'        `not_F(chr.mat)`                       = not_F(chr.mat)                      ,
#'        `not_F(FALSE)`                         = not_F(FALSE)                        ,
#'        `not_F(NA)`                            = not_F(NA)                           ,
#'        `not_F(TRUE)`                          = not_F(TRUE)                         ,
#'        `not_ge("a", "b")`                     = not_ge("a", "b")                    ,
#'        `not_ge("b", "a")`                     = not_ge("b", "a")                    ,
#'        `not_ge(1, 1)`                         = not_ge(1, 1)                        ,
#'        `not_ge(chr.mat, chr.mat)`             = not_ge(chr.mat, chr.mat)            ,
#'        `not_GE("a", "b")`                     = not_GE("a", "b")                    ,
#'        `not_GE("b", "a")`                     = not_GE("b", "a")                    ,
#'        `not_GE(1, 1)`                         = not_GE(1, 1)                        ,
#'        `not_GE(chr.mat, chr.mat)`             = not_GE(chr.mat, chr.mat)            ,
#'        `not_gt("a", "b")`                     = not_gt("a", "b")                    ,
#'        `not_gt("b", "a")`                     = not_gt("b", "a")                    ,
#'        `not_gt(1, 1)`                         = not_gt(1, 1)                        ,
#'        `not_gt(chr.mat, chr.mat)`             = not_gt(chr.mat, chr.mat)            ,
#'        `not_GT("a", "b")`                     = not_GT("a", "b")                    ,
#'        `not_GT("b", "a")`                     = not_GT("b", "a")                    ,
#'        `not_GT(1, 1)`                         = not_GT(1, 1)                        ,
#'        `not_GT(chr.mat, chr.mat)`             = not_GT(chr.mat, chr.mat)            ,
#'        `not_in(1, "", "a", 1:2)`              = not_in(1, "", "a", 1:2)             ,
#'        `not_in(1, "a", letters)`              = not_in(1, "a", letters)             ,
#'        `not_in(NULL, blank, combo)`           = not_in(NULL, blank, combo)          ,
#'        `not_IN(1, "", "a", 1:2)`              = not_IN(1, "", "a", 1:2)             ,
#'        `not_IN(1, "a", letters)`              = not_IN(1, "a", letters)             ,
#'        `not_IN(NULL, blank, combo)`           = not_IN(NULL, blank, combo)          ,
#'        `not_le("a", "b")`                     = not_le("a", "b")                    ,
#'        `not_le("b", "a")`                     = not_le("b", "a")                    ,
#'        `not_le(1, 1)`                         = not_le(1, 1)                        ,
#'        `not_le(chr.mat, chr.mat)`             = not_le(chr.mat, chr.mat)            ,
#'        `not_LE("a", "b")`                     = not_LE("a", "b")                    ,
#'        `not_LE("b", "a")`                     = not_LE("b", "a")                    ,
#'        `not_LE(1, 1)`                         = not_LE(1, 1)                        ,
#'        `not_LE(chr.mat, chr.mat)`             = not_LE(chr.mat, chr.mat)            ,
#'        `not_LG(c(TRUE, FALSE))`               = not_LG(c(TRUE, FALSE))              ,
#'        `not_LG(chr.mat)`                      = not_LG(chr.mat)                     ,
#'        `not_LG(FALSE)`                        = not_LG(FALSE)                       ,
#'        `not_LG(NA)`                           = not_LG(NA)                          ,
#'        `not_LG(TRUE)`                         = not_LG(TRUE)                        ,
#'        `not_lt("a", "b")`                     = not_lt("a", "b")                    ,
#'        `not_lt("b", "a")`                     = not_lt("b", "a")                    ,
#'        `not_lt(1, 1)`                         = not_lt(1, 1)                        ,
#'        `not_lt(chr.mat, chr.mat)`             = not_lt(chr.mat, chr.mat)            ,
#'        `not_LT("a", "b")`                     = not_LT("a", "b")                    ,
#'        `not_LT("b", "a")`                     = not_LT("b", "a")                    ,
#'        `not_LT(1, 1)`                         = not_LT(1, 1)                        ,
#'        `not_LT(chr.mat, chr.mat)`             = not_LT(chr.mat, chr.mat)            ,
#'        `not_mf(1, "", "a", 1:2)`              = not_mf(1, "", "a", 1:2)             ,
#'        `not_mf(1, "a", letters)`              = not_mf(1, "a", letters)             ,
#'        `not_mf(NULL, blank, combo)`           = not_mf(NULL, blank, combo)          ,
#'        `not_MF(1, "", "a", 1:2)`              = not_MF(1, "", "a", 1:2)             ,
#'        `not_MF(1, "a", letters)`              = not_MF(1, "a", letters)             ,
#'        `not_MF(NULL, blank, combo)`           = not_MF(NULL, blank, combo)          ,
#'        `not_NAV(1)`                           = not_NAV(1)                          ,
#'        `not_NAV(c(NA, NA))`                   = not_NAV(c(NA, NA))                  ,
#'        `not_NAV(chr.mat)`                     = not_NAV(chr.mat)                    ,
#'        `not_NAV(NA)`                          = not_NAV(NA)                         ,
#'        `not_nav(c(NA, NA), chr.mat)`          = not_nav(c(NA, NA), chr.mat)         ,
#'        `not_nav(NA), not_nav(1)`              = not_nav(NA), not_nav(1)             ,
#'        `not_OKV(1)`                           = not_OKV(1)                          ,
#'        `not_OKV(c(NA, NA))`                   = not_OKV(c(NA, NA))                  ,
#'        `not_OKV(chr.mat)`                     = not_OKV(chr.mat)                    ,
#'        `not_OKV(NA)`                          = not_OKV(NA)                         ,
#'        `not_okv(c(NA, NA)`                    = not_okv(c(NA, NA))                  ,
#'        `not_okv(chr.mat)`                     = not_okv(chr.mat)                    ,
#'        `not_okv(NA)`                          = not_okv(NA)                         ,
#'        `not_okv(1)`                           = not_okv(1)                          ,
#'        `not_seq(1, letters)`                  = not_seq(1, letters)                 ,
#'        `not_seq(1, letters)`                  = not_seq(1, letters)                 ,
#'        `not_seq(1:3, c(1, 2, 3, 2, 1))`       = not_seq(1:3, c(1, 2, 3, 2, 1))      ,
#'        `not_seq(1:3, c(1, 2, 3, 2, 1))`       = not_seq(1:3, c(1, 2, 3, 2, 1))      ,
#'        `not_seq(1:3, c(1, 2, 3))`             = not_seq(1:3, c(1, 2, 3))            ,
#'        `not_seq(1:3, c(1, 2, 3))`             = not_seq(1:3, c(1, 2, 3))            ,
#'        `not_t(c(TRUE, FALSE))`                = not_t(c(TRUE, FALSE))               ,
#'        `not_t(chr.mat)`                       = not_t(chr.mat)                      ,
#'        `not_t(NA)`                            = not_t(NA)                           ,
#'        `not_t(TRUE)`                          = not_t(TRUE)                         ,
#'        `not_t(TRUE)`                          = not_t(TRUE)                         ,
#'        `not_T(c(TRUE, FALSE))`                = not_T(c(TRUE, FALSE))               ,
#'        `not_T(chr.mat)`                       = not_T(chr.mat)                      ,
#'        `not_T(FALSE)`                         = not_T(FALSE)                        ,
#'        `not_T(NA)`                            = not_T(NA)                           ,
#'        `not_T(TRUE)`                          = not_T(TRUE)                         ,
#'        `not_tf(c(TRUE, FALSE))`               = not_tf(c(TRUE, FALSE))              ,
#'        `not_tf(chr.mat)`                      = not_tf(chr.mat)                     ,
#'        `not_tf(NA)`                           = not_tf(NA)                          ,
#'        `not_tf(TRUE)`                         = not_tf(TRUE)                        ,
#'        `not_tf(TRUE)`                         = not_tf(TRUE)                        ,
#'        `not_TF(c(TRUE, FALSE))`               = not_TF(c(TRUE, FALSE))              ,
#'        `not_TF(chr.mat)`                      = not_TF(chr.mat)                     ,
#'        `not_TF(FALSE)`                        = not_TF(FALSE)                       ,
#'        `not_TF(NA)`                           = not_TF(NA)                          ,
#'        `not_TF(TRUE)`                         = not_TF(TRUE)                        ,
#'        `not_veq(1, letters)`                  = not_veq(1, letters)                 ,
#'        `not_veq(1, letters)`                  = not_veq(1, letters)                 ,
#'        `not_veq(1:3, c(1, 2, 3, 2, 1))`       = not_veq(1:3, c(1, 2, 3, 2, 1))      ,
#'        `not_veq(1:3, c(1, 2, 3, 2, 1))`       = not_veq(1:3, c(1, 2, 3, 2, 1))      ,
#'        `not_veq(1:3, c(1, 2, 3))`             = not_veq(1:3, c(1, 2, 3))            ,
#'        `not_veq(1:3, c(1, 2, 3))`             = not_veq(1:3, c(1, 2, 3))            ,
#'        `n_t(c(list(blank), combo))`           = n_t(c(list(blank), combo))          ,
#'        `n_tf(c(list(blank), combo))`          = n_tf(c(list(blank), combo))         ,
#'        `NULL %.in.% abc`                      = NULL %.in% abc                      ,
#'        `NULL %.mf.% abc`                      = NULL %.mf.% abc                     ,
#'        `one_bl(c(blank, letters))`            = one_bl(c(blank, letters))           ,
#'        `one_dif("a", c(blank, NA, letters))`  = one_dif("a", c(blank, NA, letters)) ,
#'        `one_eq(NA, c(blank, NA, letters))`    = one_eq(NA, c(blank, NA, letters))   ,
#'        `one_f(c(list(blank), combo))`         = one_f(c(list(blank), combo))        ,
#'        `one_ge("a", c(blank, NA, letters))`   = one_ge("a", c(blank, NA, letters))  ,
#'        `one_gt("a", c(blank, NA, letters))`   = one_gt("a", c(blank, NA, letters))  ,
#'        `one_in(NULL, blank, combo)`           = one_in(NULL, blank, combo)          ,
#'        `one_le("a", c(blank, NA, letters))`   = one_le("a", c(blank, NA, letters))  ,
#'        `one_lt("a", c(blank, NA, letters))`   = one_lt("a", c(blank, NA, letters))  ,
#'        `one_mf(NULL, blank, combo)`           = one_mf(NULL, blank, combo)          ,
#'        `one_nav(c(blank, NA, letters))`       = one_nav(c(blank, NA, letters))      ,
#'        `one_nav(chr.mat)`                     = one_nav(chr.mat)                    ,
#'        `one_okv(c(blank, NA, letters))`       = one_okv(c(blank, NA, letters))      ,
#'        `one_okv(chr.mat)`                     = one_okv(chr.mat)                    ,
#'        `one_t(c(list(blank), combo))`         = one_t(c(list(blank), combo))        ,
#'        `one_tf(c(list(blank), combo))`        = one_tf(c(list(blank), combo))       ,
#'        `some_bl(c(blank, letters))`           = some_bl(c(blank, letters))          ,
#'        `some_dif("a", c(blank, NA, letters))` = some_dif("a", c(blank, NA, letters)),
#'        `some_eq(NA, c(blank, NA, letters))`   = some_eq(NA, c(blank, NA, letters))  ,
#'        `some_f(c(list(blank), combo))`        = some_f(c(list(blank), combo))       ,
#'        `some_ge("a", c(blank, NA, letters))`  = some_ge("a", c(blank, NA, letters)) ,
#'        `some_gt("a", c(blank, NA, letters))`  = some_gt("a", c(blank, NA, letters)) ,
#'        `some_in(NULL, blank, combo)`          = some_in(NULL, blank, combo)         ,
#'        `some_le("a", c(blank, NA, letters))`  = some_le("a", c(blank, NA, letters)) ,
#'        `some_lt("a", c(blank, NA, letters))`  = some_lt("a", c(blank, NA, letters)) ,
#'        `some_mf(NULL, blank, combo)`          = some_mf(NULL, blank, combo)         ,
#'        `some_nav(c(blank, NA, letters))`      = some_nav(c(blank, NA, letters))     ,
#'        `some_nav(chr.mat)`                    = some_nav(chr.mat)                   ,
#'        `some_okv(c(blank, NA, letters))`      = some_okv(c(blank, NA, letters))     ,
#'        `some_okv(chr.mat)`                    = some_okv(chr.mat)                   ,
#'        `some_t(c(list(blank), combo))`        = some_t(c(list(blank), combo))       ,
#'        `some_tf(c(list(blank), combo))`       = some_tf(c(list(blank), combo))      ,
#'        `TRUE %.and.% 42`                      = TRUE %.and.% 42                     ,
#'        `TRUE %.and.% TRUE`                    = TRUE %.and.% TRUE                   ,
#'        `TRUE %.nor.% 42`                      = TRUE %.nor.% 42                     ,
#'        `TRUE %.nor.% TRUE`                    = TRUE %.nor.% TRUE                   ,
#'        `TRUE %.xor.% 42`                      = TRUE %.xor.% 42                     ,
#'        `TRUE %.xor.% TRUE`                    = TRUE %.xor.% TRUE                   ,
#'        `TRUE %.or.% 42`                       = TRUE %.or.% 42                      ,
#'        `TRUE %.or.% TRUE`                     = TRUE %.or.% TRUE                    ,
#'        `two_bl(c(blank, letters))`            = two_bl(c(blank, letters))           ,
#'        `two_dif("a", c(blank, NA, letters))`  = two_dif("a", c(blank, NA, letters)) ,
#'        `two_eq(NA, c(blank, NA, letters))`    = two_eq(NA, c(blank, NA, letters))   ,
#'        `two_f(c(list(blank), combo))`         = two_f(c(list(blank), combo))        ,
#'        `two_ge("a", c(blank, NA, letters))`   = two_ge("a", c(blank, NA, letters))  ,
#'        `two_gt("a", c(blank, NA, letters))`   = two_gt("a", c(blank, NA, letters))  ,
#'        `two_in(NULL, blank, combo)`           = two_in(NULL, blank, combo)          ,
#'        `two_le("a", c(blank, NA, letters))`   = two_le("a", c(blank, NA, letters))  ,
#'        `two_lt("a", c(blank, NA, letters))`   = two_lt("a", c(blank, NA, letters))  ,
#'        `two_mf(NULL, blank, combo)`           = two_mf(NULL, blank, combo)          ,
#'        `two_nav(c(blank, NA, letters))`       = two_nav(c(blank, NA, letters))      ,
#'        `two_nav(chr.mat)`                     = two_nav(chr.mat)                    ,
#'        `two_okv(c(blank, NA, letters))`       = two_okv(c(blank, NA, letters))      ,
#'        `two_okv(chr.mat)`                     = two_okv(chr.mat)                    ,
#'        `two_t(c(list(blank), combo))`         = two_t(c(list(blank), combo))        ,
#'        `two_tf(c(list(blank), combo))`        = two_tf(c(list(blank), combo))       )
#' }
#' egfailsafe()
#' @export
failsafe <- function(X, Def = "uj.failsafe.err") {
  X <- tryCatch(base::identity(X), error = function(e) e, finally = NULL)
  if (rlang::is_error(X)) {
    Def <- tryCatch(base::identity(Def), error = function(e) e, finally = NULL)
    if (rlang::is_error(Def)) {Def <- "uj.failsafe.err"}
    base::attr(Def, "stack") <- uj::callers()
    Def
  } else {X}
}

#' @rdname failsafe
#' @export
is_err <- function(X) {rlang::is_error(tryCatch(base::identity(X), error = function(e) e, finally = NULL))}

#' @rdname failsafe
#' @export
not_err <- function(X) {!uj::is_err(X)}

#' @rdname failsafe
#' @export
msg_err <- function(X) {
  X <- tryCatch(base::identity(X), error = function(e) e, finally = NULL)
  if (rlang::is_error(X)) {X$message} else {NULL}
}

#' @rdname failsafe
#' @export
make_err <- function() {base::simpleError("uj.intentionally.generated error.")}

#' @rdname failsafe
#' @export
fs_def <- function(X) {if (uj::is_err(X)) {FALSE} else {!base::is.null(X)}}

#' @rdname failsafe
#' @export
fs_nll <- function(X) {if (uj::is_err(X)) {TRUE} else {base::is.null(X)}}

#' @rdname failsafe
#' @export
fs_scl <- function(X, Val) {
  if      (uj::is_err(X) | uj::is_err(Val)              ) {FALSE}
  else if (base::length(X) != 1 | base::length(Val) != 1) {FALSE}
  else if (!base::is.atomic(X) | !base::is.atomic(Val)  ) {FALSE}
  else if (uj:::.compat(X, Val)                         ) {FALSE}
  else if (base::is.na(X) & base::is.na(Val)            ) {TRUE}
  else if (base::is.na(X) | base::is.na(Val)            ) {FALSE}
  else                                                    {X == Val}
}

#' @rdname failsafe
#' @export
fs_t <- function(X) {if (uj::is_err(X)) {FALSE} else {base::isTRUE(X)}}

#' @rdname failsafe
#' @export
fs_f <- function(X) {if (uj::is_err(X)) {FALSE} else {base::isFALSE(X)}}

#' @rdname failsafe
#' @export
fs_tf <- function(X) {uj::fs_t(X) | uj::fs_f(X)}

#' @rdname failsafe
#' @export
fs_na <- function(X) {
  if (!uj::is_err(X)) {if (base::is.atomic(X)) {if (base::length(X) == 1) {return(base::is.na(X))}}}
  FALSE
}

#' @rdname failsafe
#' @export
fs_ok <- function(X) {
  if (!uj::is_err(X)) {if (base::is.atomic(X)) {if (base::length(X) == 1) {!return(base::is.na(X))}}}
  FALSE
}

#' @rdname failsafe
#' @export
fs_bl <- function(X) {
  if (!uj::is_err(X)) {if (base::is.character(X)) {if (base::length(X) == 1) {return(X == "")}}}
  FALSE
}

#' @rdname failsafe
#' @export
fs_lg <- function(X) {uj::fs_f(X) | uj::fs_t(X) | uj::fs_na(X)}

#' @rdname failsafe
#' @export
fs_vec <- function(X, Val) {
  if      (uj::is_err(X) | uj::is_err(Val)            ) {FALSE}
  else if (uj:::.mismatch_n(X, Val)                   ) {FALSE}
  else if (!base::is.atomic(X) | !base::is.atomic(Val)) {FALSE}
  else if (uj:::.compat(X, Val)                       ) {FALSE}
  else if (base::is.na(X) & base::is.na(Val)          ) {TRUE}
  else if (base::is.na(X) | base::is.na(Val)          ) {FALSE}
  else                                                  {X == Val}
}

#' @rdname failsafe
#' @export
fs_null <- function(X) {if (uj::is_err(X)) {FALSE} else {base::is.null(X)}}

#' @rdname failsafe
#' @export
scl_bl <- function(X) {uj::fs_bl(X)}

#' @rdname failsafe
#' @export
scl_t <- function(X)  {uj::fs_t(X)}

#' @rdname failsafe
#' @export
scl_f <- function(X)  {uj::fs_f(X)}

#' @rdname failsafe
#' @export
scl_tf <- function(X)  {uj::fs_tf(X)}

#' @rdname failsafe
#' @export
scl_lg <- function(X)  {uj::fs_lg(X)}

#' @rdname failsafe
#' @export
scl_na <- function(X)  {uj::fs_na(X)}

#' @rdname failsafe
#' @export
vec_bl <- function(X) {uj::fs_vec(X, "")}

#' @rdname failsafe
#' @export
vec_t <- function(X)  {uj::fs_vec(X, TRUE)}

#' @rdname failsafe
#' @export
vec_f <- function(X)  {uj::fs_vec(X, FALSE)}

#' @rdname failsafe
#' @export
vec_tf <- function(X)  {uj::fs_vec(X, TRUE) | uj::fs_vec(X, TRUE)}

#' @rdname failsafe
#' @export
vec_na <- function(X)  {uj::fs_vec(X, NA)}

#' @rdname failsafe
#' @export
fs_or <- function(X, Or = NULL) {if (!uj::is_err(X)) {X} else if (!uj::is_err(Or)) {Or} else {NULL}}

#' @rdname failsafe
#' @export
or_f <- function(X) {uj::fs_or(X, FALSE)}

#' @rdname failsafe
#' @export
or_t <- function(X) {uj::fs_or(X, TRUE)}

#' @rdname failsafe
#' @export
or_bl <- function(X) {uj::fs_or(X, "")}

#' @rdname failsafe
#' @export
or_c0 <- function(X) {uj::fs_or(X, base::character(0))}

#' @rdname failsafe
#' @export
or_i0 <- function(X) {uhj::fs_or(X, base::integer(0))}

#' @rdname failsafe
#' @export
or_l0 <- function(X) {uhj::fs_or(X, base::logical(0))}

#' @rdname failsafe
#' @export
or_n0 <- function(X) {uhj::fs_or(X, base::numeric(0))}

#' @rdname failsafe
#' @export
or_na <- function(X) {uj::fs_or(X, NA)}

#' @rdname failsafe
#' @export
or_nac <- function(X) {uj::fs_or(X, NA_character_)}

#' @rdname failsafe
#' @export
or_nai <- function(X) {uj::fs_or(X, NA_integer_)}

#' @rdname failsafe
#' @export
or_nal <- function(X) {uj::fs_or(X, NA)}

#' @rdname failsafe
#' @export
or_nar <- function(X) {uj::fs_or(X, NA_real_)}

#' @rdname failsafe
#' @export
or_null <- function(X) {uj::fs_or(X)}

#' @rdname failsafe
#' @export
is_id <- function(X, Y) {if (uj::is_err(X) | uj::is_err(Y)) {FALSE} else {base::identical(X, Y)}}

#' @rdname failsafe
#' @export
not_id <- function(X, Y) {if (uj::is_err(X) | uj::is_err(Y)) {FALSE} else {!base::identical(X, Y)}}

#' @rdname failsafe
#' @export
is_IN <- function(X, ...) {
  if (uj::not_err(X)) {
    if (base::...length() > 0 & base::length(X) == 1 & base::is.atomic(X)) {
      for (i in 1:base::...length()) {
        if (uj::not_err(base::...elt(i))) {
          Dot <- base::...elt(i)
          if (base::is.atomic(Dot)) {
            if (uj:::.compat(X, Dot)) {
              if (X %in% Dot) {return(TRUE)}}}}}}}
  FALSE
}

#' @rdname failsafe
#' @export
is_MF <- function(X, ...) {!uj::is_IN(X, ...)}

#' @rdname failsafe
#' @export
HAS <- function(X, Y) {uj::is_IN(Y, X)}

#' @rdname failsafe
#' @export
LACKS <- function(X, Y) {!uj::is_IN(Y, X)}

#' @rdname failsafe
#' @export
is_EQ <- function(X, Y) {
  if (uj::not_err(X) & uj::not_err(Y)) {
    if (base::is.atomic(X) & base::is.atomic(Y) & base::length(X) == 1 & base::length(Y) == 1) {
      if (uj:::.compat(X, Y)) {
        if (base::is.na(X) | base::is.na(Y)) {return(FALSE)}
        Y <- base::unlist(X == Y)
        base::attributes(Y) <- NULL
        return(Y)}}}
  FALSE
}

#' @rdname failsafe
#' @export
is_GE <- function(X, Y) {
  if (uj::not_err(X) & uj::not_err(Y)) {
    if (base::is.atomic(X) & base::is.atomic(Y) & base::length(X) == 1 & base::length(Y) == 1) {
      if (uj:::.compar(X, Y)) {
        Y <- base::unlist(X >= Y)
        base::attributes(Y) <- NULL
        return(Y)}}}
  FALSE
}

#' @rdname failsafe
#' @export
is_GT <- function(X, Y) {
  if (uj::not_err(X) & uj::not_err(Y)) {
    if (base::is.atomic(X) & base::is.atomic(Y) & base::length(X) == 1 & base::length(Y) == 1) {
      if (uj:::.compar(X, Y)) {
        Y <- base::unlist(X > Y)
        base::attributes(Y) <- NULL
        return(Y)}}}
  FALSE
}

#' @rdname failsafe
#' @export
is_LE <- function(X, Y) {
  if (uj::not_err(X) & uj::not_err(Y)) {
    if (base::is.atomic(X) & base::is.atomic(Y) & base::length(X) == 1 & base::length(Y) == 1) {
      if (uj:::.compar(X, Y)) {
        Y <- base::unlist(X <= Y)
        base::attributes(Y) <- NULL
        return(Y)}}}
  FALSE
}

#' @rdname failsafe
#' @export
is_LT <- function(X, Y) {
  if (uj::not_err(X) & uj::not_err(Y)) {
    if (base::is.atomic(X) & base::is.atomic(Y) & base::length(X) == 1 & base::length(Y) == 1) {
      if (uj:::.compar(X, Y)) {
        Y <- base::unlist(X < Y)
        base::attributes(Y) <- NULL
        return(Y)}}}
  FALSE
}

#' @rdname failsafe
#' @export
is_DIF <- function(X, Y) {!uj::is_EQ(X, Y)}

#' @rdname failsafe
#' @export
is_F <- function(X) {uj::fs_f(X)}

#' @rdname failsafe
#' @export
is_T <- function(X) {uj::fs_t(X)}

#' @rdname failsafe
#' @export
is_BL <- function(X) {uj::fs_bl(X)}

#' @rdname failsafe
#' @export
is_TF <- function(X) {uj::fs_tf(X)}

#' @rdname failsafe
#' @export
is_NAV <- function(X) {uj::fs_na(X)}

#' @rdname failsafe
#' @export
is_OKV <- function(X) {uj::fs_ok(X)}

#' @rdname failsafe
#' @export
is_LG <- function(X) {uj::fs_lg(X)}

#' @rdname failsafe
#' @export
is_in <- function(X, ...) {
  if (uj::not_err(X)) {if (base::is.atomic(X) & base::length(X) > 0) {return(base::sapply(X, uj::is_IN, ...))}}
  FALSE
}

#' @rdname failsafe
#' @export
is_mf <- function(X, ...) {!uj::is_in(X, ...)}

#' @rdname failsafe
#' @export
has <- function(X, Y) {uj::is_in(Y, X)}

#' @rdname failsafe
#' @export
not <- function(X) {base::isFALSE(uj::failsafe(X))}

#' @rdname failsafe
#' @export
lacks <- function(X, Y) {!uj::is_in(Y, X)}

#' @rdname failsafe
#' @export
is_eq <- function(X, Y) {
  if (uj::not_err(X) & uj::not_err(Y)) {
    if (base::is.atomic(X) & base::is.atomic(Y)) {
      nX <- base::length(X)
      nY <- base::length(Y)
      if (nX %in% base::c(1, nY) & nY %in% base::c(1, nY)) {
        if (uj:::.compat(X, Y)) {return(X == Y)}}}}
  FALSE
}

#' @rdname failsafe
#' @export
is_ge <- function(X, Y) {
  if (uj::not_err(X) & uj::not_err(Y)) {
    if (base::is.atomic(X) & base::is.atomic(Y)) {
      nX <- base::length(X)
      nY <- base::length(Y)
      if (nX %in% base::c(1, nY) & nY %in% base::c(1, nX)) {
        if (uj:::.compar(X, Y)) {return(X >= Y)}}}}
  FALSE
}

#' @rdname failsafe
#' @export
is_gt <- function(X, Y) {
  if (uj::not_err(X) & uj::not_err(Y)) {
    if (base::is.atomic(X) & base::is.atomic(Y)) {
      nX <- base::length(X)
      nY <- base::length(Y)
      if (nX %in% base::c(1, nY) & nY %in% base::c(1, nX)) {
        if (uj:::.compar(X, Y)) {return(X > Y)}}}}
  FALSE
}

#' @rdname failsafe
#' @export
is_le <- function(X, Y) {
  if (uj::not_err(X) & uj::not_err(Y)) {
    if (base::is.atomic(X) & base::is.atomic(Y)) {
      nX <- base::length(X)
      nY <- base::length(Y)
      if (nX %in% base::c(1, nY) & nY %in% base::c(1, nX)) {
        if (uj:::.compar(X, Y)) {return(X <= Y)}}}}
  FALSE
}

#' @rdname failsafe
#' @export
is_lt <- function(X, Y) {
  if (uj::not_err(X) & uj::not_err(Y)) {
    if (base::is.atomic(X) & base::is.atomic(Y)) {
      nX <- base::length(X)
      nY <- base::length(Y)
      if (nX %in% base::c(1, nY) & nY %in% base::c(1, nX)) {
        if (uj:::.compar(X, Y)) {return(X < Y)}}}}
  FALSE
}

#' @rdname failsafe
#' @export
is_dif <- function(X, Y) {!uj::is_eq(X, Y)}

#' @rdname failsafe
#' @export
is_f <- function(X) {uj::is_eq(X, FALSE)}

#' @rdname failsafe
#' @export
is_t <- function(X) {uj::is_eq(X, TRUE)}

#' @rdname failsafe
#' @export
is_bl <- function(X) {uj::is_eq(X, "")}

#' @rdname failsafe
#' @export
is_tf <- function(X) {uj::is_eq(X, FALSE) | uj::is_eq(X, TRUE)}

#' @rdname failsafe
#' @export
is_nav <- function(X) {
  if (uj::not_err(X)) {if (base::is.atomic(X)) {return(base::is.na(X))}}
  FALSE
}

#' @rdname failsafe
#' @export
is_okv <- function(X) {
  if (uj::not_err(X)) {if (base::is.atomic(X)) {return(!base::is.na(X))}}
  FALSE
}

#' @rdname failsafe
#' @export
is_seq <- function(X, Y) {
  if (uj::not_err(X) & uj::not_err(Y)) {
    if (base::is.atomic(X) & base::is.atomic(Y)) {
      if (uj:::.compat(X, Y)) {
        X <- base::unlist(X); base::attributes(X) <- NULL
        Y <- base::unlist(Y); base::attributes(Y) <- NULL
        return(base::setequal(X, Y))}}}
  FALSE
}

#' @rdname failsafe
#' @export
is_veq <- function(X, Y) {
  if (uj::not_err(X) & uj::not_err(Y)) {
    if (base::is.atomic(X) & base::is.atomic(Y)) {
      if (uj:::.compat(X, Y)) {
        if (base::length(X) == base::length(Y)) {
          if (!base::any(base::is.na(X)) & !base::any(base::is.na(Y))) {return(base::all(X == Y))}}}}}
  FALSE
}

#' @rdname failsafe
#' @export
is_sdif <- function(X, Y) {!uj::is_seq(X, Y)}

#' @rdname failsafe
#' @export
is_vdif <- function(X, Y) {!uj::is_veq(X, Y)}

#' @rdname failsafe
#' @export
OR <- function(X, Y) {uj::fs_t(X) | uj::fs_t(Y)}

#' @rdname failsafe
#' @export
AND <- function(X, Y) {uj::fs_t(X) & uj::fs_t(Y)}

#' @rdname failsafe
#' @export
XOR <- function(X, Y) {base::xor(uj::fs_t(X), uj::fs_t(Y))}

#' @rdname failsafe
#' @export
NOR <- function(X, Y) {!uj::fs_t(X) & !uj::fs_t(Y)}

#' @rdname failsafe
#' @export
or <- function(X, Y) {
  if (uj::not_err(X) & uj::not_err(Y)) {
    if (base::is.logical(X) & base::is.logical(Y)) {
      nX <- base::length(X)
      nY <- base::length(Y)
      if (nX %in% base::c(1, nY) & nY %in% base::c(1, nX)) {return(X | Y)}}}
  FALSE
}

#' @rdname failsafe
#' @export
and <- function(X, Y) {
  if (uj::not_err(X) & uj::not_err(Y)) {
    if (base::is.logical(X) & base::is.logical(Y)) {
      nX <- base::length(X)
      nY <- base::length(Y)
      if (nX %in% base::c(1, nY) & nY %in% base::c(1, nX)) {return(X & Y)}}}
  FALSE
}

#' @rdname failsafe
#' @export
one <- function(X, Y) {
  if (uj::not_err(X) & uj::not_err(Y)) {
    if (base::is.logical(X) & base::is.logical(Y)) {
      nX <- base::length(X)
      nY <- base::length(Y)
      if (nX %in% base::c(1, nY) & nY %in% base::c(1, nX)) {return(base::xor(X, Y))}}}
  FALSE
}

#' @rdname failsafe
#' @export
nor <- function(X, Y) {
  if (uj::not_err(X) & uj::not_err(Y)) {
    if (base::is.logical(X) & base::is.logical(Y)) {
      nX <- base::length(X)
      nY <- base::length(Y)
      if (nX %in% base::c(1, nY) & nY %in% base::c(1, nX)) {return(!X & !Y)}}}
  FALSE
}

#' @rdname failsafe
#' @export
not_IN <- function(X, ...) {!uj::is_IN(X, ...)}

#' @rdname failsafe
#' @export
not_MF <- function(X, ...) {!uj::is_MF(X, ...)}

#' @rdname failsafe
#' @export
not_HAS <- function(X, Y) {!uj::HAS(X, Y)}

#' @rdname failsafe
#' @export
not_LACKS <- function(X, Y) {!uj::LACKS(X, Y)}

#' @rdname failsafe
#' @export
not_EQ <- function(X, Y) {!uj::is_EQ(X, Y)}

#' @rdname failsafe
#' @export
not_GE <- function(X, Y) {!uj::is_GE(X, Y)}

#' @rdname failsafe
#' @export
not_GT <- function(X, Y) {!uj::is_GT(X, Y)}

#' @rdname failsafe
#' @export
not_LE <- function(X, Y) {!uj::is_LE(X, Y)}

#' @rdname failsafe
#' @export
not_LT <- function(X, Y) {!uj::is_LT(X, Y)}

#' @rdname failsafe
#' @export
not_DIF <- function(X, Y) {!uj::is_DIF(X, Y)}

#' @rdname failsafe
#' @export
not_F <- function(X) {!uj::fs_f(X)}

#' @rdname failsafe
#' @export
not_T <- function(X) {!uj::fs_t(X)}

#' @rdname failsafe
#' @export
not_TF <- function(X) {!uj::fs_tf(X)}

#' @rdname failsafe
#' @export
not_BL <- function(X) {!uj::fs_bl(X)}

#' @rdname failsafe
#' @export
not_NAV <- function(X) {!uj::fs_na(X)}

#' @rdname failsafe
#' @export
not_OKV <- function(X) {!uj::fs_ok(X)}

#' @rdname failsafe
#' @export
not_LG <- function(X) {!uj::fs_lg(X)}

#' @rdname failsafe
#' @export
not_in <- function(X, ...) {!uj::is_in(X, ...)}

#' @rdname failsafe
#' @export
not_mf <- function(X, ...) {!uj::is_mf(X, ...)}

#' @rdname failsafe
#' @export
not_has <- function(X, Y) {!uj::has(X, Y)}

#' @rdname failsafe
#' @export
not_lacks <- function(X, Y) {!uj::lacks(X, Y)}

#' @rdname failsafe
#' @export
not_eq <- function(X, Y) {!uj::is_eq(X, Y)}

#' @rdname failsafe
#' @export
not_ge <- function(X, Y) {!uj::is_ge(X, Y)}

#' @rdname failsafe
#' @export
not_gt <- function(X, Y) {!uj::is_gt(X, Y)}

#' @rdname failsafe
#' @export
not_le <- function(X, Y) {!uj::is_le(X, Y)}

#' @rdname failsafe
#' @export
not_lt <- function(X, Y) {!uj::is_lt(X, Y)}

#' @rdname failsafe
#' @export
not_dif <- function(X, Y) {!uj::is_dif(X, Y)}

#' @rdname failsafe
#' @export
not_f <- function(X) {!uj::is_f(X)}

#' @rdname failsafe
#' @export
not_t <- function(X) {!uj::is_t(X)}

#' @rdname failsafe
#' @export
not_tf <- function(X) {!uj::is_tf(X)}

#' @rdname failsafe
#' @export
not_bl <- function(X) {!uj::is_bl(X)}

#' @rdname failsafe
#' @export
not_nav <- function(X) {!uj::is_nav(X)}

#' @rdname failsafe
#' @export
not_okv <- function(X) {!uj::is_okv(X)}

#' @rdname failsafe
#' @export
not_seq <- function(X, Y) {!uj::is_seq(X, Y)}

#' @rdname failsafe
#' @export
not_veq <- function(X, Y) {!uj::is_veq(X, Y)}

#' @rdname failsafe
#' @export
not_sdif <- function(X, Y) {!uj::is_sdif(X, Y)}

#' @rdname failsafe
#' @export
not_vdif <- function(X, Y) {!uj::is_vdif(X, Y)}

#' @rdname failsafe
#' @export
n_in <- function(X, ...) {uj::nw(uj::is_in(X, ...))}

#' @rdname failsafe
#' @export
n_mf <- function(X, ...) {uj::nw(uj::is_mf(X, ...))}

#' @rdname failsafe
#' @export
n_has <- function(X, Y) {uj::nw(uj::has(X, Y))}

#' @rdname failsafe
#' @export
n_lacks <- function(X, Y) {uj::nw(uj::lacks(X, Y))}

#' @rdname failsafe
#' @export
n_eq <- function(X, Y) {uj::nw(uj::is_eq(X, Y))}

#' @rdname failsafe
#' @export
n_ge <- function(X, Y) {uj::nw(uj::is_ge(X, Y))}

#' @rdname failsafe
#' @export
n_gt <- function(X, Y) {uj::nw(uj::is_gt(X, Y))}

#' @rdname failsafe
#' @export
n_le <- function(X, Y) {uj::nw(uj::is_le(X, Y))}

#' @rdname failsafe
#' @export
n_lt <- function(X, Y) {uj::nw(uj::is_lt(X, Y))}

#' @rdname failsafe
#' @export
n_dif <- function(X, Y) {uj::nw(uj::is_dif(X, Y))}

#' @rdname failsafe
#' @export
n_f <- function(X) {uj::nw(uj::is_f(X))}

#' @rdname failsafe
#' @export
n_t <- function(X) {uj::nw(uj::is_t(X))}

#' @rdname failsafe
#' @export
n_tf <- function(X) {uj::nw(uj::is_tf(X))}

#' @rdname failsafe
#' @export
n_bl <- function(X) {uj::nw(uj::is_bl(X))}

#' @rdname failsafe
#' @export
n_nav <- function(X) {uj::nw(uj::is_nav(X))}

#' @rdname failsafe
#' @export
n_okv <- function(X) {uj::nw(uj::is_okv(X))}

#' @rdname failsafe
#' @export
none_f <- function(X) {uj::n_f(X) == 0}

#' @rdname failsafe
#' @export
none_t <- function(X) {uj::n_t(X) == 0}

#' @rdname failsafe
#' @export
none_tf <- function(X) {uj::n_tf(X) == 0}

#' @rdname failsafe
#' @export
none_bl <- function(X) {uj::n_bl(X) == 0}

#' @rdname failsafe
#' @export
none_eq <- function(X, Y) {uj::n_eq(X, Y) == 0}

#' @rdname failsafe
#' @export
none_ge <- function(X, Y) {uj::n_ge(X, Y) == 0}

#' @rdname failsafe
#' @export
none_gt <- function(X, Y) {uj::n_gt(X, Y) == 0}

#' @rdname failsafe
#' @export
none_le <- function(X, Y) {uj::n_le(X, Y) == 0}

#' @rdname failsafe
#' @export
none_lt <- function(X, Y) {uj::n_lt(X, Y) == 0}

#' @rdname failsafe
#' @export
none_in <- function(X, ...) {uj::n_in(X, ...) == 0}

#' @rdname failsafe
#' @export
none_mf <- function(X, ...) {uj::n_mf(X, ...) == 0}

#' @rdname failsafe
#' @export
none_dif <- function(X, Y) {uj::n_dif(X, Y) == 0}

#' @rdname failsafe
#' @export
none_nav <- function(X) {uj::n_nav(X) == 0}

#' @rdname failsafe
#' @export
none_okv <- function(X) {uj::n_okv(X) == 0}

#' @rdname failsafe
#' @export
has_none <- function(X, Y) {uj::n_has(X, Y) == 0}

#' @rdname failsafe
#' @export
lacks_none <- function(X, Y) {uj::n_lacks(X, Y) == 0}

#' @rdname failsafe
#' @export
any_f <- function(X) {uj::n_f(X) > 0}

#' @rdname failsafe
#' @export
any_t <- function(X) {uj::n_t(X) > 0}

#' @rdname failsafe
#' @export
any_bl <- function(X) {uj::n_bl(X) > 0}

#' @rdname failsafe
#' @export
any_eq <- function(X, Y) {uj::n_eq(X, Y) > 0}

#' @rdname failsafe
#' @export
any_ge <- function(X, Y) {uj::n_ge(X, Y) > 0}

#' @rdname failsafe
#' @export
any_gt <- function(X, Y) {uj::n_gt(X, Y) > 0}

#' @rdname failsafe
#' @export
any_in <- function(X, ...) {uj::n_in(X, ...) > 0}

#' @rdname failsafe
#' @export
any_le <- function(X, Y) {uj::n_le(X, Y) > 0}

#' @rdname failsafe
#' @export
any_lt <- function(X, Y) {uj::n_lt(X, Y) > 0}

#' @rdname failsafe
#' @export
any_mf <- function(X, ...) {uj::n_mf(X, ...) > 0}

#' @rdname failsafe
#' @export
any_tf <- function(X) {uj::n_tf(X) > 0}

#' @rdname failsafe
#' @export
any_dif <- function(X, Y) {uj::n_dif(X, Y) > 0}

#' @rdname failsafe
#' @export
any_nav <- function(X) {uj::n_nav(X) > 0}

#' @rdname failsafe
#' @export
any_okv <- function(X) {uj::n_okv(X) > 0}

#' @rdname failsafe
#' @export
has_any <- function(X, Y) {uj::n_has(X, Y) > 0}

#' @rdname failsafe
#' @export
lacks_any <- function(X, Y) {uj::n_lacks(X, Y) > 0}

#' @rdname failsafe
#' @export
one_f <- function(X) {uj::n_f(X) == 1}

#' @rdname failsafe
#' @export
one_t <- function(X) {uj::n_t(X) == 1}

#' @rdname failsafe
#' @export
one_bl <- function(X) {uj::n_bl(X) == 1}

#' @rdname failsafe
#' @export
one_eq <- function(X, Y) {uj::n_eq(X, Y) == 1}

#' @rdname failsafe
#' @export
one_ge <- function(X, Y) {uj::n_ge(X, Y) == 1}

#' @rdname failsafe
#' @export
one_gt <- function(X, Y) {uj::n_gt(X, Y) == 1}

#' @rdname failsafe
#' @export
one_in <- function(X, ...) {uj::n_in(X, ...) == 1}

#' @rdname failsafe
#' @export
one_le <- function(X, Y) {uj::n_le(X, Y) == 1}

#' @rdname failsafe
#' @export
one_lt <- function(X, Y) {uj::n_lt(X, Y) == 1}

#' @rdname failsafe
#' @export
one_mf <- function(X, ...) {uj::n_mf(X, ...) == 1}

#' @rdname failsafe
#' @export
one_tf <- function(X) {uj::n_tf(X) == 1}

#' @rdname failsafe
#' @export
one_dif <- function(X, Y) {uj::n_dif(X, Y) == 1}

#' @rdname failsafe
#' @export
one_nav <- function(X) {uj::n_nav(X) == 1}

#' @rdname failsafe
#' @export
one_okv <- function(X) {uj::n_okv(X) == 1}

#' @rdname failsafe
#' @export
has_one <- function(X, Y) {uj::n_has(X, Y) == 1}

#' @rdname failsafe
#' @export
lacks_one <- function(X, Y) {uj::n_lacks(X, Y) == 1}

#' @rdname failsafe
#' @export
all_f <- function(X) {base::all(uj::is_f(X))}

#' @rdname failsafe
#' @export
all_t <- function(X) {base::all(uj::is_t(X))}

#' @rdname failsafe
#' @export
all_bl <- function(X) {base::all(uj::is_bl(X))}

#' @rdname failsafe
#' @export
all_eq <- function(X, Y) {base::all(uj::is_eq(X, Y))}

#' @rdname failsafe
#' @export
all_ge <- function(X, Y) {base::all(uj::is_ge(X, Y))}

#' @rdname failsafe
#' @export
all_gt <- function(X, Y) {base::all(uj::is_gt(X, Y))}

#' @rdname failsafe
#' @export
all_in <- function(X, ...) {base::all(uj::is_in(X, ...))}

#' @rdname failsafe
#' @export
all_le <- function(X, Y) {base::all(uj::is_le(X, Y))}

#' @rdname failsafe
#' @export
all_lt <- function(X, Y) {base::all(uj::is_lt(X, Y))}

#' @rdname failsafe
#' @export
all_mf <- function(X, ...) {base::all(uj::is_mf(X, ...))}

#' @rdname failsafe
#' @export
all_tf <- function(X) {base::all(uj::is_tf(X))}

#' @rdname failsafe
#' @export
all_dif <- function(X, Y) {base::all(uj::is_dif(X, Y))}

#' @rdname failsafe
#' @export
all_nav <- function(X) {base::all(uj::is_nav(X))}

#' @rdname failsafe
#' @export
all_okv <- function(X) {base::all(uj::is_okv(X))}

#' @rdname failsafe
#' @export
has_all <- function(X, Y) {base::all(uj::has(X, Y))}

#' @rdname failsafe
#' @export
lacks_all <- function(X, Y) {base::all(uj::lacks(X, Y))}

#' @rdname failsafe
#' @export
some_f <- function(X) {uj::n_f(X) > 1}

#' @rdname failsafe
#' @export
some_t <- function(X) {uj::n_t(X) > 1}

#' @rdname failsafe
#' @export
some_bl <- function(X) {uj::n_bl(X) > 1}

#' @rdname failsafe
#' @export
some_eq <- function(X, Y) {uj::n_eq(X, Y) > 1}

#' @rdname failsafe
#' @export
some_le <- function(X, Y) {uj::n_le(X, Y) > 1}

#' @rdname failsafe
#' @export
some_ge <- function(X, Y) {uj::n_ge(X, Y) > 1}

#' @rdname failsafe
#' @export
some_gt <- function(X, Y) {uj::n_gt(X, Y) > 1}

#' @rdname failsafe
#' @export
some_in <- function(X, ...) {uj::n_in(X, ...) > 1}

#' @rdname failsafe
#' @export
some_lt <- function(X, Y) {uj::n_lt(X, Y) > 1}

#' @rdname failsafe
#' @export
some_mf <- function(X, ...) {uj::n_mf(X, ...) > 1}

#' @rdname failsafe
#' @export
some_tf <- function(X) {uj::n_tf(X) > 1}

#' @rdname failsafe
#' @export
some_dif <- function(X, Y) {uj::n_dif(X, Y) > 1}

#' @rdname failsafe
#' @export
some_nav <- function(X) {uj::n_nav(X) > 1}

#' @rdname failsafe
#' @export
some_okv <- function(X) {uj::n_okv(X) > 1}

#' @rdname failsafe
#' @export
has_some <- function(X, Y) {uj::n_has(X, Y) > 1}

#' @rdname failsafe
#' @export
lacks_some <- function(X, Y) {uj::n_lacks(X, Y) > 1}

#' @rdname failsafe
#' @export
two_f <- function(X) {uj::n_f(X) == 2}

#' @rdname failsafe
#' @export
two_t <- function(X) {uj::n_t(X) == 2}

#' @rdname failsafe
#' @export
two_bl <- function(X) {uj::n_bl(X) == 2}

#' @rdname failsafe
#' @export
two_eq <- function(X, Y) {uj::n_eq(X, Y) == 2}

#' @rdname failsafe
#' @export
two_ge <- function(X, Y) {uj::n_ge(X, Y) == 2}

#' @rdname failsafe
#' @export
two_gt <- function(X, Y) {uj::n_gt(X, Y) == 2}

#' @rdname failsafe
#' @export
two_in <- function(X, ...) {uj::n_in(X, ...) == 2}

#' @rdname failsafe
#' @export
two_le <- function(X, Y) {uj::n_le(X, Y) == 2}

#' @rdname failsafe
#' @export
two_lt <- function(X, Y) {uj::n_lt(X, Y) == 2}

#' @rdname failsafe
#' @export
two_mf <- function(X, ...) {uj::n_mf(X, ...) == 2}

#' @rdname failsafe
#' @export
two_tf <- function(X) {uj::n_tf(X) == 2}

#' @rdname failsafe
#' @export
two_dif <- function(X, Y) {uj::n_dif(X, Y) == 2}

#' @rdname failsafe
#' @export
two_nav <- function(X) {uj::n_nav(X) == 2}

#' @rdname failsafe
#' @export
two_okv <- function(X) {uj::n_okv(X) == 2}

#' @rdname failsafe
#' @export
has_two <- function(X, Y) {uj::n_has(X, Y) == 2}

#' @rdname failsafe
#' @export
lacks_two <- function(X, Y) {uj::n_lacks(X, Y) == 2}

#' @rdname failsafe
#' @export
many_f <- function(X) {uj::n_f(X) > 2}

#' @rdname failsafe
#' @export
many_t <- function(X) {uj::n_t(X) > 2}

#' @rdname failsafe
#' @export
many_bl <- function(X) {uj::n_bl(X) > 2}

#' @rdname failsafe
#' @export
many_eq <- function(X, Y) {uj::n_eq(X, Y) > 2}

#' @rdname failsafe
#' @export
many_ge <- function(X, Y) {uj::n_ge(X, Y) > 2}

#' @rdname failsafe
#' @export
many_gt <- function(X, Y) {uj::n_gt(X, Y) > 2}

#' @rdname failsafe
#' @export
many_in <- function(X, ...) {uj::n_in(X, ...) > 2}

#' @rdname failsafe
#' @export
many_le <- function(X, Y) {uj::n_le(X, Y) > 2}

#' @rdname failsafe
#' @export
many_lt <- function(X, Y) {uj::n_lt(X, Y) > 2}

#' @rdname failsafe
#' @export
many_mf <- function(X, ...) {uj::n_mf(X, ...) > 2}

#' @rdname failsafe
#' @export
many_tf <- function(X) {uj::n_tf(X) > 2}

#' @rdname failsafe
#' @export
many_dif <- function(X, Y) {uj::n_dif(X, Y) > 2}

#' @rdname failsafe
#' @export
many_nav <- function(X) {uj::n_nav(X) > 2}

#' @rdname failsafe
#' @export
many_okv <- function(X) {uj::n_okv(X) > 2}

#' @rdname failsafe
#' @export
has_many <- function(X, Y) {uj::n_has(X, Y) > 2}

#' @rdname failsafe
#' @export
lacks_many <- function(X, Y) {uj::n_lacks(X, Y) > 2}

#' @rdname failsafe
#' @export
`%.IN.%` <- function(X, Y) {uj::is_IN(X, Y)}

#' @rdname failsafe
#' @export
`%.MF.%` <- function(X, Y) {uj::is_MF(X, Y)}

#' @rdname failsafe
#' @export
`%.EQ.%` <- function(X, Y) {uj::is_EQ(X, Y)}

#' @rdname failsafe
#' @export
`%.GE.%` <- function(X, Y) {uj::is_GE(X, Y)}

#' @rdname failsafe
#' @export
`%.GT.%` <- function(X, Y) {uj::is_GT(X, Y)}

#' @rdname failsafe
#' @export
`%.LE.%` <- function(X, Y) {uj::is_LE(X, Y)}

#' @rdname failsafe
#' @export
`%.LT.%` <- function(X, Y) {uj::is_LT(X, Y)}

#' @rdname failsafe
#' @export
`%.HAS.%` <- function(X, Y) {uj::has0(X, Y)}

#' @rdname failsafe
#' @export
`%.DIF.%` <- function(X, Y) {uj::is_DIF(X, Y)}

#' @rdname failsafe
#' @export
`%.LACKS.%` <- function(X, Y) {uj::LACKS(X, Y)}

#' @rdname failsafe
#' @export
`%.in.%` <- function(X, Y) {uj::is_in(X, Y)}

#' @rdname failsafe
#' @export
`%.mf.%` <- function(X, Y) {uj::is_mf(X, Y)}

#' @rdname failsafe
#' @export
`%.eq.%` <- function(X, Y) {uj::is_eq(X, Y)}

#' @rdname failsafe
#' @export
`%.ge.%` <- function(X, Y) {uj::is_ge(X, Y)}

#' @rdname failsafe
#' @export
`%.gt.%` <- function(X, Y) {uj::is_gt(X, Y)}

#' @rdname failsafe
#' @export
`%.le.%` <- function(X, Y) {uj::is_le(X, Y)}

#' @rdname failsafe
#' @export
`%.lt.%` <- function(X, Y) {uj::is_lt(X, Y)}

#' @rdname failsafe
#' @export
`%.has.%` <- function(X, Y) {uj::has(X, Y)}

#' @rdname failsafe
#' @export
`%.dif.%` <- function(X, Y) {uj::is_dif(X, Y)}

#' @rdname failsafe
#' @export
`%.lacks.%` <- function(X, Y) {uj::lacks(X, Y)}

#' @rdname failsafe
#' @export
`%.seq.%` <- function(X, Y) {uj::is_seq(X, Y)}

#' @rdname failsafe
#' @export
`%.veq.%` <- function(X, Y) {uj::is_veq(X, Y)}

#' @rdname failsafe
#' @export
`%.neq.%` <- function(X, Y) {if (uj::is_err(X) | uj::is_err(Y)) {F} else {base::length(X) == base::length(Y)}}

#' @rdname failsafe
#' @export
`%.sdif.%` <- function(X, Y) {uj::is_sdif(X, Y)}

#' @rdname failsafe
#' @export
`%.vdif.%` <- function(X, Y) {uj::is_vdif(X, Y)}

#' @rdname failsafe
#' @export
`%.ndif.%` <- function(X, Y) {uj::ndif(X, Y)}

#' @rdname failsafe
#' @export
`%.OR.%` <- function(X, Y) {uj::OR(X, Y)}

#' @rdname failsafe
#' @export
`%.AND.%` <- function(X, Y) {uj::AND(X, Y)}

#' @rdname failsafe
#' @export
`%.XOR.%` <- function(X, Y) {uj::XOR(X, Y)}

#' @rdname failsafe
#' @export
`%.NOR.%` <- function(X, Y) {uj::NOR(X, Y)}

#' @rdname failsafe
#' @export
`%.or.%` <- function(X, Y) {uj::or(X, Y)}

#' @rdname failsafe
#' @export
`%.and.%` <- function(X, Y) {uj::and(X, Y)}

#' @rdname failsafe
#' @export
`%.xor.%` <- function(X, Y) {
  if (uj::is_err(X) | uj::is_err(Y)) {return(F)} else if (!base::is.logical(X) | !base::is.logical(Y)) {return(F)}
  nXY <- base::c(base::length(X), base::length(Y))
  Valid <- base::c(1, base::max(nXY))
  if (!base::all(nXY %in% Valid)) {F} else {base::xor(X, Y)}
}

#' @rdname failsafe
#' @export
`%.nor.%` <- function(X, Y) {uj::nor(X, Y)}
