#' @encoding UTF-8
#' @family extensions
#' @family logicals
#' @family errs
#' @title failsafe functions that **always** return a valid object.
#' @description Evaluate objects, test if an object has certain properties, and conduct binary logical operations safely. these functions never stop execution; they always produce a valid result, even if that result is an error object.
#' @section failsafe error management functions: these functions evaluate generate, check for, and manage error objects.
#' \tabular{ll}{  `make_err`   \tab generate object of class `'simpleError'`.      \cr
#'                `msg_err`    \tab get error message, if any_; otherwise, `NULL`. \cr
#'                `not_err`    \tab Does evaluating `x` not_ produce an error?.    \cr
#'                `is_err`     \tab Does evaluating `x` produce an error?           }
#' @section failsafe check for classes of objects with values:
#' \tabular{ll}{  `failsafe`   \tab Returns `x` if identity evaluation does not_ cause an error. Returns
#'                                  an error object otherwise with the attribute `stack = uj::callers()`. \cr   \tab   \cr
#'                `fs_null`    \tab `x` is `NULL`.                                                        \cr
#'                `fs_def`     \tab `x` is defined (not_ `NULL`).                                         \cr   \tab   \cr
#'                `fs_scl`     \tab `x` is scalar of a specific atomic value.                             \cr
#'                `fs_t`       \tab `x` is scalar `TRUE`.                                                 \cr
#'                `fs_f`       \tab `x` is scalar `FALSE`.                                                \cr
#'                `fs_na`      \tab `x` is scalar `NA`.                                                   \cr
#'                `fs_tf`      \tab `x` is scalar `TRUE` or scalar `FALSE`.                               \cr
#'                `scl_f`      \tab `x` is scalar `FALSE`                                                 \cr
#'                `scl_t`      \tab `x` is scalar `TRUE`                                                  \cr
#'                `scl_tf`     \tab `x` is scalar `TRUE` or `FALSE`.                                      \cr
#'                `scl_lg`     \tab `x` is scalar `TRUE`, `FALSE`, or `NA`.                               \cr
#'                `scl_bl`     \tab `x` is blank string scalar (`""`).                                    \cr
#'                `scl_na`     \tab `x` is scalar `NA`                                                    \cr   \tab   \cr
#'                `fs_vec`     \tab Elements of `x` are of a specific atomic value.                       \cr
#'                `vec_f`      \tab Elements of `x` are `FALSE`.                                          \cr
#'                `vec_t`      \tab Elements of `x` are `TRUE`.                                           \cr
#'                `vec_tf`     \tab Elements of `x` are `TRUE` or `FALSE`.                                \cr
#'                `vec_lg`     \tab Elements of `x` are `TRUE`, `FALSE`, or `NA`.                         \cr
#'                `vec_bl`     \tab Elements of `x` are blank strings.                                    \cr
#'                `vec_na`     \tab Elements of `x` are `NA`.                                               }
#' @section failsafe forced-evaluation functions with conditional return values: these functions \link[base:force]{force} evaluation of `x` returning values as shown in the following table with the first value returned when forcing evaluation does not_ produce an error and the second when it does:
#' \tabular{ll}{  `fs_or`    \tab `x` or something else if evaluating `x` produces an error \cr   \tab   \cr
#'                `or_f`     \tab `x` or `FALSE`                                            \cr
#'                `or_t`     \tab `x` or `TRUE`                                             \cr   \tab   \cr
#'                `or_c0`    \tab `x` or `character(0)`                                     \cr
#'                `or_i0`    \tab `x` or `integer(0)`                                       \cr
#'                `or_l0`    \tab `x` or `logical(0)`                                       \cr
#'                `or_n0`    \tab `x` or `numeric(0)`                                       \cr   \tab   \cr
#'                `or_bl`    \tab `x` or `""`                                               \cr   \tab   \cr
#'                `or_na`    \tab `x` or `NA`                                               \cr
#'                `or_nac`   \tab `x` or `Na_character_`                                    \cr
#'                `or_nai`   \tab `x` or `Na_integer_`                                      \cr
#'                `or_nal`   \tab `x` or `NA` (logical)                                     \cr
#'                `or_nar`   \tab `x` or `Na_real_`                                           }
#' @details failsafe binary comparison functions: these are binary functions that always produce either scalar `TRUE` or scalar `FALSE`. they return `TRUE` when two variables meet the conditions shown in the following table and `FALSE` in every other circumstance (where `fx` and `fy`)
#' \tabular{ll}{  `%.is.%`       \tab `failsafe(identical(x, y))`                                  \cr
#'                `%.isnt.%`     \tab `!(x %.is.% y)`                                              \cr   \tab   \cr
#'                `%.in.%`       \tab `sapply(failsafe(x %in% y), isTRUE)`                         \cr
#'                `%.mf.%`       \tab `!(x %.in.% y)` (missing from)                               \cr
#'                `%.has.%`      \tab `y %.in.% x` (`x` has each `y` value)                        \cr
#'                `%.lacks.%`    \tab `y %.mf.% x` (`x` lacks each `y` value)                      \cr   \tab   \cr
#'                `%.in0.%`      \tab if `x` is atomic scalar, `x %.in.% y`, otherwise `FALSE`     \cr
#'                `%.mf0.%`      \tab `x` is atomic scalar and `x %.mf.% y`                        \cr
#'                `%.has0.%`     \tab `y` is atomic scalar and `x %.has.% y`                       \cr
#'                `%.lacks0.%`   \tab `y` is atomic scalar and `x %.lacks.% y`                     \cr   \tab   \cr
#'                `%.eq.%`       \tab If `x` and `y` match\eqn{^{(1)}}, `x = y`, otherwise `FALSE` \cr
#'                `%.ge.%`       \tab If `x` and `y` match\eqn{^{(1)}}, `x ≥ y`, otherwise `FALSE` \cr
#'                `%.gt.%`       \tab If `x` and `y` match\eqn{^{(1)}}, `x > y`, otherwise `FALSE` \cr
#'                `%.le.%`       \tab If `x` and `y` match\eqn{^{(1)}}, `x ≤ y`, otherwise `FALSE` \cr
#'                `%.lt.%`       \tab If `x` and `y` match\eqn{^{(1)}}, `x < y`, otherwise `FALSE` \cr
#'                `%.dif.%`      \tab If `x` and `y` match\eqn{^{(1)}}, `x ≠ y`, otherwise `TRUE`  \cr   \tab   \cr
#'                `%.eq0.%`      \tab `x` and `y` are atomic scalar and `x = y`  \eqn{^{(2)}}      \cr
#'                `%.ge0.%`      \tab `x` and `y` are atomic scalar and `x ≥ y`  \eqn{^{(2)}}      \cr
#'                `%.gt0.%`      \tab `x` and `y` are atomic scalar and `x > y`  \eqn{^{(2)}}      \cr
#'                `%.le0.%`      \tab `x` and `y` are atomic scalar and `x ≤ y`  \eqn{^{(2)}}      \cr
#'                `%.lt0.%`      \tab `x` and `y` are atomic scalar and `x ≥ y`  \eqn{^{(2)}}      \cr
#'                `%.dif0.%`     \tab `x` and `y` are *not* atomic scalar equal  \eqn{^{(3)}}      \cr   \tab   \cr
#'                `%.seq.%`      \tab `x` and `y` are \code{\link[base]{setequal}}\eqn{^{(4)}}     \cr
#'                `%.veq.%`      \tab `x` and `y` are vector equal\eqn{^{(5)}}                     \cr
#'                `%.sdif.%`     \tab `x` and `y` are set different\eqn{^{(6)}}                    \cr
#'                `%.vdif.%`     \tab `x` and `y` are vector different\eqn{^{(7)}}                 \cr   \tab   \cr
#'                `%.or.%`       \tab values of `x` *and/or* `y` are `TRUE`                        \cr
#'                `%.and.%`      \tab values of `x` *and* `y` are `TRUE`                           \cr
#'                `%.xor.%`      \tab values of *either* `x` *or* `y` are `TRUE`                   \cr
#'                `%.nor.%`      \tab values of *neither* `x` *nor* `y` are `TRUE`                 \cr   \tab   \cr
#'                `%.or0.%`      \tab `x` *and/or* `y` are scalar `TRUE`                           \cr
#'                `%.and0.%`     \tab `x` *and* `y` are scalar `TRUE`                              \cr
#'                `%.xor0.%`     \tab *either* `x` *or* `y` is scalar `TRUE`                       \cr
#'                `%.nor0.%`     \tab *neither* `x` *nor* `y` is scalar `TRUE`                     \cr   \tab     }
#'  \tabular{l}{  \eqn{^{(1)}} Of equal length and \link[=compatible]{compatible modes}.                                                       \cr
#'                \eqn{^{(2)}} Of \link[=compatible]{compatible modes} and meeting the (in)equality.                                           \cr
#'                \eqn{^{(3)}} not meeting the requirements of `%.eq0.%`.                                                                      \cr
#'                \eqn{^{(4)}} atomic, of the same length, with the same values in the same order.                                             \cr
#'                \eqn{^{(5)}} atomic, possibly of different lengths, and containing the same unique values regardless of order or duplicates. \cr
#'                \eqn{^{(6)}} not meeting the requirements of `%.seq.%`.                                                                      \cr
#'                \eqn{^{(7)}} not meeting the requirements of `%.veq.%`.                                                                        }
#' @section failsafe scalar value, membership, and equality/inequality checking functions: these functions check for class, mode, and/or value and/or count the number of checks passed. they *always* produce `TRUE`, `FALSE`, or an integer scalar.
#' \cr\cr function names are constructed of root words, prefixes, and/or suffixes. Root word specify the type of check conducted. Prefixes and suffixes specify how to modify the results of a check or how to apply the check to each `...` argument and check whether a certain number of `...` args passed the checks.
#' \cr\cr \strong{*Root words for atomic scalar value checking functions*}
#' \tabular{ll}{  `nav`   \tab `x` is atomic and values are `NA`                      \cr
#'                `okv`   \tab `x` is atomic and values are *not* `NA`                \cr
#'                `bl`    \tab `x` is atomic and values are blanks (`""`)             \cr
#'                `lg`    \tab `x` is atomic and values are `TRUE`, `FALSE`, or `NA`. \cr
#'                `tf`    \tab `x` is atomic and values are `TRUE` or `FALSE`.        \cr
#'                `f`     \tab `x` is atomic and values are `TRUE`.                   \cr
#'                `t`     \tab `x` is atomic and valules are `FALSE`.                   }
#' \cr \strong{*Root words for atomic value membership checking functions*}
#' \tabular{ll}{  `lacks`   \tab `x %.lacks.% y` \cr
#'                `has`     \tab `x %.has.% y`   \cr
#'                `in`      \tab `x %.in.% y`    \cr
#'                `mf`      \tab `x %.mf.% y`      }
#' \cr \strong{*Root words for equality/inequality checking functions*}
#' \tabular{ll}{  `id`     \tab objects are identical                \cr
#'                `eq`     \tab values are equal                     \cr
#'                `seq`    \tab values are set equal                 \cr
#'                `veq`    \tab values are vector equal              \cr
#'                `dif`    \tab values are different                 \cr
#'                `sdif`   \tab values are set different             \cr
#'                `vdif`   \tab values are vector different          \cr
#'                `ge`     \tab `x` is greater than or equal to `y`. \cr
#'                `gt`     \tab `x` is greater than `y`.             \cr
#'                `le`     \tab `x` is less than or equal to `y`.    \cr
#'                `lt`     \tab `x` is less than `y`.                  }
#' \cr \strong{*Modifier prefixes/suffixes*}
#' \tabular{ll}{  `n`      \tab count `...` args passing the check. \cr
#'                `is`     \tab Keep the result of a check as is.   \cr
#'                `not`    \tab Negate the result of a check.         }
#' \cr \strong{*apply-and-sweep prefixes/suffixes evaluating whether a certain number of checks were passed*}
#' \cr\cr the following table contains prefixes in the first column, and in the second, the number of checks that must be passed to return `TRUE`.
#' \tabular{ll}{  `none`   \tab `0` values passed the check.   \cr
#'                `any`    \tab `> 0` values passed the check. \cr
#'                `one`    \tab `1` value passed the check.    \cr
#'                `some`   \tab `> 1` values passed the check. \cr
#'                `two`    \tab `2` values passed the check.   \cr
#'                `many`   \tab `> 2` values passed the check. \cr
#'                `all`    \tab all values passed the check.    }
#' \cr \strong{*Identity-equality, set-equality and vector-equality checking functions*}
#' \tabular{lll}{             \tab **is**       \tab **not**    \cr
#'                 **id**     \tab `is_id`      \tab `not_id`   \cr
#'                 **seq**    \tab `is_seq`     \tab `not_deq`  \cr
#'                 **veq**    \tab `is_veq`     \tab `not_veq`  \cr
#'                 **sdif**   \tab `is_sdif`    \tab `not_sdif` \cr
#'                 **vdif**   \tab `is_vdif`    \tab `not_vdif`   }
#' \cr \strong{*atomic scalar value checking functions*}
#' \tabular{lll}{              \tab **is**      \tab **not**    \cr
#'                 **TRUE**    \tab `is_t0`     \tab `not_t0`   \cr
#'                 **FALSE**   \tab `is_f0`     \tab `not_f0`   \cr
#'                 **tf**      \tab `is_tf0`    \tab `not_tf0`  \cr
#'                 **lg**      \tab `is_lg0`    \tab `not_lg0`  \cr
#'                 **bl**      \tab `is_bl0`    \tab `not_bl0`  \cr
#'                 **eq**      \tab `is_eq0`    \tab `not_eq0`  \cr
#'                 **ge**      \tab `is_ge0`    \tab `not_ge0`  \cr
#'                 **gt**      \tab `is_gt0`    \tab `not_gt0`  \cr
#'                 **le**      \tab `is_le0`    \tab `not_le0`  \cr
#'                 **lt**      \tab `is_lt0`    \tab `not_lt0`  \cr
#'                 **nav**     \tab `is_nav`    \tab `not_nav`  \cr
#'                 **okv**     \tab `is_okv`    \tab `not_okv`  \cr
#'                 **dif**     \tab `is_dif0`   \tab `not_dif0`   }
#' \cr \strong{*atomic scalar value membership checking functions*}
#' \tabular{lll}{              \tab **is**      \tab **not**    \cr
#'                 **in**      \tab `is_in0`    \tab `not_in0`  \cr
#'                 **mf**      \tab `is_mf0`    \tab `not_mf0`  \cr
#'                 **has**     \tab `has0`      \tab `not_has0` \cr
#'                 **lacks**   \tab `lacks0`    \tab `not_lacks0` }
#' \cr \strong{*atomic value checking functions*}
#' \tabular{lllllllllll}{              \tab **is**     \tab **n**     \tab **not_**       \tab **none**       \tab **one**       \tab **two**       \tab **any**       \tab  **some**      \tab  **many**      \tab  **all_** \cr
#'                         **TRUE**    \tab `is_t`     \tab `n_t`     \tab `not_t`        \tab `none_t`       \tab `one_t`       \tab `two_t`       \tab `any_t`       \tab `some_t`       \tab `many_t`       \tab `all_t`   \cr
#'                         **FALSE**   \tab `is_f`     \tab `n_f`     \tab `not_f`        \tab `none_f`       \tab `one_f`       \tab `two_f`       \tab `any_f`       \tab `some_f`       \tab `many_f`       \tab `all_f`   \cr
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
#' \cr \strong{*atomic value membership checking functions*}
#' \tabular{lllllllllll}{              \tab **is**    \tab **n**       \tab **not_**      \tab **none**       \tab **one**       \tab **two**       \tab **any**       \tab **some**       \tab **many**       \tab **all**   \cr
#'                         **in**      \tab `is_in`   \tab `n_in`      \tab `not_in`      \tab `in_none`      \tab `in_one`      \tab `in_two`      \tab `in_any`      \tab `in_some`      \tab `in_many`      \tab `in_all`  \cr
#'                         **mf**      \tab `is_mf`   \tab `n_mf`      \tab `not_mf`      \tab `mf_none`      \tab `mf_one`      \tab `mf_two`      \tab `mf_any`      \tab `mf_some`      \tab `mf_many`      \tab `mf_all`  \cr
#'                         **has**     \tab `has`     \tab `n_has`     \tab `not_has`     \tab `has_none`     \tab `has_one`     \tab `has_two`     \tab `has_any`     \tab `has_some`     \tab `has_many`     \tab `has_all` \cr
#'                         **lacks**   \tab `locks`   \tab `n_lacks`   \tab `not_lacks`   \tab `lacks_none`   \tab `lacks_one`   \tab `lacks_two`   \tab `lacks_any`   \tab `lacks_some`   \tab `lacks_many`   \tab `lacks_all` }
#' @return A logical scalar or vector.
#' @param x,y Any objects for failsafe binary functions and miscellaneous failsafe functions.
#' @param x An object or a call_ to evaluate in the environment of a parent function where the initial call_ was made.
#' @param val An atomic scalar.
#' @param or An object to return if evaluating `x` produces an error.
#' @examples
#' egfailsafe <- function() {
#'   abc <- c("a", "b", "c")
#'   abc <- abc
#'   blank <- ""
#'   mss.scl <- Na
#'   lgl.scl <- FALSE
#'   fac.scl <- factor("q", levels = c("x", "q"))
#'   fac.vec <- factor(c("x", "q"), levels = c("x", "q"))
#'   chr.mat <- matrix(c("a", "b", "c", "Na"), nrow = 2)
#'   chr.dtf <- data.frame(abc = letters[1:3], def = letters[4:6])
#'   num.vls <- list(first3 = c(1:3, NA), next3 = c(4:6, NA))
#'   combo <- list(mss.scl, lgl.scl, fac.vec, chr.mat, chr.dtf, numvls)
#'   attr(abc, "custom") <- "custom"
#'   list(`"a" %.and0.% FALSE`                   = "a" %.and.% FALSE                   ,
#'        `"a" %.in0.% abc`                      = "a" %.in.% abc                      ,
#'        `"a" %.nor0.% FALSE`                   = "a" %.or.% FALSE                    ,
#'        `"a" %.xor0.% FALSE`                   = "a" %.cor.% FALSE                   ,
#'        `"a" %.or0.% FALSE`                    = "a" %.or.% FALSE                    ,
#'        `"a" %.mf0.TRUE% abc`                  = "a" %.mf.% abc                      ,
#'        `"a" %.and.% FALSE`                    = "a" %.and.% FALSE                   ,
#'        `abs %.in.% abc`                       = "a" %.in.% abc                      ,
#'        `"a" %.nor.% FALSE`                    = "a" %.or.% FALSE                    ,
#'        `"a" %.xor.% FALSE`                    = "a" %.cor.% FALSE                   ,
#'        `"a" %.or.% FALSE`                     = "a" %.or.% FALSE                    ,
#'        `"a" %.mf.% abc`                       = "a" %.mf.% abc                      ,
#'        `1 %.in.% abc`                         = 1 %.in.% abc                        ,
#'        `1 %.mf.% abc`                         = 1 %.mf.% abc                        ,
#'        `abc %.eq0.% abc`                      = abc %.eq0.% abc                     ,
#'        `abc %.eq0.% letters[1:3]`             = abc %.eq0.% letters[1:3]            ,
#'        `abc %.eq0.% NULL`                     = abc %.eq0.% NULL                    ,
#'        `abc %.has0.% "a"`                     = abc %.has.% "a"                     ,
#'        `abc %.has0.% 1`                       = abc %.has.% 1                       ,
#'        `abc %.has0.% NULL`                    = abc %.has.% NULL                    ,
#'        `abc %.has.% letters[1:6]`             = abc %.has.% "a"                     ,
#'        `abc %.has.% 1:3`                      = abc %.has.% 1                       ,
#'        `abc %.has.% NULL`                     = abc %.has.% NULL                    ,
#'        `abc %.is.% NULL`                      = abc %.is.% NULL                     ,
#'        `abc %.lacks0.% "a"`                   = abc %.lacks.% "a"                   ,
#'        `abc %.lacks0.% 1`                     = abc %.lacks.% 1                     ,
#'        `abc %.lacks0.% NULL`                  = abc %.lacks.% NULL                  ,
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
#'        `is_bl0("")`                           = is_bl0("")                          ,
#'        `is_bl0("a")`                          = is_bl0("a")                         ,
#'        `is_bl0(c("", ""))`                    = is_bl0(c("", ""))                   ,
#'        `is_bl0(NA)`                           = is_bl0(NA)                          ,
#'        `is_dif("", "")`                       = is_dif("", "")                      ,
#'        `is_dif("word", 7)`                    = is_dif("word", 7)                   ,
#'        `is_dif(chr.mat, chr.mat)`             = is_dif(chr.mat, chr.mat)            ,
#'        `is_dif(NA, NA)`                       = is_dif(NA, NA)                      ,
#'        `is_dif0("", "")`                      = is_dif0("", "")                     ,
#'        `is_dif0("word", 7)`                   = is_dif0("word", 7)                  ,
#'        `is_dif0(chr.mat, chr.mat)`            = is_dif0(chr.mat, chr.mat)           ,
#'        `is_dif0(NA, NA)`                      = is_dif0(NA, NA)                     ,
#'        `is_eq("", "")`                        = is_eq("", "")                       ,
#'        `is_eq("word", 7)`                     = is_eq("word", 7)                    ,
#'        `is_eq(chr.mat, chr.mat)`              = is_eq(chr.mat, chr.mat)             ,
#'        `is_eq(NA, NA)`                        = is_eq(NA, NA)                       ,
#'        `is_eq0("", "")`                       = is_eq0("", "")                      ,
#'        `is_eq0("word", 7)`                    = is_eq0("word", 7)                   ,
#'        `is_eq0(chr.mat, chr.mat)`             = is_eq0(chr.mat, chr.mat)            ,
#'        `is_eq0(NA, NA)`                       = is_eq0(NA, NA)                      ,
#'        `is_err(non.existent.variable)`        = is_err(non.existent.variable)       ,
#'        `is_err(pi)`                           = is_err(pi)                          ,
#'        `is_f(c(TRUE, FALSE))`                 = is_f(c(TRUE, FALSE))                ,
#'        `is_f(chr.mat)`                        = is_f(chr.mat)                       ,
#'        `is_f(NA)`                             = is_f(NA)                            ,
#'        `is_f(TRUE)`                           = is_f(TRUE)                          ,
#'        `is_f(TRUE)`                           = is_f(TRUE)                          ,
#'        `is_f0(c(TRUE, FALSE))`                = is_f0(c(TRUE, FALSE))               ,
#'        `is_f0(chr.mat)`                       = is_f0(chr.mat)                      ,
#'        `is_f0(FALSE)`                         = is_f0(FALSE)                        ,
#'        `is_f0(NA)`                            = is_f0(NA)                           ,
#'        `is_f0(TRUE)`                          = is_f0(TRUE)                         ,
#'        `is_ge("a", "b")`                      = is_ge("a", "b")                     ,
#'        `is_ge("b", "a")`                      = is_ge("b", "a")                     ,
#'        `is_ge(1, 1)`                          = is_ge(1, 1)                         ,
#'        `is_ge(chr.mat, chr.mat)`              = is_ge(chr.mat, chr.mat)             ,
#'        `is_ge0("a", "b")`                     = is_ge0("a", "b")                    ,
#'        `is_ge0("b", "a")`                     = is_ge0("b", "a")                    ,
#'        `is_ge0(1, 1)`                         = is_ge0(1, 1)                        ,
#'        `is_ge0(chr.mat, chr.mat)`             = is_ge0(chr.mat, chr.mat)            ,
#'        `is_gt("a", "b")`                      = is_gt("a", "b")                     ,
#'        `is_gt("b", "a")`                      = is_gt("b", "a")                     ,
#'        `is_gt(1, 1)`                          = is_gt(1, 1)                         ,
#'        `is_gt(chr.mat, chr.mat)`              = is_gt(chr.mat, chr.mat)             ,
#'        `is_gt0("a", "b")`                     = is_gt0("a", "b")                    ,
#'        `is_gt0("b", "a")`                     = is_gt0("b", "a")                    ,
#'        `is_gt0(1, 1)`                         = is_gt0(1, 1)                        ,
#'        `is_gt0(chr.mat, chr.mat)`             = is_gt0(chr.mat, chr.mat)            ,
#'        `is_in(1, "", "a", 1:2)`               = is_in(1, "", "a", 1:2)              ,
#'        `is_in(1, "a", letters)`               = is_in(1, "a", letters)              ,
#'        `is_in(NULL, blank, combo)`            = is_in(NULL, blank, combo)           ,
#'        `is_in0(1, "", "a", 1:2)`              = is_in0(1, "", "a", 1:2)             ,
#'        `is_in0(1, "a", letters)`              = is_in0(1, "a", letters)             ,
#'        `is_in0(NULL, blank, combo)`           = is_in0(NULL, blank, combo)          ,
#'        `is_bl0(chr.mat)`                      = is_bl0(chr.mat)                     ,
#'        `is_le("a", "b")`                      = is_le("a", "b")                     ,
#'        `is_le("b", "a")`                      = is_le("b", "a")                     ,
#'        `is_le(1, 1)`                          = is_le(1, 1)                         ,
#'        `is_le(chr.mat, chr.mat)`              = is_le(chr.mat, chr.mat)             ,
#'        `is_le0("a", "b")`                     = is_le0("a", "b")                    ,
#'        `is_le0("b", "a")`                     = is_le0("b", "a")                    ,
#'        `is_le0(1, 1)`                         = is_le0(1, 1)                        ,
#'        `is_le0(chr.mat, chr.mat)`             = is_le0(chr.mat, chr.mat)            ,
#'        `is_lg0(c(TRUE, FALSE))`               = is_lg0(c(TRUE, FALSE))              ,
#'        `is_lg0(chr.mat)`                      = is_lg0(chr.mat)                     ,
#'        `is_lg0(FALSE)`                        = is_lg0(FALSE)                       ,
#'        `is_lg0(NA)`                           = is_lg0(NA)                          ,
#'        `is_lg0(TRUE)`                         = is_lg0(TRUE)                        ,
#'        `is_lt("a", "b")`                      = is_lt("a", "b")                     ,
#'        `is_lt("b", "a")`                      = is_lt("b", "a")                     ,
#'        `is_lt(1, 1)`                          = is_lt(1, 1)                         ,
#'        `is_lt(chr.mat, chr.mat)`              = is_lt(chr.mat, chr.mat)             ,
#'        `is_lt0("a", "b")`                     = is_lt0("a", "b")                    ,
#'        `is_lt0("b", "a")`                     = is_lt0("b", "a")                    ,
#'        `is_lt0(1, 1)`                         = is_lt0(1, 1)                        ,
#'        `is_lt0(chr.mat, chr.mat)`             = is_lt0(chr.mat, chr.mat)            ,
#'        `is_mf(1, "", "a", 1:2)`               = is_mf(1, "", "a", 1:2)              ,
#'        `is_mf(1, "a", letters)`               = is_mf(1, "a", letters)              ,
#'        `is_mf(NULL, blank, combo)`            = is_mf(NULL, blank, combo)           ,
#'        `is_mf0(1, "", "a", 1:2)`              = is_mf0(1, "", "a", 1:2)             ,
#'        `is_mf0(1, "a", letters)`              = is_mf0(1, "a", letters)             ,
#'        `is_mf0(NULL, blank, combo)`           = is_mf0(NULL, blank, combo)          ,
#'        `is_nav(1)`                            = is_nav(1)                           ,
#'        `is_nav(c(NA, NA))`                    = is_nav(c(NA, NA))                   ,
#'        `is_nav(chr.mat)`                      = is_nav(chr.mat)                     ,
#'        `is_nav(NA)`                           = is_nav(NA)                          ,
#'        `is_na0(1)`                            = is_na0(1)                           ,
#'        `is_na0(c(NA, NA))`                    = is_na0(c(NA, NA))                   ,
#'        `is_na0(chr.mat)`                      = is_na0(chr.mat)                     ,
#'        `is_na0(NA)`                           = is_na0(NA)                          ,
#'        `is_okv(1)`                            = is_okv(1)                           ,
#'        `is_okv(c(NA, NA))`                    = is_okv(c(NA, NA))                   ,
#'        `is_okv(chr.mat)`                      = is_ok(chr.mat)                      ,
#'        `is_okv(NA)`                           = is_okv(NA)                          ,
#'        `is_ok0(1)`                            = is_ok0(1)                           ,
#'        `is_ok0(c(NA, NA))`                    = is_ok0(c(NA, NA))                   ,
#'        `is_ok0(chr.mat)`                      = is_ok0(chr.mat)                     ,
#'        `is_ok0(NA)`                           = is_ok0(NA)                          ,
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
#'        `is_t0(c(TRUE, FALSE))`                = is_t0(c(TRUE, FALSE))               ,
#'        `is_t0(chr.mat)`                       = is_t0(chr.mat)                      ,
#'        `is_t0(FALSE)`                         = is_t0(FALSE)                        ,
#'        `is_t0(NA)`                            = is_t0(NA)                           ,
#'        `is_t0(TRUE)`                          = is_t0(TRUE)                         ,
#'        `is_tf(c(TRUE, FALSE))`                = is_tf(c(TRUE, FALSE))               ,
#'        `is_tf(chr.mat)`                       = is_tf(chr.mat)                      ,
#'        `is_tf(chr.mat)`                       = is_tf(chr.mat)                      ,
#'        `is_tf(NA)`                            = is_tf(NA)                           ,
#'        `is_tf(TRUE)`                          = is_tf(TRUE)                         ,
#'        `is_tf(TRUE)`                          = is_tf(TRUE)                         ,
#'        `is_tf0(c(TRUE, FALSE))`               = is_tf0(c(TRUE, FALSE))              ,
#'        `is_tf0(chr.mat)`                      = is_tf0(chr.mat)                     ,
#'        `is_tf0(FALSE)`                        = is_tf0(FALSE)                       ,
#'        `is_tf0(NA)`                           = is_tf0(NA)                          ,
#'        `is_tf0(TRUE)`                         = is_tf0(TRUE)                        ,
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
#'        `not_bl0("")`                          = not_bl0("")                         ,
#'        `not_bl0("a")`                         = not_bl0("a")                        ,
#'        `not_bl0(c("", ""))`                   = not_bl0(c("", ""))                  ,
#'        `not_bl0(chr.mat)`                     = not_bl0(chr.mat)                    ,
#'        `not_bl0(NA)`                          = not_bl0(NA)                         ,
#'        `not_dif("", "")`                      = not_dif("", "")                     ,
#'        `not_dif("word", 7)`                   = not_dif("word", 7)                  ,
#'        `not_dif(chr.mat, chr.mat)`            = not_dif(chr.mat, chr.mat)           ,
#'        `not_dif(NA, NA)`                      = not_dif(NA, NA)                     ,
#'        `not_dif0("", "")`                     = not_dif0("", "")                    ,
#'        `not_dif0("word", 7)`                  = not_dif0("word", 7)                 ,
#'        `not_dif0(chr.mat, chr.mat)`           = not_dif0(chr.mat, chr.mat)          ,
#'        `not_dif0(NA, NA)`                     = not_dif0(NA, NA)                    ,
#'        `not_eq("", "")`                       = not_eq("", "")                      ,
#'        `not_eq("word", 7)`                    = not_eq("word", 7)                   ,
#'        `not_eq(chr.mat, chr.mat)`             = not_eq(chr.mat, chr.mat)            ,
#'        `not_eq(NA, NA)`                       = not_eq(NA, NA)                      ,
#'        `not_eq0("", "")`                      = not_eq0("", "")                     ,
#'        `not_eq0("word", 7)`                   = not_eq0("word", 7)                  ,
#'        `not_eq0(chr.mat, chr.mat)`            = not_eq0(chr.mat, chr.mat)           ,
#'        `not_eq0(NA, NA)`                      = not_eq0(NA, NA)                     ,
#'        `not_err(non.existent.variable)`       = not_err(non.existent.variable)      ,
#'        `not_err(pi)`                          = not_err(pi)                         ,
#'        `not_f(c(TRUE, FALSE))`                = not_f(c(TRUE, FALSE))               ,
#'        `not_f(chr.mat)`                       = not_f(chr.mat)                      ,
#'        `not_f(NA)`                            = not_f(NA)                           ,
#'        `not_f(TRUE)`                          = not_f(TRUE)                         ,
#'        `not_f(TRUE)`                          = not_f(TRUE)                         ,
#'        `not_f0(c(TRUE, FALSE))`               = not_f0(c(TRUE, FALSE))              ,
#'        `not_f0(chr.mat)`                      = not_f0(chr.mat)                     ,
#'        `not_f0(FALSE)`                        = not_f0(FALSE)                       ,
#'        `not_f0(NA)`                           = not_f0(NA)                          ,
#'        `not_f0(TRUE)`                         = not_f0(TRUE)                        ,
#'        `not_ge("a", "b")`                     = not_ge("a", "b")                    ,
#'        `not_ge("b", "a")`                     = not_ge("b", "a")                    ,
#'        `not_ge(1, 1)`                         = not_ge(1, 1)                        ,
#'        `not_ge(chr.mat, chr.mat)`             = not_ge(chr.mat, chr.mat)            ,
#'        `not_ge0("a", "b")`                    = not_ge0("a", "b")                   ,
#'        `not_ge0("b", "a")`                    = not_ge0("b", "a")                   ,
#'        `not_ge0(1, 1)`                        = not_ge0(1, 1)                       ,
#'        `not_ge0(chr.mat, chr.mat)`            = not_ge0(chr.mat, chr.mat)           ,
#'        `not_gt("a", "b")`                     = not_gt("a", "b")                    ,
#'        `not_gt("b", "a")`                     = not_gt("b", "a")                    ,
#'        `not_gt(1, 1)`                         = not_gt(1, 1)                        ,
#'        `not_gt(chr.mat, chr.mat)`             = not_gt(chr.mat, chr.mat)            ,
#'        `not_gt0("a", "b")`                    = not_gt0("a", "b")                   ,
#'        `not_gt0("b", "a")`                    = not_gt0("b", "a")                   ,
#'        `not_gt0(1, 1)`                        = not_gt0(1, 1)                       ,
#'        `not_gt0(chr.mat, chr.mat)`            = not_gt0(chr.mat, chr.mat)           ,
#'        `not_in(1, "", "a", 1:2)`              = not_in(1, "", "a", 1:2)             ,
#'        `not_in(1, "a", letters)`              = not_in(1, "a", letters)             ,
#'        `not_in(NULL, blank, combo)`           = not_in(NULL, blank, combo)          ,
#'        `not_in0(1, "", "a", 1:2)`             = not_in0(1, "", "a", 1:2)            ,
#'        `not_in0(1, "a", letters)`             = not_in0(1, "a", letters)            ,
#'        `not_in0(NULL, blank, combo)`          = not_in0(NULL, blank, combo)         ,
#'        `not_le("a", "b")`                     = not_le("a", "b")                    ,
#'        `not_le("b", "a")`                     = not_le("b", "a")                    ,
#'        `not_le(1, 1)`                         = not_le(1, 1)                        ,
#'        `not_le(chr.mat, chr.mat)`             = not_le(chr.mat, chr.mat)            ,
#'        `not_le0("a", "b")`                    = not_le0("a", "b")                   ,
#'        `not_le0("b", "a")`                    = not_le0("b", "a")                   ,
#'        `not_le0(1, 1)`                        = not_le0(1, 1)                       ,
#'        `not_le0(chr.mat, chr.mat)`            = not_le0(chr.mat, chr.mat)           ,
#'        `not_lg0(c(TRUE, FALSE))`              = not_lg0(c(TRUE, FALSE))             ,
#'        `not_lg0(chr.mat)`                     = not_lg0(chr.mat)                    ,
#'        `not_lg0(FALSE)`                       = not_lg0(FALSE)                      ,
#'        `not_lg0(NA)`                          = not_lg0(NA)                         ,
#'        `not_lg0(TRUE)`                        = not_lg0(TRUE)                       ,
#'        `not_lt("a", "b")`                     = not_lt("a", "b")                    ,
#'        `not_lt("b", "a")`                     = not_lt("b", "a")                    ,
#'        `not_lt(1, 1)`                         = not_lt(1, 1)                        ,
#'        `not_lt(chr.mat, chr.mat)`             = not_lt(chr.mat, chr.mat)            ,
#'        `not_lt0("a", "b")`                    = not_lt0("a", "b")                   ,
#'        `not_lt0("b", "a")`                    = not_lt0("b", "a")                   ,
#'        `not_lt0(1, 1)`                        = not_lt0(1, 1)                       ,
#'        `not_lt0(chr.mat, chr.mat)`            = not_lt0(chr.mat, chr.mat)           ,
#'        `not_mf(1, "", "a", 1:2)`              = not_mf(1, "", "a", 1:2)             ,
#'        `not_mf(1, "a", letters)`              = not_mf(1, "a", letters)             ,
#'        `not_mf(NULL, blank, combo)`           = not_mf(NULL, blank, combo)          ,
#'        `not_mf0(1, "", "a", 1:2)`             = not_mf0(1, "", "a", 1:2)            ,
#'        `not_mf0(1, "a", letters)`             = not_mf0(1, "a", letters)            ,
#'        `not_mf0(NULL, blank, combo)`          = not_mf0(NULL, blank, combo)         ,
#'        `not_na0(1)`                           = not_na0(1)                          ,
#'        `not_na0(c(NA, NA))`                   = not_na0(c(NA, NA))                  ,
#'        `not_na0(chr.mat)`                     = not_na0(chr.mat)                    ,
#'        `not_na0(NA)`                          = not_na0(NA)                         ,
#'        `not_nav(c(NA, NA)), not_nav(chr.mat)` = not_nav(c(NA, NA)), not_nav(chr.mat),
#'        `not_nav(NA), not_nav(1)`              = not_nav(NA), not_nav(1)             ,
#'        `not_ok0(1)`                           = not_ok0(1)                          ,
#'        `not_ok0(c(NA, NA))`                   = not_ok0(c(NA, NA))                  ,
#'        `not_ok0(chr.mat)`                     = not_ok0(chr.mat)                    ,
#'        `not_ok0(NA)`                          = not_ok0(NA)                         ,
#'        `not_okv(c(NA, NA)), not_okv(chr.mat)` = not_okv(c(NA, NA)), not_okv(chr.mat),
#'        `not_okv(NA), not_okv(1)`              = not_okv(NA), not_okv(1)             ,
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
#'        `not_t0(c(TRUE, FALSE))`               = not_t0(c(TRUE, FALSE))              ,
#'        `not_t0(chr.mat)`                      = not_t0(chr.mat)                     ,
#'        `not_t0(FALSE)`                        = not_t0(FALSE)                       ,
#'        `not_t0(NA)`                           = not_t0(NA)                          ,
#'        `not_t0(TRUE)`                         = not_t0(TRUE)                        ,
#'        `not_tf(c(TRUE, FALSE))`               = not_tf(c(TRUE, FALSE))              ,
#'        `not_tf(chr.mat)`                      = not_tf(chr.mat)                     ,
#'        `not_tf(NA)`                           = not_tf(NA)                          ,
#'        `not_tf(TRUE)`                         = not_tf(TRUE)                        ,
#'        `not_tf(TRUE)`                         = not_tf(TRUE)                        ,
#'        `not_tf0(c(TRUE, FALSE))`              = not_tf0(c(TRUE, FALSE))             ,
#'        `not_tf0(chr.mat)`                     = not_tf0(chr.mat)                    ,
#'        `not_tf0(FALSE)`                       = not_tf0(FALSE)                      ,
#'        `not_tf0(NA)`                          = not_tf0(NA)                         ,
#'        `not_tf0(TRUE)`                        = not_tf0(TRUE)                       ,
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
#'        `one_lg(c(NA, TRUE, FALSE))`           = one_lg(c(NA, TRUE, FALSE))          ,
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
#'        `some_lg(c(NA, TRUE, FALSE))`          = some_lg(c(NA, TRUE, FALSE))         ,
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
#'        `two_lg(c(NA, TRUE, FALSE))`           = two_lg(c(NA, TRUE, FALSE))          ,
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
failsafe <- function(x, def = "uj.failsafe.err") {
  x <- tryCatch(base::identity(x), error = function(e) e, finally = NULL)
  if (rlang::is_error(x)) {
    def <- tryCatch(base::identity(def), error = function(e) e, finally = NULL)
    if (assertthat::is.error(def)) {def <- "uj.failsafe.err"}
    base::attr(def, "stack") <- uj::callers()
    def
  } else {x}
}

#' @rdname failsafe
#' @export
is_err <- function(x) {rlang::is_error(tryCatch(base::identity(x), error = function(e) e, finally = NULL))}

#' @rdname failsafe
#' @export
not_err <- function(x) {!uj::is_err(x)}

#' @rdname failsafe
#' @export
msg_err <- function(x) {
  x <- tryCatch(base::identity(x), error = function(e) e, finally = NULL)
  if (rlang::is_error(x)) {x$message} else {NULL}
}

#' @rdname failsafe
#' @export
make_err <- function() {base::simpleError("uj.intentionally.generated error.")}

#' @rdname failsafe
#' @export
fs_def <- function(x) {if (uj::is_err(x)) {FALSE} else {!base::is.null(x)}}

#' @rdname failsafe
#' @export
fs_nll <- function(x) {if (uj::is_err(x)) {TRUE} else {base::is.null(x)}}

#' @rdname failsafe
#' @export
fs_scl <- function(x, val) {
  if      (uj::is_err(x) | uj::is_err(val)              ) {FALSE}
  else if (base::length(x) != 1 | base::length(val) != 1) {FALSE}
  else if (!base::is.atomic(x) | !base::is.atomic(val)  ) {FALSE}
  else if (uj:::.compat(x, val)                         ) {FALSE}
  else if (base::is.na(x) & base::is.na(val)            ) {TRUE}
  else if (base::is.na(x) | base::is.na(val)            ) {FALSE}
  else                                                    {x == val}
}

#' @rdname failsafe
#' @export
fs_t <- function(x) {if (uj::is_err(x)) {FALSE} else {base::isTRUE(x)}}

#' @rdname failsafe
#' @export
fs_f <- function(x) {if (uj::is_err(x)) {FALSE} else {base::isFALSE(x)}}

#' @rdname failsafe
#' @export
fs_tf <- function(x) {uj::fs_t(x) | uj::fs_f(x)}

#' @rdname failsafe
#' @export
fs_na <- function(x) {
  if (!uj::is_err(x)) {if (base::is.atomic(x)) {if (base::length(x) == 1) {return(base::is.na(x))}}}
  FALSE
}

#' @rdname failsafe
#' @export
fs_ok <- function(x) {
  if (!uj::is_err(x)) {if (base::is.atomic(x)) {if (base::length(x) == 1) {!return(base::is.na(x))}}}
  FALSE
}

#' @rdname failsafe
#' @export
fs_bl <- function(x) {
  if (!uj::is_err(x)) {if (base::is.character(x)) {if (base::length(x) == 1) {return(x == "")}}}
  FALSE
}

#' @rdname failsafe
#' @export
fs_lg <- function(x) {uj::fs_f(x) | uj::fs_t(x) | uj::fs_na(x)}

#' @rdname failsafe
#' @export
fs_vec <- function(x, val) {
  if      (uj::is_err(x) | uj::is_err(val)            ) {FALSE}
  else if (uj:::.mismatch_n(x, val)                   ) {FALSE}
  else if (!base::is.atomic(x) | !base::is.atomic(val)) {FALSE}
  else if (uj:::.compat(x, val)                       ) {FALSE}
  else if (base::is.na(x) & base::is.na(val)          ) {TRUE}
  else if (base::is.na(x) | base::is.na(val)          ) {FALSE}
  else                                                  {x == val}
}

#' @rdname failsafe
#' @export
fs_null <- function(x) {if (uj::is_err(x)) {FALSE} else {base::is.null(x)}}

#' @rdname failsafe
#' @export
scl_bl <- function(x) {uj::fs_bl(x)}

#' @rdname failsafe
#' @export
scl_t <- function(x)  {uj::fs_t(x)}

#' @rdname failsafe
#' @export
scl_f <- function(x)  {uj::fs_f(x)}

#' @rdname failsafe
#' @export
scl_tf <- function(x)  {uj::fs_tf(x)}

#' @rdname failsafe
#' @export
scl_lg <- function(x)  {uj::fs_lg(x)}

#' @rdname failsafe
#' @export
scl_na <- function(x)  {uj::fs_na(x)}

#' @rdname failsafe
#' @export
vec_bl <- function(x) {uj::fs_vec(x, "")}

#' @rdname failsafe
#' @export
vec_t <- function(x)  {uj::fs_vec(x, TRUE)}

#' @rdname failsafe
#' @export
vec_f <- function(x)  {uj::fs_vec(x, FALSE)}

#' @rdname failsafe
#' @export
vec_tf <- function(x)  {uj::fs_vec(x, TRUE) | uj::fs_vec(x, TRUE)}

#' @rdname failsafe
#' @export
vec_na <- function(x)  {uj::fs_vec(x, NA)}

#' @rdname failsafe
#' @export
fs_or <- function(x, or = NULL) {if (!uj::is_err(x)) {x} else if (!uj::is_err(or)) {or} else {NULL}}

#' @rdname failsafe
#' @export
or_f <- function(x) {uj::fs_or(x, FALSE)}

#' @rdname failsafe
#' @export
or_t <- function(x) {uj::fs_or(x, TRUE)}

#' @rdname failsafe
#' @export
or_bl <- function(x) {uj::fs_or(x, "")}

#' @rdname failsafe
#' @export
or_c0 <- function(x) {uj::fs_or(x, base::character(0))}

#' @rdname failsafe
#' @export
or_i0 <- function(x) {uhj::fs_or(x, base::integer(0))}

#' @rdname failsafe
#' @export
or_l0 <- function(x) {uhj::fs_or(x, base::logical(0))}

#' @rdname failsafe
#' @export
or_n0 <- function(x) {uhj::fs_or(x, base::numeric(0))}

#' @rdname failsafe
#' @export
or_na <- function(x) {uj::fs_or(x, NA)}

#' @rdname failsafe
#' @export
or_nac <- function(x) {uj::fs_or(x, NA_character_)}

#' @rdname failsafe
#' @export
or_naI <- function(x) {uj::fs_or(x, NA_integer_)}

#' @rdname failsafe
#' @export
or_naL <- function(x) {uj::fs_or(x, NA)}

#' @rdname failsafe
#' @export
or_naR <- function(x) {uj::fs_or(x, NA_real_)}

#' @rdname failsafe
#' @export
or_null <- function(x) {uj::fs_or(x)}

#' @rdname failsafe
#' @export
is_id <- function(x, y) {if (uj::is_err(x) | uj::is_err(y)) {FALSE} else {base::identical(x, y)}}

#' @rdname failsafe
#' @export
not_id <- function(x, y) {if (uj::is_err(x) | uj::is_err(y)) {FALSE} else {!base::identical(x, y)}}

#' @rdname failsafe
#' @export
is_in0 <- function(x, ...) {
  if (uj::not_err(x)) {
    if (base::...length() > 0 & base::length(x) == 1 & base::is.atomic(x)) {
      for (i in 1:base::...length()) {
        if (uj::not_err(base::...elt(i))) {
          dot <- base::...elt(i)
          if (base::is.atomic(dot)) {
            if (uj:::.compat(x, dot)) {
              if (x %in% dot) {return(TRUE)}}}}}}}
  FALSE
}

#' @rdname failsafe
#' @export
is_mf0 <- function(x, ...) {!uj::is_in0(x, ...)}

#' @rdname failsafe
#' @export
has0 <- function(x, y) {uj::is_in0(y, x)}

#' @rdname failsafe
#' @export
lacks0 <- function(x, y) {!uj::is_in0(y, x)}

#' @rdname failsafe
#' @export
is_eq0 <- function(x, y) {
  if (uj::not_err(x) & uj::not_err(y)) {
    if (base::is.atomic(x) & base::is.atomic(y) & base::length(x) == 1 & base::length(y) == 1) {
      if (uj:::.compat(x, y)) {
        if (base::is.na(x) | base::is.na(y)) {return(FALSE)}
        y <- base::unlist(x == y)
        base::attributes(y) <- NULL
        return(y)}}}
  FALSE
}

#' @rdname failsafe
#' @export
is_ge0 <- function(x, y) {
  if (uj::not_err(x) & uj::not_err(y)) {
    if (base::is.atomic(x) & base::is.atomic(y) & base::length(x) == 1 & base::length(y) == 1) {
      if (uj:::.compar(x, y)) {
        y <- base::unlist(x >= y)
        base::attributes(y) <- NULL
        return(y)}}}
  FALSE
}

#' @rdname failsafe
#' @export
is_gt0 <- function(x, y) {
  if (uj::not_err(x) & uj::not_err(y)) {
    if (base::is.atomic(x) & base::is.atomic(y) & base::length(x) == 1 & base::length(y) == 1) {
      if (uj:::.compar(x, y)) {
        y <- base::unlist(x > y)
        base::attributes(y) <- NULL
        return(y)}}}
  FALSE
}

#' @rdname failsafe
#' @export
is_le0 <- function(x, y) {
  if (uj::not_err(x) & uj::not_err(y)) {
    if (base::is.atomic(x) & base::is.atomic(y) & base::length(x) == 1 & base::length(y) == 1) {
      if (uj:::.compar(x, y)) {
        y <- base::unlist(x <= y)
        base::attributes(y) <- NULL
        return(y)}}}
  FALSE
}

#' @rdname failsafe
#' @export
is_lt0 <- function(x, y) {
  if (uj::not_err(x) & uj::not_err(y)) {
    if (base::is.atomic(x) & base::is.atomic(y) & base::length(x) == 1 & base::length(y) == 1) {
      if (uj:::.compar(x, y)) {
        y <- base::unlist(x < y)
        base::attributes(y) <- NULL
        return(y)}}}
  FALSE
}

#' @rdname failsafe
#' @export
is_dif0 <- function(x, y) {!uj::is_eq0(x, y)}

#' @rdname failsafe
#' @export
is_f0 <- function(x) {uj::fs_f(x)}

#' @rdname failsafe
#' @export
is_t0 <- function(x) {uj::fs_t(x)}

#' @rdname failsafe
#' @export
is_bl0 <- function(x) {uj::fs_bl(x)}

#' @rdname failsafe
#' @export
is_tf0 <- function(x) {uj::fs_tf(x)}

#' @rdname failsafe
#' @export
is_na0 <- function(x) {uj::fs_na(x)}

#' @rdname failsafe
#' @export
is_ok0 <- function(x) {uj::fs_ok(x)}

#' @rdname failsafe
#' @export
is_lg0 <- function(x) {uj::fs_lg(x)}

#' @rdname failsafe
#' @export
is_in <- function(x, ...) {
  if (uj::not_err(x)) {if (base::is.atomic(x) & base::length(x) > 0) {return(base::sapply(x, uj::is_in0, ...))}}
  FALSE
}

#' @rdname failsafe
#' @export
is_mf <- function(x, ...) {!uj::is_in(x, ...)}

#' @rdname failsafe
#' @export
has <- function(x, y) {uj::is_in(y, x)}

#' @rdname failsafe
#' @export
lacks <- function(x, y) {!uj::is_in(y, x)}

#' @rdname failsafe
#' @export
is_eq <- function(x, y) {
  if (uj::not_err(x) & uj::not_err(y)) {
    if (base::is.atomic(x) & base::is.atomic(y)) {
      nx <- base::length(x)
      ny <- base::length(y)
      if (nx %in% base::c(1, ny) & ny %in% base::c(1, nx)) {
        if (uj:::.compat(x, y)) {return(x == y)}}}}
  FALSE
}

#' @rdname failsafe
#' @export
is_ge <- function(x, y) {
  if (uj::not_err(x) & uj::not_err(y)) {
    if (base::is.atomic(x) & base::is.atomic(y)) {
      nx <- base::length(x)
      ny <- base::length(y)
      if (nx %in% base::c(1, ny) & ny %in% base::c(1, nx)) {
        if (uj:::.compar(x, y)) {return(x >= y)}}}}
  FALSE
}

#' @rdname failsafe
#' @export
is_gt <- function(x, y) {
  if (uj::not_err(x) & uj::not_err(y)) {
    if (base::is.atomic(x) & base::is.atomic(y)) {
      nx <- base::length(x)
      ny <- base::length(y)
      if (nx %in% base::c(1, ny) & ny %in% base::c(1, nx)) {
        if (uj:::.compar(x, y)) {return(x > y)}}}}
  FALSE
}

#' @rdname failsafe
#' @export
is_le <- function(x, y) {
  if (uj::not_err(x) & uj::not_err(y)) {
    if (base::is.atomic(x) & base::is.atomic(y)) {
      nx <- base::length(x)
      ny <- base::length(y)
      if (nx %in% base::c(1, ny) & ny %in% base::c(1, nx)) {
        if (uj:::.compar(x, y)) {return(x <= y)}}}}
  FALSE
}

#' @rdname failsafe
#' @export
is_lt <- function(x, y) {
  if (uj::not_err(x) & uj::not_err(y)) {
    if (base::is.atomic(x) & base::is.atomic(y)) {
      nx <- base::length(x)
      ny <- base::length(y)
      if (nx %in% base::c(1, ny) & ny %in% base::c(1, nx)) {
        if (uj:::.compar(x, y)) {return(x < y)}}}}
  FALSE
}

#' @rdname failsafe
#' @export
is_dif <- function(x, y) {!uj::is_eq(x, y)}

#' @rdname failsafe
#' @export
is_f <- function(x) {uj::is_eq(x, FALSE)}

#' @rdname failsafe
#' @export
is_t <- function(x) {uj::is_eq(x, TRUE)}

#' @rdname failsafe
#' @export
is_bl <- function(x) {uj::is_eq(x, "")}

#' @rdname failsafe
#' @export
is_tf <- function(x) {uj::is_eq(x, FALSE) | uj::is_eq(x, TRUE)}

#' @rdname failsafe
#' @export
is_nav <- function(x) {
  if (uj::not_err(x)) {if (base::is.atomic(x)) {return(base::is.na(x))}}
  FALSE
}

#' @rdname failsafe
#' @export
is_okv <- function(x) {
  if (uj::not_err(x)) {if (base::is.atomic(x)) {return(!base::is.na(x))}}
  FALSE
}

#' @rdname failsafe
#' @export
is_seq <- function(x, y) {
  if (uj::not_err(x) & uj::not_err(y)) {
    if (base::is.atomic(x) & base::is.atomic(y)) {
      if (uj:::.compat(x, y)) {
        x <- base::unlist(x); base::attributes(x) <- NULL
        y <- base::unlist(y); base::attributes(y) <- NULL
        return(base::setequal(x, y))}}}
  FALSE
}

#' @rdname failsafe
#' @export
is_veq <- function(x, y) {
  if (uj::not_err(x) & uj::not_err(y)) {
    if (base::is.atomic(x) & base::is.atomic(y)) {
      if (uj:::.compat(x, y)) {
        if (base::length(x) == base::length(y)) {
          if (!base::any(base::is.na(x)) & !base::any(base::is.na(y))) {return(base::all(x == y))}}}}}
  FALSE
}

#' @rdname failsafe
#' @export
is_sdif <- function(x, y) {!uj::is_seq(x, y)}

#' @rdname failsafe
#' @export
is_vdif <- function(x, y) {!uj::is_veq(x, y)}

#' @rdname failsafe
#' @export
or0 <- function(x, y) {uj::fs_t(x) | uj::fs_t(y)}

#' @rdname failsafe
#' @export
and0 <- function(x, y) {uj::fs_t(x) & uj::fs_t(y)}

#' @rdname failsafe
#' @export
xor0 <- function(x, y) {base::xor(uj::fs_t(x), uj::fs_t(y))}

#' @rdname failsafe
#' @export
nor0 <- function(x, y) {!uj::fs_t(x) & !uj::fs_t(y)}

#' @rdname failsafe
#' @export
or <- function(x, y) {
  if (uj::not_err(x) & uj::not_err(y)) {
    if (base::is.logical(x) & base::is.logical(y)) {
      nx <- base::length(x)
      ny <- base::length(y)
      if (nx %in% base::c(1, ny) & ny %in% base::c(1, nx)) {return(x | y)}}}
  FALSE
}

#' @rdname failsafe
#' @export
and <- function(x, y) {
  if (uj::not_err(x) & uj::not_err(y)) {
    if (base::is.logical(x) & base::is.logical(y)) {
      nx <- base::length(x)
      ny <- base::length(y)
      if (nx %in% base::c(1, ny) & ny %in% base::c(1, nx)) {return(x & y)}}}
  FALSE
}

#' @rdname failsafe
#' @export
one <- function(x, y) {
  if (uj::not_err(x) & uj::not_err(y)) {
    if (base::is.logical(x) & base::is.logical(y)) {
      nx <- base::length(x)
      ny <- base::length(y)
      if (nx %in% base::c(1, ny) & ny %in% base::c(1, nx)) {return(base::xor(x, y))}}}
  FALSE
}

#' @rdname failsafe
#' @export
nor <- function(x, y) {
  if (uj::not_err(x) & uj::not_err(y)) {
    if (base::is.logical(x) & base::is.logical(y)) {
      nx <- base::length(x)
      ny <- base::length(y)
      if (nx %in% base::c(1, ny) & ny %in% base::c(1, nx)) {return(!x & !y)}}}
  FALSE
}

#' @rdname failsafe
#' @export
not_in0 <- function(x, ...) {!uj::is_in0(x, ...)}

#' @rdname failsafe
#' @export
not_mf0 <- function(x, ...) {!uj::is_mf0(x, ...)}

#' @rdname failsafe
#' @export
not_has0 <- function(x, y) {!uj::has0(x, y)}

#' @rdname failsafe
#' @export
not_lacks0 <- function(x, y) {!uj::lacks0(x, y)}

#' @rdname failsafe
#' @export
not_eq0 <- function(x, y) {!uj::is_eq0(x, y)}

#' @rdname failsafe
#' @export
not_ge0 <- function(x, y) {!uj::is_ge0(x, y)}

#' @rdname failsafe
#' @export
not_gt0 <- function(x, y) {!uj::is_gt0(x, y)}

#' @rdname failsafe
#' @export
not_le0 <- function(x, y) {!uj::is_le0(x, y)}

#' @rdname failsafe
#' @export
not_lt0 <- function(x, y) {!uj::is_lt0(x, y)}

#' @rdname failsafe
#' @export
not_dif0 <- function(x, y) {!uj::is_dif0(x, y)}

#' @rdname failsafe
#' @export
not_f0 <- function(x) {!uj::fs_f(x)}

#' @rdname failsafe
#' @export
not_t0 <- function(x) {!uj::fs_t(x)}

#' @rdname failsafe
#' @export
not_tf0 <- function(x) {!uj::fs_tf(x)}

#' @rdname failsafe
#' @export
not_bl0 <- function(x) {!uj::fs_bl(x)}

#' @rdname failsafe
#' @export
not_na0 <- function(x) {!uj::fs_na(x)}

#' @rdname failsafe
#' @export
not_ok0 <- function(x) {!uj::fs_ok(x)}

#' @rdname failsafe
#' @export
not_lg0 <- function(x) {!uj::fs_lg(x)}

#' @rdname failsafe
#' @export
not_in <- function(x, ...) {!uj::is_in(x, ...)}

#' @rdname failsafe
#' @export
not_mf <- function(x, ...) {!uj::is_mf(x, ...)}

#' @rdname failsafe
#' @export
not_has <- function(x, y) {!uj::has(x, y)}

#' @rdname failsafe
#' @export
not_lacks <- function(x, y) {!uj::lacks(x, y)}

#' @rdname failsafe
#' @export
not_eq <- function(x, y) {!uj::is_eq(x, y)}

#' @rdname failsafe
#' @export
not_ge <- function(x, y) {!uj::is_ge(x, y)}

#' @rdname failsafe
#' @export
not_gt <- function(x, y) {!uj::is_gt(x, y)}

#' @rdname failsafe
#' @export
not_le <- function(x, y) {!uj::is_le(x, y)}

#' @rdname failsafe
#' @export
not_lt <- function(x, y) {!uj::is_lt(x, y)}

#' @rdname failsafe
#' @export
not_dif <- function(x, y) {!uj::is_dif(x, y)}

#' @rdname failsafe
#' @export
not_f <- function(x) {!uj::is_f(x)}

#' @rdname failsafe
#' @export
not_t <- function(x) {!uj::is_t(x)}

#' @rdname failsafe
#' @export
not_tf <- function(x) {!uj::is_tf(x)}

#' @rdname failsafe
#' @export
not_bl <- function(x) {!uj::is_bl(x)}

#' @rdname failsafe
#' @export
not_nav <- function(x) {!uj::is_nav(x)}

#' @rdname failsafe
#' @export
not_okv <- function(x) {!uj::is_okv(x)}

#' @rdname failsafe
#' @export
not_seq <- function(x, y) {!uj::is_seq(x, y)}

#' @rdname failsafe
#' @export
not_veq <- function(x, y) {!uj::is_veq(x, y)}

#' @rdname failsafe
#' @export
not_sdif <- function(x, y) {!uj::is_sdif(x, y)}

#' @rdname failsafe
#' @export
not_vdif <- function(x, y) {!uj::is_vdif(x, y)}

#' @rdname failsafe
#' @export
n_in <- function(x, ...) {uj::nw(uj::is_in(x, ...))}

#' @rdname failsafe
#' @export
n_mf <- function(x, ...) {uj::nw(uj::is_mf(x, ...))}

#' @rdname failsafe
#' @export
n_has <- function(x, y) {uj::nw(uj::has(x, y))}

#' @rdname failsafe
#' @export
n_lacks <- function(x, y) {uj::nw(uj::lacks(x, y))}

#' @rdname failsafe
#' @export
n_eq <- function(x, y) {uj::nw(uj::is_eq(x, y))}

#' @rdname failsafe
#' @export
n_ge <- function(x, y) {uj::nw(uj::is_ge(x, y))}

#' @rdname failsafe
#' @export
n_gt <- function(x, y) {uj::nw(uj::is_gt(x, y))}

#' @rdname failsafe
#' @export
n_le <- function(x, y) {uj::nw(uj::is_le(x, y))}

#' @rdname failsafe
#' @export
n_lt <- function(x, y) {uj::nw(uj::is_lt(x, y))}

#' @rdname failsafe
#' @export
n_dif <- function(x, y) {uj::nw(uj::is_dif(x, y))}

#' @rdname failsafe
#' @export
n_f <- function(x) {uj::nw(uj::is_f(x))}

#' @rdname failsafe
#' @export
n_t <- function(x) {uj::nw(uj::is_t(x))}

#' @rdname failsafe
#' @export
n_tf <- function(x) {uj::nw(uj::is_tf(x))}

#' @rdname failsafe
#' @export
n_bl <- function(x) {uj::nw(uj::is_bl(x))}

#' @rdname failsafe
#' @export
n_nav <- function(x) {uj::nw(uj::is_nav(x))}

#' @rdname failsafe
#' @export
n_okv <- function(x) {uj::nw(uj::is_okv(x))}

#' @rdname failsafe
#' @export
none_f <- function(x) {uj::n_f(x) == 0}

#' @rdname failsafe
#' @export
none_t <- function(x) {uj::n_t(x) == 0}

#' @rdname failsafe
#' @export
none_tf <- function(x) {uj::n_tf(x) == 0}

#' @rdname failsafe
#' @export
none_bl <- function(x) {uj::n_bl(x) == 0}

#' @rdname failsafe
#' @export
none_eq <- function(x, y) {uj::n_eq(x, y) == 0}

#' @rdname failsafe
#' @export
none_ge <- function(x, y) {uj::n_ge(x, y) == 0}

#' @rdname failsafe
#' @export
none_gt <- function(x, y) {uj::n_gt(x, y) == 0}

#' @rdname failsafe
#' @export
none_le <- function(x, y) {uj::n_le(x, y) == 0}

#' @rdname failsafe
#' @export
none_lt <- function(x, y) {uj::n_lt(x, y) == 0}

#' @rdname failsafe
#' @export
none_in <- function(x, ...) {uj::n_in(x, ...) == 0}

#' @rdname failsafe
#' @export
none_mf <- function(x, ...) {uj::n_mf(x, ...) == 0}

#' @rdname failsafe
#' @export
none_dif <- function(x, y) {uj::n_dif(x, y) == 0}

#' @rdname failsafe
#' @export
none_nav <- function(x) {uj::n_nav(x) == 0}

#' @rdname failsafe
#' @export
none_okv <- function(x) {uj::n_okv(x) == 0}

#' @rdname failsafe
#' @export
has_none <- function(x, y) {uj::n_has(x, y) == 0}

#' @rdname failsafe
#' @export
lacks_none <- function(x, y) {uj::n_lacks(x, y) == 0}

#' @rdname failsafe
#' @export
any_f <- function(x) {uj::n_f(x) > 0}

#' @rdname failsafe
#' @export
any_t <- function(x) {uj::n_t(x) > 0}

#' @rdname failsafe
#' @export
any_bl <- function(x) {uj::n_bl(x) > 0}

#' @rdname failsafe
#' @export
any_eq <- function(x, y) {uj::n_eq(x, y) > 0}

#' @rdname failsafe
#' @export
any_ge <- function(x, y) {uj::n_ge(x, y) > 0}

#' @rdname failsafe
#' @export
any_gt <- function(x, y) {uj::n_gt(x, y) > 0}

#' @rdname failsafe
#' @export
any_in <- function(x, ...) {uj::n_in(x, ...) > 0}

#' @rdname failsafe
#' @export
any_le <- function(x, y) {uj::n_le(x, y) > 0}

#' @rdname failsafe
#' @export
any_lt <- function(x, y) {uj::n_lt(x, y) > 0}

#' @rdname failsafe
#' @export
any_mf <- function(x, ...) {uj::n_mf(x, ...) > 0}

#' @rdname failsafe
#' @export
any_tf <- function(x) {uj::n_tf(x) > 0}

#' @rdname failsafe
#' @export
any_dif <- function(x, y) {uj::n_dif(x, y) > 0}

#' @rdname failsafe
#' @export
any_nav <- function(x) {uj::n_nav(x) > 0}

#' @rdname failsafe
#' @export
any_okv <- function(x) {uj::n_okv(x) > 0}

#' @rdname failsafe
#' @export
has_any <- function(x, y) {uj::n_has(x, y) > 0}

#' @rdname failsafe
#' @export
lacks_any <- function(x, y) {uj::n_lacks(x, y) > 0}

#' @rdname failsafe
#' @export
one_f <- function(x) {uj::n_f(x) == 1}

#' @rdname failsafe
#' @export
one_t <- function(x) {uj::n_t(x) == 1}

#' @rdname failsafe
#' @export
one_bl <- function(x) {uj::n_bl(x) == 1}

#' @rdname failsafe
#' @export
one_eq <- function(x, y) {uj::n_eq(x, y) == 1}

#' @rdname failsafe
#' @export
one_ge <- function(x, y) {uj::n_ge(x, y) == 1}

#' @rdname failsafe
#' @export
one_gt <- function(x, y) {uj::n_gt(x, y) == 1}

#' @rdname failsafe
#' @export
one_in <- function(x, ...) {uj::n_in(x, ...) == 1}

#' @rdname failsafe
#' @export
one_le <- function(x, y) {uj::n_le(x, y) == 1}

#' @rdname failsafe
#' @export
one_lt <- function(x, y) {uj::n_lt(x, y) == 1}

#' @rdname failsafe
#' @export
one_mf <- function(x, ...) {uj::n_mf(x, ...) == 1}

#' @rdname failsafe
#' @export
one_tf <- function(x) {uj::n_tf(x) == 1}

#' @rdname failsafe
#' @export
one_dif <- function(x, y) {uj::n_dif(x, y) == 1}

#' @rdname failsafe
#' @export
one_nav <- function(x) {uj::n_nav(x) == 1}

#' @rdname failsafe
#' @export
one_okv <- function(x) {uj::n_okv(x) == 1}

#' @rdname failsafe
#' @export
has_one <- function(x, y) {uj::n_has(x, y) == 1}

#' @rdname failsafe
#' @export
lacks_one <- function(x, y) {uj::n_lacks(x, y) == 1}

#' @rdname failsafe
#' @export
all_f <- function(x) {base::all(uj::is_f(x))}

#' @rdname failsafe
#' @export
all_t <- function(x) {base::all(uj::is_t(x))}

#' @rdname failsafe
#' @export
all_bl <- function(x) {base::all(uj::is_bl(x))}

#' @rdname failsafe
#' @export
all_eq <- function(x, y) {base::all(uj::is_eq(x, y))}

#' @rdname failsafe
#' @export
all_ge <- function(x, y) {base::all(uj::is_ge(x, y))}

#' @rdname failsafe
#' @export
all_gt <- function(x, y) {base::all(uj::is_gt(x, y))}

#' @rdname failsafe
#' @export
all_in <- function(x, ...) {base::all(uj::is_in(x, ...))}

#' @rdname failsafe
#' @export
all_le <- function(x, y) {base::all(uj::is_le(x, y))}

#' @rdname failsafe
#' @export
all_lt <- function(x, y) {base::all(uj::is_lt(x, y))}

#' @rdname failsafe
#' @export
all_mf <- function(x, ...) {base::all(uj::is_mf(x, ...))}

#' @rdname failsafe
#' @export
all_tf <- function(x) {base::all(uj::is_tf(x))}

#' @rdname failsafe
#' @export
all_dif <- function(x, y) {base::all(uj::is_dif(x, y))}

#' @rdname failsafe
#' @export
all_nav <- function(x) {base::all(uj::is_nav(x))}

#' @rdname failsafe
#' @export
all_okv <- function(x) {base::all(uj::is_okv(x))}

#' @rdname failsafe
#' @export
has_all <- function(x, y) {base::all(uj::has(x, y))}

#' @rdname failsafe
#' @export
lacks_all <- function(x, y) {base::all(uj::lacks(x, y))}

#' @rdname failsafe
#' @export
some_f <- function(x) {uj::n_f(x) > 1}

#' @rdname failsafe
#' @export
some_t <- function(x) {uj::n_t(x) > 1}

#' @rdname failsafe
#' @export
some_bl <- function(x) {uj::n_bl(x) > 1}

#' @rdname failsafe
#' @export
some_eq <- function(x, y) {uj::n_eq(x, y) > 1}

#' @rdname failsafe
#' @export
some_le <- function(x, y) {uj::n_le(x, y) > 1}

#' @rdname failsafe
#' @export
some_ge <- function(x, y) {uj::n_ge(x, y) > 1}

#' @rdname failsafe
#' @export
some_gt <- function(x, y) {uj::n_gt(x, y) > 1}

#' @rdname failsafe
#' @export
some_in <- function(x, ...) {uj::n_in(x, ...) > 1}

#' @rdname failsafe
#' @export
some_lt <- function(x, y) {uj::n_lt(x, y) > 1}

#' @rdname failsafe
#' @export
some_mf <- function(x, ...) {uj::n_mf(x, ...) > 1}

#' @rdname failsafe
#' @export
some_tf <- function(x) {uj::n_tf(x) > 1}

#' @rdname failsafe
#' @export
some_dif <- function(x, y) {uj::n_dif(x, y) > 1}

#' @rdname failsafe
#' @export
some_nav <- function(x) {uj::n_nav(x) > 1}

#' @rdname failsafe
#' @export
some_okv <- function(x) {uj::n_okv(x) > 1}

#' @rdname failsafe
#' @export
has_some <- function(x, y) {uj::n_has(x, y) > 1}

#' @rdname failsafe
#' @export
lacks_some <- function(x, y) {uj::n_lacks(x, y) > 1}

#' @rdname failsafe
#' @export
two_f <- function(x) {uj::n_f(x) == 2}

#' @rdname failsafe
#' @export
two_t <- function(x) {uj::n_t(x) == 2}

#' @rdname failsafe
#' @export
two_bl <- function(x) {uj::n_bl(x) == 2}

#' @rdname failsafe
#' @export
two_eq <- function(x, y) {uj::n_eq(x, y) == 2}

#' @rdname failsafe
#' @export
two_ge <- function(x, y) {uj::n_ge(x, y) == 2}

#' @rdname failsafe
#' @export
two_gt <- function(x, y) {uj::n_gt(x, y) == 2}

#' @rdname failsafe
#' @export
two_in <- function(x, ...) {uj::n_in(x, ...) == 2}

#' @rdname failsafe
#' @export
two_le <- function(x, y) {uj::n_le(x, y) == 2}

#' @rdname failsafe
#' @export
two_lt <- function(x, y) {uj::n_lt(x, y) == 2}

#' @rdname failsafe
#' @export
two_mf <- function(x, ...) {uj::n_mf(x, ...) == 2}

#' @rdname failsafe
#' @export
two_tf <- function(x) {uj::n_tf(x) == 2}

#' @rdname failsafe
#' @export
two_dif <- function(x, y) {uj::n_dif(x, y) == 2}

#' @rdname failsafe
#' @export
two_nav <- function(x) {uj::n_nav(x) == 2}

#' @rdname failsafe
#' @export
two_okv <- function(x) {uj::n_okv(x) == 2}

#' @rdname failsafe
#' @export
has_two <- function(x, y) {uj::n_has(x, y) == 2}

#' @rdname failsafe
#' @export
lacks_two <- function(x, y) {uj::n_lacks(x, y) == 2}

#' @rdname failsafe
#' @export
many_f <- function(x) {uj::n_f(x) > 2}

#' @rdname failsafe
#' @export
many_t <- function(x) {uj::n_t(x) > 2}

#' @rdname failsafe
#' @export
many_bl <- function(x) {uj::n_bl(x) > 2}

#' @rdname failsafe
#' @export
many_eq <- function(x, y) {uj::n_eq(x, y) > 2}

#' @rdname failsafe
#' @export
many_ge <- function(x, y) {uj::n_ge(x, y) > 2}

#' @rdname failsafe
#' @export
many_gt <- function(x, y) {uj::n_gt(x, y) > 2}

#' @rdname failsafe
#' @export
many_in <- function(x, ...) {uj::n_in(x, ...) > 2}

#' @rdname failsafe
#' @export
many_le <- function(x, y) {uj::n_le(x, y) > 2}

#' @rdname failsafe
#' @export
many_lt <- function(x, y) {uj::n_lt(x, y) > 2}

#' @rdname failsafe
#' @export
many_mf <- function(x, ...) {uj::n_mf(x, ...) > 2}

#' @rdname failsafe
#' @export
many_tf <- function(x) {uj::n_tf(x) > 2}

#' @rdname failsafe
#' @export
many_dif <- function(x, y) {uj::n_dif(x, y) > 2}

#' @rdname failsafe
#' @export
many_nav <- function(x) {uj::n_nav(x) > 2}

#' @rdname failsafe
#' @export
many_okv <- function(x) {uj::n_okv(x) > 2}

#' @rdname failsafe
#' @export
has_many <- function(x, y) {uj::n_has(x, y) > 2}

#' @rdname failsafe
#' @export
lacks_many <- function(x, y) {uj::n_lacks(x, y) > 2}

#' @rdname failsafe
#' @export
`%.in0.%` <- function(x, y) {uj::is_in0(x, y)}

#' @rdname failsafe
#' @export
`%.mf0.%` <- function(x, y) {uj::is_mf0(x, y)}

#' @rdname failsafe
#' @export
`%.eq0.%` <- function(x, y) {uj::is_eq0(x, y)}

#' @rdname failsafe
#' @export
`%.ge0.%` <- function(x, y) {uj::is_ge0(x, y)}

#' @rdname failsafe
#' @export
`%.gt0.%` <- function(x, y) {uj::is_gt0(x, y)}

#' @rdname failsafe
#' @export
`%.le0.%` <- function(x, y) {uj::is_le0(x, y)}

#' @rdname failsafe
#' @export
`%.lt0.%` <- function(x, y) {uj::is_lt0(x, y)}

#' @rdname failsafe
#' @export
`%.has0.%` <- function(x, y) {uj::has0(x, y)}

#' @rdname failsafe
#' @export
`%.dif0.%` <- function(x, y) {uj::is_dif0(x, y)}

#' @rdname failsafe
#' @export
`%.lacks0.%` <- function(x, y) {uj::lacks0(x, y)}

#' @rdname failsafe
#' @export
`%.in.%` <- function(x, y) {uj::is_in(x, y)}

#' @rdname failsafe
#' @export
`%.mf.%` <- function(x, y) {uj::is_mf(x, y)}

#' @rdname failsafe
#' @export
`%.eq.%` <- function(x, y) {uj::is_eq(x, y)}

#' @rdname failsafe
#' @export
`%.ge.%` <- function(x, y) {uj::is_ge(x, y)}

#' @rdname failsafe
#' @export
`%.gt.%` <- function(x, y) {uj::is_gt(x, y)}

#' @rdname failsafe
#' @export
`%.le.%` <- function(x, y) {uj::is_le(x, y)}

#' @rdname failsafe
#' @export
`%.lt.%` <- function(x, y) {uj::is_lt(x, y)}

#' @rdname failsafe
#' @export
`%.has.%` <- function(x, y) {uj::has(x, y)}

#' @rdname failsafe
#' @export
`%.dif.%` <- function(x, y) {uj::is_dif(x, y)}

#' @rdname failsafe
#' @export
`%.lacks.%` <- function(x, y) {uj::lacks(x, y)}

#' @rdname failsafe
#' @export
`%.seq.%` <- function(x, y) {uj::is_seq(x, y)}

#' @rdname failsafe
#' @export
`%.veq.%` <- function(x, y) {uj::is_veq(x, y)}

#' @rdname failsafe
#' @export
`%.neq.%` <- function(x, y) {uj::neq(x, y)}

#' @rdname failsafe
#' @export
`%.sdif.%` <- function(x, y) {uj::is_sdif(x, y)}

#' @rdname failsafe
#' @export
`%.vdif.%` <- function(x, y) {uj::is_vdif(x, y)}

#' @rdname failsafe
#' @export
`%.ndif.%` <- function(x, y) {uj::ndif(x, y)}

#' @rdname failsafe
#' @export
`%.or0.%` <- function(x, y) {uj::or0(x, y)}

#' @rdname failsafe
#' @export
`%.and0.%` <- function(x, y) {uj::and0(x, y)}

#' @rdname failsafe
#' @export
`%.xor0.%` <- function(x, y) {uj::xor0(x, y)}

#' @rdname failsafe
#' @export
`%.nor0.%` <- function(x, y) {uj::nor0(x, y)}

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
