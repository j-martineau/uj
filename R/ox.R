#' @encoding UTF-8
#' @family strings
#' @title Oxford-comma separated lists
#' @description Create Oxford-comma separated lists with a variety of templates (see *details*).
#' \cr\cr With the exception of `{n} > length(uj::av(...))`, these functions appropriately process lists of length `1` and `2`.
#' @details Each function in this family operates from a template as shown in the following tables where `{conj}` and `{n}` represent the values of arguments `conj` and `n`; `{pref}` and `{comp}` indicate the potentially-`NULL` values of arguments `pref` and `conj`; and `[a]`, `[b]`, and `[z]` represents elements of a list.
#' \cr\cr **Simple lists**
#' \cr\cr These functions do not incorporate a number into the Oxford-comma separated list:
#' \tabular{ll}{  `ox`              \tab `'(pref) [a], [b], ..., {conj} [z]'`         \cr   \tab  }
#' \tabular{ll}{  `ox_or `          \tab `'(pref) [a], [b], ..., or [z]'`             \cr
#'                `ox_and`          \tab `'(pref) [a], [b], ..., and [z]'`            \cr   \tab  }
#' \tabular{ll}{  `ox_any `         \tab `'(pref) any of [a], [b], ..., {conj} [z]'`  \cr
#'                `ox_all `         \tab `'(pref) all of [a], [b], ..., {conj} [z]'`  \cr
#'                `ox_none`         \tab `'(pref) none of [a], [b], ..., {conj} [z]'` \cr
#'                `ox_some`         \tab `'(pref) some of [a], [b], ..., {conj} [z]'` \cr   \tab  }
#' \tabular{ll}{  `ox_either `      \tab `'(pref) either [a], [b], ..., or [z]'`      \cr
#'                `ox_neither`      \tab `'(pref) neither [a], [b], ..., nor [z]'`                }
#' \cr\cr **Numeric-comparison lists**
#' \cr\cr These functions incorporate a number into the Oxford-comma separated list:
#' \tabular{ll}{  `ox_n`            \tab `'(pref) {n} of [a], [b], ..., {conj} [z]'`                 \cr   \tab  }
#' \tabular{ll}{  `ox_exactly`      \tab `'(pref) exactly {n} of [a], [b], ..., {conj} [z]'`         \cr   \tab  }
#' \tabular{ll}{  `ox_more`         \tab `'(pref) more than {n} of [a], [b], ..., {conj} [z]'`       \cr
#'                `ox_no_more`      \tab `'(pref) no more than {n} of [a], [b], ..., {conj} [z]'`    \cr
#'                `ox_or_more`      \tab `'(pref) {n} or more of [a], [b], ..., {conj} [z]'`         \cr
#'                `ox_at_most`      \tab `'(pref) at most {n} of [a], [b], ..., {conj} [z]'`         \cr   \tab  }
#' \tabular{ll}{  `ox_less`         \tab `'(pref) less than {n} of [a], [b], ..., {conj} [z]'`       \cr
#'                `ox_or_less`      \tab `'(pref) {n} or less of [a], [b], ..., {conj} [z]'`         \cr
#'                `ox_no_less`      \tab `'(pref) no less than {n} of [a], [b], ..., {conj} [z]'`    \cr
#'                `ox_at_least`     \tab `'(pref) at least {n} of [a], [b], ..., {conj} [z]'`        \cr   \tab  }
#' \tabular{ll}{  `ox_fewer`        \tab `'(pref) fewer than {n} of [a], [b], ..., {conj} [z]'`      \cr
#'                `ox_no_fewer`     \tab `'(pref) no fewer than {n} of [a], [b], ..., {conj} [z]'`   \cr
#'                `ox_or_fewer`     \tab `'(pref) {n} or fewer of [a], [b], ..., {conj} [z]'`        \cr   \tab  }
#' \tabular{ll}{  `ox_greater`      \tab `'(pref) greater than {n} of [a], [b], ..., {conj} [z]'`    \cr
#'                `ox_no_greater`   \tab `'(pref) no greater than {n} of [a], [b], ..., {conj} [z]'` \cr
#'                `ox_or_greater`   \tab `'(pref) {n} or greater of [a], [b], ..., {conj} [z]'`        }
#' @param ... Any number of arguments coerceable to mode character.
#' @param pref A \link[=cmp_chr_scl]{complete character scalar} prefix to prepend to the list.
#' @param conj A complete character scalar conjunction to use between the next to last and last elements of the list. Typical values are `and`, `or` and `nor`.
#' @param n A \link[=cmp_psw_scl]{complete positive whole-number scalar}.
#' @param comp A complete character scalar used for comparing to `n`, such as `'at least'` or `'or fewer'`.
#' @param first `TRUE` or `FALSE` used to determine whether `comp` is placed in front of `n` rather than after `n`.
#' @return A character scalar.
#' @examples
#' egFruits <- c("apples", "bananas", "oranges")
#'
#' ox(egFruits)
#' ox("apples", "bananas", "orange")
#' ox_or(egFruits)
#' ox_and(egFruits)
#' ox_nor(egFruits)
#' ox_nor(egFruits, pref = "")
#'
#' ox(egFruits)
#' ox(egFruits, conj = "or")
#' ox(egFruits, pref = "neither", conj = "nor")
#'
#' ox(egFruits, pref = "", conj = "nor")
#' ox(egFruits, pref = "either", conj = "or")
#' ox(egFruits, pref = "all of", conj = "and")
#'
#' ox_either(egFruits)
#' ox_either("apples", "bananas", "oranges")
#'
#' ox_neither(egFruits)
#' ox_neither("apples", "bananas", "oranges")
#'
#' ox_all(egFruits)
#' ox_all("apples", "bananas", "oranges")
#'
#' ox_none(egFruits)
#' ox_none("apples", "bananas", "oranges")
#'
#' ox_n(egFruits, n = 1)
#' ox_n(egFruits, conj = "and", n = 2, comp = "from among")
#' ox_n(egFruits, conj = "and", n = 2, comp = "at least", first = TRUE)
#' ox_n(egFruits, conj = "and", n = 2, comp = "or more", first = FALSE)
#'
#' ox_exactly(egFruits, n = 2)
#' ox_less(egFruits, n = 2)
#' ox_more(egFruits, n = 2)
#' ox_fewer(egFruits, n = 2)
#' ox_greater(egFruits, n = 2)
#' ox_at_least(egFruits, n = 2)
#' ox_at_most(egFruits, n = 2)
#' ox_no_greater(egFruits, n = 2)
#' ox_no_fewer(egFruits, n = 2)
#' ox_no_more(egFruits, n = 2)
#' ox_no_less(egFruits, n = 2)
#' ox_or_more(egFruits, n = 2)
#' ox_or_greater(egFruits, n = 2)
#' ox_or_less(egFruits, n = 2)
#' ox_or_fewer(egFruits, n = 2)
#' @export
ox <- function(..., conj = "and", pref = "", quote = 0) {
  vals <- uj::av(...)
  uj::errs_if_nots(uj::N1P(vals)        , "[...] is empty."                                           ,
                   uj::cmp_atm(vals)   , "Arguments in [...] must be complete and atomic (?cmp_atm)." ,
                   uj::cmp_chr_scl(conj), "[conj] must be a complete character scalar (?cmp_chr_scl).",
                   uj::cmp_chr_scl(pref), "[pref] must be a complete character scalar (?cmp_chr_scl).",
                   uj::isIN1(quote, 0:2), "[quote] must be 0, 1, or 2."                               , PKG = "uj")
  n.vals <- uj::N(vals)
  if (n.vals == 1) {uj::errs_if_nots(conj != 'nor'                  , "[conj = 'nor'], but [...] contains only 1 atomic element.",
                                     conj != "or" | pref != "either", "[conj = 'or'] and [pref = 'either'], but [...] contains only 1 atomic element.", PKG = "uj")}
  if (pref != "") {pref <- uj::p0(pref, " ")}                                    # if pref(ix) is not empty, follow it with a space
  last <- vals[n.vals]                                                           # get the last element of X
  if (n.vals > 1) {                                                              # if there is more than one element in the list
    list <- uj::g(", ", vals[1:(n.vals - 1)])                                    # : create a comma separated list with all but the last element of X
    conj <- uj::p0(uj::f0(n.vals == 2, " ", ", "), conj, " ")                    # : if there are only 2, no additional comma to conj the last element to the list
  } else {list <- conj <- ""}                                                    # ELSE the list of non-last elements and the conj for the last item aren't needed
  uj::p0(pref, list, conj, last)                                                 # put everything together
}

#' @rdname ox
#' @export
oxford <- ox

#' @rdname ox
#' @export
oxfordCOMMA <- ox

#' @rdname ox
#' @export
ox_n <- function(..., conj = "and", comp = "", quote = 0, n = 1, first = TRUE) {
  vals <- uj::av(...)
  uj::errs_if_nots(uj::N1P(vals)       , "[...] is empty.",
                 uj::atm_cmp(vals)     , "Arguments in [...] must be complete and atomic (?cmp_atm_)."   ,
                 uj::cmp_chr_scl(conj) , "[conj] must be a complete character scalar (?cmp_chr_scl)."    ,
                 uj::cmp_chr_scl(comp) , "[comp] must be a non-NA character scalar (?cmp_chr_scl)."      ,
                 uj::isIN1(quote, 0:2) , "[quote] must be 0, 1, or 2."                                   ,
                 uj::cmp_nnw_scl(n)    , "[n] must be a non-negative whole number scalar (?cmp_nnw_scl).",
                 uj::cmp_lgl_scl(first), "[first] must be TRUE or FALSE."                                , PKG = "uj")
  pref <- uj::f0(comp == "", uj::p1(n, "of"), uj::f0(first, uj::p1(comp, n, "of"), uj::p1(n, comp, "of")))
  uj::ox(vals, conj = conj, pref = pref, quote = quote)
}

#' @rdname ox
#' @export
ox_and <- function(..., pref = "") {uj::ox(..., pref = pref, conj = "and")}

#' @rdname ox
#' @export
ox_or <- function(..., pref = "") {uj::ox(..., pref = pref, conj = "or")}

#' @rdname ox
#' @export
ox_nor <- function(..., pref = "neither") {uj::ox(..., pref = pref, conj = "nor")}

#' @rdname ox
#' @export
ox_either <- function(...) {uj::ox(..., pref = "either", conj = "or")}

#' @rdname ox
#' @export
ox_neither <- function(...) {uj::ox(..., pref = "neither", conj = "nor")}

#' @rdname ox
#' @export
ox_all <- function(..., conj = "and") {uj::ox(..., pref = "all of", conj = conj)}

#' @rdname ox
#' @export
ox_any <- function(..., conj = "or") {uj::ox(..., pref = "any of", conj = conj)}

#' @rdname ox
#' @export
ox_none <- function(..., conj = "or") {uj::ox(..., pref = "none of", conj = conj)}

#' @rdname ox
#' @export
ox_some <- function(..., conj = "and") {uj::ox(..., pref = "some of", conj = conj)}

#' @rdname ox
#' @export
ox_exactly <- function(..., conj = "or", n = 1) {uj::ox_n(..., comp = "exactly", n = n, conj = conj)}

#' @rdname ox
#' @export
ox_less <- function(..., conj = "and", n = 2) {uj::ox_n(..., comp = "less than", n = n)}

#' @rdname ox
#' @export
ox_more <- function(..., conj = "and", n = 1) {uj::ox_n(..., comp = "more than", n = n)}

#' @rdname ox
#' @export
ox_fewer <- function(..., conj = "and", n = 2) {uj::ox_n(..., comp = "fewer than", n = n)}

#' @rdname ox
#' @export
ox_greater <- function(..., conj = "and", n = 2) {uj::ox_n(..., comp = "greater than", n = n)}

#' @rdname ox
#' @export
ox_at_least <- function(..., conj = "and", n = 1) {uj::ox_n(..., comp = "at least", n = n)}

#' @rdname ox
#' @export
ox_at_most <- function(..., conj = "and", n = 1) {uj::ox_n(..., comp = "at most", n = n)}

#' @rdname ox
#' @export
ox_no_greater <- function(..., conj = "and", n = 1) {uj::ox_n(..., comp = "no greater than", n = n)}

#' @rdname ox
#' @export
ox_no_fewer <- function(..., conj = "and", n = 1) {uj::ox_n(..., comp = "no fewer than", n = n)}

#' @rdname ox
#' @export
ox_no_more <- function(..., conj = "and", n = 1) {uj::ox_n(..., comp = "no more than", n = n)}

#' @rdname ox
#' @export
ox_no_less <- function(..., conj = "and", n = 1) {uj::ox_n(..., comp = "no less than", n = n)}

#' @rdname ox
#' @export
ox_or_more <- function(..., conj = "and", n = 1) {uj::ox_n(..., comp = "or more", n = n, first = F)}

#' @rdname ox
#' @export
ox_or_greater <- function(..., conj = "and", n = 1) {uj::ox_n(..., comp = "or greater", n = n, first = F)}

#' @rdname ox
#' @export
ox_or_less <- function(..., conj = "and", n = 1) {uj::ox_n(..., comp = "or less", n = n, first = F)}

#' @rdname ox
#' @export
ox_or_fewer <- function(..., conj = "and", n = 1) {uj::ox_n(..., comp = "or fewer", n = n, first = F)}
