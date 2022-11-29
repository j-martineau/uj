#' @name ox
#' @family strings
#' @title Oxford-comma separated lists
#' @description Create Oxford-comma separated lists with a variety of templates (displayed below) where `{conj}` and `{n}` represent the values of arguments `conj` and `n`; `{pref}` and `{comp}` indicate the potentially-`NULL` values of arguments `pref` and `conj`; and `[a]`, `[b]`, and `[z]` represents elements of a list.
#' \cr\cr
#' **Functions and associated templates**\tabular{ll}{
#'   \cr`ox`            \tab`'(pref) [a], [b], ..., {conj} [z]'`
#'   \cr`ox_and`        \tab`'(pref) [a], [b], ..., and [z]'`
#'   \cr`ox_or`         \tab`'(pref) [a], [b], ..., or [z]'`
#'   \cr`ox_either`     \tab`'(pref) either [a], [b], ..., or [z]'`
#'   \cr`ox_neither`    \tab`'(pref) neither [a], [b], ..., nor [z]'`
#'   \cr`ox_any`        \tab`'(pref) any of [a], [b], ..., {conj} [z]'`
#'   \cr`ox_all`        \tab`'(pref) all of [a], [b], ..., {conj} [z]'`
#'   \cr`ox_none`       \tab`'(pref) none of [a], [b], ..., {conj} [z]'`
#'   \cr`ox_some`       \tab`'(pref) some of [a], [b], ..., {conj} [z]'`
#'   \cr                \tab  
#'   \cr`ox_n`          \tab`'(pref) {n} of [a], [b], ..., {conj} [z]'`
#'   \cr                \tab`'(pref) {n} (comp) of [a], [b], ..., {conj} [z]'`
#'   \cr                \tab`'(pref) (comp) {n} of [a], [b], ..., {conj} [z]'`
#'   \cr`ox_exactly`    \tab`'(pref) exactly {n} of [a], [b], ..., {conj} [z]'`
#'   \cr`ox_less`       \tab`'(pref) less than {n} of [a], [b], ..., {conj} [z]'`
#'   \cr`ox_more`       \tab`'(pref) more than {n} of [a], [b], ..., {conj} [z]'`
#'   \cr`ox_fewer`      \tab`'(pref) fewer than {n} of [a], [b], ..., {conj} [z]'`
#'   \cr`ox_greater`    \tab`'(pref) greater than {n} of [a], [b], ..., {conj} [z]'`
#'   \cr`ox_orless`     \tab`'(pref) {n} or less of [a], [b], ..., {conj} [z]'`
#'   \cr`ox_ormore`     \tab`'(pref) {n} or more of [a], [b], ..., {conj} [z]'`
#'   \cr`ox_orfewer`    \tab`'(pref) {n} or fewer of [a], [b], ..., {conj} [z]'`
#'   \cr`ox_orgreater`  \tab`'(pref) {n} or greater of [a], [b], ..., {conj} [z]'`
#'   \cr`ox_atmost`     \tab`'(pref) at most {n} of [a], [b], ..., {conj} [z]'`
#'   \cr`ox_atleast`    \tab`'(pref) at least {n} of [a], [b], ..., {conj} [z]'`
#'   \cr`ox_noless`     \tab`'(pref) no less than {n} of [a], [b], ..., {conj} [z]'`
#'   \cr`ox_nomore`     \tab`'(pref) no more than {n} of [a], [b], ..., {conj} [z]'`
#'   \cr`ox_nofewer`    \tab`'(pref) no fewer than {n} of [a], [b], ..., {conj} [z]'`
#'   \cr`ox_nogreater`  \tab`'(pref) no greater than {n} of [a], [b], ..., {conj} [z]'`
#' }
#' NOTE: where it makes sense these functions appropriately process lists of length `1` and `2`.
#' @param ... Any number of arguments coerceable to mode character.
#' @param pref A \link[=cmp_chr_scl]{complete character scalar} prefix to prepend to the list.
#' @param conj A complete character scalar conjunction to use between the next to last and last elements of the list. Typical values are `and`, `or` and `nor`.
#' @param n A \link[=cmp_psw_scl]{complete positive whole-number scalar}.
#' @param comp A complete character scalar used for comparing to `n`, such as `'at least'` or `'or fewer'`.
#' @param first A complete non-`NA` scalar used to determine whether `comp` is placed in front of `n` rather than after `n`.
#' @return A character scalar containing all atomic elements of `...` formatted as an Oxford-comma separated list.
#' @export
#' @examples
#' Fruits <- c("apples", "bananas", "oranges")
#'
#' ox(Fruits)
#' ox("apples", "bananas", "orange")
#' ox_or(Fruits)
#' ox_or(Fruits, pref = "neither")
#' ox_nor(Fruits, pref = "")
#' ox_nor(Fruits, pref = NULL)
#'
#' ox(Fruits)
#' ox(Fruits, conj = "or")
#' ox(Fruits, conj = "nor")
#'
#' ox(Fruits, pref = "", conj = "nor")
#' ox(Fruits, pref = "either", conj = "or")
#' ox(Fruits, pref = "all of", conj = "and")
#'
#' ox_either(Fruits)
#' ox_either("apples", "bananas", "oranges")
#'
#' ox_neither(Fruits)
#' ox_neither("apples", "bananas", "oranges")
#'
#' ox_all(Fruits)
#' ox_all("apples", "bananas", "oranges")
#'
#' ox_none(Fruits)
#' ox_none("apples", "bananas", "oranges")
#'
#' ox_n(Fruits, n = 1)
#' ox_n(Fruits, conj = "and", n = 2, comp = "from among")
#' ox_n(Fruits, conj = "and", n = 2, comp = "at least", comp.first = TRUE)
#'
#' ox_exactly(Fruits, n = 2)
#' ox_less(Fruits, n = 2)
#' ox_more(Fruits, n = 2)
#' ox_fewer(Fruits, n = 2)
#' ox_greater(Fruits, n = 2)
#' ox_atleast(Fruits, n = 2)
#' ox_atmost(Fruits, n = 2)
#' ox_nogreater(Fruits, n = 2)
#' ox_nofewer(Fruits, n = 2)
#' ox_nomore(Fruits, n = 2)
#' ox_noless(Fruits, n = 2)
#' ox_ormore(Fruits, n = 2)
#' ox_orgreater(Fruits, n = 2)
#' ox_orless(Fruits, n = 2)
#' ox_orfewer(Fruits, n = 2)
#' @export
ox <- function(..., conj = "and", pref = "", quote = 0) {
  vals <- av(...)
  errs <- c(f0(length(vals) > 0 , NULL, "\n \u2022 [...] is empty."),
            f0(icmp(vals)       , NULL, "\n \u2022 Arguments in [...] must be complete and atomic (?icmp)."),
            f0(cmp_chr_scl(conj), NULL, "\n \u2022 [conj] must be a complete character scalar (?cmp_chr_scl)."),
            f0(cmp_chr_scl(pref), NULL, "\n \u2022 [pref] must be a complete character scalar (?cmp_chr_scl)."),
            f0(isIN(quote, 0:2) , NULL, "\n \u2022 [quote] must be 0, 1, or 2."))
  if (!is.null(errs)) {stop(errs)}
  n.vals <- length(vals)
  if (n.vals == 1) {
    errs <- c(f0(conj != 'nor'                  , NULL, "\n \u2022 [conj = 'nor'], but [...] contains only 1 atomic element."),
              f0(conj != "or" | pref != "either", NULL, "\n \u2022 [conj = 'or'] and [pref = 'either'], but [...] contains only 1 atomic element."))
    if (!is.null(errs)) {stop(errs)}
  }
  if (pref != "") {pref <- paste0(pref, " ")}                                    # if pref(ix) is not empty, follow it with a space
  last <- vals[n.vals]                                                           # get the last element of X
  if (n.vals > 1) {                                                              # if there is more than one element in the list
    list <- paste0(vals[1:(n.vals - 1)], collapse = ", ")                        # : create a comma separated list with all but the last element of X
    conj <- paste0(f0(n.vals == 2, " ", ", "), conj, " ")                        # : if there are only 2, no additional comma to conj the last element to the list
  } else {list <- conj <- ""}                                                    # ELSE the list of non-last elements and the conj for the last item aren't needed
  paste0(pref, list, conj, last)                                                 # put everything together
}

#' @describeIn ox Oxford-comma-separated list including a numeric requirement.
#' @export
ox_n <- function(..., conj = "and", comp = "", quote = 0, n = 1, first = TRUE) {
  vals <- av(...)
  errs <- c(f0(length(vals) > 0 , NULL, "\n \u2022 [...] is empty."),
            f0(icmp(vals)       , NULL, "\n \u2022 Arguments in [...] must be complete and atomic (?icmp)."),
            f0(cmp_chr_scl(conj), NULL, "\n \u2022 [conj] must be a complete character scalar (?cmp_chr_scl)."),
            f0(cmp_chr_scl(comp), NULL, "\n \u2022 [comp] must be a non-NA character scalar (?cmp_chr_scl)."),
            f0(isIN(quote, 0:2) , NULL, "\n \u2022 [quote] must be 0, 1, or 2."),
            f0(cmp_nnw_scl(n)   , NULL, "\n \u2022 [n] must be a non-negative whole number scalar (?cmp_nnw_scl)."),
            f0(isTF(first)      , NULL, "\n \u2022 [first] must be TRUE or FALSE."))
  if (!is.null(errs)) {stop(errs)}
  pref <- f0(comp == "", paste(n, "of"), f0(first, paste(n, comp, "of"), paste(comp, n, "of")))
  ox(vals, conj = conj, pref = pref, quote = quote)
}

#' @describeIn ox Oxford-comma-separated 'and' list.
#' @export
ox_and <- function(..., pref = NULL) {ox(..., pref = pref, conj = "and")}

#' @describeIn ox Oxford-comma-separated 'or' list.
#' @export
ox_or <- function(..., pref = NULL) {ox(..., pref = pref, conj = "or")}

#' @describeIn ox Oxford-comma-separated 'nor' list.
#' @export
ox_nor <- function(..., pref = "neither") {ox(..., pref = pref, conj = "nor")}

#' @describeIn ox Oxford-comma-separated 'either' list.
#' @export
ox_either <- function(...) {ox(..., pref = "either", conj = "or")}

#' @describeIn ox Oxford-comma-separated 'neither/nor' list.
#' @export
ox_neither <- function(...) {ox(..., pref = "neither", conj = "nor")}

#' @describeIn ox Oxford-comma-separated 'all of' list.
#' @export
ox_all <- function(..., conj = "and") {ox(..., pref = "all of", conj = conj)}

#' @describeIn ox Oxford-comma-separated 'any of' list.
#' @export
ox_any <- function(..., conj = "and") {ox(..., pref = "any of", conj = conj)}

#' @describeIn ox Oxford-comma-separated 'all of' list.
#' @export
ox_none <- function(..., conj = "or") {ox(..., pref = "none of", conj = conj)}

#' @describeIn ox Oxford-comma-separated 'some of' list.
#' @export
ox_some <- function(..., conj = "and") {ox(..., pref = "some of", conj = conj)}

#' @describeIn ox Oxford-comma-separated 'exactly n of' list.
#' @export
ox_exactly <- function(..., conj = "or", n = 1) {ox_n(..., comp = "exactly", n = n, conj = conj)}

#' @describeIn ox Oxford-comma-separated 'less than n of' list.
#' @export
ox_less <- function(..., conj = "and", n = 2) {ox_n(..., comp = "less than", n = n)}

#' @describeIn ox Oxford-comma-separated 'more than n of' list.
#' @export
ox_more <- function(..., conj = "and", n = 1) {ox_n(..., comp = "more than", n = n)}

#' @describeIn ox Oxford-comma-separated 'fewer than n of' list.
#' @export
ox_fewer <- function(..., conj = "and", n = 2) {ox_n(..., comp = "fewer than", n = n)}

#' @describeIn ox Oxford-comma-separated 'greater than n of' list.
#' @export
ox_greater <- function(..., conj = "and", n = 2) {ox_n(..., comp = "greater than", n = n)}

#' @describeIn ox Oxford-comma-separated 'at least n of' list.
#' @export
ox_atleast <- function(..., conj = "and", n = 1) {ox_n(..., comp = "at least", n = n)}

#' @describeIn ox Oxford-comma-separated 'at most n of' list.
#' @export
ox_atmost <- function(..., conj = "and", n = 1) {ox_n(..., comp = "at most", n = n)}

#' @describeIn ox Oxford-comma-separated 'no greater than n of' list.
#' @export
ox_nogreater <- function(..., conj = "and", n = 1) {ox_n(..., comp = "no greater than", n = n)}

#' @describeIn ox Oxford-comma-separated 'no fewer than n of' list.
#' @export
ox_nofewer <- function(..., conj = "and", n = 1) {ox_n(..., comp = "no fewer than", n = n)}

#' @describeIn ox Oxford-comma-separated 'no more than n of' list.
#' @export
ox_nomore <- function(..., conj = "and", n = 1) {ox_n(..., comp = "no more than", n = n)}

#' @describeIn ox Oxford-comma-separated 'no less than n of' list.
#' @export
ox_noless <- function(..., conj = "and", n = 1) {ox_n(..., comp = "no less than", n = n)}

#' @describeIn ox Oxford-comma-separated 'n or more of' list.
#' @export
ox_ormore <- function(..., conj = "and", n = 1) {ox_n(..., comp = "or more", n = n, comp.first = F)}

#' @describeIn ox Oxford-comma-separated 'n or greater of' list.
#' @export
ox_orgreater <- function(..., conj = "and", n = 1) {ox_n(..., comp = "or greater", n = n, comp.first = F)}

#' @describeIn ox Oxford-comma-separated 'n or less of' list.
#' @export
ox_orless <- function(..., conj = "and", n = 1) {ox_n(..., comp = "or less", n = n, comp.first = F)}

#' @describeIn ox Oxford-comma-separated 'n or fewer of' list.
#' @export
ox_orfewer <- function(..., conj = "and", n = 1) {ox_n(..., comp = "or fewer", n = n, comp.first = F)}
