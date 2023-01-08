#' @encoding UTF-8
#' @family strings
#' @title Oxford-comma separated lists
#' @description Create Oxford-comma separated lists with a variety of templates (displayed below) where `{conj}` and `{n}` represent the values of arguments `conj` and `n`; `{pref}` and `{comp}` indicate the potentially-`NULL` values of arguments `pref` and `conj`; and `[a]`, `[b]`, and `[z]` represents elements of a list.
#' \cr\cr  With the exception of `{n} > length(uj::av(...))`, these functions appropriately process lists of length `1` and `2`.
#' \cr\cr **Functions and associated templates**
#' \tabular{rl}{
#'           **Function**   \tab **Associated template**
#'   \cr                    \tab   
#'   \cr             `ox`   \tab `'(pref) [a], [b], ..., {conj} [z]'`
#'   \cr                    \tab   
#'   \cr           `ox_n`   \tab `'(pref) {n} of [a], [b], ..., {conj} [z]'`
#'   \cr                    \tab `'(pref) {n} (comp) of [a], [b], ..., {conj} [z]'`
#'   \cr                    \tab `'(pref) (comp) {n} of [a], [b], ..., {conj} [z]'`
#'   \cr                    \tab   
#'   \cr          `ox_or`   \tab `'(pref) [a], [b], ..., or [z]'`
#'   \cr         `ox_and`   \tab `'(pref) [a], [b], ..., and [z]'`
#'   \cr      `ox_either`   \tab `'(pref) either [a], [b], ..., or [z]'`
#'   \cr     `ox_neither`   \tab `'(pref) neither [a], [b], ..., nor [z]'`
#'   \cr                    \tab   
#'   \cr         `ox_any`   \tab `'(pref) any of [a], [b], ..., {conj} [z]'`
#'   \cr         `ox_all`   \tab `'(pref) all of [a], [b], ..., {conj} [z]'`
#'   \cr        `ox_none`   \tab `'(pref) none of [a], [b], ..., {conj} [z]'`
#'   \cr        `ox_some`   \tab `'(pref) some of [a], [b], ..., {conj} [z]'`
#'   \cr                    \tab   
#'   \cr      `ox_atmost`   \tab `'(pref) at most {n} of [a], [b], ..., {conj} [z]'`
#'   \cr     `ox_exactly`   \tab `'(pref) exactly {n} of [a], [b], ..., {conj} [z]'`
#'   \cr     `ox_atleast`   \tab `'(pref) at least {n} of [a], [b], ..., {conj} [z]'`
#'   \cr        `ox_less`   \tab `'(pref) less than {n} of [a], [b], ..., {conj} [z]'`
#'   \cr        `ox_more`   \tab `'(pref) more than {n} of [a], [b], ..., {conj} [z]'`
#'   \cr       `ox_fewer`   \tab `'(pref) fewer than {n} of [a], [b], ..., {conj} [z]'`
#'   \cr      `ox_noless`   \tab `'(pref) no less than {n} of [a], [b], ..., {conj} [z]'`
#'   \cr      `ox_nomore`   \tab `'(pref) no more than {n} of [a], [b], ..., {conj} [z]'`
#'   \cr     `ox_greater`   \tab `'(pref) greater than {n} of [a], [b], ..., {conj} [z]'`
#'   \cr     `ox_nofewer`   \tab `'(pref) no fewer than {n} of [a], [b], ..., {conj} [z]'`
#'   \cr   `ox_nogreater`   \tab `'(pref) no greater than {n} of [a], [b], ..., {conj} [z]'`
#'   \cr                    \tab   
#'   \cr      `ox_orless`   \tab `'(pref) {n} or less of [a], [b], ..., {conj} [z]'`
#'   \cr      `ox_ormore`   \tab `'(pref) {n} or more of [a], [b], ..., {conj} [z]'`
#'   \cr     `ox_orfewer`   \tab `'(pref) {n} or fewer of [a], [b], ..., {conj} [z]'`
#'   \cr   `ox_orgreater`   \tab `'(pref) {n} or greater of [a], [b], ..., {conj} [z]'`
#' }
#' @param ... Any number of arguments coerceable to mode character.
#' @param pref A \link[=cmp_chr_scl]{complete character scalar} prefix to prepend to the list.
#' @param conj A complete character scalar conjunction to use between the next to last and last elements of the list. Typical values are `and`, `or` and `nor`.
#' @param n A \link[=cmp_psw_scl]{complete positive whole-number scalar}.
#' @param comp A complete character scalar used for comparing to `n`, such as `'at least'` or `'or fewer'`.
#' @param first A non-`NA` logical scalar used to determine whether `comp` is placed in front of `n` rather than after `n`.
#' @return A character scalar.
#' @export
#' @examples
#' Fruits <- c("apples", "bananas", "oranges")
#'
#' ox(Fruits)
#' ox("apples", "bananas", "orange")
#' ox_or(Fruits)
#' ox_and(Fruits)
#' ox_nor(Fruits)
#' ox_nor(Fruits, pref = "")
#'
#' ox(Fruits)
#' ox(Fruits, conj = "or")
#' ox(Fruits, pref = "neither", conj = "nor")
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
#' ox_n(Fruits, conj = "and", n = 2, comp = "at least", first = TRUE)
#' ox_n(Fruits, conj = "and", n = 2, comp = "or more", first = FALSE)
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
  vals <- uj::av(...)
  errs <- base::c(uj::f0(base::length(vals) > 0, NULL, "[...] is empty."),
                  uj::f0(uj::icmp(vals)        , NULL, "Arguments in [...] must be complete and atomic (?icmp)."),
                  uj::f0(uj::cmp_chr_scl(conj) , NULL, "[conj] must be a complete character scalar (?cmp_chr_scl)."),
                  uj::f0(uj::cmp_chr_scl(pref) , NULL, "[pref] must be a complete character scalar (?cmp_chr_scl)."),
                  uj::f0(uj::isIN(quote, 0:2)  , NULL, "[quote] must be 0, 1, or 2."))
  if (!base::is.null(errs)) {stop(uj:::.errs(errs))}
  n.vals <- base::length(vals)
  if (n.vals == 1) {
    errs <- base::c(uj::f0(conj != 'nor'                  , NULL, "[conj = 'nor'], but [...] contains only 1 atomic element."),
                    uj::f0(conj != "or" | pref != "either", NULL, "[conj = 'or'] and [pref = 'either'], but [...] contains only 1 atomic element."))
    if (!base::is.null(errs)) {stop(uj:::.errs(errs))}
  }
  if (pref != "") {pref <- base::paste0(pref, " ")}                              # if pref(ix) is not empty, follow it with a space
  last <- vals[n.vals]                                                           # get the last element of X
  if (n.vals > 1) {                                                              # if there is more than one element in the list
    list <- base::paste0(vals[1:(n.vals - 1)], collapse = ", ")                  # : create a comma separated list with all but the last element of X
    conj <- base::paste0(uj::f0(n.vals == 2, " ", ", "), conj, " ")              # : if there are only 2, no additional comma to conj the last element to the list
  } else {list <- conj <- ""}                                                    # ELSE the list of non-last elements and the conj for the last item aren't needed
  base::paste0(pref, list, conj, last)                                           # put everything together
}

#' @rdname ox
#' @export
oxford <- ox

#' @rdname ox
#' @export
oxford_comma <- ox

#' @rdname ox
#' @export
ox_n <- function(..., conj = "and", comp = "", quote = 0, n = 1, first = TRUE) {
  vals <- uj::av(...)
  errs <- base::c(uj::f0(base::length(vals) > 0, NULL, "[...] is empty."),
                  uj::f0(uj::icmp(vals)        , NULL, "Arguments in [...] must be complete and atomic (?icmp)."),
                  uj::f0(uj::cmp_chr_scl(conj) , NULL, "[conj] must be a complete character scalar (?cmp_chr_scl)."),
                  uj::f0(uj::cmp_chr_scl(comp) , NULL, "[comp] must be a non-NA character scalar (?cmp_chr_scl)."),
                  uj::f0(uj::isIN(quote, 0:2)  , NULL, "[quote] must be 0, 1, or 2."),
                  uj::f0(uj::cmp_nnw_scl(n)    , NULL, "[n] must be a non-negative whole number scalar (?cmp_nnw_scl)."),
                  uj::f0(uj::isTF(first)       , NULL, "[first] must be TRUE or FALSE."))
  if (!base::is.null(errs)) {stop(uj:::.errs(errs))}
  pref <- uj::f0(comp == "", base::paste(n, "of"), uj::f0(first, base::paste(comp, n, "of"), base::paste(n, comp, "of")))
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
ox_atleast <- function(..., conj = "and", n = 1) {uj::ox_n(..., comp = "at least", n = n)}

#' @rdname ox
#' @export
ox_atmost <- function(..., conj = "and", n = 1) {uj::ox_n(..., comp = "at most", n = n)}

#' @rdname ox
#' @export
ox_nogreater <- function(..., conj = "and", n = 1) {uj::ox_n(..., comp = "no greater than", n = n)}

#' @rdname ox
#' @export
ox_nofewer <- function(..., conj = "and", n = 1) {uj::ox_n(..., comp = "no fewer than", n = n)}

#' @rdname ox
#' @export
ox_nomore <- function(..., conj = "and", n = 1) {uj::ox_n(..., comp = "no more than", n = n)}

#' @rdname ox
#' @export
ox_noless <- function(..., conj = "and", n = 1) {uj::ox_n(..., comp = "no less than", n = n)}

#' @rdname ox
#' @export
ox_ormore <- function(..., conj = "and", n = 1) {uj::ox_n(..., comp = "or more", n = n, first = F)}

#' @rdname ox
#' @export
ox_orgreater <- function(..., conj = "and", n = 1) {uj::ox_n(..., comp = "or greater", n = n, first = F)}

#' @rdname ox
#' @export
ox_orless <- function(..., conj = "and", n = 1) {uj::ox_n(..., comp = "or less", n = n, first = F)}

#' @rdname ox
#' @export
ox_orfewer <- function(..., conj = "and", n = 1) {uj::ox_n(..., comp = "or fewer", n = n, first = F)}
