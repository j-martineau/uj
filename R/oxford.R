#' @name Oxford
#' @family strings
#' @title Oxford comma separated lists.
#' @description Create a variety of Oxford comma separated lists.
#' @details \strong{\code{ox}}
#'   \cr Creates an Oxford separated list with \code{conj} as the conjunction
#'   separating the next to last and last elements the list and preceded by the
#'   prefix \code{pref}.
#'   \cr \cr
#'   \strong{\code{ox_and} and \code{ox_or}}
#'   \cr Create \code{'and'} and \code{'or'} separated Oxford-comma lists,
#'   respectively
#'   \cr \cr
#'   \strong{\code{ox_nor}}
#'   \cr Creates a \code{'nor'} separated Oxford comma list with default (but
#'   changeable) prefix of \code{'neither'}.
#'   \cr \cr
#'   \strong{\code{ox_either} and \code{ox_neither}}
#'   \cr Create \code{'either'} and \code{'neither'} prefixed Oxford comma
#'   lists, respectively, with default (but changeable) conjunction of \code{'or'}
#'   and \code{'nor'}, respectively.
#'   \cr \cr
#'   \strong{\code{ox_all}, \code{ox_any}, \code{ox_none}, and \code{ox_some}}
#'   \cr Create \code{'all of'}, \code{'any of'}, \code{'none of'}, and
#'   \code{'some of'} prefixed Oxford comma lists, (respectively).
#'   \cr\cr
#'   \strong{\code{ox_n}}
#'   \cr Creates Oxford comma lists specifying a number (\code{n}), a comparison
#'   (\code{comp}), and whether that comparison comes before or after the list
#'   (\code{comp.first}) to create a list of the form \code{'{n} {comp} of
#'   {list}'} or of the form \code{'{comp} {n} of {list}'} where \code{list} is
#'   the Oxford comma list with the next to last and last elements separated by
#'   \code{conj}.
#'   \cr\cr
#'   \strong{\code{ox_exactly}, \code{ox_less}, \code{ox_more}, \code{ox_fewer},
#'   \code{ox_greater, ox_atleast}, \code{ox_atmost}, \code{ox_nogreater},
#'   \code{ox_nofewer}, \code{ox_nomore}, and \code{ox_noless}}
#'   \cr These functions call \code{ox_n} with \code{comp.first = TRUE} and,
#'   respectively, \code{comp} equal to \code{'exactly'}, \code{'less than'},
#'   \code{'more than'}, \code{'fewer than'}, \code{'greater than'}, \code{'at
#'   least'}, \code{'at most'}, \code{'no greater than'}, \code{'no fewer
#'   than'}, \code{'new more than'}, and \code{'no less than'}, respectively.
#'   \cr\cr
#'   \strong{\code{ox_ormore}, \code{ox_orgreater}, \code{ox_orless}, and
#'   \code{ox_orfewer}}
#'   \cr These functions call \code{ox_n} with \code{comp.first = FALSE} and,
#'   respectively, \code{comp} equal to \code{'or more'}, \code{'or greater'},
#'   \code{'or less'}, and \code{'or fewer'}, respectively.
#' @param ... Any number of arguments coerceable to mode character.
#' @param pref A pref to prepend to the resulting character scalar. See the
#'   \bold{Details} section for more information
#' @param conj The conjunction to use between the next to last and last elements
#'   of the list. See details.
#' @param n A integer scalar.
#' @param comp A character scalar used for comparing to \code{n}, such as
#'   \code{'at least'} or \code{'or fewer'}.
#' @param comp.first A logical scalar used to determine whether \code{comp} is
#'   placed in front of \code{n} rather than after \code{n}..
#' @return A character scalar containing all atomic elements of \code{...}
#'   formatted as an Oxford-comma separated list.
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
  x  <- list(...)
  VN <- length(x) > 0
  VL <- f0(!VN, T, all(lengths(x) > 0))
  VX <- f0(!VN | !VL, T, xcmp(x))
  VJ <- cmp_chr_scl(conj)
  VP <- cmp_chr_scl(pref)
  VQ <- isIN(quote, 0:2)
  E  <- NULL
  if (!VN) {E <- c(E, "\n  * [...] is empty.")}
  if (!VL) {E <- c(E, "\n  * An argument in [...] is empty.")}
  if (!VX) {E <- c(E, "\n  * Arguments in [...] must be non-NA and atomic.")}
  if (!VJ) {E <- c(E, "\n  * [conj] must be a non-NA character scalar.")}
  if (!VP) {E <- c(E, "\n  * [pref] must be a non-NA character scalar.")}
  if (!VQ) {E <- c(E, "\n  * [quote] must be 0, 1, or 2.")}
  if (xdef(E)) {stop(E)}
  x <- av(x)
  N <- length(x)
  if (N == 1) {
    VJ <- conj != 'nor'
    VP <- conj != "or" | pref != "either"
    E <- NULL
    if (!VJ) {E <- c(E, "\n  * [conj = 'nor'], but [...] contains only 1 atomic element.")}
    if (!VP) {E <- c(E, "\n  * [conj = 'or'] and [pref = 'either'], but [...] contains only 1 atomic element.")}
    if (xdef(E)) {stop(E)}
  }
  if (pref != "") {pref <- paste0(pref, " ")}                                    # If pref(ix) is not empty, follow it with a space
  Last <- x[N]                                                                   # Get the last element of X
  if (N > 1) {                                                                   # If there is more than one element in the list
    List <- paste0(x[1:(N - 1)], collapse = ", ")                                # > Create a comma separated list with all but the last element of X
    conj <- paste0(f0(N == 2, " ", ", "), conj, " ")                            # > If there are only 2, no addiitional comma to conj the last element to the list
                    # > If there are only 2, no addiitional comma to conj the last element to the list
  } else {List <- conj <- ""}                                                    # Otherwise the list of non-last elements and the conj for the last item aren't needed
  paste0(pref, List, conj, Last)                                                 # Put everything together
}

#' @rdname Oxford
#' @export
ox_n <- function(..., conj = "and", comp = "", quote = 0, n = 1, comp.first = TRUE) {
  x  <- list(...)
  V0 <- length(x) > 0
  VL <- f0(!V0, T, all(lengths(x) > 0))
  VX <- f0(!V0 | !VL, T, xcmp(x))
  VJ <- cmp_chr_scl(conj)
  VC <- cmp_chr_scl(comp)
  VQ <- isIN(quote, 0:2)
  VN <- cmp_nnw_scl(n)
  VF <- isTF(comp.first)
  x  <- av(x)
  E  <- NULL
  if (!V0) {E <- c(E, "\n  * [...] is empty.")}
  if (!VL) {E <- c(E, "\n  * An argument in [...] is empty.")}
  if (!VX) {E <- c(E, "\n  * Arguments in [...] must be non-NA and atomic.")}
  if (!VJ) {E <- c(E, "\n  * [conj] must be a non-NA character scalar.")}
  if (!VC) {E <- c(E, "\n  * [pref] must be a non-NA character scalar.")}
  if (!VQ) {E <- c(E, "\n  * [quote] must be 0, 1, or 2.")}
  if (!VN) {E <- c(E, "\n  * [n] must be a non-negative whole number scalar.")}
  if (!VF) {E <- c(E, "\n  * [comp.first] must be TRUE or FALSE.")}
  if (xdef(E)) {stop(E)}
  pref <- f0(comp == "", paste(n, "of"),
          f0(comp.first, paste(n, comp, "of"), paste(comp, n, "of")))
  ox(x, conj = conj, pref = pref, quote = quote)                                 # Create the Oxford separated list with the custom n-based prefix
}

#' @rdname Oxford
#' @export
ox_and <- function(..., pref = NULL) {ox(..., pref = pref, conj = "and")}

#' @rdname Oxford
#' @export
ox_or <- function(..., pref = NULL) {ox(..., pref = pref, conj = "or")}

#' @rdname Oxford
#' @export
ox_nor <- function(..., pref = "neither") {ox(..., pref = pref, conj = "nor")}

#' @rdname Oxford
#' @export
ox_either <- function(...) {ox(..., pref = "either", conj = "or")}

#' @rdname Oxford
#' @export
ox_neither <- function(...) {ox(..., pref = "neither", conj = "nor")}

#' @rdname Oxford
#' @export
ox_all <- function(..., conj = "and") {ox(..., pref = "all of", conj = conj)}

#' @rdname Oxford
#' @export
ox_any <- function(..., conj = "and") {ox(..., pref = "any of", conj = conj)}

#' @rdname Oxford
#' @export
ox_none <- function(..., conj = "or") {ox(..., pref = "none of", conj = conj)}

#' @rdname Oxford
#' @export
ox_some <- function(..., conj = "and") {ox(..., pref = "some of", conj = conj)}

#' @rdname Oxford
#' @export
ox_exactly <- function(..., conj = "or", n = 1) {ox_n(..., comp = "exactly", n = n, conj = conj)}

#' @rdname Oxford
#' @export
ox_less <- function(..., conj = "and", n = 2) {ox_n(..., comp = "less than", n = n)}

#' @rdname Oxford
#' @export
ox_more <- function(..., conj = "and", n = 1) {ox_n(..., comp = "more than", n = n)}

#' @rdname Oxford
#' @export
ox_fewer <- function(..., conj = "and", n = 2) {ox_n(..., comp = "fewer than", n = n)}

#' @rdname Oxford
#' @export
ox_greater <- function(..., conj = "and", n = 2) {ox_n(..., comp = "greater than", n = n)}

#' @rdname Oxford
#' @export
ox_atleast <- function(..., conj = "and", n = 1) {ox_n(..., comp = "at least", n = n)}

#' @rdname Oxford
#' @export
ox_atmost <- function(..., conj = "and", n = 1) {ox_n(..., comp = "at most", n = n)}

#' @rdname Oxford
#' @export
ox_nogreater <- function(..., conj = "and", n = 1) {ox_n(..., comp = "no greater than", n = n)}

#' @rdname Oxford
#' @export
ox_nofewer <- function(..., conj = "and", n = 1) {ox_n(..., comp = "no fewer than", n = n)}

#' @rdname Oxford
#' @export
ox_nomore <- function(..., conj = "and", n = 1) {ox_n(..., comp = "no more than", n = n)}

#' @rdname Oxford
#' @export
ox_noless <- function(..., conj = "and", n = 1) {ox_n(..., comp = "no less than", n = n)}

#' @rdname Oxford
#' @export
ox_ormore <- function(..., conj = "and", n = 1) {ox_n(..., comp = "or more", n = n, comp.first = F)}

#' @rdname Oxford
#' @export
ox_orgreater <- function(..., conj = "and", n = 1) {ox_n(..., comp = "or greater", n = n, comp.first = F)}

#' @rdname Oxford
#' @export
ox_orless <- function(..., conj = "and", n = 1) {ox_n(..., comp = "or less", n = n, comp.first = F)}

#' @rdname Oxford
#' @export
ox_orfewer <- function(..., conj = "and", n = 1) {ox_n(..., comp = "or fewer", n = n, comp.first = F)}
