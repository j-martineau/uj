#' @name ats
#' @title Manage attributes
#' @description Get names of, add new, remove, and extract specific object attributes.
#' @details
#' \tabular{ll}{  `rm_at, xat`   \tab Removes attributes of `x` with names matching unquoted `...` args  \eqn{^{(1)}}. \cr   \tab     }
#' \tabular{ll}{  `at_names`     \tab Thinly wraps `names(attributes(x))`.                                             \cr   \tab     }
#' \tabular{ll}{  `add_at`       \tab Adds named `...` args to attributes of `x`.                                      \cr   \tab     }
#' \tabular{ll}{  `at`           \tab Gets attributes of `x` with names matching unquoted `...` args  \eqn{^{(1)}}.                 \cr
#'                               \tab \eqn{^{(1)}} No `...` args indicates all attributes of `x`.                                     }
#' @param x An R object.
#' @param ... Optional unquoted names of attributes of `x` or character objects. When none are provided, indicates all attributes of `x`.
#' @param a `TRUE` or `FALSE` indicating whether to \link[av]{atomize} the result and convert the resulting vector to character.
#' @return **An object**                                   \cr `add_at, rm_at, xat, at`
#' \cr\cr  **The** `NULL` **object or a character vector** \cr `at_names`
#' @export
at <- function(x, ..., a = FALSE) {
  uj::err_if_not(uj::isTF1(a), "[a] must be scalar TRUE or scalar FALSE.", PKG = "uj")
  y <- NULL
  if (uj::ND1P()) {
    dots <- uj::flex_dots(..., GLUE = F)
    for (dot in dots) {y <- base::c(y, uj::run("temp <- base::list(`", dot, "` = x$`", dot, "`)"))}
  } else {y <- base::attributes(x)}
  uj::f0(a, uj::av(y), y)
}

#' @rdname ats
#' @export
at_names <- function(x) {uj::EN(base::attributes(x))}

#' @rdname ats
#' @export
add_at <- function(x, ...) {
  n <- uj::ND()
  uj::err_if(n == 0, "There must be at least one [...] arg." , PKG = "uj")
  labs <- uj::DN()
  if (uj::N0(labs)) {labs <- base::rep.int("", n)}
  vars <- uj::asCHR(base::match.call())
  vars <- vars[2 + 1:n]
  i <- labs == ""
  labs[i] <- vars[i]
  dots <- base::list(...)
  uj::EN(dots) <- labs
  atts <- base::attributes(x)
  uj::err_if(uj::anyIN(labs, uj::EN(atts)), "Names of [...] args may not be the same as names of existing attributes.", PKG = "uj")
  uj::err_if(uj::NDIF(labs, uj::U(labs))  , "Names of [...] args must be unique."                                     , PKG = "uj")
  base::attributes(x) <- base::c(atts, dots)
  x
}

#' @rdname ats
#' @export
rm_at <- function(x, ...) {
  if (uj::ND1P()) {
    rm.ats <- uj::flex_dots(..., glue. = F)
    curr.ats <- uj::EN(base::attributes(x))
    for (rm.at in rm.ats) {if (uj::isIN1(rm.at, curr.ats)) {base::attr(x, rm.at) <- NULL}}
  } else {base::attributes(x) <- NULL}
  x
}

#' @rdname ats
#' @export
xat <- rm_at
