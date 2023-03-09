#' @name ats
#' @title Manage attributes
#' @description Get names of, add new, remove, and extract specific object attributes.
#' @details
#' \tabular{ll}{  `rm_at, xat`   \tab Removes attributes of `x` with names matching unquoted `...` args  \eqn{^{(1)}}. \cr   \tab   \cr
#'                `at_names`     \tab Thinly wraps `names(attributes(x))`.                                             \cr   \tab   \cr
#'                `add_at`       \tab Adds named `...` args to attributes of `x`.                                      \cr   \tab   \cr
#'                `at`           \tab Gets attributes of `x` with names matching unquoted `...` args  \eqn{^{(1)}}.    \cr   \tab     }
#'  \tabular{l}{  \eqn{^{(1)}} No `...` args indicates all attributes of `x`.}
#' @param x An R object.
#' @param ... Optional unquoted names of attributes of `x` or character objects. When none are provided, indicates all attributes of `x`.
#' @param a `TRUE` or `FALSE` indicating whether to \link[av]{atomize} the result and convert the resulting vector to character.
#' @return **An object**                                   \cr\cr `add_at, rm_at, xat, at`
#' \cr\cr  **The** `NULL` **object or a character vector** \cr\cr `at_names`
#' @export
at <- function(x, ..., a = FALSE) {
  if (!uj:::.cmp_lgl_scl(a)) {uj::stopperr("[a] must be scalar TRUE or scalar FALSE.", PKG = "uj")}
  y <- NULL
  if (base::...length() > 0) {
    dots <- uj::flex_dots(..., GLUE = F)
    for (dot in dots) {y <- base::c(y, uj::run("temp <- base::list(`", dot, "` = x$`", dot, "`)"))}
  } else {y <- base::attributes(x)}
  uj::f0(a, uj::av(y), y)
}

#' @rdname ats
#' @export
at_names <- function(x) {base::names(base::attributes(x))}

#' @rdname ats
#' @export
add_at <- function(x, ...) {
  n <- base::...length()
  if (n == 0) {uj::stopperr("There must be at least one [...] arg." , PKG = "uj")}
  labs <- base::...names()
  if (base::length(labs) == 0) {labs <- base::rep.int("", n)}
  vars <- base::as.character(base::match.call())
  vars <- vars[2 + 1:n]
  i <- labs == ""
  labs[i] <- vars[i]
  dots <- base::list(...)
  base::names(dots) <- labs
  atts <- base::attributes(x)
  if (uj::any(labs %in% base::names(atts))) {uj::stopperr("Names of [...] args may not be the same as names of existing attributes.", PKG = "uj")}
  if (base::length(labs) != base::length(base::unique(labs))) {uj::stopperr("Names of [...] args must be unique.", PKG = "uj")}
  base::attributes(x) <- base::c(atts, dots)
  x
}

#' @rdname ats
#' @export
rm_at <- function(x, ...) {
  if (base::...length() > 0) {
    rm.ats <- uj::flex_dots(..., glue. = F)
    curr.ats <- base::names(base::attributes(x))
    for (rm.at in rm.ats) {if (rm.at %in% curr.ats) {base::attr(x, rm.at) <- NULL}}
  } else {base::attributes(x) <- NULL}
  x
}

#' @rdname ats
#' @export
xat <- rm_at
