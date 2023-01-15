#' @name ats
#' @title Manage attributes
#' @description \tabular{rl}{
#'      `at_names`   \tab Thinly wraps `names(attributes(x))`.
#'   \cr  `add_at`   \tab Adds named `...` args to attributes of `x`.
#'   \cr     `xat`   \tab Removes attributes of `x` with names matching unquoted `...` args\eqn{^1}.
#'   \cr      `at`   \tab Gets attributes of `x` with names matching unquoted `...` args\eqn{^1}.
#' }
#' ` `\eqn{^{1.}} No `...` args indicates all attributes of `x`.
#' @param x An R object.
#' @param ... Optional unquoted names of attributes of `x` or character objects. When none are provided, indicates all attributes of `x`.
#' @param a `TRUE` or `FALSE` indicating whether to \link[av]{atomize} the result and convert the resulting vector to character.
#' @return *An R object* \cr   `add_ats, xats, ats`
#'   \cr\cr `NULL` *or a character vector* \cr   `at_names`
#' @export
at <- function(x, ..., a = FALSE) {
  if (!base::isTRUE(a) & !base::isFALSE(a)) {stop(uj::format_errs(pkg = "uj", "[a] must be TRUE or FALSE."))}
  out <- NULL
  if (base::...length() > 0) {
    args <- uj::str_dots(..., glue. = F)
    for (arg in args) {out <- base::c(out, uj::run("temp <- base::list(`", arg, "` = x$`", arg, "`)"))}
  } else {out <- base::attributes(x)}
  uj::f0(a, uj::av(out), out)
}

#' @rdname ats
#' @export
at_names <- function(x) {base::names(base::attributes(x))}

#' @rdname ats
#' @export
add_at <- function(x, ...) {
  n <- base::...length()
  if (n == 0) {stop(uj::format_errs(pkg = "uj", "There must be at least one [...] arg."))}
  labs <- base::...names()
  if (base::length(labs) == 0) {labs <- base::rep.int("", n)}
  vars <- base::as.character(base::match.call())
  vars <- vars[2 + 1:n]
  i <- labs == ""
  labs[i] <- vars[i]
  dots <- base::list(...)
  base::names(dots) <- labs
  atts <- base::attributes(x)
  if (base::any(labs %in% base::names(atts))) {stop(uj::format_errs(pkg = "uj", "Names of [...] args may not be the same as names of existing attributes."))}
  if (base::length(base::unique(labs)) < base::length(labs)) {stop(uj::format_errs(pkg = "uj", "Names of [...] args must be unique."))}
  base::attributes(x) <- base::c(atts, dots)
  x
}

#' @rdname ats
#' @export
xat <- function(x, ...) {
  if (base::...length() > 0) {
    rm.ats <- uj::str_dots(..., glue. = F)
    curr.ats <- base::names(base::attributes(x))
    for (rm.at in rm.ats) {if (uj::isIN(rm.at, curr.ats)) {base::attr(x, rm.at) <- NULL}}
  } else {base::attributes(x) <- NULL}
  x
}
