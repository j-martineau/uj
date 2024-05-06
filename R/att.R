#' @title Manage attributes
#' @description Get names of, add new, remove, and extract specific object attributes.
#' @details
#' \tabular{ll}{  `rm_att, xatt`   \tab Removes attributes of `x` with names matching unquoted `...` args  \eqn{^{(1)}}. \cr   \tab   \cr
#'                `att_names`      \tab Thinly wraps `names(attributes(x))`.                                             \cr   \tab   \cr
#'                `add_att`        \tab Adds named `...` args to attributes of `x`.                                      \cr   \tab   \cr
#'                `set_att`        \tab Sets attributes of `x` to the named values of `...`                              \cr   \tab   \cr
#'                `atts`           \tab Gets all attributes of `x`                                                       \cr   \tab   \cr
#'                `att`            \tab Gets attributes of `x` with names matching unquoted `...` args  \eqn{^{(1)}}.    \cr   \tab     }
#'  \tabular{l}{  \eqn{^{(1)}} No `...` args indicates all attributes of `x`.}
#' @param x An R object.
#' @param ... Optional unquoted names of attributes of `x` or character objects. When none are provided, indicates all attributes of `x`.
#' @param .a `TRUE` or `FALSE` indicating whether to \link[av]{atomize} the result and convert the resulting vector to character.
#' @return **An object**
#' \cr\cr `add_att, set_att, rm_att, xatt`
#' \cr\cr  **THE** `NULL` **object or a list**
#' \cr\cr `atts, att`
#' \cr\cr  **The** `NULL` **object or a character vector**
#' \cr\cr `att_names`
#' @export
att <- function(x, ..., .a = FALSE) {
  if (!uj::.cmp_lgl_scl(.a)) {uj::stopperr("[.a] must be scalar TRUE or scalar FALSE.", pkg = "uj")}
  atts     <- base::attributes(x)
  attNames <- base::names(atts)
  getNames <- base::as.character(uj::av(...))
  nGet     <- base::length(getNames)
  if (nGet > 1) {
    Y <- NULL
    for (getName in getNames) {
      if (getName %in% base::names(atts)) {getVal <- atts[[getName]]}
      else {getVal <- NULL}
      getVal <- base::list(val = getVal)
      base::names(getVal) <- getName
      Y <- base::c(Y, getVal)
    }
  } else if (nGet == 1) {
    if (getNames %in% attNames) {y <- atts[[getNames]]}
    else {y <- NULL}
  } else {atts}
  uj::f0(.a, uj::av(y), y)
}

#' @rdname att
#' @export
atts <- function(x) {base::attributes(x)}

#' @rdname att
#' @export
att_names <- function(x) {base::names(base::attributes(x))}

#' @rdname att
#' @export
is_att <- function(x, name) {
  if (!uj::.cmp_chr_scl(name)) {uj::stopperr("[name] must be .a complete character scalar (?cmp_chr_scl).", pkg = "uj")}
  name %in% base::names(base::attributes(x))
}

#' @rdname att
#' @export
add_att <- function(x, ...) {
  n <- base::...length()
  if (n == 0) {uj::stopperr("There must be at least one [...] arg." , pkg = "uj")}
  dots <- base::list(...)
  atts <- base::attributes(x)
  dotNames <- base::names(dots)
  attNames <- base::names(atts)
  if (base::any(dotNames %in% attNames)) {uj::stopperr("Names of [...] args may not be the same as names of existing attributes.", pkg = "uj")}
  if (base::length(dotNames) != base::length(base::unique(dotNames))) {uj::stopperr("Names of [...] args must be unique.", pkg = "uj")}
  for (i in 1:N) {base::attr(x, dotNames[i]) <- base::...elt(i)}
  x
}

#' @rdname att
#' @export
set_att <- function(x, ...) {
  ndots <- base::...length()
  dotNames <- base::...names()
  nuDotNames <- base::length(base::unique(dotNames))
  if (ndots == 0) {uj::stopperr("There must be at least one [...] arg." , pkg = "uj")}
  if (ndots != nuDotNames) {uj::stopperr("Names of [...] args must be unique.", pkg = "uj")}
  for (i in 1:ndots) {base::attr(x, dotNames[i]) <- base::...elt(i)}
  x
}

#' @rdname att
#' @export
rm_att <- function(x, ...) {
  if (base::...length() == 0) {uj::stopperr("There must be at least one [...] arg." , pkg = "uj")}
  dropNames <- base::as.character(uj::av(...))
  attNames <- base::names(base::attributes(x))
  for (dropName in dropNames) {if (dropName %in% attNames) {base::attr(x, dropName) <- NULL}}
  x
}

#' @rdname att
#' @export
xatt <- rm_att
