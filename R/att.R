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
#' @param .A `TRUE` or `FALSE` indicating whether to \link[av]{atomize} the result and convert the resulting vector to character.
#' @return **An object**
#' \cr\cr `add_att, set_att, rm_att, xatt`
#' \cr\cr  **THE** `NULL` **object or a list**
#' \cr\cr `atts, att`
#' \cr\cr  **The** `NULL` **object or a character vector**
#' \cr\cr `att_names`
#' @export
att <- function(x, ..., .A = FALSE) {
  if (!uj:::.cmp_lgl_scl(.A)) {uj::stopperr("[.A] must be scalar TRUE or scalar FALSE.", .PKG = "uj")}
  Atts <- base::attributes(x)
  AttNames <- base::names(Atts)
  GetNames <- base::as.character(uj::av(...))
  nGet <- base::length(GetNames)
  if (nGet > 1) {
    Y <- NULL
    for (GetName in GetNames) {
      if (GetName %in% base::names(Atts)) {GetVal <- Atts[[GetName]]}
      else {GetVal <- NULL}
      GetVal <- base::list(val = GetVal)
      base::names(GetVal) <- GetName
      Y <- base::c(Y, GetVal)
    }
  } else if (nGet == 1) {
    if (GetNames %in% AttNames) {Y <- Atts[[GetNames]]}
    else {Y <- NULL}
  } else {Atts}
  uj::f0(.A, uj::av(Y), Y)
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
  if (!uj:::.cmp_chr_scl(name)) {uj::stopperr("[name] must be .A complete character scalar (?cmp_chr_scl).", .PKG = "uj")}
  name %in% base::names(base::attributes(x))
}

#' @rdname att
#' @export
add_att <- function(x, ...) {
  N <- base::...length()
  if (N == 0) {uj::stopperr("There must be at least one [...] arg." , .PKG = "uj")}
  Dots <- base::list(...)
  Atts <- base::attributes(x)
  DotNames <- base::names(Dots)
  AttNames <- base::names(Atts)
  if (base::any(DotNames %in% AttNames)) {uj::stopperr("Names of [...] args may not be the same as names of existing attributes.", .PKG = "uj")}
  if (base::length(DotNames) != base::length(base::unique(DotNames))) {uj::stopperr("Names of [...] args must be unique.", .PKG = "uj")}
  for (i in 1:N) {base::attr(x, DotNames[i]) <- base::...elt(i)}
  x
}

#' @rdname att
#' @export
set_att <- function(x, ...) {
  nDots <- base::...length()
  DotNames <- base::...names()
  nuDotNames <- base::length(base::unique(DotNames))
  if (nDots == 0) {uj::stopperr("There must be at least one [...] arg." , .PKG = "uj")}
  if (nDots != nuDotNames) {uj::stopperr("Names of [...] args must be unique.", .PKG = "uj")}
  for (i in 1:nDots) {base::attr(x, DotNames[i]) <- base::...elt(i)}
  x
}

#' @rdname att
#' @export
rm_att <- function(x, ...) {
  if (base::...length() == 0) {uj::stopperr("There must be at least one [...] arg." , .PKG = "uj")}
  DropNames <- base::as.character(uj::av(...))
  AttNames <- base::names(base::attributes(x))
  for (DropName in DropNames) {if (DropName %in% AttNames) {base::attr(x, DropName) <- NULL}}
  x
}

#' @rdname att
#' @export
xatt <- rm_att
