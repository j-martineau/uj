#' @title Manage attributes
#' @description Get names of, add new, remove, and extract specific object attributes.
#' @details
#' \tabular{ll}{  `rm_at, xat`   \tab Removes attributes of `X` with names matching unquoted `...` args  \eqn{^{(1)}}. \cr   \tab   \cr
#'                `at_names`     \tab Thinly wraps `names(attributes(X))`.                                             \cr   \tab   \cr
#'                `add_at`       \tab Adds named `...` args to attributes of `X`.                                      \cr   \tab   \cr
#'                `set_at`       \tab Sets attributes of `X` to the named values of `...`                              \cr
#'                `at`           \tab Gets attributes of `X` with names matching unquoted `...` args  \eqn{^{(1)}}.    \cr   \tab     }
#'  \tabular{l}{  \eqn{^{(1)}} No `...` args indicates all attributes of `X`.}
#' @param X An R object.
#' @param ... Optional unquoted names of attributes of `X` or character objects. When none are provided, indicates all attributes of `X`.
#' @param A `TRUE` or `FALSE` indicating whether to \link[av]{atomize} the result and convert the resulting vector to character.
#' @return **An object**                                   \cr\cr `add_at, set_at, rm_at, xat, at`
#' \cr\cr  **The** `NULL` **object or A character vector** \cr\cr `at_names`
#' @export
at <- function(X, ..., A = FALSE) {
  if (!uj:::.cmp_lgl_scl(A)) {uj::stopperr("[A] must be scalar TRUE or scalar FALSE.", PKG = "uj")}
  Attrs <- base::attributes(X)
  AttrNames <- base::names(Attrs)
  GetNames <- base::as.character(uj::av(...))
  nGet <- base::length(GetNames)
  if (nGet > 1) {
    Y <- NULL
    for (GetName in GetNames) {
      if (GetName %in% base::names(Attrs)) {GetVal <- Attrs[[GetName]]}
      else {GetVal <- NULL}
      GetVal <- base::list(val = GetVal)
      base::names(GetVal) <- GetName
      Y <- base::c(Y, GetVal)
    }
  } else if (nGet == 1) {
    if (GetNames %in% AttrNames) {Y <- Attrs[[GetNames]]}
    else {Y <- NULL}
  } else {Attrs}
  uj::f0(A, uj::av(Y), Y)
}

#' @rdname at
#' @export
at_names <- function(X) {base::names(base::attributes(X))}

#' @rdname at
#' @export
is_at <- function(X, Name) {
  if (!uj:::.cmp_chr_scl(Name)) {uj::stopperr("[Name] must be A complete character scalar (?cmp_chr_scl).", PKG = "uj")}
  Name %in% base::names(base::attributes(X))
}

#' @rdname at
#' @export
add_at <- function(X, ...) {
  N <- base::...length()
  if (N == 0) {uj::stopperr("There must be at least one [...] arg." , PKG = "uj")}
  Dots <- base::list(...)
  atts <- base::attributes(X)
  DotNames <- base::names(Dots)
  AttrNames <- base::names(atts)
  if (base::any(DotNames %in% AttrNames)) {uj::stopperr("Names of [...] args may not be the same as names of existing attributes.", PKG = "uj")}
  if (base::length(DotNames) != base::length(base::unique(DotNames))) {uj::stopperr("Names of [...] args must be unique.", PKG = "uj")}
  for (i in 1:N) {base::attr(X, DotNames[i]) <- base::...elt(i)}
  X
}

#' @rdname at
#' @export
set_at <- function(X, ...) {
  nDots <- base::...length()
  DotNames <- base::...names()
  nuDotNames <- base::length(base::unique(DotNames))
  if (nDots == 0) {uj::stopperr("There must be at least one [...] arg." , PKG = "uj")}
  if (nDots != nuDotNames) {uj::stopperr("Names of [...] args must be unique.", PKG = "uj")}
  for (i in 1:nDots) {base::attr(X, DotNames[i]) <- base::...elt(i)}
  X
}

#' @rdname at
#' @export
rm_at <- function(X, ...) {
  if (base::...length() == 0) {uj::stopperr("There must be at least one [...] arg." , PKG = "uj")}
  DropNames <- base::as.character(uj::av(...))
  AttrNames <- base::names(base::attributes(X))
  for (DropName in DropNames) {if (DropName %in% AttrNames) {base::attr(X, DropName) <- NULL}}
  X
}

#' @rdname at
#' @export
xat <- rm_at
