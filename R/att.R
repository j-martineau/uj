#' @encoding UTF-8
#' @title Attribute Management Functions
#' @description Get attributes of an object, names of attributes of an object, determine whether there is a named attribute of an object, add attributes to an object, modify values of attributes of an object, and remove attributes of an object.
#' @param x,.x An R object.
#' @param ... For `add_att` and `set_att`, named arguments. For `rm_att/xatt` and `att` optional character objects (also optionally pipe delimited). When none are provided, indicates all attributes of `.x`.
#' @param .a `TRUE` or `FALSE` indicating whether to \link[=av]{atomize} the result and convert the resulting vector to character.
#' @param name A complete character scalar (?cmp_chr_scl).
#' @examples
#' egAtt <- function() {
#'   x1 <- matrix(
#'     NA,
#'     nrow = 2,
#'     ncol = 2,
#'     dimnames = list(rows = c("r1", "r3"), cols = c("c1", "c2"))
#'   )
#'   x2 <- add_att(x1, added = "added attribute")
#'   x3 <- set_att(x2, added = "new attribute value")
#'   x4 <- rm_att(x2, "added")
#'   x5 <- xatt(x2)
#'   x6 <- xatts(x2)
#'   list(
#'     x1 = x1, x2 = x2, x3 = x3, x4 = x4, x5 = x5, x6 = x6,
#'     att.names.x1 = att_names(x1), att.names.x2 = att_names(x2),
#'     att.names.x3 = att_names(x3), att.names.x4 = att_names(x4),
#'     att.names.x5 = att_names(x5), att.names.x6 = att_names(x6),
#'     atts.x1 = atts(x1), atts.x2 = atts(x2), atts.x3 = atts(x3),
#'     atts.x4 = atts(x4), atts.x5 = atts(x5), atts.x6 = atts(x6),
#'     att.x2.added = att(x2, "added"), att.x2.a = att(x2, .a = T),
#'     att.x2 = att(x2), is.att.x1.added = is_att(x1, "added"),
#'     is.att.x2.added = is_att(x2, "added")
#'   )
#' }
#' egAtt <- egAtt()
#' egAtt
#' @export
att_funs <- function() {utils::help("att_funs", package = "uj")}

#' @describeIn att_funs Get the attributes of `.x` named in `...`, optionally \link[=av]{atomizing} the result when `.a = TRUE`. When there are no `...` args, gets all attributes of `.x`. Return value varies depending on `x` and `...`.
#' @export
att <- function(.x, ..., .a = FALSE) {
  if (base::...length() > 0) {
    if (!uj::.cmp_lgl_scl(.a)) {uj::stopperr("[.a] must be scalar TRUE or scalar FALSE.")}
    atts     <- base::attributes(.x)
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
  } else {base::attributes(.x)}
}

#' @describeIn att_funs Gets the attributes of `x`. Thinly wraps `attributes(x)`. Returns either `NULL` or a named list.
#' @export
atts <- function(x) {base::attributes(x)}

#' @describeIn att_funs Gets the names of attributes of `x`. Thinly wraps `names(attributes(x))`. Returns either `NULL` or a character vector.
#' @export
att_names <- function(x) {base::names(base::attributes(x))}

#' @describeIn att_funs Evaluates whether `name` is the name of an attribute of `x`. Returns a logical scalar.
#' @export
is_att <- function(x, name) {
  if (!uj::.cmp_chr_scl(name)) {uj::stopperr("[name] must be .a complete character scalar (?cmp_chr_scl).")}
  name %in% base::names(base::attributes(x))
}

#' @describeIn att_funs Adds attributes to `.x` as specified in `...` where attribute names are the names of `...` args and attribute values are the values of `...` args. Returns `x` with added attributes as specified in `...`.
#' @export
add_att <- function(.x, ...) {
  n <- base::...length()
  if (n == 0) {uj::stopperr("There must be at least one [...] arg." )}
  dots <- base::list(...)
  atts <- base::attributes(.x)
  dotNames <- base::names(dots)
  attNames <- base::names(atts)
  if (base::any(dotNames %in% attNames)) {uj::stopperr("Names of [...] args may not be the same as names of existing attributes.")}
  if (base::length(dotNames) != base::length(base::unique(dotNames))) {uj::stopperr("Names of [...] args must be unique.")}
  for (i in 1:n) {base::attr(.x, dotNames[i]) <- base::...elt(i)}
  .x
}

#' @describeIn att_funs Sets attributes of `.x` to the values specified in `...` where names of modified attributes are the names of `...` args and the values of modified attributes are the values of `...` args. Returns `x` with modified attribute values.
#' @export
set_att <- function(.x, ...) {
  ndots <- base::...length()
  dotNames <- base::...names()
  nuDotNames <- base::length(base::unique(dotNames))
  if (ndots == 0) {uj::stopperr("There must be at least one [...] arg." )}
  if (ndots != nuDotNames) {uj::stopperr("Names of [...] args must be unique.")}
  for (i in 1:ndots) {base::attr(.x, dotNames[i]) <- base::...elt(i)}
  .x
}

#' @describeIn att_funs Removes from `x` any attributes named in `...` (where `...` is \link[=av]{atomized} to a character vector and split along pipes (`'|'`)). When there are no `...` args, removes all attributes from `x`. Returns `x` with the specified attributes removed.
#' @export
rm_att <- function(x, ...) {
  if (base::...length() > 0) {
    dropNames <- uj::av(base::strsplit(base::as.character(uj::av(...)), "|", fixed = T))
    attNames <- base::names(base::attributes(x))
    for (dropName in dropNames) {if (dropName %in% attNames) {base::attr(x, dropName) <- NULL}}
  } else {base::attributes(x) <- NULL}
  x
}

#' @describeIn att_funs An alias for `rm_att`.
#' @export
xatt <- rm_att

#' @describeIn att_funs Removes all attributes from `x`. Returns `x` with all attributes removed.
#' @export
rm_atts <- function(x) {
  base::attributes(x) <- NULL
  x
}

#' @describeIn att_funs An alias for `rm_atts`.
#' @export
xatts <- rm_atts
