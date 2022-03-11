#' @name sss
#' @family props
#' @title State of an object
#' @description An object's state (of completeness) is defined for non-empty
#'   atomic tibbles, non-empty atomic lists, non-empty atomic vectors, and
#'   non-empty atomic arrays. For all others, state is undefined and returned as
#'   \code{NULL}. The following table summarizes valid state properties.
#'   \tabular{lll}{
#'   \emph{Value}\tab\emph{Name}        \tab\emph{Characteristics}
#'   \cr\code{'cmp'}\tab complete       \tab non-empty, atomic, no
#'                                           \code{NA} values.
#'   \cr\code{'mss'}\tab missing        \tab non-empty, atomic, only
#'                                           \code{NA} values.
#'   \cr\code{'prt'}\tab partial        \tab non-empty, atomic, both
#'                                           \code{NA} and non-\code{NA} values.
#'   \cr\code{'nas'}\tab\code{NA} scalar\tab atomic \code{NA} scalar.
#'   \cr\code{'oks'}\tab\code{OK} scalar\tab atomic non-\code{NA} scalar
#'   }
#' @param x An object.
#' @param xxx A character scalar containing one or more values from
#'   \code{sss_vals()} separated by pipes.
#' @param ... Additional arguments to \code{\link{meets}} containing value and
#'   element/row/column count restrictions.
#' @details \strong{\code{sss_vals}}
#'   \cr Gets all valid state properties.
#'   \cr\cr
#'   \strong{\code{sss}}
#'   \cr Gets a character scalar containing all state properties from
#'   \code{sss_vals()} that is applicable to \code{x}, if defined. If not
#'   defined, returns \code{NULL}.
#'   \cr\cr
#'   \strong{\code{is_sss}}
#'   \cr Evaluates whether \code{x} has a state property represented in
#'   \code{xxx}.
#'   \cr\cr
#'   \strong{Additional Arguments in \code{...}}
#'   \cr Submitting additional arguments to \code{is_sss} via \code{...}
#'   allows for checking not just the state but whether length, number of rows,
#'   number of columns, and element values meet flexible criteria.
#' @return \code{sss_vals()} returns a character vector. \code{sss(x)}
#'   returns a character scalar or \code{NULL}. All others return either
#'   \code{TRUE} or \code{FALSE}.
#' @export
sss_vals <- function() {
  x <- c('cmp', 'mss', 'nas', 'oks', 'prt')
  names(x) <- rep.int("sss", length(x))
  x
}

#' @rdname sss
#' @export
xcmp <- function(x) {
  if      (xatm(x) & length(x) > 0) {!any(is.na(x))}
  else if (xatb(x) & length(x) > 0) {!any(is.na(x))}
  else if (xavl(x) & length(x) > 0) {!any(is.na(unlist(x)))}
  else {F}
}

#' @rdname sss
#' @export
xmss <- function(x) {
  if      (xatm(x) & length(x) > 0) {all(is.na(x))}
  else if (xatb(x) & length(x) > 0) {all(is.na(x))}
  else if (xavl(x) & length(x) > 0) {all(is.na(unlist(x)))}
  else {F}
}

#' @rdname sss
#' @export
xnas <- function(x) {isNa(x)}

#' @rdname sss
#' @export
xoks <- function(x) {isOk(x)}

#' @rdname sss
#' @export
xprt <- function(x) {
  A <- unlist(x)
  if      (xatm(x) & length(x) > 0) {any(is.na(x)) & any(!is.na(x))}
  else if (xatb(x) & length(x) > 0) {any(is.na(x)) & any(!is.na(x))}
  else if (xavl(x) & length(x) > 0) {any(is.na(A)) & any(!is.na(A))}
  else {F}
}

#' @rdname sss
#' @export
sss <- function(x) {
  c(if (xcmp(x)) {'cmp'} else {NULL},
    if (xprt(x)) {'prt'} else {NULL},
    if (xmss(x)) {'mss'} else {NULL},
    if (xnas(x)) {'nas'} else {NULL},
    if (xoks(x)) {'oks'} else {NULL})
}

#' @rdname sss
#' @export
is_sss <- function(x, xxx, ...) {
  if (!cmp_chr_scl(x)) {stop("\n  * [xxx] must be a complete character scalar.")}
  Valid <- sss_vals()
  XXX <- strsplit(xxx , "|", fixed = T)[[1]]
  Valid <- all(XXX %in% Valid)
  if (!Valid) {stop("\n  * [xxx] contains a value not in sss_vals(), after splitting [xxx] on pipes.")}
  if (!meets(x, ...)) {return(FALSE)}
  Observed <- xxx(x)
  for (Expected in XXX) {if (Expected == Observed) {return(TRUE)}}
  return(FALSE)
}
