#' @name state
#' @family props
#' @title State of Completeness (state) Property Family
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
#' @param states A character scalar containing one or more values from
#'   \code{state_vals()} separated by pipes.
#' @param ... Additional arguments to \code{\link{meets}} containing value and
#'   element/row/column count restrictions.
#' @details \strong{\code{state_vals}}
#'   \cr Gets all valid state properties.
#'   \cr\cr
#'   \strong{\code{state}}
#'   \cr Gets a character scalar containing the state property from
#'   \code{state_vals()} that is applicable to \code{x}, if defined. If not
#'   defined, returns \code{NULL}.
#'   \cr\cr
#'   \strong{\code{is_state}}
#'   \cr Evaluates whether one of the state properties in \code{states} is
#'   applicable to \code{x}.
#'   \cr\cr
#'   \strong{\code{xstt}}
#'   \cr Evaluates whether \code{x} has the state property represented by
#'   \code{stt}.
#'   \cr\cr
#'   \strong{Additional Arguments in \code{...}}
#'   \cr Submitting additional arguments to \code{is_state} via \code{...}
#'   allows for checking not just the state but whether length, number of rows,
#'   number of columns, and element values meet flexible criteria.
#' @return \code{state_vals()} returns a character vector. \code{state(x)}
#'   returns a character scalar or \code{NULL}. All others return either
#'   \code{TRUE} or \code{FALSE}.
#' @export
state_vals <- function() {
  x <- c('cmp', 'mss', 'nas', 'oks', 'prt')
  names(x) <- rep.int("state", length(x))
  x
}

#' @rdname state
#' @export
xcmp <- function(x) {
  if      (xatm(x) & length(x) > 0) {!any(is.na(x))}
  else if (xatb(x) & length(x) > 0) {!any(is.na(x))}
  else if (xavl(x) & length(x) > 0) {!any(is.na(unlist(x)))}
  else {F}
}

#' @rdname state
#' @export
xmss <- function(x) {
  if      (xatm(x) & length(x) > 0) {all(is.na(x))}
  else if (xatb(x) & length(x) > 0) {all(is.na(x))}
  else if (xavl(x) & length(x) > 0) {all(is.na(unlist(x)))}
  else {F}
}

#' @rdname state
#' @export
xnas <- function(x) {isNa(x)}

#' @rdname state
#' @export
xoks <- function(x) {isOk(x)}

#' @rdname state
#' @export
xprt <- function(x) {
  A <- unlist(x)
  if      (xatm(x) & length(x) > 0) {any(is.na(x)) & any(!is.na(x))}
  else if (xatb(x) & length(x) > 0) {any(is.na(x)) & any(!is.na(x))}
  else if (xavl(x) & length(x) > 0) {any(is.na(A)) & any(!is.na(A))}
  else {F}
}

#' @rdname state
#' @export
states <- function(x) {
  c(if (xcmp(x)) {'cmp'} else {NULL},
    if (xprt(x)) {'prt'} else {NULL},
    if (xmss(x)) {'mss'} else {NULL},
    if (xnas(x)) {'nas'} else {NULL},
    if (xoks(x)) {'oks'} else {NULL})
}

#' @rdname state
#' @export
is_state <- function(x, states, ...) {
  if (!cmp_chr_scl(x)) {stop("\n  * [states] must be a complete character scalar.")}
  Valid <- state_vals()
  Props <- strsplit(states , "|", fixed = T)[[1]]
  Valid <- all(Props %in% Valid)
  if (!Valid) {stop("\n  * [states] contains a value not in state_vals(), after splitting [states] on pipes.")}
  if (!meets(x, ...)) {return(FALSE)}
  Observed <- states(x)
  for (Expected in Props) {if (Expected == Observed) {return(TRUE)}}
  return(FALSE)
}
