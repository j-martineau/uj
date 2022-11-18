#' @name fff.
#' @family props
#' @title Object form properties
#' @description Gets a vector of form properties from \code{fff_vals()} that are
#'   applicable to \code{x}. An object's form properties have to do with the
#'   number of \link[=ddd]{defined dimensions} and/or \link[=eee]{effective
#'   dimensions} plus some additional restrictions:\tabular{lll}{
#'   FUNDAMENTAL   \tab FUNDAMENTAL   \tab FUNDAMENTAL                       \cr
#'   TYPE VALUE    \tab TYPE NAME     \tab TYPE DEFINITION                   \cr
#'   \code{'emp'}\tab empty         \tab Non-\code{NULL}, length = 0.        \cr
#'   \code{'pop'}\tab populated     \tab Length > 0.                         \cr
#'   \code{'pnt'}\tab point         \tab Containing 1 element (includes 1-by-1
#'                                       data.frames).                       \cr
#'   \code{'lin'}\tab linear        \tab Effectively 1-dimensional.          \cr
#'   \code{'row'}\tab 1-row         \tab Row matrix or row data.frame.       \cr
#'   \code{'col'}\tab 1-column      \tab Column matrix or column data.frame. \cr
#'   \code{'rct'}\tab rectangular   \tab 2+ by 2+ matrix or data.frame  .    \cr
#'   \code{'sqr'}\tab square        \tab Square atomic matrix.               \cr
#'   \code{'sld'}\tab solid         \tab Array with 2+ index positions in 3+
#'                                       dimensions.                           }
#'   Functions related to form properties are described in the following
#'   table:\tabular{ll}{
#'   FUNCTION          \tab WHAT THE                                         \cr
#'   FORMAT            \tab FUNCTION DOES                                    \cr
#'   \code{i[fff]}       \tab Evaluates whether an object is of the form
#'                          represented by \code{***}.                       \cr
#'   \code{fff}        \tab Gets a character scalar containing the forms of an
#'                          object.                                          \cr
#'   \code{ifff}       \tab Evaluates an object for a specific form and any
#'                          additional properties specified in \code{...}.   \cr
#'   \code{fff_vals}   \tab Gets a character vector of all possible form
#'                          property values.                                   }
#' @param x An object.
#' @param fff \link[=cmp_chr_scl]{Complete character scalar} containing one or
#'   more values from \code{fff_vals()} separated by pipes and/or underscores.
#'   Combinations of forms can be specified by separating them with underscores.
#'   Separating forms or combinations of forms with pipes will result in a value
#'   of \code{TRUE} if any of them applies to \code{x}.
#' @inheritDotParams meets.
#' @inheritSection meets. Specifying Additional Property Requirements
#' @return \tabular{lll}{
#'   \code{fff_vals} and \code{fff}\tab  \tab A character vector.            \cr
#'   \code{ifff} and \code{i[fff]} \tab  \tab A logical scalar.                }
#' @export
fff. <- function() {help("fff.", "uj")}

#' @rdname fff.
#' @export
fff <- function(x) {
  nr <- nrow(x); nc <- ncol(x); nl <- is.null(x); rm <- nr > 1; r1 <- nr == 1
  nd <- nddd(x); ne <- neee(x); n  <- length( x); cm <- cn > 1; c1 <- cn == 1
  eq <- nr == nc
  c(f0(nd != 2, NULL, f0(rm & c1, 'col', NULL)),
    f0(n  != 0, NULL, f0(nl     , 'emp', NULL)),
    f0(ne != 1, NULL,             'lin'       ),
    f0(ne != 0, NULL,             'pnt'       ),
    f0(nd != 2, NULL, f0(rm & cm, 'rct', NULL)),
    f0(nd != 2, NULL, f0(cm & r1, 'row', NULL)),
    f0(ne <= 2, NULL,             'sld'       ),
    f0(nd != 2, NULL, f0(rm & eq, 'sqr', NULL)))
}

#' @rdname fff.
#' @export
iemp <- function(x) {f0(length(x) != 0, F, !is.null(x))}

#' @rdname fff.
#' @export
ipnt <- function(x) {neee(x) == 0}

#' @rdname fff.
#' @export
ilin <- function(x) {neee(x) == 1}

#' @rdname fff.
#' @export
irow <- function(x) {f0(!nddd(x) == 2, F, nrow(x) == 1 & ncol(x) > 1)}

#' @rdname fff.
#' @export
icol <- function(x) {f0(!nddd(x) == 2, F, nrow(x) > 1 & ncol(x) == 1)}

#' @rdname fff.
#' @export
irct <- function(x) {f0(!nddd(x) == 2, F, nrow(x) > 1 & ncol(x) > 1)}

#' @rdname fff.
#' @export
isqr <- function(x) {f0(!nddd(x) == 2, F, nrow(x) > 1 & ncol(x) == nrow(x))}

#' @rdname fff.
#' @export
isld <- function(x) {neee(x) > 2}

#' @rdname fff.
#' @export
fff_vals <- function() {c('col', 'emp', 'lin', 'pnt', 'rct', 'row', 'sqr', 'sld')}

#' @rdname fff.
#' @export
ifff <- function(x, fff, ...) {
  if (!cmp_chr_scl(fff)) {stop("\n \u2022 [fff] must be a non-NA character scalar.")}
  valid.fff <- ttt_vals()
  fff.combos <- strsplit(fff, "|", fixed = T)[[1]]
  new.fff <- unlist(strsplit(fff.combos, "_", fixed = T))
  if (!all(new.fff %in% valid.fff)) {stop("\n \u2022 [fff = '", fff, "'] specifies a 'form' property not in [uj::fff_vals() = c(", paste0(paste0("'", fff_vals(), "'"), collapse = ", "), ")].")}
  ippp(x, fff, ...)
}
