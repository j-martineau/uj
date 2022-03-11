#' @name eee
#' @family props
#' @title Effective Dimensionality (eee) Property Family
#' @description Effective dimensionality of a non-empty object is defined as
#'   the number of dimensions with multiple indexed positions. Effective
#'   dimensionality is undefined for empty objects. The following table
#'   describes values of effective dimensionality, property names assigned to
#'   them, and a definition of those values.
#'   \tabular{llll}{
#'     \emph{Effective Dim}\tab\emph{Value}
#'     \tab\emph{Name}\tab\emph{Qualifying Objects}
#'     \cr\code{NaN}\tab\code{'eUD'}\tab effectively dimensionally undefined.
#'                  \tab\code{NULL} or of length 0.
#'     \cr\code{0}  \tab\code{'e0D'}\tab effectively \code{0}-D
#'                  \tab vector of length 1, vlist of length 1, array of length
#'                       1, or code{1 × 1} tibble.
#'     \cr\code{1}  \tab\code{'e1D'}\tab effectively \code{1}-D
#'                  \tab vector or length 2 or more, vlist of length 2 or more
#'                       or non-empty array with multiple index positions in
#'                       only \code{1} dimension.
#'     \cr\code{2}  \tab\code{'e2D'}\tab Effectively \code{2}-D
#'                  \tab tibble with multiple rows and multiple columns,
#'                       matrix with multiple rows and multiple columns,-
#'                       non-empty array with multiple index positions in
#'                       only \code{2} dimensions.
#'     \cr\code{≥ 3}\tab\code{'eHD'}\tab Effectively hyper-D
#'                  \tab non-empty array with multiple index positions in at
#'                       least 3 dimensions.
#'   }
#' @param x An object.
#' @param xxx A character scalar containing one or more values from
#'   \code{eee_vals()} separated by pipes and/or underscores ("."). Combinations
#'   of effective dimensionality  properties can be specified by separating them
#'   with underscores. Separating effective dimensionality properties or
#'   combinations of effective dimensionality properties with pipes will result
#'   in a value of \code{TRUE} if any of them applies to \code{x}.
#' @param ... Additional arguments to \code{\link{meets}} containing value and
#'   element/row/column count restrictions.
#' @details \strong{\code{eee_vals}}
#'   \cr Gets all valid effective dimensionality property values.
#'   \cr\cr
#'   \strong{\code{neee}}
#'   \cr Gets the \strong{number} of effective dimensions of \code{x} (i.e., not
#'   a character scalar value from \code{eee_vals()}.
#'   \cr\cr
#'   \strong{\code{eee}}
#'   \cr Returns the name of the effective dimensionality property from
#'   \code{eee_vals()} applicable to \code{x}.
#'   \cr\cr
#'   \strong{\code{is_eee}}
#'   \cr Evaluates whether any (combination) property in \code{xxx} is an
#'   effective dimensionality property applicable to \code{x}
#'   \cr\cr
#'   \strong{\code{xeUD}, \code{xe0D}, \code{xe1D}, \code{xe2D}, and
#'   \code{xeHD}}
#'   \cr Evaluate whether the effective dimensionality of \code{x} is,
#'   respectively, \code{NaN} (undefined), \code{0}, \code{1}, \code{2}, and
#'   \code{3+}.
#'   \cr\cr
#'   \strong{Additional Arguments in \code{...}}
#'   \cr Submitting additional arguments to \code{is_eee} via \code{...} allows
#'   for checking not just the effective dimensionality but also whether length,
#'   number of rows, number of columns, and element values meet flexible
#'   criteria.
#' @return \code{eee_vals} and \code{eee} return a character scalar or
#'   vector of effective dimensionality property values. \code{eee(x)} returns
#'   \code{NaN} or a non-negative whole-number scalar. All others return either
#'   \code{TRUE} or \code{FALSE}.
#' @export
eee_vals <- function() {
  x <- c('e0D', 'e1D', 'e2D', 'eHD', 'eUD')
  names(x) <- rep.int("eee", length(x))
  x
}

#' @rdname eee
#' @export
xe0D <- function(x) {eee(x) == 0}

#' @rdname eee
#' @export
xe1D <- function(x) {eee(x) == 1}

#' @rdname eee
#' @export
xe2D <- function(x) {eee(x) == 2}

#' @rdname eee
#' @export
xeHD <- function(x) {eee(x) > 2}

#' @rdname eee
#' @export
xeUD <- function(x) {identical(eee(x), NaN)}

neee <- function(x) {f0(xeUD(x), NaN, f0(xe0D(x), 0, f0(xe1D(x), 1, f0(xe2D(x), 2, f0(length(which(dim(x) < 1)) > 0, 0, length(which(dim(x) > 1)))))))}

#' @rdname eee
#' @export
eee <- function(x) {
  E <- neee(x)
  f0(is.nan(E), 'eUD', f0(0 == E, 'e0D', f0(1 == E, 'e1D', f0(2 == E, 'e2D', f0(3 <= E, 'eHD', NULL)))))
}

#' @rdname eee
#' @export
is_eee <- function(x, xxx, ...) {
  if (!cmp_chr_scl(x)) {stop("\n  * [xxx] must be a complete character scalar.")}
  ValidVec <- eee_vals()
  Combos   <- strsplit(xxx , "|", fixed = T)[[1]]
  XXX      <- strsplit(Combos, ".", fixed = T)[[1]]
  Valid    <- all(XXX %in% ValidVec)
  if (!Valid) {stop("\n  * [xxx] contains a value not in eee_vals(), after splitting [xxx] on pipes and underscores.")}
  is_xxx(x, xxx, ...)
}
