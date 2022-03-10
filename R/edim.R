#' @name edim
#' @family props
#' @title Effective Dimensionality (edim) Property Family
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
#' @param edims A character scalar containing one or more values from
#'   \code{props_edim()} separated by pipes and/or underscores (".").
#'   Combinations of edim properties can be specified by separating them with
#'   underscores. Separating edim properties or combinations of edim properties
#'   with pipes will result in a value of \code{TRUE} if any of them applies to
#'   \code{x}.
#' @param ... Additional arguments to \code{\link{meets}} containing value and
#'   element/row/column count restrictions.
#' @details \strong{\code{edim_vals}}
#'   \cr Gets all valid effective dimensionality property values.
#'   \cr\cr
#'   \strong{\code{edim}}
#'   \cr Gets the \strong{number} of effective dimensions of \code{x} (i.e., not
#'   a character scalar value from \code{edim_vals()}.
#'   \cr\cr
#'   \strong{\code{is_edim}}
#'   \cr Evaluates whether any property in \code{edims} is an effective
#'   dimensionality erty applicable to \code{x}
#'   \cr\cr
#'   \strong{\code{xeUD}, \code{xe0D}, \code{xe1D}, \code{xe2D}, and
#'   \code{xeHD}}
#'   \cr Evaluate whether the effective dimensionality of \code{x} is,
#'   respectively, \code{NaN} (undefined), \code{0}, \code{1}, \code{2}, and
#'   \code{3+}.
#'   \cr\cr
#'   \strong{Additional Arguments in \code{...}}
#'   \cr Submitting additional arguments to \code{is_ddim} via \code{...} allows
#'   for checking not just the ddim but whether length, number of rows, number
#'   of columns, and element values meet flexible criteria.
#' @return \code{edim_vals} and \code{edims} return a character scalar or
#'   vector. effective dimensionality erty values. \code{edim(x)} returns
#'   \code{NaN} or a non-negative whole-number scalar. All others return either
#'   \code{TRUE} or \code{FALSE}.
#' @export
edim_vals <- function() {
  x <- c('e0D', 'e1D', 'e2D', 'eHD', 'eUD')
  names(x) <- rep.int("edim", length(x))
  x
}

#' @rdname edim
#' @export
edim <- function(x) {f0(length(x) == 0, NaN, f0(is.vector(x), f0(length(x) == 1, 0, 1), length(which(dim(x) > 1))))}

#' @rdname edim
#' @export
xe0D <- function(x) {edim(x) == 0}

#' @rdname edim
#' @export
xe1D <- function(x) {edim(x) == 1}

#' @rdname edim
#' @export
xe2D <- function(x) {edim(x) == 2}

#' @rdname edim
#' @export
xeHD <- function(x) {edim(x) > 2}

#' @rdname edim
#' @export
xeUD <- function(x) {identical(edim(x), NaN)}

#' @rdname edim
#' @export
edims <- function(x) {
  E <- edim(x)
  f0(is.nan(E), 'eUD', f0(0 == E, 'e0D', f0(1 == E, 'e1D', f0(2 == E, 'e2D', f0(3 <= E, 'eHD', NULL)))))
}

#' @rdname edim
#' @export
is_edim <- function(x, edims, ...) {
  if (!cmp_chr_scl(x)) {stop("\n  * [edims] must be a complete character scalar.")}
  ValidVec <- edim_vals()
  Combos   <- strsplit(edims , "|", fixed = T)[[1]]
  PropVec  <- strsplit(Combos, ".", fixed = T)[[1]]
  Valid    <- all(PropVec %in% ValidVec)
  if (!Valid) {stop("\n  * [edims] contains a value not in edim_vals(), after splitting [edims] on pipes and underscores.")}
  is_prop(x, edims, ...)
}
