#' @name is_ccc.
#' @family props
#' @title Mode-Agnostic Extended Class
#' @description Extended classes are not formally defined as a new classes, but
#'   are dynamically evaluated for characteristics through a call to
#'   \code{is_ccc(x)} where \code{ccc} is the extended class. \code{TRUE} or
#'   \code{FALSE} is always returned, and \code{FALSE} is always returned for
#'   the \code{NULL} object. In this package, the following types of objects are
#'   new and are defined as given in the following table.\tabular{lll}{
#'   EXTENDED CLASS   \tab EXTENDED CLASS   \tab CHARACTERISTICS OF          \cr
#'   PROPERTY VALUE   \tab PROPERTY NAME    \tab QUALIFYING OBJECTS          \cr
#'   \code{'vls'}\tab vlist   \tab Non-data.frame (vector) list              \cr
#'   \code{'gen'}\tab generic \tab Any vector, vlist, or array               \cr
#'   \code{'scl'}\tab scalar  \tab Length-1 generic                          \cr
#'   \code{'mvc'}\tab multivec\tab Multiple-element vector, multiple-element
#'                                 vlist, or multiple-element array with 1
#'                                 \link[eee]{effective dimension}           \cr
#'   \code{'dtf'}\tab dtf     \tab Data.frame                                \cr
#'   \code{'vec'}\tab vec     \tab Scalar or multivec                        \cr
#'   \code{'vtp'}\tab vtype   \tab \link[iemp]{Empty} or \link[ipop]{populated}
#'                                 scalar, vec, or vlist.                      }
#'   There are additional functions checking for these extended classes, but
#'   restricted to atomic, specific extended modes, and complete (no \code{NA}
#'   values) and of specific extended mode defined in \code{\link{ccc}},
#'   \code{\link{mmm_ccc}}, and \code{\link{cmp_mmm_ccc}}, respectively.
#' @param x Any object.
#' @export
is_ccc. <- function() {help("is_ccc.", package = "uj")}

#' @describeIn is_ccc. Is \code{x} an array?
#' @export
is_arr <- function(x) {is.array(x) | is.vector(x)}

#' @describeIn is_ccc. Is \code{x} a generic?
#' @export
is_gen <- function(x) {is.vector(x) | is.array(x)}

#' @describeIn is_ccc. Is \code{x} a dtf?
#' @export
is_dtf <- function(x) {is.data.frame(x)}

#' @describeIn is_ccc. Is \code{x} a vlist?
#' @export
is_vls <- function(x) {is.list(x) & !is.data.frame(x)}

#' @describeIn is_ccc. Is \code{x} a vtype?
#' @export
is_vtp <- function(x) {(length(x) == 0 & !is.null(x)) | is.vector(x) | is.array(x)}

#' @describeIn is_ccc. Is \code{x} a matrix?
#' @export
is_mat <- function(x) {is.matrix(x)}

#' @describeIn is_ccc. Is \code{x} a multivec?
#' @export
is_mvc <- function(x) {length(x) > 1 & !is.data.frame(x) & (is.vector(x) | neee(x) == 1)}

#' @describeIn is_ccc. Is \code{x} a scalar?
#' @export
is_scl <- function(x) {is_gen(x) & length(x) == 1}

#' @describeIn is_ccc. Is \code{x} a vec?
#' @export
is_vec <- function(x) {is_scl(x) | is_mvc(x)}
