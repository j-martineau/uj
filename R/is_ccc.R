#' @name is_ccc_uj
#' @family props
#' @title Mode-Agnostic Extended Class
#' @description Extended classes are not formally defined as a new classes, but
#'   are dynamically evaluated for characteristics through a call to
#'   \code{is_ccc(x)} where 'ccc') is the extended \code{TRUE} or \code{FALSE}
#'   is always returned, and \code{FALSE} is always returned for the \code{NULL}
#'   object. In this package, the following types of objects are new and are
#'   defined as given in the following table.\tabular{lll}{
#'     EXTENDED CLASS \tab EXTENDED CLASS \tab CHARACTERISTICS               \cr
#'     PROPERTY VALUE \tab PROPERTY NAME  \tab QUALIFYING                    \cr
#'     \code{'vls'}   \tab vlist          \tab non-data.frame (vector) list  \cr
#'     \code{'gen'}   \tab generic        \tab any vector, vlist, or array   \cr
#'     \code{'scl'}   \tab scalar         \tab length-1 generic              \cr
#'     \code{'mvc'}   \tab multivec       \tab multiple-element vector,
#'                                        multiple-element vlist, or
#'                                        multiple-element array with 1
#'                                        \link[=eee]{effective dimension}   \cr
#'     \code{'tab'}   \tab tabular        \tab \code{\link[base]{data.frame}}\cr
#'     \code{'vec'}   \tab vec            \tab scalar or mvec                \cr
#'     \code{'vtp'}   \tab vtype          \tab \code{link[=xemp]{empty}} or
#'                                        \code{link[=xpop]{populated}} scalar,
#'                                        vector, or vlist.                    }
#'   There are additional functions checking for these extended classes, but
#'   restricted to atomic, specific extended modes, and complete (no \code{NA}
#'   values) and of specific extended mode defined in \code{\link{'ccc'}},
#'   \code{\link{mmm_ccc}}, and \code{\link{cmp_mmm_ccc}}, respectively.
#' @export
is_ccc_uj <- function() {help("is_ccc", package = "uj")}

#' @describeIn is_ccc_uj Is \code{x} any array?
#' @export
is_arr <- function(x) {is.array(x) | is.vector(x)}

#' @describeIn is_ccc_uj Is \code{x} a generic?
#' @export
is_gen <- function(x) {is.vector(x) | is.array(x)}

#' @describeIn is_ccc_uj Is \code{x} a tabular?
#' @export
is_tab <- function(x) {is.data.frame(x)}

#' @describeIn is_ccc_uj Is \code{x} a vlist?
#' @export
is_vls <- function(x) {is.list(x) & !is.data.frame(x)}

#' @describeIn is_ccc_uj Is \code{x} a vtype?
#' @export
is_vtp <- function(x) {(length(x) == 0 & !is.null(x)) | is.vector(x) | is.array(x)}

#' @describeIn is_ccc_uj Is \code{x} a matrix?
#' @export
is_mat <- function(x) {is.matrix(x)}

#' @describeIn is_ccc_uj Is \code{x} an mvect?
#' @export
is_mvc <- function(x) {length(x) > 1 & !is.data.frame(x) & (is.vector(x) | neee(x) == 1)}

#' @describeIn is_ccc_uj Is \code{x} a scalar?
#' @export
is_scl <- function(x) {is_gen(x) & length(x) == 1}

#' @describeIn is_ccc_uj Is \code{x} a vector?
#' @export
is_vec <- function(x) {is_scl(x) | is_mvc(x)}
