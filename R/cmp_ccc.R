#' @name cmp_ccc.
#' @family props
#' @title Completeness + (atomic) Extended Class Properties
#' @description \tabular{ll}{
#'   \code{cmp_ccc_vals}   \tab Gets a character vector of all possible
#'                              \link[=icmp]{completeness} + atomic
#'                              \link[=ccc]{extended class} properties.      \cr
#'   \code{cmp_[ccc]}      \tab Evaluates whether \code{x} is
#'                              \link[=icmp]{complete} and of the atomic
#'                              \link[=ccc]{extended class} represented by the
#'                              placeholder \code{[ccc]}.                      }
#' @param x An R object.
#' @return \tabular{ll}{
#'   \code{cmp_mmm_vals}   \tab A character vector.                          \cr
#'   \code{cmp_[ccc]}      \tab A logical scalar.                              }
#' @export
cmp_ccc. <- function() {help("cmp_ccc.", package = "uj")}

#' @rdname cmp_ccc.
#' @export
cmp_ccc_vals <- function() {paste0("cmp_", ccc_vals())}

#' @rdname cmp_ccc.
#' @export
cmp_arr <- function(x) {atm_arr(x) & !any(is.na(av(x)))}

#' @rdname cmp_ccc.
#' @export
cmp_gen <- function(x) {atm_gen(x) & !any(is.na(av(x)))}

#' @rdname cmp_ccc.
#' @export
cmp_dtf <- function(x) {atm_dtf(x) & !any(is.na(av(x)))}

#' @rdname cmp_ccc.
#' @export
cmp_vls <- function(x) {atm_vls(x) & !any(is.na(av(x)))}

#' @rdname cmp_ccc.
#' @export
cmp_mat <- function(x) {atm_mat(x) & !any(is.na(av(x)))}

#' @rdname cmp_ccc.
#' @export
cmp_mvc <- function(x) {atm_mvc(x) & !any(is.na(av(x)))}

#' @rdname cmp_ccc.
#' @export
cmp_scl <- function(x) {atm_scl(x) & !any(is.na(av(x)))}

#' @rdname cmp_ccc.
#' @export
cmp_vec <- function(x) {atm_vec(x) & !any(is.na(av(x)))}
