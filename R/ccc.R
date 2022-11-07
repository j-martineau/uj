#' @name ccc.
#' @family props
#' @title Atomic extended class properties
#' @description Extended classes are not formally defined as a new classes, but
#'   are dynamically evaluated for characteristics through a call to
#'   \code{iccc(x.)} where the function name (e.g., 'ccc') is the extended class
#'   name (see \code{is_ccc} for mode-agnostic extended classes. \code{TRUE} or
#'   \code{FALSE} is always returned, and \code{FALSE} is always returned for
#'   the \code{NULL} object.
#'   \cr\cr
#'   There are two sets of functions for these extended classes in the form
#'   \code{atm_ccc} which allows an option to check for extended mode
#'   (\code{\link{is_mmm}}) or the form \code{iccc} which does not check for
#'   mode beyond being atomic. The are listed in the table below.
#'   \tabular{lll}{
#'     ATOMIC    \tab \strong{atm_ccc  }\tab \strong{iccc}\cr
#'     CLASS     \tab FUNCTION          \tab FUNCTION     \cr
#'     array     \tab \code{atm_arr  }  \tab \code{iarr}  \cr
#'     generic   \tab \code{atm_gen}    \tab \code{igen}  \cr
#'     dtf       \tab \code{atm_dtf}    \tab \code{idtf}  \cr
#'     vlist     \tab \code{atm_vls}    \tab \code{ivls}  \cr
#'     vtype     \tab \code{atm_vtp}    \tab \code{ivtp}  \cr
#'     matrix    \tab \code{atm_mat}    \tab \code{imat}  \cr
#'     multivec  \tab \code{atm_mvc}    \tab \code{imvc}  \cr
#'     scalar    \tab \code{atm_scl}    \tab \code{iscl}  \cr
#'     vec       \tab \code{atm_vec}    \tab \code{ivec}    }
#' @param x. An object.
#' @param xxx. A character scalar containing one or more values from
#'   \code{ccc_vals()} separated by pipes and/or underscores. Combinations of
#'   extended classes can be specified by separating them with underscores.
#'   Separating extended classes or combinations of extended classes with pipes
#'   will result in a value of \code{TRUE} if any of them applies to \code{x.}.
#' @param ... Additional arguments to \code{\link{meets}} containing value and
#'   element/row/column count restrictions.
#' @section Submitting additional arguments to \code{ccc} via \code{...}:
#'   Allows for checking not just the ccc but whether length, number of rows,
#'   number of columns, and element values meet flexible criteria.
#' @return \code{ccc_vals} returns a character vector containing all valid
#'   extended class property values. \code{ccc} returns a character scalar
#'   or vector containing all extended class properties from
#'   \code{ccc_vals()} applicable to \code{x.}. All others return either
#'   \code{TRUE} or \code{FALSE}.
#' @export
ccc. <- function() {help("ccc.", package = "uj")}

#' @describeIn ccc. Is \code{x.} an atomic array (optionally checking
#'   extended mode)?
#' @export
atm_arr <- function(x., xxx. = NULL) {
  vxxx <- ifelse(is.null(xxx.), T, isIN(xxx., mmm_vals()))
  if (!vxxx) {stop("\n • [xxx.] must be NULL or a character scalar value from mmm_vals().")}
  if (!pop_arr(x.)) {F}
  else if (!is.atomic(x.)) {F}
  else if (is.null(xxx.)) {T}
  else {run("i", xxx., "(x.)")}
}

#' @describeIn ccc. Is \code{x.} an atomic dtf (optionally checking extended
#'   mode)?
#' @export
atm_dtf <- function(x., xxx. = NULL) {
  vxxx <- ifelse(is.null(xxx.), T, isIN(xxx., mmm_vals()))
  if (!vxxx) {stop("\n • [xxx.] must be NULL or a character scalar value from mmm_vals().")}
  if (!pop_tbl(x.)) {F}
  else if (!all(apply(x., 2, is.atomic))) {F}
  else if (is.null(xxx.)) {T}
  else {
    code <- paste0("i", xxx., "(x.[ , ", 1:length(x.), "])")
    code <- paste0(code, collapse = ", ")
    code <- paste0("all(", code, ")")
    run(code)
}}

#' @describeIn ccc. Is \code{x.} an atomic generic (optionally checking extended
#'   mode)?
#' @export
atm_gen <- function(x., xxx. = NULL) {
  vxxx <- ifelse(is.null(xxx.), T, isIN(xxx., mmm_vals()))
  if (!vxxx) {stop("\n • [xxx.] must be NULL or a character scalar value from mmm_vals().")}
  if (!pop_gen(x.)) {F}
  else if (!is.atomic(x.)) {F}
  else if (is.null(xxx.)) {T}
  else {run("i", xxx., "(x.)")}
}

#' @describeIn ccc. Is \code{x.} an atomic vlist (optionally checking extended
#'   mode)?
#' @export
atm_vls <- function(x., xxx. = NULL) {
  vxxx <- ifelse(is.null(xxx.), T, isIN(xxx., mmm_vals()))
  if (!vxxx) {stop("\n • [xxx.] must be NULL or a character scalar value from mmm_vals().")}
  if (!pop_vls(x.)) {F}
  else if (!all(sapply(x., is.atomic))) {F}
  else if (is.null(xxx.)) {T}
  else {
    out <- paste0("i", xxx., "(x.[[", 1:length(x.), "]])")
    out <- paste0(out, collapse = ", ")
    run(paste0("all(", out, ")"))
}}

#' @describeIn ccc. Is \code{x.} an atomic vtype (optionally checking extended
#'   mode)?
#' @export
atm_vtp <- function(x., xxx. = NULL) {
  vx <- ifelse(is.null(xxx.), T, isIN(xxx., mmm_vals()))
  if (!vx) {stop("\n • [xxx.] must be NULL or a character scalar value from mmm_vals().")}
  if (!pop_vtp(x.)) {F}
  else if (!is.atomic(x.)) {F}
  else if (is.null(xxx.)) {T}
  else {run("i", xxx., "(x.)")}
}

#' @describeIn ccc. Is \code{x.} an atomic matrix (optionally checking extended
#'   mode)?
#' @export
atm_mat <- function(x., xxx. = NULL) {
  vxxx <- ifelse(is.null(xxx.), T, isIN(xxx., mmm_vals()))
  if (!vxxx) {stop("\n • [xxx.] must be NULL or a character scalar value from mmm_vals().")}
  if (!any_mat(x.)) {F}
  else if (!is.atomic(x.)) {F}
  else if (is.null(xxx.)) {T}
  else {run("i", xxx., "(x.)")}
}

#' @describeIn ccc. Is \code{x.} an atomic multivec (optionally checking
#'   extended mode)?
#' @export
atm_mvc <- function(x., xxx. = NULL) {
  vxxx <- ifelse(is.null(xxx.), T, isIN(xxx., mmm_vals()))
  if (!vxxx) {stop("\n • [xxx.] must be NULL or a character scalar value from mmm_vals().")}
  if (!any_mvc(x.)) {F}
  else if (!is.atomic(x.)) {F}
  else if (is.null(mmm)) {T}
  else {run("i", mmm, "(x.)")}
}

#' @describeIn ccc. Is \code{x.} an atomic scalar (optionally checking extended
#'   mode)?
#' @export
atm_scl <- function(x., xxx. = NULL) {
  vxxx <- ifelse(is.null(xxx.), T, isIN(xxx., mmm_vals()))
  if (!vxxx) {stop("\n • [xxx.] must be NULL or a character scalar value from mmm_vals().")}
  if (!any_scl(x.)) {F}
  else if (!is.atomic(x.)) {F}
  else if (is.null(xxx.)) {T}
  else {run("i", xxx., "(x.)")}
}

#' @describeIn ccc. Is \code{x.} an atomic vec (optionally checking extended
#'   mode)?
#' @export
atm_vec <- function(x., xxx. = NULL) {
  vxxx <- ifelse(is.null(xxx.), T, isIN(xxx., mmm_vals()))
  if (!vxxx) {stop("\n • [xxx.] must be NULL or a character scalar value from mmm_vals().")}
  if (!any_vec(x.)) {F}
  else if (!is.atomic(x.)) {F}
  else if (is.null(xxx.)) {T}
  else {run("i", xxx., "(x.)")}
}

#' @describeIn ccc. Get a character vector of all possible atomic extended
#'   classes.
#' @export
ccc_vals <- function() {
  x. <- c('arr', 'gen', 'dtf', 'vls', 'vtp', 'mat', 'mvc', 'scl', 'vec')
  names(x.) <- rep.int("ccc", length(x.))
  x.
}

#' @describeIn ccc. Is \code{x.} an atomic array?
#' @export
iarr <- function(x.) {atm_arr(x.)}

#' @describeIn ccc. Is \code{x.} an atomic generic?
#' @export
igen <- function(x.) {atm_gen(x.)}

#' @describeIn ccc. Is \code{x.} an atomic dtf?
#' @export
idtf <- function(x.) {atm_dtf(x.)}

#' @describeIn ccc. Is \code{x.} an atomic vlist?
#' @export
ivls <- function(x.) {atm_vls(x.)}

#' @describeIn ccc. Is \code{x.} an atomic vtype?
#' @export
ivtp <- function(x.) {atm_vtp(x.)}

#' @describeIn ccc. Is \code{x.} an atomic matrix?
#' @export
imat <- function(x.) {atm_mat(x.)}

#' @describeIn ccc. Is \code{x.} an atomic multivec?
#' @export
imvc <- function(x.) {atm_mvc(x.)}

#' @describeIn ccc. Is \code{x.} an atomic scalar?
#' @export
iscl <- function(x.) {atm_scl(x.)}

#' @describeIn ccc. Is \code{x.} an atomic vec?
#' @export
ivec <- function(x.) {atm_vec(x.)}

#' @describeIn ccc. Gets a vector of properties from \code{ccc_vals()} that are
#'   applicable to \code{x.}.
#' @export
ccc <- function(x.) {
  c(if (atm_arr(x.)) {'arr'} else {NULL},
    if (atm_gen(x.)) {'agn'} else {NULL},
    if (atm_tbl(x.)) {'atb'} else {NULL},
    if (atm_vls(x.)) {'avl'} else {NULL},
    if (atm_vtp(x.)) {'avt'} else {NULL},
    if (atm_mat(x.)) {'mat'} else {NULL},
    if (atm_mvc(x.)) {'mvc'} else {NULL},
    if (atm_scl(x.)) {'scl'} else {NULL},
    if (atm_vec(x.)) {'vec'} else {NULL})
}

#' @describeIn ccc. Evaluates whether any (combination) property in \code{xxx.}
#'   is an extended class property applicable to \code{x.}.
#' @export
iccc <- function(x., xxx., ...) {
  if (!cmp_chr_scl(xxx.)) {stop("\n • [xxx.] must be a complete character scalar (?cmp_chr_scl).")}
  valid. <- ccc_vals()
  combos. <- av(strsplit(xxx., "|", fixed = T))
  newxxx. <- av(strsplit(combos., "_", fixed = T))
  valid. <- all(newxxx. %in% valid.)
  if (!valid.) {stop("\n • [xxx.] contains a value not in ccc_vals(), after splitting [xxx.] on pipes and underscores.")}
  ixxx(x., xxx., ...)
}
