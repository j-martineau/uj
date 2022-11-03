#' @name cmp_mmm_vec
#' @family props
#' @title Complete + Extended Mode + Atomic Vect (cmp + mmm + vec)
#' @description Get all possible complete + extended mode + atomic vect
#'   properties.
#' @details See \code{\link{mmm}}, and \code{\link{cmp_vect}}.
#' @param x An object
#' @return \code{TRUE} or \code{FALSE}.
#' @export
cmp_mmm_vec_vals <- function() {
  x <- paste0("cmp_", c("atm", mmm_vals()), "_vec")
  names(x) <- rep.int("cmp_mmm_vec", length(x))
  x
}

#' @describeIn cmp_mmm_vec Is \code{x} a complete atomic vect?
#' @export
cmp_atm_vec <- function(x) {cmp_vec(x, "atm")}

#' @describeIn cmp_mmm_vec Is \code{x} a complete character atomic vect?
#' @export
cmp_chr_vec <- function(x) {cmp_vec(x, "chr")}

#' @describeIn cmp_mmm_vec Is \code{x} a complete onechar atomic vect?
#' @export
cmp_ch1_vec <- function(x) {cmp_vec(x, "ch1")}

#' @describeIn cmp_mmm_vec Is \code{x} a complete color  atomic vect?
#' @export
cmp_clr_vec <- function(x) {cmp_vec(x, "clr")}

#' @describeIn cmp_mmm_vec Is \code{x} a complete even-numeric atomic vect?
#' @export
cmp_evn_vec <- function(x) {cmp_vec(x, "evn")}

#' @describeIn cmp_mmm_vec Is \code{x} a complete factor atomic vect?
#' @export
cmp_fac_vec <- function(x) {cmp_vec(x, "fac")}

#' @describeIn cmp_mmm_vec Is \code{x} a complete fractional-numeric atomic
#'   vect?
#' @export
cmp_frc_vec <- function(x) {cmp_vec(x, "frc")}

#' @describeIn cmp_mmm_vec Is \code{x} a complete indexer atomic vect?
#' @export
cmp_ind_vec <- function(x) {cmp_vec(x, "ind")}

#' @describeIn cmp_mmm_vec Is \code{x} a complete logical atomic vect?
#' @export
cmp_lgl_vec <- function(x) {cmp_vec(x, "lgl")}

#' @describeIn cmp_mmm_vec Is \code{x} a complete negative-numeric atomic vect?
#' @export
cmp_neg_vec <- function(x) {cmp_vec(x, "neg")}

#' @describeIn cmp_mmm_vec Is \code{x} a complete negative-whole-numeric atomic
#'   vect?
#' @export
cmp_ngw_vec <- function(x) {cmp_vec(x, "ngw")}

#' @describeIn cmp_mmm_vec Is \code{x} a complete non-negative-numeric atomic
#'   vect?
#' @export
cmp_nng_vec <- function(x) {cmp_vec(x, "nng")}

#' @describeIn cmp_mmm_vec Is \code{x} a complete non-negative-whole-numeric
#'   atomic vect?
#' @export
cmp_nnw_vec <- function(x) {cmp_vec(x, "nnw")}

#' @describeIn cmp_mmm_vec Is \code{x} a complete non-positive-numeric atomic
#'   vect?
#' @export
cmp_nps_vec <- function(x) {cmp_vec(x, "nps")}

#' @describeIn cmp_mmm_vec Is \code{x} a complete non-positive-whole-numeric
#'   atomic vect?
#' @export
cmp_npw_vec <- function(x) {cmp_vec(x, "npw")}

#' @describeIn cmp_mmm_vec Is \code{x} a complete non-sortable atomic vect?
#' @export
cmp_nst_vec <- function(x) {cmp_vec(x, "nst")}

#' @describeIn cmp_mmm_vec Is \code{x} a complete numeric atomic vect?
#' @export
cmp_num_vec <- function(x) {cmp_vec(x, "num")}

#' @describeIn cmp_mmm_vec Is \code{x} a complete odd-numeric atomic vect?
#' @export
cmp_odd_vec <- function(x) {cmp_vec(x, "odd")}

#' @describeIn cmp_mmm_vec Is \code{x} a complete ordered-factor atomic vect?
#' @export
cmp_ord_vec <- function(x) {cmp_vec(x, "ord")}

#' @describeIn cmp_mmm_vec Is \code{x} a complete percent-numeric atomic vect?
#' @export
cmp_pct_vec <- function(x) {cmp_vec(x, "pct")}

#' @describeIn cmp_mmm_vec Is \code{x} a complete positive-numeric atomic vect?
#' @export
cmp_pos_vec <- function(x) {cmp_vec(x, "pos")}

#' @describeIn cmp_mmm_vec Is \code{x} a complete proportion-numeric atomic
#'   vect?
#' @export
cmp_ppn_vec <- function(x) {cmp_vec(x, "ppn")}

#' @describeIn cmp_mmm_vec Is \code{x} a complete positive-whole-numeric atomic
#'   vect?
#' @export
cmp_psw_vec <- function(x) {cmp_vec(x, "psw")}

#' @describeIn cmp_mmm_vec Is \code{x} a complete sortable atomic vect?
#' @export
cmp_srt_vec <- function(x) {cmp_vec(x, "srt")}

#' @describeIn cmp_mmm_vec Is \code{x} a complete string atomic vect?
#' @export
cmp_str_vec <- function(x) {cmp_vec(x, "str")}

#' @describeIn cmp_mmm_vec Is \code{x} a complete unordered factor atomic vect?
#' @export
cmp_uno_vec <- function(x) {cmp_vec(x, "uno")}

#' @describeIn cmp_mmm_vec Is \code{x} a complete whole-numeric atomic vect?
#' @export
cmp_whl_vec <- function(x) {cmp_vec(x, "whl")}
