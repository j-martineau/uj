#' @name cmp_mmm_gen
#' @family props
#' @title Complete + Extended Mode + Atomic Generic (cmp + mmm + avl; includes
#'   atomic tibbles and atomic vlists)
#' @description Get all possible complete + extended mode + atomic generic
#'   properties.
#' @details See \code{\link{mmm}}, and \code{\link{cmp_gen}}.
#' @param x An object
#' @return \code{TRUE} or \code{FALSE}
#' @export
cmp_mmm_gen_vals <- function() {
  x <- paste0("cmp_", c("atm", mmm_vals()), "_gen")
  names(x) <- rep.int("cmp_mmm_gen", length(x))
  x
}

#' @describeIn cmp_mmm_gen Is \code{x} a complete atomic generic?
#' @export
cmp_atm_gen <- function(x) {cmp_gen(x) & iatm(x)}

#' @describeIn cmp_mmm_gen Is \code{x} a complete character atomic generic?
#' @export
cmp_chr_gen <- function(x) {cmp_gen(x, "chr")}

#' @describeIn cmp_mmm_gen Is \code{x} a complete onechar atomic generic?
#' @export
cmp_ch1_gen <- function(x) {cmp_gen(x, "ch1")}

#' @describeIn cmp_mmm_gen Is \code{x} a complete color atomic generic?
#' @export
cmp_clr_gen <- function(x) {cmp_gen(x, "clr")}

#' @describeIn cmp_mmm_gen Is \code{x} a complete even-number atomic generic?
#' @export
cmp_evn_gen <- function(x) {cmp_gen(x, "evn")}

#' @describeIn cmp_mmm_gen Is \code{x} a complete factor atomic generic?
#' @export
cmp_fac_gen <- function(x) {cmp_gen(x, "fac")}

#' @describeIn cmp_mmm_gen Is \code{x} a complete fractional-numeric atomic
#'   generic?
#' @export
cmp_frc_gen <- function(x) {cmp_gen(x, "frc")}

#' @describeIn cmp_mmm_gen Is \code{x} a complete indexer atomic generic?
#' @export
cmp_ind_gen <- function(x) {cmp_gen(x, "ind")}

#' @describeIn cmp_mmm_gen Is \code{x} a complete logical atomic generic?
#' @export
cmp_lgl_gen <- function(x) {cmp_gen(x, "lgl")}

#' @describeIn cmp_mmm_gen Is \code{x} a complete negative-number atomic generic?
#' @export
cmp_neg_gen <- function(x) {cmp_gen(x, "neg")}

#' @describeIn cmp_mmm_gen Is \code{x} a complete negative-whole-number atomic
#'   generic?
#' @export
cmp_ngw_gen <- function(x) {cmp_gen(x, "ngw")}

#' @describeIn cmp_mmm_gen Is \code{x} a complete non-negative-number atomic
#'   generic?
#' @export
cmp_nng_gen <- function(x) {cmp_gen(x, "nng")}

#' @describeIn cmp_mmm_gen Is \code{x} a complete non-negative-whole-number
#'   atomic generic?
#' @export
cmp_nnw_gen <- function(x) {cmp_gen(x, "nnw")}

#' @describeIn cmp_mmm_gen Is \code{x} a complete non-positive-number atomic
#'   generic?
#' @export
cmp_nps_gen <- function(x) {cmp_gen(x, "nps")}

#' @describeIn cmp_mmm_gen Is \code{x} a complete non-positive-whole-number
#'   atomic generic?
#' @export
cmp_npw_gen <- function(x) {cmp_gen(x, "npw")}

#' @describeIn cmp_mmm_gen Is \code{x} a complete non-sortable atomic generic?
#' @export
cmp_nst_gen <- function(x) {cmp_gen(x, "nst")}

#' @describeIn cmp_mmm_gen Is \code{x} a complete number atomic generic?
#' @export
cmp_num_gen <- function(x) {cmp_gen(x, "num")}

#' @describeIn cmp_mmm_gen Is \code{x} a complete odd-number atomic generic?
#' @export
cmp_odd_gen <- function(x) {cmp_gen(x, "odd")}

#' @describeIn cmp_mmm_gen Is \code{x} a complete ordered-factor atomic generic?
#' @export
cmp_ord_gen <- function(x) {cmp_gen(x, "ord")}

#' @describeIn cmp_mmm_gen Is \code{x} a complete percent-number atomic generic?
#' @export
cmp_pct_gen <- function(x) {cmp_gen(x, "pct")}

#' @describeIn cmp_mmm_gen Is \code{x} a complete positive-number atomic
#'   generic?
#' @export
cmp_pos_gen <- function(x) {cmp_gen(x, "pos")}

#' @describeIn cmp_mmm_gen Is \code{x} a complete proportion-number atomic
#'   generic?
#' @export
cmp_ppn_gen <- function(x) {cmp_gen(x, "ppn")}

#' @describeIn cmp_mmm_gen Is \code{x} a complete positive-whole-number atomic
#'   generic?
#' @export
cmp_psw_gen <- function(x) {cmp_gen(x, "psw")}

#' @describeIn cmp_mmm_gen Is \code{x} a complete sortable atomic generic?
#' @export
cmp_srt_gen <- function(x) {cmp_gen(x, "srt")}

#' @describeIn cmp_mmm_gen Is \code{x} a complete string atomic generic?
#' @export
cmp_str_gen <- function(x) {cmp_gen(x, "str")}

#' @describeIn cmp_mmm_gen Is \code{x} a complete unordered-factor atomic
#'   generic?
#' @export
cmp_uno_gen <- function(x) {cmp_gen(x, "uno")}

#' @describeIn cmp_mmm_gen Is \code{x} a complete whole-number atomic generic?
#' @export
cmp_whl_gen <- function(x) {cmp_gen(x, "whl")}
