#' @name ttt_ccc_uj
#' @family props
#' @title Extended class + fundamental type properties
#' @param x An object.
#' @param xxx Optional character scalar extended mode (see
#'   \code{\link{mmm_vals}}).
#' @return Logical scalar.
#' @export
ttt_ccc_uj <- function() {help("ttt_ccc_uj", package = "uj")}

#' @describeIn ttt_ccc_uj Get a character vector of all possible fundamental
#'   type plus extended class properties.
#' @export
ttt_ccc_vals <- function() {av(apply(expand.grid(ttt = c("any", "atm", "cmp", "emp", "pop", "rcr"), ccc = c("arr", "gen", "mat", "mvc", "scl", "tib", "vec", "vls", "vtp")), 2, dw0))}

# any_ccc ####

#' @describeIn ttt_ccc_uj Is \code{x} an array of any type (\code{\link{iarr}})?
#' @export
any_arr <- function(x) {is.array(x)}

#' @describeIn ttt_ccc_uj Is \code{x} any type of generic (\code{\link{igen}})?
#' @export
any_gen <- function(x) {if (is.vector(x) | is.array(x)) {T} else {is.data.frame(x)}}

#' @describeIn ttt_ccc_uj Is \code{x} a matrix of any type (\code{\link{imat}})?
#' @export
any_mat <- function(x) {is.matrix(x)}

#' @describeIn ttt_ccc_uj Is \code{x} an mvect of any type (\code{\link{imvc}})?
#' @export
any_mvc <- function(x) {if (length(x) < 2) {F} else if (is.vector(x)) {T} else if (!is.array(x)) {F} else {length(which(dim(x) > 1) == 1)}}

#' @describeIn ttt_ccc_uj Is \code{x} is any type of scalar
#'   (\code{\link{iscl}})?
#' @export
any_scl <- function(x) {if (length(x) != 1) {F} else {is.vector(x) | is.array(x)}}

#' @describeIn ttt_ccc_uj Is \code{x} any type of tabular (\code{\link{itbl}})?
#' @export
any_tbl <- function(x) {is.data.frame(x)}

#' @describeIn ttt_ccc_uj Is \code{x} any type of vec (\code{\link{ivec}})?
#' @export
any_vec <- function(x) {if (length(x) == 0) {F} else if (is.vector(x)) {T} else if (!is.array(x)) {F} else {length(which(dim(x) > 1)) <= 1}}

#' @describeIn ttt_ccc_uj Is \code{x} any type of vlist (\code{\link{ivls}})?
#' @export
any_vls <- function(x) {if (is.data.frame(x)) {F} else {is.list(x)}}

#' @describeIn ttt_ccc_uj Is \code{x} any type of vtype (\code{\link{ivtp}})?
#' @export
any_vtp <- function(x) {if (is.data.frame(x)) {F} else if (length(x) < 2) {T} else {ipop(x) & ie1D(x)}}

# atm_ccc ####

#' @describeIn ttt_ccc_uj Is \code{x} an atomic array?
#' @export
atm_arr <- function(x, xxx = NULL) {
  vxxx <- ifelse(is.null(xxx), T, isIN(xxx, mmm_vals()))
  if (!vxxx) {stop("\n • [xxx] must be NULL or a character scalar value from mmm_vals().")}
  if (!pop_arr(x)) {F}
  else if (!is.atomic(x)) {F}
  else if (is.null(xxx)) {T}
  else {run("i", xxx, "(x)")}
}

#' @describeIn ttt_ccc_uj Is \code{x} an atomic generic (\code{\link{igen}})?
#' @export
atm_gen <- function(x, xxx = NULL) {
  vxxx <- ifelse(is.null(xxx), T, isIN(xxx, mmm_vals()))
  if (!vxxx) {stop("\n • [xxx] must be NULL or a character scalar value from mmm_vals().")}
  if (!pop_gen(x)) {F}
  else if (!is.atomic(x)) {F}
  else if (is.null(xxx)) {T}
  else {run("i", xxx, "(x)")}
}

#' @describeIn ttt_ccc_uj Is \code{x} an atomic matrix?
#' @export
atm_mat <- function(x, xxx = NULL) {
  vxxx <- ifelse(is.null(xxx), T, isIN(xxx, mmm_vals()))
  if (!vxxx) {stop("\n • [xxx] must be NULL or a character scalar value from mmm_vals().")}
  if (!any_mat(x)) {F}
  else if (!is.atomic(x)) {F}
  else if (is.null(xxx)) {T}
  else {run("i", xxx, "(x)")}
}

#' @describeIn ttt_ccc_uj Is \code{x} an atomic mvect (\code{\link{imvc}})?
#' @export
atm_mvc <- function(x, xxx = NULL) {
  vxxx <- ifelse(is.null(xxx), T, isIN(xxx, mmm_vals()))
  if (!vxxx) {stop("\n • [xxx] must be NULL or a character scalar value from mmm_vals().")}
  if (!any_mvc(x)) {F}
  else if (!is.atomic(x)) {F}
  else if (is.null(mmm)) {T}
  else {run("i", mmm, "(x)")}
}

#' @describeIn ttt_ccc_uj Is \code{x} is an atomic scalar (\code{\link{iscl}})?
#' @export
atm_scl <- function(x, xxx = NULL) {
  vxxx <- ifelse(is.null(xxx), T, isIN(xxx, mmm_vals()))
  if (!vxxx) {stop("\n • [xxx] must be NULL or a character scalar value from mmm_vals().")}
  if (!any_scl(x)) {F}
  else if (!is.atomic(x)) {F}
  else if (is.null(xxx)) {T}
  else {run("i", xxx, "(x)")}
}

#' @describeIn ttt_ccc_uj Is \code{x} an atomic tabular (\code{\link{itbl}})?
#' @export
atm_tbl <- function(x, xxx = NULL) {
  vxxx <- ifelse(is.null(xxx), T, isIN(xxx, mmm_vals()))
  if (!vxxx) {stop("\n • [xxx] must be NULL or a character scalar value from mmm_vals().")}
  if (!pop_tbl(x)) {F}
  else if (!all(apply(x, 2, is.atomic))) {F}
  else if (is.null(xxx)) {T}
  else {
    code <- paste0("i", xxx, "(x[ , ", 1:length(x), "])")
    code <- paste0(code, collapse = ", ")
    code <- paste0("all(", code, ")")
    run(code)
}}

#' @describeIn ttt_ccc_uj Is \code{x} an atomic vec (\code{\link{ivec}})?
#' @export
atm_vec <- function(x, xxx = NULL) {
  vxxx <- ifelse(is.null(xxx), T, isIN(xxx, mmm_vals()))
  if (!vxxx) {stop("\n • [xxx] must be NULL or a character scalar value from mmm_vals().")}
  if (!any_vec(x)) {F}
  else if (!is.atomic(x)) {F}
  else if (is.null(xxx)) {T}
  else {run("i", xxx, "(x)")}
}

#' @describeIn ttt_ccc_uj Is \code{x} an atomic vlist (\code{\link{ivls}})?
#' @export
atm_vls <- function(x, xxx = NULL) {
  vxxx <- ifelse(is.null(xxx), T, isIN(xxx, mmm_vals()))
  if (!vxxx) {stop("\n • [xxx] must be NULL or a character scalar value from mmm_vals().")}
  if (!pop_vls(x)) {F}
  else if (!all(sapply(x, is.atomic))) {F}
  else if (is.null(xxx)) {T}
  else {
    out <- paste0("i", xxx, "(x[[", 1:length(x), "]])")
    out <- paste0(out, collapse = ", ")
    run(paste0("all(", out, ")"))
}}

#' @describeIn ttt_ccc_uj Is \code{x} an atomic vtype (\code{\link{ivtp}})?
#' @export
atm_vtp <- function(x, xxx = NULL) {
  vx <- ifelse(is.null(xxx), T, isIN(xxx, mmm_vals()))
  if (!vx) {stop("\n • [xxx] must be NULL or a character scalar value from mmm_vals().")}
  if (!pop_vtp(x)) {F}
  else if (!is.atomic(x)) {F}
  else if (is.null(xxx)) {T}
  else {run("i", xxx, "(x)")}
}

# pop_ccc ####

#' @describeIn ttt_ccc_uj Is \code{x} a populated (\code{\link{ipop}}) array?
#' @export
pop_arr <- function(x, xxx = NULL) {length(x) > 0 & f0.(inll(xxx), any_arr(x), atm_arr(x, xxx))}

#' @describeIn ttt_ccc_uj Is \code{x} a populated (\code{\link{ipop}}) generic?
#' @export
pop_gen <- function(x, xxx = NULL) {length(x) > 0 & f0.(inll(xxx), any_gen(x), atm_gen(x, xxx))}

#' @describeIn ttt_ccc_uj Is \code{x} a populated (\code{\link{ipop}}) matrix?
#' @export
pop_mat <- function(x, xxx = NULL) {length(x) > 0 & f0.(inll(xxx), any_mat(x), atm_mat(x, xxx))}

#' @describeIn ttt_ccc_uj Is \code{x} a populated (\code{\link{ipop}}) multivec?
#' @export
pop_mvc <- function(x, xxx = NULL) {length(x) > 0 & f0.(inll(xxx), any_mvc(x), atm_mvc(x, xxx))}

#' @describeIn ttt_ccc_uj Is \code{x} a populated (\code{\link{ipop}}) scalar?
#' @export
pop_scl <- function(x, xxx = NULL) {length(x) > 0 & f0.(inll(xxx), any_scl(x), atm_scl(x, xxx))}

#' @describeIn ttt_ccc_uj Is \code{x} a populated (\code{\link{ipop}}) tabular?
#' @export
pop_tbl <- function(x, xxx = NULL) {length(x) > 0 & f0.(inll(xxx), any_tbl(x), atm_tbl(x, xxx))}

#' @describeIn ttt_ccc_uj Is \code{x} a populated (\code{\link{ipop}}) vec?
#' @export
pop_vec <- function(x, xxx = NULL) {length(x) > 0 & f0.(inll(xxx), any_vec(x), atm_vec(x, xxx))}

#' @describeIn ttt_ccc_uj Is \code{x} a populated (\code{\link{ipop}}) vlist?
#' @export
pop_vls <- function(x, xxx = NULL) {length(x) > 0 & f0.(inll(xxx), any_vls(x), atm_vls(x, xxx))}

#' @describeIn ttt_ccc_uj Is \code{x} a populated (\code{\link{ipop}}) vtype?
#' @export
pop_vtp <- function(x, xxx = NULL) {length(x) > 0 & f0.(inll(xxx), any_vtp(x), atm_vtp(x, xxx))}

#' @describeIn ttt_ccc_uj Is \code{x} a complete array? (\code{\link{icmp}},
#'   \code{\link{iarr}})
#' @export
cmp_arr <- function(x) {icmp(x) & iarr(x)}

#' @describeIn ttt_ccc_uj Is \code{x} a complete generic? (\code{\link{icmp}},
#'   \code{\link{igen}})
#' @export
cmp_gen <- function(x) {icmp(x) & igen(x)}

#' @describeIn ttt_ccc_uj Is \code{x} a complete matrix? (\code{\link{icmp}},
#'   \code{\link{imat}})
#' @export
cmp_mat <- function(x) {icmp(x) & imat(x)}

#' @describeIn ttt_ccc_uj Is \code{x} a complete multivec? (\code{\link{icmp}},
#'   \code{\link{imvc}})
#' @export
cmp_mvc <- function(x) {icmp(x) & imvc(x)}

#' @describeIn ttt_ccc_uj Is \code{x} a complete scalar? (\code{\link{icmp}},
#'   \code{\link{iscl}})
#' @export
cmp_scl <- function(x) {icmp(x) & iscl(x)}

#' @describeIn ttt_ccc_uj Is \code{x} a complete tabular? (\code{\link{icmp}},
#'   \code{\link{itab}})
#' @export
cmp_atb <- function(x) {icmp(x) & itab(x)}

#' @describeIn ttt_ccc_uj Is \code{x} a complete vector? (\code{\link{icmp}},
#'   \code{\link{ivec}})
#' @export
cmp_vec <- function(x) {icmp(x) & ivec(x)}

#' @describeIn ttt_ccc_uj Is \code{x} a complete vlist? (\code{\link{icmp}},
#'   \code{\link{ivls}})
#' @export
cmp_vls <- function(x) {icmp(x) & ivls(x)}

#' @describeIn ttt_ccc_uj Is \code{x} a complete vtype? (\code{\link{icmp}},
#'   \code{\link{ivtp}})
#' @export
cmp_avt <- function(x) {icmp(x) & ivtp(x)}

#' @describeIn ttt_ccc_uj Is \code{x} an empty array? (\code{\link{iemp}},
#'   \code{\link{iarr}})
#' @export
emp_arr <- function(x) {iemp(x) & iarr(x)}

#' @describeIn ttt_ccc_uj Is \code{x} an empty generic? (\code{\link{iemp}},
#'   \code{\link{igen}})
#' @export
emp_gen <- function(x) {iemp(x) & igen(x)}

#' @describeIn ttt_ccc_uj Is \code{x} an empty matrix? (\code{\link{iemp}},
#'   \code{\link{imat}})
#' @export
emp_mat <- function(x) {iemp(x) & imat(x)}

#' @describeIn ttt_ccc_uj Is \code{x} an empty multivec? (\code{\link{iemp}},
#'   \code{\link{imvc}})
#' @export
emp_mvc <- function(x) {iemp(x) & imvc(x)}

#' @describeIn ttt_ccc_uj Is \code{x} an empty scalar? (\code{\link{iemp}},
#'   \code{\link{iscl}})
#' @export
emp_scl <- function(x) {iemp(x) & iscl(x)}

#' @describeIn ttt_ccc_uj Is \code{x} an empty tabular? (\code{\link{iemp}},
#'   \code{\link{itab}})
#' @export
emp_atb <- function(x) {iemp(x) & itab(x)}

#' @describeIn ttt_ccc_uj Is \code{x} an empty vector? (\code{\link{iemp}},
#'   \code{\link{ivec}})
#' @export
emp_vec <- function(x) {iemp(x) & ivec(x)}

#' @describeIn ttt_ccc_uj Is \code{x} an empty vlist? (\code{\link{iemp}},
#'   \code{\link{ivls}})
#' @export
emp_vls <- function(x) {iemp(x) & ivls(x)}

#' @describeIn ttt_ccc_uj Is \code{x} an empty vtype? (\code{\link{iemp}},
#'   \code{\link{ivtp}})
#' @export
emp_avt <- function(x) {iemp(x) & ivtp(x)}

#' @describeIn ttt_ccc_uj Is \code{x} a recursive array? (\code{\link{ircr}},
#'   \code{\link{iarr}})
#' @export
rcr_arr <- function(x) {ircr(x) & is_arr(x)}

#' @describeIn ttt_ccc_uj Is \code{x} a recursive generic? (\code{\link{ircr}},
#'   \code{\link{igen}})
#' @export
rcr_gen <- function(x) {ircr(x) & is_gen(x)}

#' @describeIn ttt_ccc_uj Is \code{x} a recursive matrix? (\code{\link{ircr}},
#'   \code{\link{imat}})
#' @export
rcr_mat <- function(x) {ircr(x) & is_mat(x)}

#' @describeIn ttt_ccc_uj Is \code{x} a recursive multivec? (\code{\link{ircr}},
#'   \code{\link{imvc}})
#' @export
rcr_mvc <- function(x) {ircr(x) & is_mvc(x)}

#' @describeIn ttt_ccc_uj Is \code{x} a recursive scalar? (\code{\link{ircr}},
#'   \code{\link{iscl}})
#' @export
rcr_scl <- function(x) {ircr(x) & is_scl(x)}

#' @describeIn ttt_ccc_uj Is \code{x} a recursive tabular? (\code{\link{ircr}},
#'   \code{\link{itab}})
#' @export
rcr_tab <- function(x) {ircr(x) & is_tab(x)}

#' @describeIn ttt_ccc_uj Is \code{x} a recursive vector? (\code{\link{ircr}},
#'   \code{\link{ivec}})
#' @export
rcr_vec <- function(x) {ircr(x) & is_vec(x)}

#' @describeIn ttt_ccc_uj Is \code{x} a recursive vlist? (\code{\link{ircr}},
#'   \code{\link{ivls}})
#' @export
rcr_vls <- function(x) {ircr(x) & is_vls(x)}

#' @describeIn ttt_ccc_uj Is \code{x} a recursive vtype? (\code{\link{ircr}},
#'   \code{\link{ivtp}})
#' @export
rcr_vtp <- function(x) {ircr(x) & is_vtp(x)}
