#' @name ttt_ccc
#' @title Check whether \code{x} of a certain \link[=ccc]{extended class} + a
#'   certain \link[=ttt]{fundamental type}?
#' @param x An object.
#' @param xxx Optional character scalar extended mode (see
#'   \code{\link{mmm_vals}}).
#' @return Logical scalar.
#' @export
ttt_ccc <- NULL

#' @describeIn ttt_ccc Get a character vector of all possible fundamental
#'   type plus extended class properties.
#' @export
ttt_ccc_vals <- function() {av(apply(expand.grid(ttt = c("any", "atm", "cmp", "emp", "pop", "rcr"), ccc = c("arr", "gen", "mat", "mvc", "scl", "tib", "vec", "vls", "vtp")), 2, dw0))}

# any_ccc ####

#' @describeIn ttt_ccc Is \code{x} an array of any type?
#' @export
any_arr <- function(x) {is.array(x)}

#' @describeIn ttt_ccc Is \code{x} any type of generic?
#' @export
any_gen <- function(x) {if (is.vector(x) | is.array(x)) {T} else {is.data.frame(x)}}

#' @describeIn ttt_ccc Is \code{x} a matrix of any type?
#' @export
any_mat <- function(x) {is.matrix(x)}

#' @describeIn ttt_ccc Is \code{x} an mvect of any type?
#' @export
any_mvc <- function(x) {if (length(x) < 2) {F} else if (is.vector(x)) {T} else if (!is.array(x)) {F} else {length(which(dim(x) > 1) == 1)}}

#' @describeIn ttt_ccc Is \code{x} is any type of scalar?
#' @export
any_scl <- function(x) {if (length(x) != 1) {F} else {is.vector(x) | is.array(x)}}

#' @describeIn ttt_ccc Is \code{x} any type of tibble?
#' @export
any_tbl <- function(x) {is.data.frame(x)}

#' @describeIn ttt_ccc Is \code{x} any type of vec?
#' @export
any_vec <- function(x) {if (length(x) == 0) {F} else if (is.vector(x)) {T} else if (!is.array(x)) {F} else {length(which(dim(x) > 1)) <= 1}}

#' @describeIn ttt_ccc Is \code{x} any type of vlist?
#' @export
any_vls <- function(x) {if (is.data.frame(x)) {F} else {is.list(x)}}

#' @describeIn ttt_ccc Is \code{x} any type of vtype?
#' @export
any_vtp <- function(x) {if (is.data.frame(x)) {F} else if (length(x) < 2) {T} else {ipop(x) & ie1D(x)}}

# atm_ccc ####

#' @describeIn ttt_ccc Is \code{x} an atomic array?
#' @export
atm_arr <- function(x, xxx = NULL) {
  vxxx <- ifelse(is.null(xxx), T, isIN(xxx, mmm_vals()))
  if (!vxxx) {stop("\n • [xxx] must be NULL or a character scalar value from mmm_vals().")}
  if (!pop_arr(x)) {F}
  else if (!is.atomic(x)) {F}
  else if (is.null(xxx)) {T}
  else {run("i", xxx, "(x)")}
}

#' @describeIn ttt_ccc Is \code{x} an atomic generic?
#' @export
atm_gen <- function(x, xxx = NULL) {
  vxxx <- ifelse(is.null(xxx), T, isIN(xxx, mmm_vals()))
  if (!vxxx) {stop("\n • [xxx] must be NULL or a character scalar value from mmm_vals().")}
  if (!pop_gen(x)) {F}
  else if (!is.atomic(x)) {F}
  else if (is.null(xxx)) {T}
  else {run("i", xxx, "(x)")}
}

#' @describeIn ttt_ccc Is \code{x} an atomic matrix?
#' @export
atm_mat <- function(x, xxx = NULL) {
  vxxx <- ifelse(is.null(xxx), T, isIN(xxx, mmm_vals()))
  if (!vxxx) {stop("\n • [xxx] must be NULL or a character scalar value from mmm_vals().")}
  if (!any_mat(x)) {F}
  else if (!is.atomic(x)) {F}
  else if (is.null(xxx)) {T}
  else {run("i", xxx, "(x)")}
}

#' @describeIn ttt_ccc Is \code{x} an atomic mvect?
#' @export
atm_mvc <- function(x, xxx = NULL) {
  vxxx <- ifelse(is.null(xxx), T, isIN(xxx, mmm_vals()))
  if (!vxxx) {stop("\n • [xxx] must be NULL or a character scalar value from mmm_vals().")}
  if (!any_mvc(x)) {F}
  else if (!is.atomic(x)) {F}
  else if (is.null(mmm)) {T}
  else {run("i", mmm, "(x)")}
}

#' @describeIn ttt_ccc Is \code{x} is an atomic scalar?
#' @export
atm_scl <- function(x, xxx = NULL) {
  vxxx <- ifelse(is.null(xxx), T, isIN(xxx, mmm_vals()))
  if (!vxxx) {stop("\n • [xxx] must be NULL or a character scalar value from mmm_vals().")}
  if (!any_scl(x)) {F}
  else if (!is.atomic(x)) {F}
  else if (is.null(xxx)) {T}
  else {run("i", xxx, "(x)")}
}

#' @describeIn ttt_ccc Is \code{x} an atomic tibble?
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

#' @describeIn ttt_ccc Is \code{x} an atomic vec?
#' @export
atm_vec <- function(x, xxx = NULL) {
  vxxx <- ifelse(is.null(xxx), T, isIN(xxx, mmm_vals()))
  if (!vxxx) {stop("\n • [xxx] must be NULL or a character scalar value from mmm_vals().")}
  if (!any_vec(x)) {F}
  else if (!is.atomic(x)) {F}
  else if (is.null(xxx)) {T}
  else {run("i", xxx, "(x)")}
}

#' @describeIn ttt_ccc Is \code{x} an atomic vlist?
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

#' @describeIn ttt_ccc Is \code{x} an atomic \code{vtype}?
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

#' @describeIn ttt_ccc Is \code{x} a populated array?
#' @export
pop_arr <- function(x, xxx = NULL) {length(x) > 0 & f0.(inll(xxx), any_arr(x), atm_arr(x, xxx))}

#' @describeIn ttt_ccc Is \code{x} a populated generic?
#' @export
pop_gen <- function(x, xxx = NULL) {length(x) > 0 & f0.(inll(xxx), any_gen(x), atm_gen(x, xxx))}

#' @describeIn ttt_ccc Is \code{x} a populated generic?
#' @export
pop_mat <- function(x, xxx = NULL) {length(x) > 0 & f0.(inll(xxx), any_mat(x), atm_mat(x, xxx))}

#' @describeIn ttt_ccc Is \code{x} a populated generic?
#' @export
pop_mvc <- function(x, xxx = NULL) {length(x) > 0 & f0.(inll(xxx), any_mvc(x), atm_mvc(x, xxx))}

#' @describeIn ttt_ccc Is \code{x} a populated generic?
#' @export
pop_scl <- function(x, xxx = NULL) {length(x) > 0 & f0.(inll(xxx), any_scl(x), atm_scl(x, xxx))}

#' @describeIn ttt_ccc Is \code{x} a populated tibble?
#' @export
pop_tbl <- function(x, xxx = NULL) {length(x) > 0 & f0.(inll(xxx), any_tbl(x), atm_tbl(x, xxx))}

#' @describeIn ttt_ccc Is \code{x} a populated tibble?
#' @export
pop_vec <- function(x, xxx = NULL) {length(x) > 0 & f0.(inll(xxx), any_vec(x), atm_vec(x, xxx))}

#' @describeIn ttt_ccc Is \code{x} a populated vlist?
#' @export
pop_vls <- function(x, xxx = NULL) {length(x) > 0 & f0.(inll(xxx), any_vls(x), atm_vls(x, xxx))}

#' @describeIn ttt_ccc Is \code{x} a populated \code{vtype}?
#' @export
pop_vtp <- function(x, xxx = NULL) {length(x) > 0 & f0.(inll(xxx), any_vtp(x), atm_vtp(x, xxx))}

# cmp_ccc ####

#' @describeIn ttt_ccc Is \code{x} a complete array?
#' @export
cmp_arr <- function(x, xxx = NULL) {f0.(pop_arr(x, xxx), !any(is.na(av(x))), F)}

#' @describeIn ttt_ccc Is \code{x} a complete generic?
#' @export
cmp_gen <- function(x, xxx = NULL) {f0.(pop_gen(x, xxx), !any(is.na(av(x))), F)}

#' @describeIn ttt_ccc Is \code{x} a complete atomic matrix?
#' @export
cmp_mat <- function(x, xxx = NULL) {f0.(pop_mat(x, xxx), !any(is.na(av(x))), F)}

#' @describeIn ttt_ccc Is \code{x} a complete atomic mvect?
#' @export
cmp_mvc <- function(x, xxx = NULL) {f0.(pop_mvc(x, xxx), !any(is.na(av(x))), F)}

#' @describeIn ttt_ccc Is \code{x} is an complete atomic scalar?
#' @export
cmp_scl <- function(x, xxx = NULL) {f0.(pop_scl(x, xxx), !any(is.na(av(x))), F)}

#' @describeIn ttt_ccc Is \code{x} a complete atomic tibble?
#' @export
cmp_tbl <- function(x, xxx = NULL) {f0.(pop_tbl(x, xxx), !any(is.na(av(x))), F)}

#' @describeIn ttt_ccc Is \code{x} a complete atomic vec?
#' @export
cmp_vec <- function(x, xxx = NULL) {f0.(pop_vec(x, xxx), !any(is.na(av(x))), F)}

#' @describeIn ttt_ccc Is \code{x} a complete atomic \code{vtype}?
#' @export
cmp_vtp <- function(x, xxx = NULL) {f0.(pop_vtp(x, xxx), !any(is.na(av(x))), F)}

#' @describeIn ttt_ccc Is \code{x} a complete vlist?
#' @export
cmp_vls <- function(x, xxx = NULL) {f0.(pop_vls(x, xxx), !any(is.na(av(x))), F)}

# emp_ccc ####

#' @describeIn ttt_ccc Is \code{x} an empty array?
#' @export
emp_arr <- function(x) {any_arr(x) & length(x) == 0}

#' @describeIn ttt_ccc Is \code{x} an empty generic?
#' @export
emp_gen <- function(x) {any_gen(x) & length(x) > 0}

#' @describeIn ttt_ccc Is \code{x} an empty generic?
#' @export
emp_mat <- function(x) {any_mat(x) & length(x) > 0}

#' @describeIn ttt_ccc Is \code{x} an empty generic?
#' @export
emp_mvc <- function(x) {F}

#' @describeIn ttt_ccc Is \code{x} an empty generic?
#' @export
emp_scl <- function(x) {F}

#' @describeIn ttt_ccc Is \code{x} an empty tibble?
#' @export
emp_tbl <- function(x) {tibble::is_tibble(x) & length(x) == 0}

#' @describeIn ttt_ccc Is \code{x} an empty generic?
#' @export
emp_vec <- function(x) {F}

#' @describeIn ttt_ccc Is \code{x} an empty vlist?
#' @export
emp_vls <- function(x) {any_vls(x) & length(x) == 0}

#' @describeIn ttt_ccc Is \code{x} an empty \code{vtype}?
#' @export
emp_vtp <- function(x) {any_vtp(x) & length(x) == 0}

# rcr_ccc ####

#' @describeIn ttt_ccc Is \code{x} a recursive array?
#' @export
rcr_arr <- function(x) {pop_arr(x) & is.recursive(x)}

#' @describeIn ttt_ccc Is \code{x} a recursive generic?
#' @export
rcr_gen <- function(x)  {pop_gen(x) & is.recursive(x)}

#' @describeIn ttt_ccc Is \code{x} a recursive matrix?
#' @export
rcr_mat <- function(x) {pop_mat(x) & is.recursive(x)}

#' @describeIn ttt_ccc Is \code{x} a recursive mvect?
#' @export
rcr_mvc <- function(x) {pop_mvc(x) & is.recursive(x)}

#' @describeIn ttt_ccc Is \code{x} is a recursive scalar?
#' @export
rcr_scl <- function(x) {pop_scl(x) & is.recursive(x)}

#' @describeIn ttt_ccc Is \code{x} a recursive tibble?
#' @export
rcr_tbl <- function(x) {pop_tbl(x) & any(apply(x, 2, is.recursive))}

#' @describeIn ttt_ccc Is \code{x} a recursive vec?
#' @export
rcr_vec <- function(x) {pop_vec(x) & is.recursive(x)}

#' @describeIn ttt_ccc Is \code{x} a recursive vlist?
#' @export
rcr_vls <- function(x) {pop_vls(x) & is.recursive(x)}

#' @describeIn ttt_ccc Is \code{x} a recursive \code{vtype}?
#' @export
rcr_vtp <- function(x) {pop_vtp(x) & is.recursive(x)}
