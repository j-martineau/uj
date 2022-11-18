#' @name ttt_ccc.
#' @family props
#' @title Fundamental Type + Extended Class Properties
#' @description \tabular{ll}{
#'   \code{ttt_ccc_vals}   \tab Gets a character vector of all possible
#'                              \link[=ttt]{fundamental type} +
#'                              \link[=ccc]{extended class} properties.      \cr
#'   \code{[ttt]_[ccc]}    \tab Evaluates whether \code{x} is of the
#'                              \link[=ttt]{fundamental type} represented by the
#'                              placeholder \code{[ttt]} and of the
#'                              \link[=ccc]{extended class} represented by the
#'                              placeholder \code{[ccc]}.                      }
#' @param x An R object.
#' @return \tabular{ll}{
#'   \code{ttt_ccc_vals}   \tab A character vector.                          \cr
#'   \code{[ttt]_[ccc]}    \tab A logical scalar.                              }
#' @export
ttt_ccc. <- function() {help("ttt_ccc.", package = "uj")}

#' @rdname ttt_ccc.
#' @export
ttt_ccc_vals <- function() {
  join <- function(x) {paste0(av(x), collapse = "_")}
  sort(av(apply(expand.grid(ttt = ttt_vals(), ccc = ccc_vals()), 1, join)))
}

# atm_ccc ####

#' @rdname ttt_ccc.
#' @export
atm_arr <- function(x, mmm = NULL) {
  vmmm <- ifelse(is.null(mmm), T, isIN(mmm, mmm_vals()))
  if (!vmmm) {stop("\n \u2022 [mmm] must be NULL or a character scalar value from mmm_vals().")}
  if (!pop_arr(x)) {F} else if (!is.atomic(x)) {F} else if (is.null(mmm)) {T} else {run("i", mmm, "(x)")}
}

#' @rdname ttt_ccc.
#' @export
atm_gen <- function(x, mmm = NULL) {
  vmmm <- ifelse(is.null(mmm), T, isIN(mmm, mmm_vals()))
  if (!vmmm) {stop("\n \u2022 [mmm] must be NULL or a character scalar value from mmm_vals().")}
  if (!pop_gen(x)) {F} else if (!is.atomic(x)) {F} else if (is.null(mmm)) {T} else {run("i", mmm, "(x)")}
}

#' @rdname ttt_ccc.
#' @export
atm_mat <- function(x, mmm = NULL) {
  vmmm <- ifelse(is.null(mmm), T, isIN(mmm, mmm_vals()))
  if (!vmmm) {stop("\n \u2022 [mmm] must be NULL or a character scalar value from mmm_vals().")}
  if (!imat(x)) {F} else if (!is.atomic(x)) {F} else if (is.null(mmm)) {T} else {run("i", mmm, "(x)")}
}

#' @rdname ttt_ccc.
#' @export
atm_mvc <- function(x, mmm = NULL) {
  vmmm <- ifelse(is.null(mmm), T, isIN(mmm, mmm_vals()))
  if (!vmmm) {stop("\n \u2022 [mmm] must be NULL or a character scalar value from mmm_vals().")}
  if (!imvc(x)) {F} else if (!is.atomic(x)) {F} else if (is.null(mmm)) {T} else {run("i", mmm, "(x)")}
}

#' @rdname ttt_ccc.
#' @export
atm_scl <- function(x, mmm = NULL) {
  vmmm <- ifelse(is.null(mmm), T, isIN(mmm, mmm_vals()))
  if (!vmmm) {stop("\n \u2022 [mmm] must be NULL or a character scalar value from mmm_vals().")}
  if (!iscl(x)) {F} else if (!is.atomic(x)) {F} else if (is.null(mmm)) {T} else {run("i", mmm, "(x)")}
}

#' @rdname ttt_ccc.
#' @export
atm_dtf <- function(x, mmm = NULL) {
  vmmm <- ifelse(is.null(mmm), T, isIN(mmm, mmm_vals()))
  if (!vmmm) {stop("\n \u2022 [mmm] must be NULL or a character scalar value from mmm_vals().")}
  if (nrow(x) * ncol(x) == 0) {F} else if (!all(apply(x, 2, is.atomic))) {F} else if (is.null(mmm)) {T} else {
    code <- paste0("i", mmm, "(x[ , ", 1:length(x), "])")
    code <- paste0(code, collapse = ", ")
    code <- paste0("all(", code, ")")
    run(code)
}}

#' @rdname ttt_ccc.
#' @export
atm_vec <- function(x, mmm = NULL) {
  vmmm <- ifelse(is.null(mmm), T, isIN(mmm, mmm_vals()))
  if (!vmmm) {stop("\n \u2022 [mmm] must be NULL or a character scalar value from mmm_vals().")}
  if (!ivec(x)) {F} else if (!is.atomic(x)) {F} else if (is.null(mmm)) {T} else {run("i", mmm, "(x)")}
}

#' @rdname ttt_ccc.
#' @export
atm_vls <- function(x, mmm = NULL) {
  vmmm <- ifelse(is.null(mmm), T, isIN(mmm, mmm_vals()))
  if (!vmmm) {stop("\n \u2022 [mmm] must be NULL or a character scalar value from mmm_vals().")}
  if (!pop_vls(x)) {F} else if (!all(sapply(x, is.atomic))) {F} else if (is.null(mmm)) {T} else {
    out <- paste0("i", mmm, "(x[[", 1:length(x), "]])")
    out <- paste0(out, collapse = ", ")
    run(paste0("all(", out, ")"))
}}

# pop_ccc ####

#' @rdname ttt_ccc.
#' @export
pop_arr <- function(x, mmm = NULL) {length(x) > 0 & f0(inll(mmm), iarr(x), atm_arr(x, mmm))}

#' @rdname ttt_ccc.
#' @export
pop_gen <- function(x, mmm = NULL) {length(x) > 0 & f0(inll(mmm), igen(x), atm_gen(x, mmm))}

#' @rdname ttt_ccc.
#' @export
pop_mat <- function(x, mmm = NULL) {length(x) > 0 & f0(inll(mmm), imat(x), atm_mat(x, mmm))}

#' @rdname ttt_ccc.
#' @export
pop_mvc <- function(x, mmm = NULL) {length(x) > 0 & f0(inll(mmm), imvc(x), atm_mvc(x, mmm))}

#' @rdname ttt_ccc.
#' @export
pop_scl <- function(x, mmm = NULL) {length(x) > 0 & f0(inll(mmm), iscl(x), atm_scl(x, mmm))}

#' @rdname ttt_ccc.
#' @export
pop_dtf <- function(x, mmm = NULL) {length(x) > 0 & f0(inll(mmm), idtf(x), atm_dtf(x, mmm))}

#' @rdname ttt_ccc.
#' @export
pop_vec <- function(x, mmm = NULL) {length(x) > 0 & f0(inll(mmm), ivec(x), atm_vec(x, mmm))}

#' @rdname ttt_ccc.
#' @export
pop_vls <- function(x, mmm = NULL) {length(x) > 0 & f0(inll(mmm), ivls(x), atm_vls(x, mmm))}

# cmp_ccc ####

#' @rdname ttt_ccc.
#' @export
cmp_arr <- function(x) {icmp(x) & iarr(x)}

#' @rdname ttt_ccc.
#' @export
cmp_gen <- function(x) {icmp(x) & igen(x)}

#' @rdname ttt_ccc.
#' @export
cmp_mat <- function(x) {icmp(x) & imat(x)}

#' @rdname ttt_ccc.
#' @export
cmp_mvc <- function(x) {icmp(x) & imvc(x)}

#' @rdname ttt_ccc.
#' @export
cmp_scl <- function(x) {icmp(x) & iscl(x)}

#' @rdname ttt_ccc.
#' @export
cmp_dtf <- function(x) {icmp(x) & idtf(x)}

#' @rdname ttt_ccc.
#' @export
cmp_vec <- function(x) {icmp(x) & ivec(x)}

#' @rdname ttt_ccc.
#' @export
cmp_vls <- function(x) {icmp(x) & ivls(x)}

# emp_ccc ####

#' @rdname ttt_ccc.
#' @export
emp_arr <- function(x) {iemp(x) & iarr(x)}

#' @rdname ttt_ccc.
#' @export
emp_gen <- function(x) {iemp(x) & igen(x)}

#' @rdname ttt_ccc.
#' @export
emp_mat <- function(x) {iemp(x) & imat(x)}

#' @rdname ttt_ccc.
#' @export
emp_mvc <- function(x) {iemp(x) & imvc(x)}

#' @rdname ttt_ccc.
#' @export
emp_scl <- function(x) {iemp(x) & iscl(x)}

#' @rdname ttt_ccc.
#' @export
emp_dtf <- function(x) {iemp(x) & idtf(x)}

#' @rdname ttt_ccc.
#' @export
emp_vec <- function(x) {iemp(x) & ivec(x)}

#' @rdname ttt_ccc.
#' @export
emp_vls <- function(x) {iemp(x) & ivls(x)}

# rcr_ccc ####

#' @rdname ttt_ccc.
#' @export
rcr_arr <- function(x) {ircr(x) & iarr(x)}

#' @rdname ttt_ccc.
#' @export
rcr_gen <- function(x) {ircr(x) & igen(x)}

#' @rdname ttt_ccc.
#' @export
rcr_mat <- function(x) {ircr(x) & imat(x)}

#' @rdname ttt_ccc.
#' @export
rcr_mvc <- function(x) {ircr(x) & imvc(x)}

#' @rdname ttt_ccc.
#' @export
rcr_scl <- function(x) {ircr(x) & iscl(x)}

#' @rdname ttt_ccc.
#' @export
rcr_dtf <- function(x) {ircr(x) & idtf(x)}

#' @rdname ttt_ccc.
#' @export
rcr_vec <- function(x) {ircr(x) & ivec(x)}

#' @rdname ttt_ccc.
#' @export
rcr_vls <- function(x) {ircr(x) & ivls(x)}
