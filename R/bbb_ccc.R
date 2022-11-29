#' @family props
#' @title Base + extended class properties
#' @description \itemize{
#'   \item **`bbb_ccc`**: evaluates whether `x` matches the base property specified in the argument `ccc` and matches the extended class specified in the argument `ccc` subject to any restrictions in `...`.
#'   \item **`xxx_yyy`**: evaluates whether `x` matches base property `xxx` and extended class `yyy` subject to any restrictions in `...` where `xxx` and `yyy` are placeholders for any given base property and any given extended class property, respectively.
#'   \item **`bbb_ccc_props`**: gets a character vector of all possible base + extended class properties.
#' }
#' @param x An R object
#' @param bbb A character scalar containing a base property from `bbb_props()`.
#' @param ccc A character scalar containing an extended class property from `ccc_props()`.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return \itemize{
#'   \item **`ccc_mmm_props`**: a character vector.
#'   \item **`ccc_mmm, xxx_yyy`**: a logical scalar.
#' }
#' @export
bbb_ccc <- function(x, bbb, ccc, ...) {
  BBB <- c("atm", "nil", "pop", "rcr")
  errs <- c(.meets_errs(x, ...),
            f0(f0(length(bbb) != 1 | !is.character(bbb), F, f0(is.na(bbb), F, bbb %in% BBB  )), NULL, "\n \u2022 [bbb] is not a scalar value from c('atm', 'nil', 'pop', 'rcr'."),
            f0(f0(length(ccc) != 1 | !is.character(ccc), F, f0(is.na(ccc), F, ccc %in% .cccs)), NULL, '\n \u2022 [ccc] is not a scalar value from ccc_props().'))
  if (!is.null(errs)) {stop(errs)}
  else if (!meets(x, ...)) {F}
  else if (!run(paste0('.i', ccc, '(x)'))) {F}
  else if (bbb == "nil") {length(x) == 0}
  else if (length(x) == 0) {F}
  else if (bbb == "pop") {T}
  else if (bbb == "atm") {
    if (ccc == "dtf") {all(apply(x, 2, is.atomic))}
    else if (ccc == "vls") {if (all(lengths(x) > 0)) {all(sapply(x, is.atomic))}}
    else {is.atomic(x)}
  } else if (bbb == "rcr") {
    if (ccc == "dtf") {any(apply(x, 2, is.recursive))}
    else if (ccc == "vls") {any(sapply(x, is.recursive))}
    else {is.recursive(x)}
  }
}

#' @rdname bbb_ccc
#' @export
bbb_ccc_props <- function() {sort(av(apply(expand.grid(bbb = c("atm", "nil", "pop", "rcr"), ccc = .cccs), 1, paste0, collapse = '_')))}


#' @rdname bbb_ccc
#' @export
atm_arr <- function(x, ...) {bbb_ccc(x, 'atm', 'arr', ...)}

#' @rdname bbb_ccc
#' @export
atm_dtf <- function(x, ...) {bbb_ccc(x, 'atm', 'dtf', ...)}

#' @rdname bbb_ccc
#' @export
atm_gen <- function(x, ...) {bbb_ccc(x, 'atm', 'gen', ...)}

#' @rdname bbb_ccc
#' @export
atm_mat <- function(x, ...) {bbb_ccc(x, 'atm', 'mat', ...)}

#' @rdname bbb_ccc
#' @export
atm_mvc <- function(x, ...) {bbb_ccc(x, 'atm', 'mvc', ...)}

#' @rdname bbb_ccc
#' @export
atm_scl <- function(x, ...) {bbb_ccc(x, 'atm', 'scl', ...)}

#' @rdname bbb_ccc
#' @export
atm_vec <- function(x, ...) {bbb_ccc(x, 'atm', 'vec', ...)}

#' @rdname bbb_ccc
#' @export
atm_vls <- function(x, ...) {bbb_ccc(x, 'atm', 'vls', ...)}

#' @rdname bbb_ccc
#' @export
nil_arr <- function(x, ...) {bbb_ccc(x, 'nil', 'arr', ...)}

#' @rdname bbb_ccc
#' @export
nil_dtf <- function(x, ...) {bbb_ccc(x, 'nil', 'dtf', ...)}

#' @rdname bbb_ccc
#' @export
nil_gen <- function(x, ...) {bbb_ccc(x, 'nil', 'gen', ...)}

#' @rdname bbb_ccc
#' @export
nil_mat <- function(x, ...) {bbb_ccc(x, 'nil', 'mat', ...)}

#' @rdname bbb_ccc
#' @export
nil_mvc <- function(x, ...) {bbb_ccc(x, 'nil', 'mvc', ...)}

#' @rdname bbb_ccc
#' @export
nil_scl <- function(x, ...) {bbb_ccc(x, 'nil', 'scl', ...)}

#' @rdname bbb_ccc
#' @export
nil_vec <- function(x, ...) {bbb_ccc(x, 'nil', 'vec', ...)}

#' @rdname bbb_ccc
#' @export
nil_vls <- function(x, ...) {bbb_ccc(x, 'nil', 'vls', ...)}

#' @rdname bbb_ccc
#' @export
pop_arr <- function(x, ...) {bbb_ccc(x, 'pop', 'arr', ...)}

#' @rdname bbb_ccc
#' @export
pop_dtf <- function(x, ...) {bbb_ccc(x, 'pop', 'dtf', ...)}

#' @rdname bbb_ccc
#' @export
pop_gen <- function(x, ...) {bbb_ccc(x, 'pop', 'gen', ...)}

#' @rdname bbb_ccc
#' @export
pop_mat <- function(x, ...) {bbb_ccc(x, 'pop', 'mat', ...)}

#' @rdname bbb_ccc
#' @export
pop_mvc <- function(x, ...) {bbb_ccc(x, 'pop', 'mvc', ...)}

#' @rdname bbb_ccc
#' @export
pop_scl <- function(x, ...) {bbb_ccc(x, 'pop', 'scl', ...)}

#' @rdname bbb_ccc
#' @export
pop_vec <- function(x, ...) {bbb_ccc(x, 'pop', 'vec', ...)}

#' @rdname bbb_ccc
#' @export
pop_vls <- function(x, ...) {bbb_ccc(x, 'pop', 'vls', ...)}

#' @rdname bbb_ccc
#' @export
rcr_arr <- function(x, ...) {bbb_ccc(x, 'rcr', 'arr', ...)}

#' @rdname bbb_ccc
#' @export
rcr_dtf <- function(x, ...) {bbb_ccc(x, 'rcr', 'dtf', ...)}

#' @rdname bbb_ccc
#' @export
rcr_gen <- function(x, ...) {bbb_ccc(x, 'rcr', 'gen', ...)}

#' @rdname bbb_ccc
#' @export
rcr_mat <- function(x, ...) {bbb_ccc(x, 'rcr', 'mat', ...)}

#' @rdname bbb_ccc
#' @export
rcr_mvc <- function(x, ...) {bbb_ccc(x, 'rcr', 'mvc', ...)}

#' @rdname bbb_ccc
#' @export
rcr_scl <- function(x, ...) {bbb_ccc(x, 'rcr', 'scl', ...)}

#' @rdname bbb_ccc
#' @export
rcr_vec <- function(x, ...) {bbb_ccc(x, 'rcr', 'vec', ...)}

#' @rdname bbb_ccc
#' @export
rcr_vls <- function(x, ...) {bbb_ccc(x, 'rcr', 'vls', ...)}