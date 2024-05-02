# internals ####

.delim_errs <- function(args, a) {
  ns <- uj::ns(args$dots)
  two <- "D" %in% base::names(args)
  dots <- N > 0
  pops <- uj::f0(!dots, T, base::all(ns > 0))
  ccsd <- ppp::.cmp_chr_scl(args$d)
  CCSD <- uj::f0(!two, T, ppp::.cmp_chr_scl(args$D))
  atms <- uj::f0(!CCSD, T, base::all(base::sapply(args$dots, ppp::atm_vec)))
  recs <- uj::f0(a | !pops, T, uj::recyclable_ns(base::max(ns) / ns))
  errs <- NULL
  if (!dots) {errs <- base::c(errs, "[...] is empty.")}
  if (!pops) {errs <- base::c(errs, "An argument in [...] is of length 0.")}
  if (!ccsd) {errs <- base::c(errs, "[d] must be a complete character scalar (?cmp_chr_scl).")}
  if (!CCSD) {errs <- base::c(errs, "[D] must be a complete character scalar (?cmp_chr_scl).")}
  if (!atms) {errs <- base::c(errs, "Arguments in [...] must be atomic vecs (?pop_vec).")}
  if (!recs) {errs <- base::c(errs, "Arguments in [...] are not of recyclable lengths (?recyclable).")}
  errs
}

# externals ####

#' @name delim
#' @encoding UTF-8
#' @family strings
#' @title Error-checked string delimiting
#' @description Simplified and extended `base::paste` and `base::paste0`. There are both primary functions using user-specified delimiters and convenience functions for common delimiters.
#' @details **Primary functions**
#' \tabular{ll}{  `dww`   \tab Delimit elements within each (atomic vector) `...` argument using delimiter `d`, then delimit elements within the resulting vector using delimiter
#'                             `D`. Produces a character scalar of `...length()` substrings delimited by `D` where each substring contains sub-substrings delimited by `d`.                \cr   \tab   \cr
#'                `daw`   \tab Delimit across corresponding elements of (recyclable atomic vector) `...` arguments using delimiter `d`, then delimit elements within the resulting
#'                             character vector using delimiter `D`. Produces a character scalar of `...length()` substrings delimited by `D` where each substring contains
#'                             `max(lengths(list(...)))` sub-substrings delimited by `d`.                                                                                                  \cr   \tab   \cr
#'                `dw`    \tab Delimits elements within each (atomic vector) `...` argument using delimiter `d`. Produces a character vector of `...length()` substrings delimited by `d`. \cr   \tab   \cr
#'                `da`    \tab Delimits across corresponding elements of (recyclable atomic vector) `...` arguments using delimiter `d`. Produces a character vector of
#'                             `max(lengths(list(...)))` substrings delimited by `d`.                                                                                                                     }
#' \cr **Common-delimiter convenience functions**
#' \cr\cr Convenience function names are constructed by append `1` or `2` codes for common delimiters to the function name as follows (where `{x}` and `{y}` are placeholders for common-delimiter codes):
#' \tabular{ll}{  `daw_{x}{y}`   \tab Delimits across using `{x}` then within using `{y}`. \cr   \tab   \cr
#'                `dww_{x}{y}`   \tab Delimits within using `{x}` then again using `{y}`.  \cr   \tab   \cr
#'                `da_{x}`       \tab Delimits across using `{x}`.                                     \cr
#'                `dw_{x}`       \tab Delimits within using `{x}`.                                       }
#' \cr Common delimiters are encoded as:
#' \tabular{lll}{  **Code**   \tab **Name**                      \tab   **Delimiter** \cr
#'                 `'0'`      \tab blank                         \tab   `''`          \cr
#'                 `'1'`      \tab space                         \tab   `' '`         \cr
#'                 `'b'`      \tab broken pipe                   \tab   `'¦'`         \cr
#'                 `'c'`      \tab colon                         \tab   `':'`         \cr
#'                 `'d'`      \tab dot                           \tab   `'.'`         \cr
#'                 `'g'`      \tab grammatical comma\eqn{^{(1)}} \tab   `', '`        \cr
#'                 `'p'`      \tab pipe                          \tab   `'|'`         \cr
#'                 `'q'`      \tab back-tick quote               \tab   `'``'`        \cr
#'                 `'s'`      \tab simple comma\eqn{^{(1)}}      \tab   `','`         \cr
#'                 `'t'`      \tab tilde                         \tab   `'~'`           }
#'   \tabular{l}{  \eqn{^{(1)}} 'Grammatical comma' vs. 'simple comma' indicates whether the function produces grammatical comma-delimited lists vs. simple comma-delimited values (e.g., `'1, 2, 3'` vs. `'1,2,3'`). }
#' @param ... An arbitrary number of atomic vector arguments to be delimited. Argument in `...` must be recyclable for functions that delimit across `...` arguments as the first or only step (i.e., functions with names beginning with `da`).
#' @param d,D \link[=chr_scl]{Character scalar} delimiters.
#' @return **A character scalar** \cr\cr `daw_{x}{y}, dww_{x}{y}, daw, dww`
#' \cr\cr  **A character vector** \cr\cr `da_{x}, dw_{x}, da, dw`
#' @examples
#' # delimit across using delimiter '|'.
#' # aliases paste(1:3, 4:6, sep = '|').
#' da('|', 1:3, 4:6)
#'
#' # delimit within using delimiter ':'.
#' # aliases sapply(list(1:3, 4:6), paste0, collapse = ':').
#' dw(':', 1:3, 4:6)
#'
#' # delimit across using '|', then within using ':'
#' # aliases paste(..., sep = d, collapse = D)
#' daw('|', ':', 1:3, 4:6)
#'
#' # delimit within using '|' then again within using ':'.
#' # aliases paste0(sapply(list(...), paste0, collapse = d), collapse = D).
#' dww('|', ':', 1:3, 4:6)
#'
#' # delimit across using pipe (encoded by 'P' suffix in function name).
#' da_p(1:3, 4:6)
#'
#' # delimit within using colon (encoded by 'C' suffix in function name).
#' dw_c(1:3, 4:6)
#'
#' # delimit across using pipe, then within using colon.
#' daw_pc(1:3, 4:6)
#'
#' # delimit within using colon, then again using pipe.
#' dww_cp(1:3, 4:6)
#' @export
delim <- function() {utils::help("delim", package = "uj")}

#' @rdname delim
#' @export
da <- function(d, ...) {
  if (base::...length() == 0) {return("")}
  errs <- uj:::.delim_errs(base::list(d = d, Dots =  base::list(...)), .a = TRUE)
  if (!base::is.null(errs)) {ppp::stopperr(errs, pkg = "uj")}
  base::paste(..., sep = d)
}

#' @rdname delim
#' @export
dw <- function(d, ...) {
  dw0 <- function(x) {base::paste0(uj::av(x), collapse = d)}
  if (base::...length() == 0) {return("")}
  errs <- uj:::.delim_errs(base::list(d = d, Dots =  base::list(...)), .a = TRUE)
  if (!base::is.null(errs)) {ppp::stopperr(errs, pkg = "uj")}
  base::sapply(base::list(...), dw0)
}

#' @rdname delim
#' @export
daw <- function(d, D, ...) {
  if (base::...length() == 0) {return("")}
  errs <- uj:::.delim_errs(base::list(d = d, D = D, Dots =  base::list(...)), .a = TRUE)
  if (!base::is.null(errs)) {ppp::stopperr(errs, pkg = "uj")}
  x <- base::paste0(..., sep = d)
  base::paste0(uj::av(x), collapse = D)
}

#' @rdname delim
#' @export
dww <- function(d, D, ...) {
  dw0 <- function(x) {base::paste0(uj::av(x), collapse = d)}
  if (base::...length() == 0) {return("")}
  errs <- uj:::.delim_errs(base::list(d = d, D = D, Dots =  base::list(...)), .a = TRUE)
  if (!base::is.null(errs)) {ppp::stopperr(errs, pkg = "uj")}
  x <- base::sapply(base::list(...), dw0)
  base::paste0(uj::av(x), collapse = D)
}

#' @rdname delim
#' @export
da_0 <- function(...) {uj::da('', ...)}

#' @rdname delim
#' @export
da_1 <- function(...) {uj::da(' ', ...)}

#' @rdname delim
#' @export
da0 <- da_0

#' @rdname delim
#' @export
da1 <- da_1

#' @rdname delim
#' @export
da_b <- function(...) {uj::da('¦', ...)}

#' @rdname delim
#' @export
da_c <- function(...) {uj::da(':', ...)}

#' @rdname delim
#' @export
da_d <- function(...) {uj::da('.', ...)}

#' @rdname delim
#' @export
da_g <- function(...) {uj::da(', ', ...)}

#' @rdname delim
#' @export
da_p <- function(...) {uj::da('|', ...)}

#' @rdname delim
#' @export
da_q <- function(...) {uj::da('`', ...)}

#' @rdname delim
#' @export
da_s <- function(...) {uj::da(',', ...)}

#' @rdname delim
#' @export
da_t <- function(...) {uj::da('~', ...)}

#' @rdname delim
#' @export
dw_0 <- function(...) {uj::dw('', ...)}

#' @rdname delim
#' @export
dw0 <- dw_0

#' @rdname delim
#' @export
dw_1 <- function(...) {uj::dw(' ', ...)}

#' @rdname delim
#' @export
dw1 <- dw_1

#' @rdname delim
#' @export
dw_b <- function(...) {uj::dw('¦', ...)}

#' @rdname delim
#' @export
dw_c <- function(...) {uj::dw(':', ...)}

#' @rdname delim
#' @export
dw_d <- function(...) {uj::dw('.', ...)}

#' @rdname delim
#' @export
dw_g <- function(...) {uj::dw(', ', ...)}

#' @rdname delim
#' @export
dw_p <- function(...) {uj::dw('|', ...)}

#' @rdname delim
#' @export
dw_q <- function(...) {uj::dw('`', ...)}

#' @rdname delim
#' @export
dw_s <- function(...) {uj::dw(',', ...)}

#' @rdname delim
#' @export
dw_t <- function(...) {uj::dw('~', ...)}

#' @rdname delim
#' @export
daw_00 <- function(...) {uj::daw('', '', ...)}

#' @rdname delim
#' @export
daw_01 <- function(...) {uj::daw('', ' ', ...)}

#' @rdname delim
#' @export
daw00 <- daw_00

#' @rdname delim
#' @export
daw01 <- daw_01

#' @rdname delim
#' @export
daw_0b <- function(...) {uj::daw('', '¦', ...)}

#' @rdname delim
#' @export
daw_0c <- function(...) {uj::daw('', ':', ...)}

#' @rdname delim
#' @export
daw_0d <- function(...) {uj::daw('', '.', ...)}

#' @rdname delim
#' @export
daw_0g <- function(...) {uj::daw('', ', ', ...)}

#' @rdname delim
#' @export
daw_0p <- function(...) {uj::daw('', '|', ...)}

#' @rdname delim
#' @export
daw_0q <- function(...) {uj::daw('', '`', ...)}

#' @rdname delim
#' @export
daw_0s <- function(...) {uj::daw('', ',', ...)}

#' @rdname delim
#' @export
daw_0t <- function(...) {uj::daw('', '~', ...)}

#' @rdname delim
#' @export
daw_10 <- function(...) {uj::daw(' ', '', ...)}

#' @rdname delim
#' @export
daw_11 <- function(...) {uj::daw(' ', ' ', ...)}

#' @rdname delim
#' @export
daw10 <- daw_10

#' @rdname delim
#' @export
daw11 <- daw_11

#' @rdname delim
#' @export
daw_1b <- function(...) {uj::daw(' ', '¦', ...)}

#' @rdname delim
#' @export
daw_1c <- function(...) {uj::daw(' ', ':', ...)}

#' @rdname delim
#' @export
daw_1d <- function(...) {uj::daw(' ', '.', ...)}

#' @rdname delim
#' @export
daw_1g <- function(...) {uj::daw(' ', ', ', ...)}

#' @rdname delim
#' @export
daw_1p <- function(...) {uj::daw(' ', '|', ...)}

#' @rdname delim
#' @export
daw_1q <- function(...) {uj::daw(' ', '`', ...)}

#' @rdname delim
#' @export
daw_1s <- function(...) {uj::daw(' ', ',', ...)}

#' @rdname delim
#' @export
daw_1t <- function(...) {uj::daw(' ', '~', ...)}

#' @rdname delim
#' @export
daw_b0 <- function(...) {uj::daw('¦', '', ...)}

#' @rdname delim
#' @export
daw_b1 <- function(...) {uj::daw('¦', ' ', ...)}

#' @rdname delim
#' @export
daw_bb <- function(...) {uj::daw('¦', '¦', ...)}

#' @rdname delim
#' @export
daw_bc <- function(...) {uj::daw('¦', ':', ...)}

#' @rdname delim
#' @export
daw_bd <- function(...) {uj::daw('¦', '.', ...)}

#' @rdname delim
#' @export
daw_bg <- function(...) {uj::daw('¦', ', ', ...)}

#' @rdname delim
#' @export
daw_bp <- function(...) {uj::daw('¦', '|', ...)}

#' @rdname delim
#' @export
daw_bq <- function(...) {uj::daw('¦', '`', ...)}

#' @rdname delim
#' @export
daw_bs <- function(...) {uj::daw('¦', ',', ...)}

#' @rdname delim
#' @export
daw_bt <- function(...) {uj::daw('¦', '~', ...)}

#' @rdname delim
#' @export
daw_c0 <- function(...) {uj::daw(':', '', ...)}

#' @rdname delim
#' @export
daw_c1 <- function(...) {uj::daw(':', ' ', ...)}

#' @rdname delim
#' @export
daw_cb <- function(...) {uj::daw(':', '¦', ...)}

#' @rdname delim
#' @export
daw_cc <- function(...) {uj::daw(':', ':', ...)}

#' @rdname delim
#' @export
daw_cd <- function(...) {uj::daw(':', '.', ...)}

#' @rdname delim
#' @export
daw_cg <- function(...) {uj::daw(':', ', ', ...)}

#' @rdname delim
#' @export
daw_cp <- function(...) {uj::daw(':', '|', ...)}

#' @rdname delim
#' @export
daw_cq <- function(...) {uj::daw(':', '`', ...)}

#' @rdname delim
#' @export
daw_cs <- function(...) {uj::daw(':', ',', ...)}

#' @rdname delim
#' @export
daw_ct <- function(...) {uj::daw(':', '~', ...)}

#' @rdname delim
#' @export
daw_d0 <- function(...) {uj::daw('.', '', ...)}

#' @rdname delim
#' @export
daw_d1 <- function(...) {uj::daw('.', ' ', ...)}

#' @rdname delim
#' @export
daw_db <- function(...) {uj::daw('.', '¦', ...)}

#' @rdname delim
#' @export
daw_dc <- function(...) {uj::daw('.', ':', ...)}

#' @rdname delim
#' @export
daw_dd <- function(...) {uj::daw('.', '.', ...)}

#' @rdname delim
#' @export
daw_dg <- function(...) {uj::daw('.', ', ', ...)}

#' @rdname delim
#' @export
daw_dp <- function(...) {uj::daw('.', '|', ...)}

#' @rdname delim
#' @export
daw_dq <- function(...) {uj::daw('.', '`', ...)}

#' @rdname delim
#' @export
daw_ds <- function(...) {uj::daw('.', ',', ...)}

#' @rdname delim
#' @export
daw_dt <- function(...) {uj::daw('.', '~', ...)}

#' @rdname delim
#' @export
daw_g0 <- function(...) {uj::daw(', ', '', ...)}

#' @rdname delim
#' @export
daw_g1 <- function(...) {uj::daw(', ', ' ', ...)}

#' @rdname delim
#' @export
daw_gb <- function(...) {uj::daw(', ', '¦', ...)}

#' @rdname delim
#' @export
daw_gc <- function(...) {uj::daw(', ', ':', ...)}

#' @rdname delim
#' @export
daw_gd <- function(...) {uj::daw(', ', '.', ...)}

#' @rdname delim
#' @export
daw_gg <- function(...) {uj::daw(', ', ', ', ...)}

#' @rdname delim
#' @export
daw_gp <- function(...) {uj::daw(', ', '|', ...)}

#' @rdname delim
#' @export
daw_gq <- function(...) {uj::daw(', ', '`', ...)}

#' @rdname delim
#' @export
daw_gs <- function(...) {uj::daw(', ', ',', ...)}

#' @rdname delim
#' @export
daw_gt <- function(...) {uj::daw(', ', '~', ...)}

#' @rdname delim
#' @export
daw_p0 <- function(...) {uj::daw('|', '', ...)}

#' @rdname delim
#' @export
daw_p1 <- function(...) {uj::daw('|', ' ', ...)}

#' @rdname delim
#' @export
daw_pb <- function(...) {uj::daw('|', '¦', ...)}

#' @rdname delim
#' @export
daw_pc <- function(...) {uj::daw('|', ':', ...)}

#' @rdname delim
#' @export
daw_pd <- function(...) {uj::daw('|', '.', ...)}

#' @rdname delim
#' @export
daw_pg <- function(...) {uj::daw('|', ', ', ...)}

#' @rdname delim
#' @export
daw_pp <- function(...) {uj::daw('|', '|', ...)}

#' @rdname delim
#' @export
daw_pq <- function(...) {uj::daw('|', '`', ...)}

#' @rdname delim
#' @export
daw_ps <- function(...) {uj::daw('|', ',', ...)}

#' @rdname delim
#' @export
daw_pt <- function(...) {uj::daw('|', '~', ...)}

#' @rdname delim
#' @export
daw_q0 <- function(...) {uj::daw('`', '', ...)}

#' @rdname delim
#' @export
daw_q1 <- function(...) {uj::daw('`', ' ', ...)}

#' @rdname delim
#' @export
daw_qb <- function(...) {uj::daw('`', '¦', ...)}

#' @rdname delim
#' @export
daw_qc <- function(...) {uj::daw('`', ':', ...)}

#' @rdname delim
#' @export
daw_qd <- function(...) {uj::daw('`', '.', ...)}

#' @rdname delim
#' @export
daw_qg <- function(...) {uj::daw('`', ', ', ...)}

#' @rdname delim
#' @export
daw_qp <- function(...) {uj::daw('`', '|', ...)}

#' @rdname delim
#' @export
daw_qq <- function(...) {uj::daw('`', '`', ...)}

#' @rdname delim
#' @export
daw_qs <- function(...) {uj::daw('`', ',', ...)}

#' @rdname delim
#' @export
daw_qt <- function(...) {uj::daw('`', '~', ...)}

#' @rdname delim
#' @export
daw_s0 <- function(...) {uj::daw(',', '', ...)}

#' @rdname delim
#' @export
daw_s1 <- function(...) {uj::daw(',', ' ', ...)}

#' @rdname delim
#' @export
daw_sb <- function(...) {uj::daw(',', '¦', ...)}

#' @rdname delim
#' @export
daw_sc <- function(...) {uj::daw(',', ':', ...)}

#' @rdname delim
#' @export
daw_sd <- function(...) {uj::daw(',', '.', ...)}

#' @rdname delim
#' @export
daw_sg <- function(...) {uj::daw(',', ', ', ...)}

#' @rdname delim
#' @export
daw_sp <- function(...) {uj::daw(',', '|', ...)}

#' @rdname delim
#' @export
daw_sq <- function(...) {uj::daw(',', '`', ...)}

#' @rdname delim
#' @export
daw_ss <- function(...) {uj::daw(',', ',', ...)}

#' @rdname delim
#' @export
daw_st <- function(...) {uj::daw(',', '~', ...)}

#' @rdname delim
#' @export
daw_t0 <- function(...) {uj::daw('~', '', ...)}

#' @rdname delim
#' @export
daw_t1 <- function(...) {uj::daw('~', ' ', ...)}

#' @rdname delim
#' @export
daw_tb <- function(...) {uj::daw('~', '¦', ...)}

#' @rdname delim
#' @export
daw_tc <- function(...) {uj::daw('~', ':', ...)}

#' @rdname delim
#' @export
daw_td <- function(...) {uj::daw('~', '.', ...)}

#' @rdname delim
#' @export
daw_tg <- function(...) {uj::daw('~', ', ', ...)}

#' @rdname delim
#' @export
daw_tp <- function(...) {uj::daw('~', '|', ...)}

#' @rdname delim
#' @export
daw_tq <- function(...) {uj::daw('~', '`', ...)}

#' @rdname delim
#' @export
daw_ts <- function(...) {uj::daw('~', ',', ...)}

#' @rdname delim
#' @export
daw_tt <- function(...) {uj::daw('~', '~', ...)}

#' @rdname delim
#' @export
dww_00 <- function(...) {uj::dww('', '', ...)}

#' @rdname delim
#' @export
dww_01 <- function(...) {uj::dww('', ' ', ...)}

#' @rdname delim
#' @export
dww_0b <- function(...) {uj::dww('', '¦', ...)}

#' @rdname delim
#' @export
dww_0c <- function(...) {uj::dww('', ':', ...)}

#' @rdname delim
#' @export
dww_0d <- function(...) {uj::dww('', '.', ...)}

#' @rdname delim
#' @export
dww_0g <- function(...) {uj::dww('', ', ', ...)}

#' @rdname delim
#' @export
dww_0p <- function(...) {uj::dww('', '|', ...)}

#' @rdname delim
#' @export
dww_0q <- function(...) {uj::dww('', '`', ...)}

#' @rdname delim
#' @export
dww_0s <- function(...) {uj::dww('', ',', ...)}

#' @rdname delim
#' @export
dww_0t <- function(...) {uj::dww('', '~', ...)}

#' @rdname delim
#' @export
dww_10 <- function(...) {uj::dww(' ', '', ...)}

#' @rdname delim
#' @export
dww_11 <- function(...) {uj::dww(' ', ' ', ...)}

#' @rdname delim
#' @export
dww_1b <- function(...) {uj::dww(' ', '¦', ...)}

#' @rdname delim
#' @export
dww_1c <- function(...) {uj::dww(' ', ':', ...)}

#' @rdname delim
#' @export
dww_1d <- function(...) {uj::dww(' ', '.', ...)}

#' @rdname delim
#' @export
dww_1g <- function(...) {uj::dww(' ', ', ', ...)}

#' @rdname delim
#' @export
dww_1p <- function(...) {uj::dww(' ', '|', ...)}

#' @rdname delim
#' @export
dww_1q <- function(...) {uj::dww(' ', '`', ...)}

#' @rdname delim
#' @export
dww_1s <- function(...) {uj::dww(' ', ',', ...)}

#' @rdname delim
#' @export
dww_1t <- function(...) {uj::dww(' ', '~', ...)}

#' @rdname delim
#' @export
dww_b0 <- function(...) {uj::dww('¦', '', ...)}

#' @rdname delim
#' @export
dww_b1 <- function(...) {uj::dww('¦', ' ', ...)}

#' @rdname delim
#' @export
dww_bb <- function(...) {uj::dww('¦', '¦', ...)}

#' @rdname delim
#' @export
dww_bc <- function(...) {uj::dww('¦', ':', ...)}

#' @rdname delim
#' @export
dww_bd <- function(...) {uj::dww('¦', '.', ...)}

#' @rdname delim
#' @export
dww_bg <- function(...) {uj::dww('¦', ', ', ...)}

#' @rdname delim
#' @export
dww_bp <- function(...) {uj::dww('¦', '|', ...)}

#' @rdname delim
#' @export
dww_bq <- function(...) {uj::dww('¦', '`', ...)}

#' @rdname delim
#' @export
dww_bs <- function(...) {uj::dww('¦', ',', ...)}

#' @rdname delim
#' @export
dww_bt <- function(...) {uj::dww('¦', '~', ...)}

#' @rdname delim
#' @export
dww_c0 <- function(...) {uj::dww(':', '', ...)}

#' @rdname delim
#' @export
dww_c1 <- function(...) {uj::dww(':', ' ', ...)}

#' @rdname delim
#' @export
dww_cb <- function(...) {uj::dww(':', '¦', ...)}

#' @rdname delim
#' @export
dww_cc <- function(...) {uj::dww(':', ':', ...)}

#' @rdname delim
#' @export
dww_cd <- function(...) {uj::dww(':', '.', ...)}

#' @rdname delim
#' @export
dww_cg <- function(...) {uj::dww(':', ', ', ...)}

#' @rdname delim
#' @export
dww_cp <- function(...) {uj::dww(':', '|', ...)}

#' @rdname delim
#' @export
dww_cq <- function(...) {uj::dww(':', '`', ...)}

#' @rdname delim
#' @export
dww_cs <- function(...) {uj::dww(':', ',', ...)}

#' @rdname delim
#' @export
dww_ct <- function(...) {uj::dww(':', '~', ...)}

#' @rdname delim
#' @export
dww_d0 <- function(...) {uj::dww('.', '', ...)}

#' @rdname delim
#' @export
dww_d1 <- function(...) {uj::dww('.', ' ', ...)}

#' @rdname delim
#' @export
dww_db <- function(...) {uj::dww('.', '¦', ...)}

#' @rdname delim
#' @export
dww_dc <- function(...) {uj::dww('.', ':', ...)}

#' @rdname delim
#' @export
dww_dd <- function(...) {uj::dww('.', '.', ...)}

#' @rdname delim
#' @export
dww_dg <- function(...) {uj::dww('.', ', ', ...)}

#' @rdname delim
#' @export
dww_dp <- function(...) {uj::dww('.', '|', ...)}

#' @rdname delim
#' @export
dww_dq <- function(...) {uj::dww('.', '`', ...)}

#' @rdname delim
#' @export
dww_ds <- function(...) {uj::dww('.', ',', ...)}

#' @rdname delim
#' @export
dww_dt <- function(...) {uj::dww('.', '~', ...)}

#' @rdname delim
#' @export
dww_g0 <- function(...) {uj::dww(', ', '', ...)}

#' @rdname delim
#' @export
dww_g1 <- function(...) {uj::dww(', ', ' ', ...)}

#' @rdname delim
#' @export
dww_gb <- function(...) {uj::dww(', ', '¦', ...)}

#' @rdname delim
#' @export
dww_gc <- function(...) {uj::dww(', ', ':', ...)}

#' @rdname delim
#' @export
dww_gd <- function(...) {uj::dww(', ', '.', ...)}

#' @rdname delim
#' @export
dww_gg <- function(...) {uj::dww(', ', ', ', ...)}

#' @rdname delim
#' @export
dww_gp <- function(...) {uj::dww(', ', '|', ...)}

#' @rdname delim
#' @export
dww_gq <- function(...) {uj::dww(', ', '`', ...)}

#' @rdname delim
#' @export
dww_gs <- function(...) {uj::dww(', ', ',', ...)}

#' @rdname delim
#' @export
dww_gt <- function(...) {uj::dww(', ', '~', ...)}

#' @rdname delim
#' @export
dww_p0 <- function(...) {uj::dww('|', '', ...)}

#' @rdname delim
#' @export
dww_p1 <- function(...) {uj::dww('|', ' ', ...)}

#' @rdname delim
#' @export
dww_pb <- function(...) {uj::dww('|', '¦', ...)}

#' @rdname delim
#' @export
dww_pc <- function(...) {uj::dww('|', ':', ...)}

#' @rdname delim
#' @export
dww_pd <- function(...) {uj::dww('|', '.', ...)}

#' @rdname delim
#' @export
dww_pg <- function(...) {uj::dww('|', ', ', ...)}

#' @rdname delim
#' @export
dww_pp <- function(...) {uj::dww('|', '|', ...)}

#' @rdname delim
#' @export
dww_pq <- function(...) {uj::dww('|', '`', ...)}

#' @rdname delim
#' @export
dww_ps <- function(...) {uj::dww('|', ',', ...)}

#' @rdname delim
#' @export
dww_pt <- function(...) {uj::dww('|', '~', ...)}

#' @rdname delim
#' @export
dww_q0 <- function(...) {uj::dww('`', '', ...)}

#' @rdname delim
#' @export
dww_q1 <- function(...) {uj::dww('`', ' ', ...)}

#' @rdname delim
#' @export
dww_qb <- function(...) {uj::dww('`', '¦', ...)}

#' @rdname delim
#' @export
dww_qc <- function(...) {uj::dww('`', ':', ...)}

#' @rdname delim
#' @export
dww_qd <- function(...) {uj::dww('`', '.', ...)}

#' @rdname delim
#' @export
dww_qg <- function(...) {uj::dww('`', ', ', ...)}

#' @rdname delim
#' @export
dww_qp <- function(...) {uj::dww('`', '|', ...)}

#' @rdname delim
#' @export
dww_qq <- function(...) {uj::dww('`', '`', ...)}

#' @rdname delim
#' @export
dww_qs <- function(...) {uj::dww('`', ',', ...)}

#' @rdname delim
#' @export
dww_qt <- function(...) {uj::dww('`', '~', ...)}

#' @rdname delim
#' @export
dww_s0 <- function(...) {uj::dww(',', '', ...)}

#' @rdname delim
#' @export
dww_s1 <- function(...) {uj::dww(',', ' ', ...)}

#' @rdname delim
#' @export
dww_sb <- function(...) {uj::dww(',', '¦', ...)}

#' @rdname delim
#' @export
dww_sc <- function(...) {uj::dww(',', ':', ...)}

#' @rdname delim
#' @export
dww_sd <- function(...) {uj::dww(',', '.', ...)}

#' @rdname delim
#' @export
dww_sg <- function(...) {uj::dww(',', ', ', ...)}

#' @rdname delim
#' @export
dww_sp <- function(...) {uj::dww(',', '|', ...)}

#' @rdname delim
#' @export
dww_sq <- function(...) {uj::dww(',', '`', ...)}

#' @rdname delim
#' @export
dww_ss <- function(...) {uj::dww(',', ',', ...)}

#' @rdname delim
#' @export
dww_st <- function(...) {uj::dww(',', '~', ...)}

#' @rdname delim
#' @export
dww_t0 <- function(...) {uj::dww('~', '', ...)}

#' @rdname delim
#' @export
dww_t1 <- function(...) {uj::dww('~', ' ', ...)}

#' @rdname delim
#' @export
dww_tb <- function(...) {uj::dww('~', '¦', ...)}

#' @rdname delim
#' @export
dww_tc <- function(...) {uj::dww('~', ':', ...)}

#' @rdname delim
#' @export
dww_td <- function(...) {uj::dww('~', '.', ...)}

#' @rdname delim
#' @export
dww_tg <- function(...) {uj::dww('~', ', ', ...)}

#' @rdname delim
#' @export
dww_tp <- function(...) {uj::dww('~', '|', ...)}

#' @rdname delim
#' @export
dww_tq <- function(...) {uj::dww('~', '`', ...)}

#' @rdname delim
#' @export
dww_ts <- function(...) {uj::dww('~', ',', ...)}

#' @rdname delim
#' @export
dww_tt <- function(...) {uj::dww('~', '~', ...)}
