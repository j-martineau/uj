#' @name compatible
#' @family props
#' @title Are objects compatible?
#' @description \tabular{rl}{
#'            `compatible`   \tab Evaluates whether all `...` arguments are compatible, meaning all numeric, all character, all logical, all unordered factor with the same levels, or ordered factor with the same levels in the same order.
#'   \cr `compatible_dtfs`   \tab Evaluates whether all `...` arguments are \link[=atm_dtf]{atomic data.frames}. For row binding, also evaluates whether they have the same number of columns and all corresponding columns are compatible. For column binding, also evaluates whether they have the same number of rows.
#'   \cr `compatible_atms`   \tab Evaluates whether all `...` arguments are compatible matrices and (for row binding) have the same number of columns or (for column binding) have the same number of rows.
#' }
#' @param ... An arbitrary number of arguments to be checked for compatibility with each other.
#' @param rec. A non-\code{NA} logical scalar indicating whether arguments in `...` must be recyclable to be compatible.
#' @param b Either `'c'` for column binding or `'r'` for row binding.
#' @return A logical scalar.
#' @examples
#' n0 <- 0
#' n7 <- 0:7
#' n9 <- 0:9
#'
#' c0 <- as.character(n0)
#' c7 <- as.character(n7)
#' c9 <- as.character(n9)
#'
#' l0 <- n0 < 4
#' l7 <- n7 < 3
#' l9 <- n9 < 2
#'
#' dtf7.ncl. <- data.frame(n = n7, c = c7, l = l7)
#' dtf9.ncl. <- data.frame(n = n9, c = c9, l = l9)
#' dtf7.cln. <- data.frame(C = c7, L = l7, N = n7)
#' dtf9.cln. <- data.frame(C = c9, L = l9, N = n9)
#'
#' u00 <- factor(c0, levels = c0, ordered = F)
#' u07 <- factor(c0, levels = c7, ordered = F)
#' u09 <- factor(c0, levels = c9, ordered = F)
#' u79 <- factor(c7, levels = c9, ordered = F)
#' u99 <- factor(c9, levels = c9, ordered = F)
#'
#' o00 <- factor(c0, levels = c0, ordered = F)
#' o07 <- factor(c0, levels = c7, ordered = F)
#' o09 <- factor(c0, levels = c9, ordered = F)
#' o79 <- factor(c7, levels = c9, ordered = F)
#' o99 <- factor(c9, levels = c9, ordered = F)
#'
#' compatible(n0, n7, n9)
#' compatible(c0, c7, c9, rec. = T)
#'
#' compatible(c0, c7, c9)
#' compatible(l0, l7, l9, rec. = T)
#'
#' compatible(l0, l7, l9)
#' compatible(n0, n7, n9, rec. = T)
#'
#' compatible(u09, u79, u99)
#' compatible(u00, u07, u09)
#'
#' compatible(o09, o79, o99)
#' compatible(o00, o07, o09)
#'
#' dtf7.ncl. <- data.frame(n = n7, c = c7, l = l7)
#' dtf9.ncl. <- data.frame(n = n9, c = c9, l = l9)
#' dtf7.cln. <- data.frame(C = c7, L = l7, N = n7)
#' dtf9.cln. <- data.frame(C = c9, L = l9, N = n9)
#'
#' compatible_dtfs("r", dtf7.ncl., dtf9.ncl.)
#' compatible_dtfs("c", dtf7.ncl., dtf9.ncl.)
#' compatible_dtfs("r", dtf7.ncl., dtf7.cln.)
#' compatible_dtfs("c", dtf7.ncl., dtf7.cln.)
#' @export
compatible <- function(..., rec. = FALSE) {
  x <- list(...)
  n <- length(x)
  errs <- c(f0(n >= 2    , NULL, "[...] must contain multiple arguments."),
            f0(isTF(rec.), NULL, "[rec.] must be TRUE or FALSE."         ))
  if (!is.null(errs)) {stop(.errs(errs))}
  if (rec.) {
    un <- unique(lengths(x))
    nr <- max(un) / un
    if (any(nr != round(nr))) {return(F)}
  }
  chr <- all(sapply(x, is.character))
  lgl <- all(sapply(x, is.logical))
  num <- all(sapply(x, is.numeric))
  ord <- all(sapply(x, is.ordered))
  uno <- all(sapply(x, is.factor)) & !any(sapply(x, is.ordered))
  if (chr | lgl | num) {return(T)}
  if (!ord & !uno) {return(F)}
  levs <- sapply(x, levels)
  if (ord) {for (i in 2:n) {if (!identical(levs[[i]], levs[[i - 1]])) {return(F)}}}
  if (uno) {for (i in 2:n) {if (!setequal( levs[[i]], levs[[i - 1]])) {return(F)}}}
  T
}

#' @rdname compatible
#' @export
compatible_mats <- function(b, ...) {
  x <- list(...)
  n <- length(x)
  errs <- c(f0(f0(!ch1_scl(b), F, isIN(b, c("c", "r"))), NULL, "[b] must be character scalar 'c' or 'r'."         ),
            f0(f0(n < 2, F, all(sapply(x, atm_mat   ))), NULL, "[...] must contain 2+ atomic matrices (?atm_mat)."))
  if (!is.null(errs)) {stop(.errs(errs))}
  for (i in 2:n) {
    curr <- x[[i]]
    prev <- x[[i - 1]]
    if (b == "r" & ncol(prev) != ncol(curr)) {return(F)}
    if (b == "c" & nrow(prev) != nrow(curr)) {return(F)}
    if (!compatible(prev, curr)) {return(F)}
  }
  T
}

#' @rdname compatible
#' @export
compatible_dtfs <- function(b, ...) {
  x <- list(...)
  n <- length(x)
  errs <- c(f0(f0(!cmp_ch1_scl(b), F, b %in% c("c", "r")), NULL, "[b] must be character scalar 'c' or 'r'."                  ),
            f0(f0(n < 2, F,  all(sapply(x, atm_dtf))    ), NULL, "[...] must contain multiple atomic data.frames (?atm_dtf)."))
  if (!is.null(errs)) {stop(.errs(errs))}
  for (i in 2:n) {
    curr <- x[[i]]
    prev <- x[[i - 1]]
    if (b == "c" & nrow(curr) != nrow(prev)) {return(F)}
    if (b == "r") {
      if (ncol(curr) != ncol(prev)) {return(F)}
      if (!identical(colnames(curr), colnames(prev))) {return(F)}
      for (j in 1:ncol(curr)) {if (!compatible(curr[[j]], prev[[j]])) {return(F)}}
  }}
  T
}
