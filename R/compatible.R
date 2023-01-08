#' @encoding UTF-8
#' @family extensions
#' @family properties
#' @title Are objects compatible?
#' @description \tabular{rl}{
#'              `compatible`   \tab Evaluates whether all `...` arguments are compatible, meaning all numeric, all character, all logical, all unordered factor with the same levels, or ordered factor with the same levels in the same order.
#'   \cr                       \tab  
#'   \cr   `compatible_dtfs`   \tab Evaluates whether all `...` arguments are \link[=atm_dtf]{atomic data.frames}. For row binding, also evaluates whether they have the same number of columns and all corresponding columns are compatible. For column binding, also evaluates whether they have the same number of rows.
#'   \cr                       \tab  
#'   \cr   `compatible_atms`   \tab Evaluates whether all `...` arguments are compatible matrices and (for row binding) have the same number of columns or (for column binding) have the same number of rows.
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
  x <- base::list(...)
  n <- base::length(x)
  errs <- base::c(uj::f0(n >= 2        , NULL, "[...] must contain multiple arguments."),
                  uj::f0(uj::isTF(rec.), NULL, "[rec.] must be TRUE or FALSE."         ))
  if (!base::is.null(errs)) {stop(uj:::.errs(errs))}
  if (rec.) {
    un <- base::unique(base::lengths(x))
    nr <- base::max(un) / un
    if (base::any(nr != base::round(nr))) {return(F)}
  }
  chr <- base::all(base::sapply(x, is.character))
  lgl <- base::all(base::sapply(x, is.logical))
  num <- base::all(base::sapply(x, is.numeric))
  ord <- base::all(base::sapply(x, is.ordered))
  uno <- base::all(base::sapply(x, is.factor)) & !base::any(base::sapply(x, is.ordered))
  if (chr | lgl | num) {return(T)}
  if (!ord & !uno) {return(F)}
  levs <- base::sapply(x, levels)
  if (ord) {for (i in 2:n) {if (!base::identical(levs[[i]], levs[[i - 1]])) {return(F)}}}
  if (uno) {for (i in 2:n) {if (!base::setequal( levs[[i]], levs[[i - 1]])) {return(F)}}}
  T
}

#' @rdname compatible
#' @export
compatible_mats <- function(b, ...) {
  x <- base::list(...)
  n <- base::length(x)
  errs <- base::c(uj::f0(uj::f0(!uj::ch1_scl(b), F, uj::isIN(b, c("c", "r" )))    , NULL, "[b] must be character scalar 'c' or 'r'."         ),
                  uj::f0(uj::f0(n < 2, F, base::all(base::sapply(x, uj::atm_mat))), NULL, "[...] must contain 2+ atomic matrices (?atm_mat)."))
  if (!base::is.null(errs)) {stop(uj:::.errs(errs))}
  for (i in 2:n) {
    curr <- x[[i]]
    prev <- x[[i - 1]]
    if (b == "r" & base::ncol(prev) != base::ncol(curr)) {return(F)}
    if (b == "c" & base::nrow(prev) != base::nrow(curr)) {return(F)}
    if (!uj::compatible(prev, curr)) {return(F)}
  }
  T
}

#' @rdname compatible
#' @export
compatible_dtfs <- function(b, ...) {
  x <- base::list(...)
  n <- base::length(x)
  errs <- base::c(uj::f0(uj::f0(!uj::cmp_ch1_scl(b), F, b %in% c("c", "r"))        , NULL, "[b] must be character scalar 'c' or 'r'."                  ),
                  uj::f0(uj::f0(n < 2, F,  base::all(base::sapply(x, uj::atm_dtf))), NULL, "[...] must contain multiple atomic data.frames (?atm_dtf)."))
  if (!base::is.null(errs)) {stop(uj:::.errs(errs))}
  for (i in 2:n) {
    curr <- x[[i]]
    prev <- x[[i - 1]]
    if (b == "c" & base::nrow(curr) != nbase::row(prev)) {return(F)}
    if (b == "r") {
      if (base::ncol(curr) != base::ncol(prev)) {return(F)}
      if (!base::identical(base::colnames(curr), base::colnames(prev))) {return(F)}
      for (j in 1:base::ncol(curr)) {if (!uj::compatible(curr[[j]], prev[[j]])) {return(F)}}
  }}
  T
}
