#' @encoding UTF-8
#' @family properties
#' @title Are objects compatible?
#' @description
#' \tabular{ll}{  `compatible`        \tab Evaluates whether all `...` arguments are compatible, meaning all numeric, all character, all logical, all unordered factor with the same levels, or ordered factor with the same levels in the same order.  \cr   \tab   \cr
#'                `compatible_xy`     \tab Evaluates whether all `X` and `Y` are compatible.                                                                                                                                                            \cr   \tab   \cr
#'                `compatible_mats`   \tab Evaluates whether all `...` arguments are compatible matrices and (for row binding) have the same number of columns or (for column binding) have the same number of rows.                                    \cr   \tab   \cr
#'                `compatible_dtfs`   \tab Evaluates whether all `...` arguments are \link[=atm_dtf]{atomic data.frames}. For row binding, also evaluates whether they have the same number of columns and all corresponding columns are compatible.
#'                                         For column binding, also evaluates whether they have the same number of rows.                                                                                                                                               }
#' @param ... An arbitrary number of arguments to be checked for compatibility with each other.
#' @param dim Integer scalar indicating type of binding to check for compatibility (1 = row, 2 = col).
#' @param .rec `TRUE` or `FALSE` indicating whether arguments in `...` must be recyclable to be compatible.
#' @return A logical scalar.
#' @examples
#' egn0 <- 0
#' egN7 <- 0:7
#' egN9 <- 0:9
#' egL0 <- egn0 < 4
#' egL7 <- egN7 < 3
#' egL9 <- egN9 < 2
#' egC0 <- as.character(egn0)
#' egC7 <- as.character(egN7)
#' egC9 <- as.character(egN9)
#' u00 <- factor(egC0, levels = egC0, ordered = F)
#' u07 <- factor(egC0, levels = egC7, ordered = F)
#' u09 <- factor(egC0, levels = egC9, ordered = F)
#' u79 <- factor(egC7, levels = egC9, ordered = F)
#' u99 <- factor(egC9, levels = egC9, ordered = F)
#' o00 <- factor(egC0, levels = egC0, ordered = F)
#' o07 <- factor(egC0, levels = egC7, ordered = F)
#' o09 <- factor(egC0, levels = egC9, ordered = F)
#' o79 <- factor(egC7, levels = egC9, ordered = F)
#' o99 <- factor(egC9, levels = egC9, ordered = F)
#' egDTFncl7 <- data.frame(N = egN7, C = egC7, L = egL7)
#' egDTFncl9 <- data.frame(N = egN9, C = egC9, L = egL9)
#' egDTFcln7 <- data.frame(C = egC7, L = egL7, N = egN7)
#' egDTFcln9 <- data.frame(C = egC9, L = egL9, N = egN9)
#' compatible(egn0, egN7, egN9)
#' compatible(egC0, egC7, egC9, .rec = T)
#' compatible(egC0, egC7, egC9)
#' compatible(egL0, egL7, egL9, .rec = T)
#' compatible(egL0, egL7, egL9)
#' compatible(egn0, egN7, egN9, .rec = T)
#' compatible(u09, u79, u99)
#' compatible(u00, u07, u09)
#' compatible(o09, o79, o99)
#' compatible(o00, o07, o09)
#' compatible_dtfs("r", dtfNCL7, dtfNCL9)
#' compatible_dtfs("c", dtfNCL7, dtfNCL9)
#' compatible_dtfs("r", dtfNCL7, dtfCLN7)
#' compatible_dtfs("c", dtfNCL7, dtfCLN7)
#' @export
compatible <- function(..., .rec = FALSE) {
  x <- base::list(...)
  n <- base::length(x)
  errs <- NULL
  if (n < 2) {errs <- base::c(errs, "[...] must contain multiple arguments.")}
  if (!uj::.cmp_lgl_scl(.rec)) {errs <- base::c(errs, "[.rec] must be TRUE or FALSE.")}
  if (!base::is.null(errs)) {uj::stopperr(errs)}
  if (.rec) {
    un <- base::length(base::unique(x))
    nr <- base::max(un) / un
    if (base::any(nr != base::round(nr))) {return(F)}
  }
  chr <- base::all(base::sapply(x, base::is.character))
  lgl <- base::all(base::sapply(x, base::is.logical))
  num <- base::all(base::sapply(x, base::is.numeric))
  ord <- base::all(base::sapply(x, base::is.ordered))
  uno <- base::all(base::sapply(x, base::is.factor)) & !base::any(base::sapply(x, base::is.ordered))
  if (!chr & !lgl & !num) {
    if (ord | uno) {
      levs <- base::sapply(x, base::levels)
      if (ord) {for (i in 2:n) {
        currLevs <- levs[[i]]
        prevLevs <- levs[[i - 1]]
        if (base::length(currLevs) != base::length(prevLevs)) {return(F)}
        chr2 <- base::is.character(currLevs) & base::is.character(prevLevs)
        num2 <- base::is.numeric(currLevs) & base::is.numeric(prevLevs)
        if (!chr2 & !num2) {return(F)}
        if (base::any(currLevs != prevLevs)) {return(F)}
      }} else if (uno) {for (i in 2:n) {
        currLevs <- levs[[i]]
        prevLevs <- levs[[i - 1]]
        if (base::length(currLevs) != base::length(prevLevs)) {return(F)}
        chr2 <- base::is.character(currLevs) & base::is.character(prevLevs)
        num2 <- base::is.numeric(currLevs) & base::is.numeric(prevLevs)
        if (!chr2 & !num2) {return(F)}
        if (!base::setequal(currLevs, prevLevs)) {return(F)}
      }}
      T
    } else {F}
  } else {T}
}

#' @rdname compatible
#' @export
compatible_xy <- function(x, y, .rec = FALSE) {uj::compatible(x, y, .rec = .rec)}

#' @rdname compatible
#' @export
compatible_mats <- function(dim, ...) {
  x <- base::list(...)
  n <- base::length(x)
  errs <- NULL
  if (!uj::.cmp_num_scl(dim, valid = 1:2)) {errs <- base::c(errs, "[dim] must be integer scalar 1 or 2.")}
  if (n < 2) {errs <- base::c(errs, "[...] must contain 2+ atomic matrices (?atm_mat).")}
  else if (!base::all(base::sapply(x, uj::.atm_mat))) {errs <- base::c(errs, "[...] must contain 2+ atomic matrices (?atm_mat).")}
  if (!base::is.null(errs)) {uj::stopperr(errs)}
  for (i in 2:n) {
    curr <- x[[i]]
    prev <- x[[i - 1]]
    if      (dim == 1 & base::ncol(prev) != base::ncol(curr)) {return(F)}
    else if (dim == 2 & base::nrow(prev) != base::nrow(curr)) {return(F)}
    else if (!uj::compatible(prev, curr)) {return(F)}
  }
  T
}

#' @rdname compatible
#' @export
compatible_dtfs <- function(dim, ...) {
  x <- base::list(...)
  n <- base::length(x)
  errs <- NULL
  if (!uj::.cmp_num_scl(dim, valid = 1:2)) {errs <- base::c(errs, "[dim] must be integer scalar 1 or 2.")}
  if (n < 2) {errs <- base::c(errs, "[...] must contain 2+ atomic data.frames (?atm_dtf).")}
  else if (!base::all(base::sapply(x, uj::.atm_dtf))) {errs <- base::c(errs, "[...] must contain 2+ atomic data.frames (?atm_dtf).")}
  if (!base::is.null(errs)) {uj::stopperr(errs)}
  for (i in 2:n) {
    curr <- x[[i]]
    prev <- x[[i - 1]]
    if (dim == 2 & base::nrow(curr) != base::nrow(prev)) {return(F)}
    if (dim == 1) {
      if (base::ncol(curr) != base::ncol(prev)) {return(F)}
      if (base::any(base::colnames(curr) != base::colnames(prev))) {return(F)}
      for (j in 1:base::ncol(curr)) {if (!uj::compatible(curr[[j]], prev[[j]])) {return(F)}}
  }}
  T
}
