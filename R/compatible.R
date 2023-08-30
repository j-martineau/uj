#' @encoding UTF-8
#' @family extensions
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
#' @param .REC `TRUE` or `FALSE` indicating whether arguments in `...` must be recyclable to be compatible.
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
#' compatible(egC0, egC7, egC9, .REC = T)
#' compatible(egC0, egC7, egC9)
#' compatible(egL0, egL7, egL9, .REC = T)
#' compatible(egL0, egL7, egL9)
#' compatible(egn0, egN7, egN9, .REC = T)
#' compatible(u09, u79, u99)
#' compatible(u00, u07, u09)
#' compatible(o09, o79, o99)
#' compatible(o00, o07, o09)
#' compatible_dtfs("r", dtfNCL7, dtfNCL9)
#' compatible_dtfs("c", dtfNCL7, dtfNCL9)
#' compatible_dtfs("r", dtfNCL7, dtfCLN7)
#' compatible_dtfs("c", dtfNCL7, dtfCLN7)
#' @export
compatible <- function(..., .REC = FALSE) {
  X <- base::list(...)
  N <- base::length(X)
  Errors <- NULL
  if (N < 2) {Errors <- base::c(Errors, "[...] must contain multiple arguments.")}
  if (!uj:::.cmp_lgl_scl(.REC)) {Errors <- base::c(Errors, "[.REC] must be TRUE or FALSE.")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, .PKG = "uj")}
  if (.REC) {
    UN <- base::length(base::unique(X))
    NR <- base::max(UN) / UN
    if (base::any(NR != base::round(NR))) {return(F)}
  }
  Chr <- base::all(base::sapply(X, base::is.character))
  Lgl <- base::all(base::sapply(X, base::is.logical))
  Num <- base::all(base::sapply(X, base::is.numeric))
  Ord <- base::all(base::sapply(X, base::is.ordered))
  Uno <- base::all(base::sapply(X, base::is.factor)) & !base::any(base::sapply(X, base::is.ordered))
  if (!Chr & !Lgl & !Num) {
    if (Ord | Uno) {
      Levs <- base::sapply(X, levels)
      if (Ord) {for (i in 2:N) {
        CurrLevs <- Levs[[i]]
        PrevLevs <- Levs[[i - 1]]
        if (base::length(CurrLevs) != base::length(PrevLevs)) {return(F)}
        CHR <- base::is.character(CurrLevs) & base::is.character(PrevLevs)
        NUM <- base::is.numeric(CurrLevs) & base::is.numeric(PrevLevs)
        if (!CHR & !NUM) {return(F)}
        if (base::any(CurrLevs != PrevLevs)) {return(F)}
      }} else if (Uno) {for (i in 2:N) {
        CurrLevs <- Levs[[i]]
        PrevLevs <- Levs[[i - 1]]
        if (base::length(CurrLevs) != base::length(PrevLevs)) {return(F)}
        CHR <- base::is.character(CurrLevs) & base::is.character(PrevLevs)
        NUM <- base::is.numeric(CurrLevs) & base::is.numeric(PrevLevs)
        if (!CHR & !NUM) {return(F)}
        if (!base::setequal(CurrLevs, PrevLevs)) {return(F)}
      }}
      T
    } else {F}
  } else {T}
}

#' @rdname compatible
#' @export
compatible_xy <- function(x, y, .REC = FALSE) {uj::compatible(x, y, .REC = .REC)}

#' @rdname compatible
#' @export
compatible_mats <- function(dim, ...) {
  X <- base::list(...)
  N <- base::length(X)
  Errors <- NULL
  if (!uj:::.cmp_num_scl(dim, .VALID = 1:2)) {Errors <- base::c(Errors, "[dim] must be integer scalar 1 or 2.")}
  if (N < 2) {Errors <- base::c(Errors, "[...] must contain 2+ atomic matrices (?atm_mat).")}
  else if (!base::all(base::sapply(X, uj:::.atm_mat))) {Errors <- base::c(Errors, "[...] must contain 2+ atomic matrices (?atm_mat).")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, .PKG = "uj")}
  for (i in 2:N) {
    Curr <- X[[i]]
    Prev <- X[[i - 1]]
    if      (dim == 1 & base::ncol(Prev) != base::ncol(Curr)) {return(F)}
    else if (dim == 2 & base::nrow(Prev) != base::nrow(Curr)) {return(F)}
    else if (!uj::compatible(Prev, Curr)) {return(F)}
  }
  T
}

#' @rdname compatible
#' @export
compatible_dtfs <- function(dim, ...) {
  X <- base::list(...)
  N <- base::length(X)
  Errors <- NULL
  if (!uj:::.cmp_num_scl(dim, .VALID = 1:2)) {Errors <- base::c(Errors, "[dim] must be integer scalar 1 or 2.")}
  if (N < 2) {Errors <- base::c(Errors, "[...] must contain 2+ atomic data.frames (?atm_dtf).")}
  else if (!base::all(base::sapply(X, uj:::.atm_dtf))) {Errors <- base::c(Errors, "[...] must contain 2+ atomic data.frames (?atm_dtf).")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, .PKG = "uj")}
  for (i in 2:N) {
    Curr <- X[[i]]
    Prev <- X[[i - 1]]
    if (dim == 2 & base::nrow(Curr) != base::nrow(Prev)) {return(F)}
    if (dim == 1) {
      if (base::ncol(Curr) != base::ncol(Prev)) {return(F)}
      if (base::any(base::colnames(Curr) != base::colnames(Prev))) {return(F)}
      for (j in 1:base::ncol(Curr)) {if (!uj::compatible(Curr[[j]], Prev[[j]])) {return(F)}}
  }}
  T
}
