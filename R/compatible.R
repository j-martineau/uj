#' @name compatible
#' @family props
#' @title Are objects compatible?
#' @section Functions in this Family:
#'   \strong{\code{compatible}}
#'   \cr Evaluates whether all \code{...} arguments are compatible, meaning
#'   all numeric, all character, all logical, all unordered factor with the same
#'   levels, or ordered factor with the same levels in the same order.
#'   \cr\cr
#'   \strong{\code{compatible_dtfs}}
#'   \cr Evaluates whether all \code{...} arguments are
#'   \link[=atm_dtf]{atomic data.frames}. For row binding, also evaluates
#'   whether they have the same number of columns and all corresponding columns
#'   are compatible. For column binding, also evaluates whether they have the
#'   same number of rows.
#'   \cr\cr
#'   \strong{\code{compatible_atms}}
#'   \cr Evaluates whether all \code{...} arguments are compatible matrices
#'   and (for row binding) have the same number of columns or (for column
#'   binding) have the same number of rows.
#' @param ... An arbitrary number of arguments to be checked for compatibility
#'   with each other.
#' @param rec. A non-\code{NA} logical scalar indicating whether arguments in
#'   \code{...} must be recyclable to be compatible.
#' @param b Either \code{'c'} for column binding or \code{'r'} for row binding.
#' @return A logical scalar.
#' @examples
#' N0 <- 0
#' N7 <- 0:7
#' N9 <- 0:9
#' Chr0 <- as.character(Num0)
#' Chr7 <- as.character(Num7)
#' Chr9 <- as.character(Num9)
#' Lgc0 <- Num0 < 4
#' Lgc7 <- Num7 < 3
#' Lgc9 <- Num9 < 2
#' Uno00 <- factor(C0, levels = C0, ordered = F)
#' Uno09 <- factor(C0, levels = C9, ordered = F)
#' Uno77 <- factor(C7, levels = C7, ordered = F)
#' Uno79 <- factor(C7, levels = C9, ordered = F)
#' Uno99 <- factor(C9, levels = C9, ordered = F)
#' Ord00 <- factor(C0, levels = C0, ordered = F)
#' Ord09 <- factor(C0, levels = C9, ordered = F)
#' Ord77 <- factor(C7, levels = C7, ordered = F)
#' Ord79 <- factor(C7, levels = C9, ordered = F)
#' Ord99 <- factor(C9, levels = C9, ordered = F)
#' compatible(N0, N7, N9)
#' compatible(C0, C7, C9)
#' compatible(L0, L7, L9)
#' compatible(UA0, U7A, U9A)
#' compatible(O0, )
#' compatible(ChrZero, ChrSeven, ChrDigits, recycle = F)
#' compatible(LgcZero, LgcSeven, LgcDigits, recycle = F)
#' compatible(NumZero, NumSeven, NumDigits, recycle = F)
#' compatible(list(letters), LETTERS, as.character(0:9))
#' @export
compatible <- function(..., rec. = TRUE) {
  x <- list(...)
  n <- length(x)
  errs <- c(f0(n >= 2    , NULL, "\n \u2022 [...] must contain multiple arguments."),
            f0(isTF(rec.), NULL, "\n \u2022 [rec.] must be TRUE or FALSE."         ))
  if (!is.null(errs)) {stop(errs)}
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
  errs <- c(f0(f0(!ch1_scl(b), F, isIN(b, c("c", "r"))), NULL, "\n \u2022 [b] must be character scalar 'c' or 'r'."         ),
            f0(f0(n < 2, F, all(sapply(x, atm_mat   ))), NULL, "\n \u2022 [...] must contain 2+ atomic matrices (?atm_mat)."))
  if (!is.null(errs)) {stop(errs)}
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
  errs <- c(f0(f0(!cmp_ch1_scl(b), F, b %in% c("c", "r")), NULL, "\n \u2022 [b] must be character scalar 'c' or 'r'."                  ),
            f0(f0(n < 2, F,  all(sapply(x, atm_dtf))    ), NULL, "\n \u2022 [...] must contain multiple atomic data.frames (?atm_dtf)."))
  if (!is.null(errs)) {stop(errs)}
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
