#' @name compatible.
#' @family props
#' @title Are objects compatible?
#' @description Compatibility means that objects or corresponding variables in
#'   data.frames arguments are either:\itemize{
#'     \item all numeric
#'     \item all character
#'     \item all logical
#'     \item all unordered factor with the same levels, or
#'     \item ordered factor with the same levels in the same order.            }
#'   FOR \link[=atm_dtf]{ATOMIC DATA.FRAMES}:
#'   \cr Arguments are compatible for row binding if they have identical column
#'   names and their respective columns are of compatible modes as defined
#'   above. Arguments are compatible for column binding if they have the same
#'   number of rows.
#'   \cr\cr
#'   FOR \link[=atm_mat]{ATOMIC MATRICES}:
#' \cr Arguments are compatible for row binding if their modes are compatible
#' and they have the same number of columns. Arguments are compatible for column
#' binding if their modes are compatible and they the same number of rows.
#' @param ... An arbitrary number of arguments to be checked for compatibility
#'   with each other.
#' @param recyclable. A non-\code{NA} logical scalar indicating whether
#'   arguments in \code{...} must be recyclable to be compatible_
#' @param bind. A \link[=cmp_chr_scl]{Complete character scalar} indicating how
#'   to bind: \code{'c'} or \code{'r'} for column vs. row binding, respectively.
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
compatible. <- function() {help("compatible.", package = "uj")}

#' @rdname compatible.
#' @export
compatible <- function(..., recyclable. = TRUE) {
  dots <- list(...)
  n.dots <- length(dots)
  errs <- c(f0(n.dots < 2        , "\n \u2022 [...] must contain multiple arguments.", NULL),
            f0(!isTF(recyclable.), "\n \u2022 [recyclable.] must be TRUE or FALSE."  , NULL))
  if (idef(errs)) {stop(errs)}
  if (recyclable.) {
    unq.ns <- unique(lengths(dots))
    n.reps <- max(unq.ns) / unq.ns
    if (any(n.reps != round(n.reps))) {return(F)}
  }
  is.chr <- all(sapply(dots, is.character))
  is.lgl <- all(sapply(dots, is.logical))
  is.num <- all(sapply(dots, is.numeric))
  is.ord <- all(sapply(dots, is.ordered))
  is.uno <- all(sapply(dots, is.factor)) & !any(sapply(dots, is.ordered))
  if (is.chr | is.lgl | is.num) {return(T)}
  if (!is.ord & !is.uno) {return(F)}
  fac.levs <- sapply(dots, levels)
  if (is.ord) {for (i in 2:n.dots) {if (!identical(fac.levs[[i]], fac.levs[[i - 1]])) {return(F)}}}
  if (is.uno) {for (i in 2:n.dots) {if (!setequal(fac.levs[[i]], fac.levs[[i - 1]])) {return(F)}}}
  T
}

#' @rdname compatible.
#' @export
compatible_mats <- function(..., bind. = "c") {
  dots <- list(...)
  n.dots <- length(dots)
  errs <- c(f0(!f0(n.dots < 2, F,  all(sapply(dots, atm_mat)))      , "\n \u2022 [...] must contain multiple atomic matrices (?atm_mat).", NULL),
            f0(!f0(!cmp_ch1_scl(bind.), F, isIN(bind., c("c", "r"))), "\n \u2022 [bind.] must be character scalar 'c' or 'r'."           , NULL))
  if (idef(errs)) {stop(errs)}
  for (i in 2:n.dots) {
    curr <- dots[[i]]
    prev <- dots[[i - 1]]
    if (bind. == "r" & ncol(prev) != ncol(curr)) {return(F)}
    if (bind. == "c" & nrow(prev) != nrow(curr)) {return(F)}
    if (!compatible(prev, curr)) {return(F)}
  }
  T
}

#' @rdname compatible.
#' @export
compatible_dtfs <- function(..., bind. = "c") {
  dots <- list(...)
  n.dots <- length(dots)
  errs <- c(f0(!f0(n.dots < 2, F,  all(sapply(dots, atm_dtf)))    , "\n \u2022 [...] must contain multiple atomic data.frames (?atm_dtf).", NULL),
            f0(!f0(!cmp_ch1_scl(bind.), F, bind. %in% c("c", "r")), "\n \u2022 [bind.] must be character scalar 'c' or 'r'."              , NULL))
  if (idef(errs)) {stop(errs)}
  for (i in 2:n.dots) {
    curr <- dots[[i]]
    prev <- dots[[i - 1]]
    if (bind. == "c" & nrow(curr) != nrow(prev)) {return(F)}
    if (bind. == "r") {
      if (ncol(curr) != ncol(prev)) {return(F)}
      if (!identical(colnames(curr), colnames(prev))) {return(F)}
      for (j in 1:ncol(curr)) {if (!compatible(curr[[j]], prev[[j]])) {return(F)}}
  }}
  T
}
