#' @name compatible
#' @family props
#' @title Are objects compatible?
#' @description Compatibility means that arguments are either (a) all numeric,
#'   (b) all character, (c) all logical, (d) all unordered factor with the same
#'   levels, or (e) ordered factor with the same levels in the same order.
#'   \cr\cr
#'   \emph{For \link[=is_atm_tibble]{atomic tibbles}} (applicable function is
#'   \code{compatible_atbs}): Arguments are compatible for row binding if they
#'   have identical column names and their respective columns are of compatible
#'   modes. Arguments are compatible for column binding if they have the same
#'   number of rows.
#'   \cr\cr
#'   \emph{For atomic matrices} (\code{compatible_mats}): Arguments are
#'   compatible for column vs. row binding, if their modes are compatible and
#'   they have, respectively the same number of rows vs. columns.
#' @details \strong{\code{compatible}}
#'   \cr Determines whether modes of all arguments in \code{...} are
#'   compatible, meaning that all are character, logical, numeric, ordered
#'   factor with the same set of levels (in the same order), or unordered factor
#'   with the same set of levels (in any order).
#'   \cr\cr
#'   \strong{\code{compatible_mats}}
#'   \cr Determines whether matrices are compatible for row or column binding.
#'   \cr\cr
#'   \strong{\code{compatible_atbs}}
#'   \cr Determines whether \link[=is_atm_tibble]{atomic tibbles} are compatible
#'   for row or column binding.
#' @param ... An arbitrary number of arguments to be checked for compatibility
#'   with each other.
#' @param recyclable. A logical scalar indicating whether arguments in
#'   \code{...} must be recyclable to be compatible_
#' @param bind. \code{'c'} or \code{'r'} for column vs. row binding.
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
compatible <- function(..., recyclable. = TRUE) {
  x  <- list(...)                                                                 # arguments in [...] as a list
  N  <- length(x)
  VX <- N >= 2
  VR <- isTF(recyclable.)
  E  <- NULL
  if (!VX) {E <- c(E, "\n  * [...] must contain multiple arguments.")}
  if (!VR) {E <- c(E, "\n  * [recyclable.] must be TRUE or FALSE.")}
  if (xdef(E)) {stop(E)}
  if (recyclable.) {
    R <- unique(lengths(x))                                                      # unique set of argument length
    R <- max(R) / R                                                              # number of replications needed for recycling
    if (any(R != round(R))) {return(F)}                                          # if arguments must be recyclable and any rep is fractional, not comparable
  }
  CH <- all(sapply(x, is.character))
  LG <- all(sapply(x, is.logical  ))
  NM <- all(sapply(x, is.numeric  ))
  OF <- all(sapply(x, is.ordered  ))
  UF <- all(sapply(x, is.factor   )) & !any(sapply(x, is.ordered))
  if ( CH |  LG | NM) {return(T)}
  if (!OF & !UF     ) {return(F)}
  LV <- sapply(x, levels)
  if (OF) {for (i in 2:N) {if (!identical(LV[[i]], LV[[i - 1]])) {return(F)}}}
  if (UF) {for (i in 2:N) {if (!setequal( LV[[i]], LV[[i - 1]])) {return(F)}}}
  T
}

#' @rdname compatible
#' @export
compatible_mats <- function(..., bind. = "c") {
  x <- list(...)
  N <- length(x)
  VX <- f0(N < 2, F,  all(sapply(x, xmat)))
  VB <- f0(!cmp_ch1_scl(bind.), F, bind. %in% c("c", "r"))
  E <- NULL
  if (!VX) {E <- c(E, "\n  * [...] must multiple (and only) atomic matrices.")}
  if (!VB) {E <- c(E, "\n  * [bind.] must be 'c' or 'r'.")}
  if (xdef(E)) {stop(E)}
  R <- bind. == "r"                                                              # row bind?
  C <- bind. == "c"                                                              # col bind?
  for (i in 2:N) {                                                               # for [...elt(2)] and above
    Y <- x[[i]]; Z <- x[[i - 1]]                                                 # > extract it and the previous argument
    if (R & ncol(Y) != ncol(Z)) {return(F)}                                      # > if row binding, but number of columns differ, not compatible
    if (C & nrow(Y) != nrow(Z)) {return(F)}                                      # > if column binding, but number of rows differ, not compatible
    if (!compatible(Y, Z)) {return(F)}                                           # > if args are not mode-compatible, not compatible
  }
  T
}

#' @rdname compatible
#' @export
compatible_atbs <- function(..., bind. = "c") {
  x <- list(...)
  N <- length(x)
  VX <- f0(N < 2, F,  all(sapply(x, xatb)))
  VB <- f0(!cmp_ch1_scl(bind.), F, bind. %in% c("c", "r"))
  E <- NULL
  if (!VX) {E <- c(E, "\n  * [...] must multiple (and only) atomic tibbles.")}
  if (!VB) {E <- c(E, "\n  * [bind.] must be 'c' or 'r'.")}
  if (xdef(E)) {stop(E)}
  R <- bind. == "r"                                                               # row bind?
  C <- bind. == "c"                                                               # col bind?
  for (i in 2:N) {                                                               # for [...elt(2)] and above
    Y <- x[[i]]; Z <- x[[i - 1]]                                                 # > extract it and the previous argument
    if (R & ncol(Y) != ncol(Z)) {return(F)}                                      # > if row binding, but number of columns differ, not compatible
    if (C & nrow(Y) != nrow(Z)) {return(F)}                                      # > if column binding, but number of rows differ, not compatible
    if (R & !identical(colnames(Y), colnames(Z))) {return(F)}                    # > if col names must match but they do not, not compatible
    for (j in 1:ncol(Y)) {if (!compatible(Y[[j]], Z[[j]])) {return(F)}}       # > if args are not mode-compatible, not compatible
  }
  T
}
