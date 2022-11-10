#' @name compatible.
#' @family props
#' @title Are objects compatible?
#' @description Compatibility means that arguments are either (a) all numeric,
#'   (b) all character, (c) all logical, (d) all unordered factor with the same
#'   levels, or (e) ordered factor with the same levels in the same order.
#'   \cr\cr
#'   \emph{For \link[idtf]{atomic dtfs}}: Arguments are compatible for row
#'   binding if they have identical column names and their respective columns
#'   are of compatible modes. Arguments are compatible for column binding if
#'   they have the same number of rows.
#'   \cr\cr
#'   \emph{For atomic matrices}: Arguments are compatible for column vs. row
#'   binding if their modes are compatible and they have, respectively the same
#'   number of rows vs. columns.
#' @param ... An arbitrary number of arguments to be checked for compatibility
#'   with each other.
#' @param recyclable. \link[cmp_lgl_scl]{Complete logical scalar }indicating
#'   whether arguments in \code{...} must be recyclable to be compatible_
#' @param bind. \link[cmp_chr_scl]{Complete character scalar} indicating how to
#'   bind: \code{'c'} or \code{'r'} for column vs. row binding, respectively.
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

#' @describeIn compatible. Determines whether modes of all arguments in
#'   \code{...} are compatible, meaning that all are character, logical,
#'   numeric, ordered factor with the same set of levels (in the same order), or
#'   unordered factor with the same set of levels (in any order).
#' @export
compatible <- function(..., recyclable. = TRUE) {
  x <- list(...)                                                                # arguments in [...] as a list
  n <- length(x)
  vx <- n >= 2
  vr <- isTF(recyclable.)
  err <- NULL
  if (!vx) {err <- c(err, "\n • [...] must contain multiple arguments.")}
  if (!vr) {err <- c(err, "\n • [recyclable.] must be TRUE or FALSE.")}
  if (idef(err)) {stop(err)}
  if (recyclable.) {
    out <- unique(lengths(x))                                                    # unique set of argument length
    out <- max(out) / out                                                        # number of replications needed for recycling
    if (any(out != round(out))) {return(F)}                                      # if arguments must be recyclable and any rep is fractional, not comparable
  }
  chr <- all(sapply(x, is.character))
  lgl <- all(sapply(x, is.logical  ))
  num <- all(sapply(x, is.numeric  ))
  ord <- all(sapply(x, is.ordered  ))
  uno <- all(sapply(x, is.factor   )) & !any(sapply(x, is.ordered))
  if ( chr |  lgl | num) {return(T)}
  if (!ord & !uno       ) {return(F)}
  levs <- sapply(x, levels)
  if (ord) {for (i in 2:n) {if (!identical(levs[[i]], levs[[i - 1]])) {return(F)}}}
  if (uno) {for (i in 2:n) {if (!setequal( levs[[i]], levs[[i - 1]])) {return(F)}}}
  T
}

#' @describeIn compatible. Are all matrices in \code{...} compatible for
#'   binding?
#' @export
compatible_mats <- function(..., bind. = "c") {
  x <- list(...)
  n <- length(x)
  vx <- f0(n < 2, F,  all(sapply(x, imat)))
  vb <- f0(!cmp_ch1_scl(bind.), F, bind. %in% c("c", "r"))
  err <- NULL
  if (!vx) {err <- c(err, "\n • [...] must contain multiple (and only) atomic matrices (?imat).")}
  if (!vb) {err <- c(err, "\n • [bind.] must be 'c' or 'r'.")}
  if (idef(err)) {stop(err)}
  r <- bind. == "r"                                                              # row bind?
  c <- bind. == "c"                                                              # col bind?
  for (i in 2:n) {                                                               # for [...elt(2)] and above
    y <- x[[i]]; z <- x[[i - 1]]                                                 # > extract it and the previous argument
    if (r & ncol(y) != ncol(z)) {return(F)}                                      # > if row binding, but number of columns differ, not compatible
    if (c & nrow(y) != nrow(z)) {return(F)}                                      # > if column binding, but number of rows differ, not compatible
    if (!compatible(y, z)) {return(F)}                                           # > if args are not mode-compatible, not compatible
  }
  T
}

#' @describeIn compatible. Are all atomic tibbles in \code{...} compatible for
#'   binding?
#' @export
compatible_atbs <- function(..., bind. = "c") {
  x <- list(...)
  n <- length(x.)
  vx <- f0(n < 2, F,  all(sapply(x, idtf)))
  vb <- f0(!cmp_ch1_scl(bind.), F, bind. %in% c("c", "r"))
  err <- NULL
  if (!vx) {err <- c(err, "\n • [...] must contain multiple (and only) atomic dtfs (?idtf)")}
  if (!vb) {err <- c(err, "\n • [bind.] must be 'c' or 'r'.")}
  if (idef(err)) {stop(err)}
  r <- bind. == "r"                                                              # row bind?
  c <- bind. == "c"                                                              # col bind?
  for (i in 2:n) {                                                               # for [...elt(2)] and above
    y <- x[[i]]; z <- x[[i - 1]]                                                 # > extract it and the previous argument
    if (r & ncol(y) != ncol(z)) {return(F)}                                      # > if row binding, but number of columns differ, not compatible
    if (c & nrow(y) != nrow(z)) {return(F)}                                      # > if column binding, but number of rows differ, not compatible
    if (r & !identical(colnames(y), colnames(z))) {return(F)}                    # > if col names must match but they do not, not compatible
    for (j in 1:ncol(y)) {if (!compatible(y[[j]], z[[j]])) {return(F)}}          # > if args are not mode-compatible, not compatible
  }
  T
}
