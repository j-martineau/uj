#' @name xb
#' @family meta
#' @title Robustly Error-Checked Row and Column Binding for Atomic Matrices and
#'   Atomic Tibbles.
#' @description Requires all arguments in \code{...} to be either (a)
#'   \link[=compatible_mats]{compatible atomic matrices} or (b)
#'   \link[=compatible_atbs]{compatible atomic tibbles}.
#' @details \strong{\code{cb}}
#'   \cr Column binds arguments in \code{...}.
#'   \cr\cr
#'   \strong{\code{rb}}
#'   \cr Row binds arguments in \code{...}.
#'   \cr\cr
#'   \strong{\code{xb}}
#'   \cr If \code{bind. = 'c'}, column binds arguments in \code{...}.
#'   \cr\cr
#'   If \code{bind. = 'r'}, row binds arguments in \code{...}.
#'   \cr\cr
#'   If \code{bind. = NA}, attempts to row bind and column bind arguments in
#'   \code{...}. If both are successful, processes an error indicating ambiguous
#'   dimension for binding. If neither is successful, processes an associated
#'   error. Otherwise, returns the result that was successful.
#' @param bind. \code{'r'} to indicate row binding, \code{'c'} to indicate
#'   column, or \code{NA} to identify whether either row or column binding is
#'   successful and return whichever was successful.
#' @param ... Either
#'   \link[=compatible_mats]{atomic matrices compatible for binding} or
#'   \link[=compatible_atbs]{atomic tibbles compatible for binding}.
#' @return An \link[=is_atm_tibble]{atomic tibble} or an atomic matrix.
#' @export
xb <- function(..., bind. = NA) {
  VN <- ...length() > 1
  VA <- f0(!VN, T, allply(list(...), xatb))
  VM <- f0(!VN, T, allply(list(...), xmat))
  VX <- VA | VM
  VB <- isNa(bind.) | isIN(bind., c("r", "c"))
  E <- NULL
  if (!VB) {E <- c(E, "\n  * [bind.] must be NA, 'r', or 'c'")}
  if (!VN) {E <- c(E, "\n  * [...] contains less than 2 arguments.")}
  if (!VX) {E <- c(E, "\n  * [...] must contain (a) only atomic matrices or (b) only atomic tibbles.")}
  if (xdef(E)) {stop(E)}
  XC <- f0(VA, " atomic tibbles in [...] ", " atomic matrices in [...] ")
  if (bind. %EQ% "r") {
    OP <- "are incompatible for row binding."
    if      (VM & !compatible_mats(..., bind = "r")) {stop("\n  * The", XC, OP)}
    else if (VA & !compatible_atbs(..., bind = "r")) {stop("\n  * The", XC, OP)}
    rbind(...)
  }
  else if (bind. %EQ% "c") {
    OP <- "are incompatible for column binding."
    if      (VM & !compatible_mats(..., bind = "c")) {stop("\n  * The", XC, OP)}
    else if (VA & !compatible_atbs(..., bind = "c")) {stop("\n  * The", XC, OP)}
    cbind(...)
  }
  if (isNa(bind.)) {
    OP1 <- c("\n  * The", XC, "are incompatible for binding")
    OP2 <- c("\n  * Whether to row or column bind the", XC, "was ambiguous")
    XB  <- " (i.e., both row and column binding"
    RC <- tryCatch(xb(..., bind = "c"), error = function(e) e, finally = NULL)
    RR <- tryCatch(xb(..., bind = "r"), error = function(e) e, finally = NULL)
    if (isERR( RC) & isERR( RR)) {stop(OP1, XB, "failed)."   )}
    if (notERR(RC) & notERR(RR)) {stop(OP2, XB, "succeeded).")}
    if (notERR(RC)) {RC} else {RR}
  }
}

#' @rdname xb
#' @export
cb <- function(...) {xb(..., bind. = "c")}

#' @rdname xb
#' @export
rb <- function(...) {xb(..., bind. = "r")}
