#' @name xb.
#' @family extensions
#' @title Error-checked row and column binding
#' @description \tabular{ll}{
#'   \code{xb.}   \tab Column or row bind or \link[=atm_dtf]{atomic data.frames}
#'                     in \code{...}. If \code{bind = NA}, \code{xb} attempts to
#'                     both row and column bind arguments in \code{...}. If both
#'                     are successful, processes an error indicating ambiguous
#'                     dimension for binding. If neither is successful,
#'                     processes an associated error. Otherwise, returns the
#'                     result that was successful.                           \cr
#'   \code{cb}    \tab Column bind an arbitrary number of atomic matrices or
#'                     \link[=atm_dtf]{atomic data.frames}.                  \cr
#'   \code{rb}    \tab Row bind an arbitrary number of atomic matrices or
#'                     \link[=atm_dtf]{atomic data.frames}.                    }
#'   Requires all arguments in \code{...} to be \code{\link{compatible}} atomic
#'   matrices or \link[=atm_dtf]{atomic data.frames}.
#' @param bind \link[=ch1_scl]{Onechar scalar} indicating how to bind:
#'   \code{'r'} for row binding, \code{'c'} for column-binding, \code{NA} to
#'   identify whether either row or column binding is successful and return
#'   whichever was successful.
#' @param ... Multiple \code{\link{compatible}} atomic matrices or multiple
#'   compatible \link[=atm_dtf]{atomic data.frames}.
#' @return An \link[=atm_dtf]{atomic data.frame} or an atomic matrix.
#' @export
xb. <- function() {help("xb.", package = "uj")}

#' @rdname xb.
#' @export
xb <- function(..., bind = NA) {
  ok.dots <- ...length() > 1
  ok.dtfs <- f0(!ok.dots, T, allply(list(...), idtf))
  ok.mats <- f0(!ok.dots, T, allply(list(...), imat))
  ok.cccs <- ok.dtfs | ok.mats
  ok.bind <- isNa(bind) | isIN(bind, c("r", "c"))
  err <- NULL
  if (!ok.bind) {err <- c(err, "\n \u2022 [bind] must be NA, 'r', or 'c'")}
  if (!ok.dots) {err <- c(err, "\n \u2022 [...] contains less than 2 arguments.")}
  if (!ok.cccs) {err <- c(err, "\n \u2022 [...] must contain (a) only atomic matrices or (b) only atomic tabulars.")}
  if (idef(err)) {stop(err)}
  infix <- f0(ok.dtfs, " atomic data.frames in [...] ", " atomic matrices in [...] ")
  if (bind %EQ% "r") {
    suffix <- "are incompatible for row binding."
    if (ok.mats & !compatible_mats(..., bind = "r")) {stop("\n \u2022 The", infix, suffix)}
    else if (ok.dtfs & !compatible_dtfs(..., bind = "r")) {stop("\n \u2022 The", infix, suffix)}
    rbind(...)
  }
  else if (bind %EQ% "c") {
    suffix <- "are incompatible for column binding."
    if (ok.mats & !compatible_mats(..., bind = "c")) {stop("\n \u2022 The", infix, suffix)}
    else if (ok.dtfs & !compatible_dtfs(..., bind = "c")) {stop("\n \u2022 The", infix, suffix)}
    cbind(...)
  }
  if (isNa(bind)) {
    prefix1 <- c("\n \u2022 The", infix, "are incompatible for binding")
    prefix2 <- c("\n \u2022 Whether to row or column bind the", infix, "was ambiguous")
    infix <- " (i.e., both row and column binding"
    try.cb <- tryCatch(xb(..., bind = "c"), error = function(e) e, finally = NULL)
    try.rb <- tryCatch(xb(..., bind = "r"), error = function(e) e, finally = NULL)
    if (isERR(try.cb) &  isERR(try.rb)) {stop(prefix1, infix, "failed)."   )}
    if (notERR(try.cb) & notERR(try.rb)) {stop(prefix2, infix, "succeeded).")}
    if (notERR(try.cb)) {try.cb} else {try.rb}
  }
}

#' @rdname xb.
#' @export
cb <- function(...) {xb(..., bind = "c")}

#' @rdname xb.
#' @export
rb <- function(...) {xb(..., bind = "r")}
