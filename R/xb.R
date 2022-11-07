#' @name xb.
#' @family extensions
#' @title Error-checked row and column binding
#' @description Requires all arguments in \code{...} to be either (a)
#'   \code{\link{compatible}} atomic matrices or (b) \code{\link{compatible}}
#'   \code{link[idts]{atomic dfs}}. If \code{bind. = NA}, \code{xb} attempts to
#'   row bind and column bind arguments in \code{...}. If both are successful,
#'   processes an error indicating ambiguous dimension for binding. If neither
#'   is successful, processes an associated error. Otherwise, returns the result
#'   that was successful.
#' @param bind. \code{'r'} to indicate row binding, \code{'c'} to indicate
#'   column, or \code{NA} to identify whether either row or column binding is
#'   successful and return whichever was successful.
#' @param ...,x.,y. Either atomic matrices compatible for binding or atomic
#'   dtfs compatible for binding (see \code{\link{compatible}}).
#' @return An \code{\link[idtf]{atomic dtf}} or an atomic matrix.
#' @export
xb. <- function() {help("xb.", package = "uj")}

#' @describeIn xb. Column bind an arbitrary number of matrices or data frames.
#' @export
xb <- function(..., bind. = NA) {
  vn. <- ...length() > 1
  vt. <- f0(!vn., T, allply(list(...), idtf))
  vm. <- f0(!vn., T, allply(list(...), imat))
  vx. <- vt. | vm.
  vb. <- isNa(bind.) | isIN(bind., c("r", "c"))
  err. <- NULL
  if (!vb.) {err. <- c(err., "\n • [bind.] must be NA, 'r', or 'c'")}
  if (!vn.) {err. <- c(err., "\n • [...] contains less than 2 arguments.")}
  if (!vx.) {err. <- c(err., "\n • [...] must contain (a) only atomic matrices or (b) only atomic tabulars.")}
  if (idef(err.)) {stop(err.)}
  in. <- f0(vt., " atomic tibbles in [...] ", " atomic matrices in [...] ")
  if (bind. %EQ% "r") {
    suff. <- "are incompatible for row binding."
    if      (vm. & !compatible_mats(..., bind. = "r")) {stop("\n • The", in., suff.)}
    else if (vt. & !compatible_atbs(..., bind. = "r")) {stop("\n • The", in., suff.)}
    rbind(...)
  }
  else if (bind. %EQ% "c") {
    suff. <- "are incompatible for column binding."
    if      (vm. & !compatible_mats(..., bind. = "c")) {stop("\n • The", in., suff.)}
    else if (vt. & !compatible_atbs(..., bind. = "c")) {stop("\n • The", in., suff.)}
    cbind(...)
  }
  if (isNa(bind.)) {
    pref1. <- c("\n  * The", in., "are incompatible for binding")
    pref2. <- c("\n  * Whether to row or column bind the", in., "was ambiguous")
    in.  <- " (i.e., both row and column binding"
    tryc. <- tryCatch(xb(..., bind. = "c"), error = function(e.) e., finally = NULL)
    tryr. <- tryCatch(xb(..., bind. = "r"), error = function(e.) e., finally = NULL)
    if ( isERR(tryc.) &  isERR(tryr.)) {stop(pref1., in., "failed)."   )}
    if (notERR(tryc.) & notERR(tryr.)) {stop(pref2., in., "succeeded).")}
    if (notERR(tryc.)) {tryc.} else {tryr.}
  }
}

#' @describeIn xb. Column bind an arbitrary number of matrices or data frames.
#' @export
cb <- function(...) {xb(..., bind. = "c")}

#' @describeIn xb. Row bind an arbitrary number of matrices or data frames.
#' @export
rb <- function(...) {xb(..., bind. = "r")}
