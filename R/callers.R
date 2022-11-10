#' @name callers.
#' @family extensions
#' @family environments
#' @title Functions in the Call Stack
#' @param gens \link[cmp_psw_vec]{Complete positive whole-number vec} giving the
#'   number(s) of generations back in the function call stack to go.
#' @return \code{callers} and \code{caller} return a character scalar or vector.
#'   \code{ncallers} returns a non-negative whole-number scalar.
#' @examples
#' fun.a <- function() {A <- 0; fun.b()}
#' fun.b <- function() {B <- 1; fun.c()}
#' fun.c <- function() {
#'   cat("\n callstack(): " , paste0("'", callstack(), "'"))
#'   cat("\n caller(1)  : '", caller(1), "'")
#'   cat("\n caller(2)  : '", caller(2), "'")
#'   cat("\n exist('A', err = F, gens = 1): ", exist('A', err = F, gens = 1))
#'   cat("\n exist('A', err = F, gens = 2): ", exist('A', err = F, gens = 2))
#'   cat("\n exist('B', err = F, gens = 1): ", exist('B', err = F, gens = 1))
#'   cat("\n exist('B', err = F, gens = 2): ", exist('B', err = F, gens = 2))
#'   cat("\n vget('A', err = F, gens = 2) : ", vget('A', err = F, gens = 2))
#'   cat("\n vget('A', err = F, gens = 1) : ", vget('A', err = F, gens = 1))
#'   cat("\n vget('B', err = F, gens = 2) : ", vget('B', err = F, gens = 2))
#'   cat("\n vget('B', err = F, gens = 1) : ", vget('B', err = F, gens = 1))
#'   vSet('A', 'A', gens = 2)
#'   vSet('B', 'B', gens = 1)
#'   cat("\n vget('A', gens = 2) : ", vget('A', err = F, gens = 2))
#'   cat("\n vget('B', gens = 1) : ", vget('B', err = F, gens = 1))
#' }
#' fun.a()
#' @export
callers. <- function() {help("callers.", package = "uj")}

#' @describeIn callers. Gets names of all calling functions, with the immediate
#'   calling function in the first position and, if \code{gens.} is not
#'   \code{NULL}, selects the \code{gens.}-th element(s) from the call stack.
#' @export
callers <- function(gens = NULL) {
  if (!inll(gens) & !cmp_psw_vec(gens)) {stop("\n • [gens] must be NULL or a positive whole-number vec (?cmp_psw_vec).")}
  x <- rev(as.character(sys.calls()))                                            # get the call stack
  if (length(x) >= 3) {                                                          # if the length of the call stack is > 4, it was not called from the command line, so
    paren <- av(regexpr("(", x, fixed = T))                                      # > find the position of the parenthesis in each line of the call stack
    x     <- substr(x, 1, paren - 1)                                             # > extract the function names (i.e., before the open parentheses)
    x <- x[3:length(x)]                                                          # > select the relevant parts of the call stack
  }
  else {x <- "command line"}                                                     # otherwise, the function was called at the command line
  if (inll(gens)) {gens <- 1:length(x)}
  if (max(gens) > length(x)) {stop("\n • One or more elements of [gens] is greater than the number of calling functions.")}
  x[gens]                                                                        # otherwise, if the specified generations exist, return just those gens
}

#' @describeIn callers. Get the immediate calling function.
#' @export
caller <- function() {callers(1)}

#' @describeIn callers. Get the number of calling functions in the call stack.
#' @export
ncallers <- function() {length(callers())}
