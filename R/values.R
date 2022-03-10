#' @name values
#' @family meta
#' @title Calling functions and objects in their immediate environments
#' @description Check for the existence of an object in the environment of a
#'   specific calling function (\code{vexist}), get the value of an object in
#'   the environment of a specific calling function (\code{vget}), and set the
#'   value of an object in the environment of a specific calling function
#'   (\code{vset}).
#' @param name A character scalar giving the name of an object.
#' @param val A value to place into the object specified by \code{name}.
#' @param err A logical scalar whether to throw an error if the object specified
#'   by \code{name} does not exist (i.e., rather than returning \code{FALSE}).
#' @param gens The number of generations back in the function call stack to go.
#' @return \code{exist} returns a logical scalar indicating whether an object
#'   named \code{name} exists in the environment of the calling function
#'   \code{gens} generations back. \code{vget} returns the value of an object
#'   named \code{name} in the function \code{gens} generations back. \code{vset}
#'   sets the value of an object named \code{name} in the function \code{gens}
#'   generations back to the value \code{val}.
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
exist <- function(name, err = T, gens = 1) {
  VN <- cmp_chr_scl(name)
  VE <- isTF(err)
  VG <- cmp_psw_scl(gens)
  E <- NULL
  if (!VN) {E <- c(E, "\n * [name] must be a non-NA character scalar.")}
  if (!VE) {E <- c(E, "\n * [err] must be TRUE or FALSE.")}
  if (!VG) {E <- c(E, "\n * [gens] must be a positive whole number scalar.")}
  if (xdef(E)) {stop(E)}
  Env <- parent.frame(gens + 1)                                                  # Get the environment of the requested generation in the call stack
  if (exists(name, Env, inherits = F)) {return(T)}                               # If an object with the specified name exists in the specified environment, return T
  if (!err) {return(F)}                                                          # If an error should not immediately be thrown, return F
  if (gens > 1) {Targ <- c("target function (", callers(gens + 1), ") ", gens, " generations back from the ")}
  else {Targ <- ""}
  Call <- callers(1)                                                          # Get the name of the immediate calling function
  stop("\n  * No object named [name = '", name, "'] exists in the environment of the ", Targ, "calling function (", Call, ").")
}

#' @rdname values
#' @export
vget <- function(name, err = T, gens = 1) {
  VN <- cmp_chr_scl(name)
  VE <- isTF(err)
  VG <- cmp_psw_scl(gens)
  E <- NULL
  if (!VN) {E <- c(E, "\n * [name] must be a non-NA character scalar.")}
  if (!VE) {E <- c(E, "\n * [err] must be TRUE or FALSE.")}
  if (!VG) {E <- c(E, "\n * [gens] must be a positive whole number scalar.")}
  if (xdef(E)) {stop(E)}
  if (!exist(name, err, gens)) {NULL}                                            # If the named object does not exist in the target function, return NULL
  else {get(name, envir = parent.frame(gens + 1), inherits = F)}                 # Otherwise, get the value of the named object and return it.
}

#' @rdname values
#' @export
vset <- function(name, val, gens = 1) {
  VN <- cmp_chr_scl(name)
  VG <- cmp_psw_scl(gens)
  E <- NULL
  if (!VN) {E <- c(E, "\n * [name] must be a non-NA character scalar.")}
  if (!VG) {E <- c(E, "\n * [gens] must be a positive whole number scalar.")}
  if (xdef(E)) {stop(E)}
  assign(name, val, envir = parent.frame(gens + 1), inherits = F)                # Set the value of the named object in the target functions environment to the specified value
}
