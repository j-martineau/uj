#' @name envir_vals
#' @family extensions
#' @family environments
#' @title Objects in Environments of Calling Functions
#' @description All functions in this family operate in the environment of the calling function `gens` generations back in the call stack.
#' \itemize{
#'   \item **`vexists`**: checks for the existence of an object in the environment of the `gens`-specified calling function.
#'   \item **`vget`**: gets the value of an object in the environment of the `gens`-specified calling function.
#'   \item **`vset`**: sets the value of an object in the environment of the `gens`-specified calling function.
#' }
#' @param name A \link[=cmp_chr_scl]{complete character scalar} giving the name of an object.
#' @param val A value to place into the object specified by `name`.
#' @param err A non`NA` logical scalar indicating whether to throw an error if the object specified by `name` does not exist (i.e., rather than returning `FALSE`).
#' @param gens A \link[=cmp_psw_scl]{complete positive whole-number scalar} giving the number of generations back in the function call stack to go.
#' @return \itemize{
#'   \item **`vexists`**: a logical scalar.
#'   \item **`vget`**: an R object.
#'   \item **`vset`**: `NULL` (called for side effect).
#' }
#' @export
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
vexists <- function(name, err = T, gens = 1) {
  errs <- c(f0(cmp_chr_scl(name), NULL, "\n \u2022 [name] must be a complete character scalar (?cmp_chr_scl)."),
            f0(isTF(err)        , NULL, "\n \u2022 [err] must be TRUE or FALSE."),
            f0(cmp_psw_scl(gens), NULL, "\n \u2022 [gens] must be a positive whole number scalar (?cmp_psw_scl)."))
  if (!is.null(errs)) {stop(errs)}
  env <- parent.frame(gens + 1)
  if (exists(name, env, inherits = F)) {return(T)}
  if (!err) {return(F)}
  if (gens > 1) {targ <- c("target function (", callers(gens + 1), ") ", gens, " generations back from the ")}
  else {targ <- ""}
  call <- callers(1)
  stop("\n \u2022 No object named [name = '", name, "'] exists in the environment of the ", targ, "calling function (", call, ").")
}

#' @rdname envir_vals
#' @export
vget <- function(name, err = T, gens = 1) {
  errs <- c(f0(cmp_chr_scl(name), NULL, "\n \u2022 [name] must be a complete character scalar (?cmp_chr_scl)."),
            f0(isTF(err)        , NULL, "\n \u2022 [err] must be TRUE or FALSE."),
            f0(cmp_psw_scl(gens), NULL, "\n \u2022 [gens] must be a positive whole number scalar (?cmp_psw_scl)."))
  if (!is.null(errs)) {stop(errs)}
  if (!vexists(name, err, gens)) {NULL}
  else {get(name, envir = parent.frame(gens + 1), inherits = F)}
}

#' @rdname envir_vals
#' @export
vset <- function(name, val, gens = 1) {
  errs <- c(f0(cmp_chr_scl(name), NULL, "\n \u2022 [name] must be a complete character scalar (?cmp_chr_scl)."),
            f0(cmp_psw_scl(gens), NULL, "\n \u2022 [gens] must be a positive whole number scalar (?cmp_psw_scl)."))
  if (!is.null(errs)) {stop(errs)}
  assign(name, val, envir = parent.frame(gens + 1), inherits = F)
}
