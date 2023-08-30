#' @encoding UTF-8
#' @family environments
#' @family extensions
#' @title Objects in environments of calling functions
#' @description All functions in this family operate in the environment of the calling function `.GENS` generations back in the call stack.
#' \tabular{ll}{  `value_exists, val_exists, vexists`   \tab Checks for the existence of an object in the environment of the `.GENS`-specified calling function. \cr   \tab   \cr
#'                `get_value, get_val, getv`            \tab Gets the value of an object in the environment of the `.GENS`-specified calling function.           \cr   \tab   \cr
#'                `set_value, set_val, setv`            \tab Sets the value of an object in the environment of the `.GENS`-specified calling function.                          }
#' @param name A \link[=cmp_chr_scl]{complete character scalar} giving the name of an object.
#' @param val A value to place into the object specified by `name`.
#' @param .ERR A non`NA` logical scalar indicating whether to throw an error if the object specified by `name` does not exist (i.e., rather than returning `FALSE`).
#' @param .GENS A \link[=cmp_psw_scl]{complete positive whole-number scalar} giving the number of generations back in the function call stack to go.
#' @return **A logical scalar**      \cr\cr `value_exists, val_exists, vexists`
#' \cr\cr  **The** `NULL` **object** \cr\cr `set_value, set_val, setv`
#' \cr\cr  **An object**             \cr\cr `get_value, get_val, getv`
#' @examples
#' egFunC <- function() {
#'   parent <- callers(1)
#'   grand  <- callers(2)
#'
#'   say("\n Parent Function: '", parent, "'")
#'   say("\n Grandparent Function: '", grand , "'")
#'   say("\n")
#'   say("\n Variable 'A' exists in parent function '", parent, "': ", val_exists('A', .ERR = FALSE, .GENS = 1))
#'   say("\n Variable 'B' exists in parent function '", parent, "': ", val_exists('B', .ERR = FALSE, .GENS = 1))
#'   say("\n")
#'   say("\n Variable 'A' exists in grandparent function '", grand, "': ", val_exists('A', .ERR = FALSE, .GENS = 2))
#'   say("\n Variable 'B' exists in grandparent function '", grand, "': ", val_exists('B', .ERR = FALSE, .GENS = 2))
#'   say("\n")
#'   say("\n Value of variable 'B' in parent function '", parent, "': ", get_value('B', .ERR = FALSE, .GENS = 1))
#'   say("\n Value of variable 'A' in grandparent function '", grand, "': ", get_value('A', .ERR = FALSE, .GENS = 2))
#'
#'   vSET('A', 'A', .GENS = 1)
#'   vSET('B', 'B', .GENS = 2)
#'
#'   say("\n")
#'   say("\n vSET('A', 'A', .GENS = 1)")
#'   say("\n vSET('B', 'B', .GENS = 2)")
#'   say("\n")
#'   say("\n Variable 'A' exists in parent function '", parent, "': ", val_exists('A', .ERR = FALSE, .GENS = 1))
#'   say("\n Variable 'B' exists in parent function '", parent, "': ", val_exists('B', .ERR = FALSE, .GENS = 1))
#'   say("\n")
#'   say("\n Variable 'A' exists in grandparent function '", grand, "': ", val_exists('A', .ERR = FALSE, .GENS = 2))
#'   say("\n Variable 'B' exists in grandparent function '", grand, "': ", val_exists('B', .ERR = FALSE, .GENS = 2))
#'   say("\n")
#'   say("\n Value of variable 'A' in parent function '", parent, "': ", get_value('A', .ERR = FALSE, .GENS = 1))
#'   say("\n Value of variable 'B' in parent function '", parent, "': ", get_value('B', .ERR = FALSE, .GENS = 1))
#'   say("\n")
#'   say("\n Value of variable 'A' in grandparent function '", grand, "': ", get_value('A', .ERR = FALSE, .GENS = 2))
#'   say("\n Value of variable 'B' in grandparent function '", grand, "': ", get_value('B', .ERR = FALSE, .GENS = 2))
#' }
#' egFunB <- function() {B <- 1; egFunC()}
#' egFunA <- function() {A <- 0; egFunB()}
#' egFunA()
#' @export
value_exists <- function(name, .ERR = T, .GENS = 1) {
  uj:::.value_errs(name = name, .ERR = .ERR, .GENS = .GENS)
  if (!base::exists(name, base::parent.frame(.GENS + 1), inherits = F)) {
    Call <- uj::caller()
    Targ <- uj::callers(.GENS + 1)
    if (.GENS > 1) {Targ <- base::c("target function (", Targ, ") ", .GENS, " generations back from the ")} else {Targ <- ""}
    if (.ERR) {uj::stopperr(base::paste0("No object named [name = '", name, "'] exists in the environment of the ", Targ, "calling function (", Call, ")."), .PKG = "uj")}
    F
  } else {T}
}

#' @rdname value_exists
#' @export
get_value <- function(name, .ERR = T, .GENS = 1) {
  uj:::.value_errs(name = name, .ERR = .ERR, .GENS = .GENS)
  if (base::exists(name, base::parent.frame(.GENS + 1), inherits = F)) {base::get(name, envir = base::parent.frame(.GENS + 1), inherits = F)} else {NULL}
}

#' @rdname value_exists
#' @export
set_value <- function(name, val, .GENS = 1) {
  uj:::.value_errs(name = name, .ERR = F, .GENS = .GENS)
  base::assign(name, val, envir = base::parent.frame(.GENS + 1), inherits = F)
}

#' @rdname value_exists
#' @export
val_exists <- value_exists

#' @rdname value_exists
#' @export
get_val <- get_value

#' @rdname value_exists
#' @export
set_val <- set_value

#' @rdname value_exists
#' @export
vexists <- value_exists

#' @rdname value_exists
#' @export
getv <- get_value

#' @rdname value_exists
#' @export
setv <- set_value
