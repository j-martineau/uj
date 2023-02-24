#' @encoding UTF-8
#' @family environments
#' @family extensions
#' @title Objects in environments of calling functions
#' @description All functions in this family operate in the environment of the calling function `gens` generations back in the call stack.
#' \tabular{ll}{  `value_exists, val_exists, vexists`   \tab Checks for the existence of an object in the environment of the `gens`-specified calling function. \cr   \tab   \cr
#'                `get_value, get_val, vget`            \tab Gets the value of an object in the environment of the `gens`-specified calling function.           \cr   \tab   \cr
#'                `set_value, set_val, vset`            \tab Sets the value of an object in the environment of the `gens`-specified calling function.                          }
#' @param name A \link[=cmp_chr_scl]{complete character scalar} giving the name of an object.
#' @param val A value to place into the object specified by `name`.
#' @param err A non`NA` logical scalar indicating whether to throw an error if the object specified by `name` does not exist (i.e., rather than returning `FALSE`).
#' @param gens A \link[=cmp_psw_scl]{complete positive whole-number scalar} giving the number of generations back in the function call stack to go.
#' @return **A logical scalar**      \cr\cr `value_exists, val_exists, vexists`
#' \cr\cr  **The** `NULL` **object** \cr\cr `set_value, set_val, vset`
#' \cr\cr  **An object**             \cr\cr `get_value, get_val, vget`
#' @examples
#' egFunC <- function() {
#'   parent <- callers(1)
#'   grand  <- callers(2)
#'
#'   say("\n Parent Function: '", parent, "'")
#'   say("\n Grandparent Function: '", grand , "'")
#'   say("\n")
#'   say("\n Variable 'A' exists in parent function '", parent, "': ", val_exists('A', err = FALSE, gens = 1))
#'   say("\n Variable 'B' exists in parent function '", parent, "': ", val_exists('B', err = FALSE, gens = 1))
#'   say("\n")
#'   say("\n Variable 'A' exists in grandparent function '", grand, "': ", val_exists('A', err = FALSE, gens = 2))
#'   say("\n Variable 'B' exists in grandparent function '", grand, "': ", val_exists('B', err = FALSE, gens = 2))
#'   say("\n")
#'   say("\n Value of variable 'B' in parent function '", parent, "': ", get_value('B', err = FALSE, gens = 1))
#'   say("\n Value of variable 'A' in grandparent function '", grand, "': ", get_value('A', err = FALSE, gens = 2))
#'
#'   vSET('A', 'A', gens = 1)
#'   vSET('B', 'B', gens = 2)
#'
#'   say("\n")
#'   say("\n vSET('A', 'A', gens = 1)")
#'   say("\n vSET('B', 'B', gens = 2)")
#'   say("\n")
#'   say("\n Variable 'A' exists in parent function '", parent, "': ", val_exists('A', err = FALSE, gens = 1))
#'   say("\n Variable 'B' exists in parent function '", parent, "': ", val_exists('B', err = FALSE, gens = 1))
#'   say("\n")
#'   say("\n Variable 'A' exists in grandparent function '", grand, "': ", val_exists('A', err = FALSE, gens = 2))
#'   say("\n Variable 'B' exists in grandparent function '", grand, "': ", val_exists('B', err = FALSE, gens = 2))
#'   say("\n")
#'   say("\n Value of variable 'A' in parent function '", parent, "': ", get_value('A', err = FALSE, gens = 1))
#'   say("\n Value of variable 'B' in parent function '", parent, "': ", get_value('B', err = FALSE, gens = 1))
#'   say("\n")
#'   say("\n Value of variable 'A' in grandparent function '", grand, "': ", get_value('A', err = FALSE, gens = 2))
#'   say("\n Value of variable 'B' in grandparent function '", grand, "': ", get_value('B', err = FALSE, gens = 2))
#' }
#' egFunB <- function() {B <- 1; egFunC()}
#' egFunA <- function() {A <- 0; egFunB()}
#' egFunA()
#' @export
value_exists <- function(name, err = T, gens = 1) {
  uj::errs_if_nots(uj::cmp_chr_scl(name), "[name] must be a complete character scalar (?cmp_chr_scl)."   ,
                   uj::cmp_lgl_scl(err) , "[err] must be TRUE or FALSE."                                 ,
                   uj::cmp_psw_scl(gens), "[gens] must be a positive whole number scalar (?cmp_psw_scl).", PKG = "uj")
  if (!base::exists(name, base::parent.frame(gens + 1), inherits = F)) {
    call <- uj::caller()
    targ <- uj::callers(gens + 1)
    targ <- uj::f0(gens > 1, base::c("target function (", targ, ") ", gens, " generations back from the "), "")
    uj::err_if_not(!err, "No object named [name = '", name, "'] exists in the environment of the ", targ, "calling function (", call, ").", PKG = "uj")
    F
  } else {T}
}

#' @rdname value_exists
#' @export
get_value <- function(name, err = T, gens = 1) {
  uj::errs_if_nots(uj::cmp_chr_scl(name), "[name] must be a complete character scalar (?cmp_chr_scl)."   ,
                   uj::cmp_lgl_scl(err) , "[err] must be TRUE or FALSE."                                 ,
                   uj::cmp_psw_scl(gens), "[gens] must be a positive whole number scalar (?cmp_psw_scl).", PKG = "uj")
  uj::f0(!uj::value_exists(name, err, gens + 1), NULL, base::get(name, envir = base::parent.frame(gens + 1), inherits = F))
}

#' @rdname value_exists
#' @export
set_value <- function(name, val, gens = 1) {
  uj::errs_if_nots(uj::cmp_chr_scl(name), "[name] must be a complete character scalar (?cmp_chr_scl)."   ,
                   uj::cmp_psw_scl(gens), "[gens] must be a positive whole number scalar (?cmp_psw_scl).", PKG = "uj")
  base::assign(name, val, envir = base::parent.frame(gens + 1), inherits = F)
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
vget <- get_value

#' @rdname value_exists
#' @export
vset <- set_value
