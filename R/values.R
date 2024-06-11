# PASSED TEST 05-08=24

# internals ####

.value_errs <- function(name, err = TRUE, gens = 1) {
  fun <- uj::caller()
  errs <- NULL
  if (!uj::.cmp_chr_scl(name)) {errs <- base::c(errs, "[name] must be a complete character scalar (?cmp_chr_scl).")}
  if (!uj::.cmp_psw_scl(gens)) {errs <- base::c(errs, "[gens] must be a positive whole number scalar (?cmp_psw_scl).")}
  if (!uj::.cmp_lgl_scl(err )) {errs <- base::c(errs, "[err] must be TRUE or FALSE.")}
  if (!base::is.null(errs)) {uj::stopperr(errs, fun = fun)}
}

# exported ####

#' @encoding UTF-8
#' @title Named values (unique to package `uj`)
#' @description Get names, definitions, and values of special `uj` package named values.
#' @details
#' \tabular{ll}{  `value_names, val_names, vnames`   \tab Get names of all package values.                                                                                                                                                                                        \cr   \tab   \cr
#'                `value_table, val_table, vtable`   \tab Get a data.frame of all `uj`-specific named values with three columns: `name`, `value`, and `description`, where named values with multiple elements are pipe-delimited and all values are converted to mode character. \cr   \tab   \cr
#'                `values, values, vs`               \tab Get a named list where names are the arguments submitted to `v(...)`, element values are the values of the contents of the named element, and the attribute `'descriptions'` gives associated verbose descriptions.     \cr   \tab   \cr
#'                `value, val, v`                    \tab Gets package values by name.                                                                                                                                                                                                           }
#' @param vec Logical scalar indicating whether to collapse multi-element values of the result using a pipe (|) delimiter.
#' @param ... Unquoted, comma-separated list of names of values to return. If multiple values are specified, they are coerced into a single atomic vector result.
#' @return **A character vector**         \cr\cr `value_names, val_names, vnames`
#' \cr\cr  **A data.frame**               \cr\cr `value_table, val_table, vtable`
#' \cr\cr  **A \link[=VLS]{vlist}**       \cr\cr `values, values, vs`
#' \cr\cr  **An atomic scalar or vector** \cr\cr `value, val, v`
#' @examples
#' egValues <- function() {
#'
#'   print(uj::v(.))
#'   print(uj::v(pipe, pipe1, eq))
#'   print(uj::vnames())
#'   print(uj::values())
#'   print(uj::vtable())
#'
#' }
#'
#' egValues()
#' @export
values_help <- function() {utils::help("values_help", package = "uj")}

#' @describeIn values_help Gets package values by name
#' @export
values <- function(vec = FALSE) {
  Out <- uj::.pkgVals
  base::attr(Out, "Defs") <- uj::.pkgDefs
  uj::f0(vec, uj::av(base::sapply(Out, base::paste0, collapse = "|")), Out)
}

#' @describeIn values_help Get names of all `uj` package constant values. Returns a character scalar.
#' @export
value_names <- function() {base::names(uj::values())}

#' @describeIn values_help Get plain language descriptions of `uj` package constant values. Returns a character scalar.
#' @export
value_descriptions <- function() {base::attr(uj::values(), "Descriptions")}

#' @describeIn values_help Builds a table of `uj` package constants and plain language descriptions.
#' @export
value_table <- function() {tibble::tibble(Name = uj::value_names(), Value = uj::values(vec = TRUE), Description = uj::value_descriptions())}

#' @describeIn values_help Gets a single package `uj` constant value.
#' @export
value <- function(...) {
  X <- base::as.character(base::match.call())
  X <- X[2:base::length(X)]
  uj::av(uj::values()[X])
}

#' @describeIn values_help An alias for `values`.
#' @export
vals <- values

#' @describeIn values_help An alias for `value_names`.
#' @export
val_names <- value_names

#' @describeIn values_help An alias for `value_table`.
#' @export
val_table <- value_table

#' @describeIn values_help An alias for `value`.
#' @export
val <- value

#' @describeIn values_help An alias for `values`
#' @export
vs <- values

#' @describeIn values_help An alias for `value_names`
#' @export
vnames <- value_names

#' @describeIn values_help An alias for `value_table`
#' @export
vtable <- value_table

#' @describeIn values_help An alias for `value`
#' @export
v <- value
