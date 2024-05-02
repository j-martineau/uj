#' @encoding UTF-8
#' @family extensions
#' @title Flexibly row bind two data frames
#' @description Checks for compatibility of variables with the same name in both variables. Adds `NA`-populated variables to each data frame as needed to allow for row binding. Variables in the resulting data.frame are sorted left to right in ascending order of variable name.
#' @param x,y Atomic data.frames.
#' @return A data.frame.
#' @export
flex_rbind <- function(x, y) {
  errs <- NULL
  if (!ppp::.atm_dtf(x)) {errs <- base::c(errs, "[x] must be an atomic data.frame (?atm_dtf).")}
  if (!ppp::.atm_dtf(y)) {errs <- base::c(errs, "[y] must be an atomic data.frame (?atm_dtf).")}
  if (uj::.DEF(errs)) {ppp::stopperr(errs, pkg = "uj")}
  xNames <- base::colnames(x)
  yNames <- base::colnames(y)
  sharedNames <- base::intersect(xNames, yNames)
  if (base::length(sharedNames) == 0) {ppp::stopperr("No variables in [x] and [y] have the same name.", pkg = "uj")}
  allNames <- base::unique(base::c(xNames, yNames))
  uniqueNamesY <- allNames[!(allNames %in% xNames)]
  uniqueNamesX <- allNames[!(allNames %in% yNames)]
  for (name in sharedNames) {if (!uj:::.compat(x[[name]], y[[name]])) {ppp::stopperr("Variables contained in both [x] and [y] must be compatible.", pkg = "uj")}}
  for (name in uniqueNamesX) {
    newCol <- tibble::tibble(y = base::rep.int(NA, base::nrow(y)))
    base::colnames(newCol) <- name
    y <- base::cbind(y, newCol)
  }
  for (name in uniqueNamesY) {
    newCol <- tibble::tibble(x = base::rep.int(NA, base::nrow(x)))
    base::colnames(newCol) <- name
    x <- base::cbind(x, newCol)
  }
  y <- y[ , allNames]
  x <- x[ , allNames]
  base::rbind(x, y)
}
