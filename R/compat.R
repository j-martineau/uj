#' @encoding UTF-8
#' @title Evaluate Whether `x` and `y` Are of Compatible Modes
#' @param x,y Objects to evaluate.
#' @return A logical scalar.
#' egCompat <- function() {
#'   chr1 <- letters
#'   chr2 <- base::as.character(0:9)
#'   num1 <- 0:9
#'   num2 <- pi
#'   lgl1 <- base::sample(base::c(T, F), 10, replace = T)
#'   lgl2 <- F
#'   uno1 <- base::factor(letters)
#'   uno2 <- base::factor(base::sample(uno1, size = 26))
#'   uno3 <- base::factor(0:9)
#'   ord1 <- base::ordered(letters, levels = letters)
#'   ord2 <- base::ordered(base::sample(ord1, 26), levels = letters)
#'   ord3 <- base::ordered(LETTERS, levels = LETTERS)
#'   ord4 <- base::ordered(LETTERS, levels = sample(ord3, 26))
#'   base::list(chr1.chr2 = uj::compat(chr1, chr2),
#'              chr1.ord1 = uj::compat(chr1, num1),
#'              num1.num2 = uj::compat(num1, num2),
#'              num1.ord1 = uj::compat(num1, ord1),
#'              lgl1.lgl2 = uj::compat(lgl1, lgl2),
#'              lgl1.ord1 = uj::compat(lgl1, ord1),
#'              uno1.uno2 = uj::compat(uno1, uno2),
#'              uno1.uno3 = uj::compat(uno1, uno3),
#'              uno1.ord1 = uj::compat(uno1, ord1),
#'              ord1.ord2 = uj::compat(ord1, ord2),
#'              ord1.ord3 = uj::compat(ord1, ord3),
#'              ord3.ord4 = uj::compat(ord3, ord4))
#' }
#'
#' egCompat()
#' @export
compat <- function(x, y) {
  if (base::is.factor(x) & base::is.factor(y)) {
    xLevels <- base::levels(x)
    yLevels <- base::levels(y)
    if (base::length(xLevels) != base::length(yLevels)) {F}
    else if ( base::is.ordered(x) &  base::is.ordered(y) & base::all(xLevels == yLevels   )) {T}
    else if (!base::is.ordered(x) & !base::is.ordered(y) & base::setequal(xLevels, yLevels)) {T}
    else {F}
  } else if (base::is.character(x) & base::is.character(y)) {T}
  else if (base::is.logical(x) & base::is.logical(y)) {T}
  else if (base::is.numeric(x) & base::is.numeric(y)) {T}
  else {F}
}

#' @encoding UTF-8
#' @title Simple Oxford-Comma Lists
#' @param x A character vector to be formatted as an Oxford-comma list.
#' @param join A character scalar conjunction for between the next to last and last elements of `x`.
#' @return A character scalar.
#' egOxVals <- function() {
#'   base::list(and.letters = uj::ox_vals(letters, "and"),
#'              or.digits   = uj::ox_vals(0:9    , "or" ))
#' }
#'
#' egOxVals()
#' @export
ox_vals <- function(x, join) {
  n <- base::length(x)
  if (n == 1) {x} else if (n == 2) {base::paste0(x[1], " ", join, " ", x[2])} else {base::paste0(base::paste0(x[1:(n - 1)], collapse = ", "), ", ", join, " ", x[n])}
}
