#' @encoding UTF-8
#' @title Convert an Item Score Block to an Item Score Matrix
#' @description Takes an item score block (a data frame with a single item score string variable) to an item score matrix.
#' @param x A data frame with a single item score string variable with one row per person.
#' @return An \eqn{n_{person} \times n_{item}} item score matrix.
#' @export
score_block_to_matrix <- function(x) {
  nc <- base::unique(base::nchar(x))
  if (base::length(nc) == 1) {
    split <- function(z) {
      z <- base::strsplit(z, "", fixed = T)
      z <- base::unlist(z, T, F)
      base::attributes(z) <- NULL
      base::matrix(z, ncol = 1)
    }
    y <- base::vapply(x, split, base::rep(NA_character_, nc), USE.NAMES = F)
    base::t(y)
  } else {uj::stopper("There are different numbers of item scores per person.")}
}
