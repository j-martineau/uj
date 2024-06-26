% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compat.R
\encoding{UTF-8}
\name{compat}
\alias{compat}
\title{Evaluate Whether \code{x} and \code{y} Are of Compatible Modes}
\usage{
compat(x, y)
}
\arguments{
\item{x, y}{Objects to evaluate.}
}
\value{
A logical scalar.
egCompat <- function() {
chr1 <- letters
chr2 <- base::as.character(0:9)
num1 <- 0:9
num2 <- pi
lgl1 <- base::sample(base::c(T, F), 10, replace = T)
lgl2 <- F
uno1 <- base::factor(letters)
uno2 <- base::factor(base::sample(uno1, size = 26))
uno3 <- base::factor(0:9)
ord1 <- base::ordered(letters, levels = letters)
ord2 <- base::ordered(base::sample(ord1, 26), levels = letters)
ord3 <- base::ordered(LETTERS, levels = LETTERS)
ord4 <- base::ordered(LETTERS, levels = sample(ord3, 26))
base::list(chr1.chr2 = uj::compat(chr1, chr2),
chr1.ord1 = uj::compat(chr1, num1),
num1.num2 = uj::compat(num1, num2),
num1.ord1 = uj::compat(num1, ord1),
lgl1.lgl2 = uj::compat(lgl1, lgl2),
lgl1.ord1 = uj::compat(lgl1, ord1),
uno1.uno2 = uj::compat(uno1, uno2),
uno1.uno3 = uj::compat(uno1, uno3),
uno1.ord1 = uj::compat(uno1, ord1),
ord1.ord2 = uj::compat(ord1, ord2),
ord1.ord3 = uj::compat(ord1, ord3),
ord3.ord4 = uj::compat(ord3, ord4))
}

egCompat()
}
\description{
Evaluate Whether \code{x} and \code{y} Are of Compatible Modes
}
