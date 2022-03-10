#' @name delim
#' @family strings
#' @title Delimit strings
#' @description Simplifies and extends \code{paste} and \code{paste0}, allowing
#'   for 'delimiting across' and 'delimiting within' arguments in \code{...},
#'   and a combination of delimiting within and across arguments, in any order.
#'   Also offers convenience functions for specific delimiters.
#' @details \strong{code{da}}
#'   \cr Delimits across corresponding elements of arguments in \code{...}.
#'   \code{da(..., a. = value)} is identical to \code{paste(..., sep = value)}.
#'   \cr\cr
#'   \strong{\code{dw}}
#'   \cr Delimits within arguments in \code{...}. \code{dw(..., w. = value)} is
#'   equivalent to calling \code{sapply(list(...), paste0, collapse = value)}.
#'   \cr\cr
#'   \strong{\code{daw}}
#'   \cr Delimits across then within \code{daw(..., a. = a.val, w. = w.val)} is
#'   identical to \code{paste(..., sep = a.val, collapse = w.val)}
#'   \cr\cr
#'   \strong{\code{dwa}}
#'   \cr Delimits within then across \code{dwa(..., w. = w.val, a. = a.val)} is
#'   equivalent to \code{paste(sapply(list(...), paste0, collapse = dw), sep =
#'   a.val)}.
#'   \cr\cr
#'   \strong{Extensions}
#'   \cr Functions are extended for specific delimiters, signified by the
#'   following extension characters:
#'   \tabular{lll}{
#'     \strong{Character}\tab\strong{Name}\tab\strong{Delimiter Invoked}
#'     \cr\code{'0'}     \tab Blank       \tab\code{""}
#'     \cr\code{'1'}     \tab Space       \tab\code{" "}
#'     \cr\code{'D'}     \tab Dot         \tab\code{"."}
#'     \cr\code{'P'}     \tab Pipe        \tab\code{"|"}
#'     \cr\code{'B'}     \tab Broken Pipe \tab\code{"¦"}
#'   }
#'   \strong{\code{daA}}
#'   \cr Delimits across using the extension character A.
#'   \cr\cr
#'   \strong{\code{dwW}}
#'   \cr Delimits within using the extension character W.
#'   \cr\cr
#'   \strong{\code{dawAW}}
#'   \cr Delimits across using the extension character A then delimits within
#'   using the extension character W.
#'   \cr\cr
#'   \strong{\code{dwaWA}}
#'   \cr Delimits within using the extension character W then delimits across
#'   using the extension character A.
#' @param ... An arbitrary number of atomic vector arguments to be delimited.
#'   Argument in \code{...} must be recyclable for \code{da}, \code{daw},
#'   \code{dax}, and \code{dawxy}.
#' @param a.,w. Character scalar across delimiter vs. within delimiter,
#'   respectively. Defaults to a blank string (i.e., \code{""}).
#' @return A character scalar, character vector, or list of character vectors.
#' @export
da <- function(..., a. = " ") {
  x <- list(...)
  N <- length(x)
  L <- lengths(x)
  VN <- N > 0
  VL <- f0(!VN, T, all(L > 0))
  VX <- f0(!VN | !VL, T, all(sapply(x, xvec)))
  VR <- f0(!VX, T, all(round(max(N) / N) == max(N) / N))
  VD <- chr_scl(a.)
  E  <- NULL
  if (!VN) {E <- c(E, "\n  * [...] is empty.")}
  if (!VL) {E <- c(E, "\n  * [...] contains an empty element.")}
  if (!VX) {E <- c(E, "\n  * Arguments in [...] must be non-empty atomic vectors.")}
  if (!VR) {E <- c(E, "\n  * Arguments in [...] are not recyclable.")}
  if (!VD) {E <- c(E, "\n  * [a.] must be a non-NA character scalar.")}
  if (xdef(E)) {stop(E)}
  paste(..., sep = a.)
}

#' @rdname delim
#' @export
dw <- function(..., w. = " ") {
  x <- list(...)
  N <- length(x)
  L <- lengths(x)
  VN <- N > 0
  VL <- f0(!VN, T, all(L > 0))
  VX <- f0(!VN | !VL, T, all(sapply(x, xvec)))
  VD <- chr_scl(w.)
  E <- NULL
  if (!VN) {E <- c(E, "\n  * [...] is empty.")}
  if (!VL) {E <- c(E, "\n  * [...] contains an empty element.")}
  if (!VX) {E <- c(E, "\n  * Arguments in [...] must be non-empty atomic vectors.")}
  if (!VD) {E <- c(E, "\n  * [w.] must be a non-NA character scalar.")}
  if (xdef(E)) {stop(E)}
  sapply(list(...), paste0, collapse = w.)
}

#' @rdname delim
#' @export
daw <- function(..., a. = "", w. = "") {
  x <- list(...)
  N <- length(x)
  L <- lengths(x)
  VN <- N > 0
  VL <- f0(!VN, T, all(L > 0))
  VX <- f0(!VN | !VL, T, all(sapply(x, xvec)))
  VR <- f0(!VX, T, all(round(max(N) / N) == max(N) / N))
  VA <- chr_scl(a.)
  VW <- chr_scl(w.)
  E  <- NULL
  if (!VN) {E <- c(E, "\n  * [...] is empty.")}
  if (!VL) {E <- c(E, "\n  * [...] contains an empty element.")}
  if (!VX) {E <- c(E, "\n  * Arguments in [...] must be non-empty atomic vectors.")}
  if (!VR) {E <- c(E, "\n  * Arguments in [...] are not recyclable.")}
  if (!VA) {E <- c(E, "\n  * [a.] must be a non-NA character scalar.")}
  if (!VW) {E <- c(E, "\n  * [w.] must be a non-NA character scalar.")}
  if (xdef(E)) {stop(E)}
  paste0(paste(..., sep = a.), collapse = w.)
}

#' @rdname delim
#' @export
dwa <- function(..., w. = "", a. = "") {
  x  <- list(...)
  N  <- length(x)
  L  <- lengths(x)
  VN <- N > 0
  VL <- f0(!VN, T, all(L > 0))
  VX <- f0(!VN | !VL, T, all(sapply(x, xvec)))
  VW <- chr_scl(w.)
  VA <- chr_scl(a.)
  E  <- NULL
  if (!VN) {E <- c(E, "\n  * [...] is empty.")}
  if (!VL) {E <- c(E, "\n  * [...] contains an empty element.")}
  if (!VX) {E <- c(E, "\n  * Arguments in [...] must be non-empty atomic vectors.")}
  if (!VW) {E <- c(E, "\n  * [w.] must be a non-NA character scalar.")}
  if (!VA) {E <- c(E, "\n  * [a.] must be a non-NA character scalar.")}
  if (xdef(E)) {stop(E)}
  paste(sapply(list(...), paste0, collapse = w.), sep = a.)
}

#' @rdname delim
#' @export
da0 <- function(...) {da(..., a. = "")}

#' @rdname delim
#' @export
da1 <- function(...) {da(..., a. = " ")}

#' @rdname delim
#' @export
daD <- function(...) {da(..., a. = ".")}

#' @rdname delim
#' @export
daP <- function(...) {da(..., a. = "|")}

#' @rdname delim
#' @export
daB <- function(...) {da(..., a. = "¦")}

#' @rdname delim
#' @export
dw0 <- function(...) {dw(..., w. = "")}

#' @rdname delim
#' @export
dw1 <- function(...) {dw(..., w. = " ")}

#' @rdname delim
#' @export
dwD <- function(...) {dw(..., w. = ".")}

#' @rdname delim
#' @export
dwP <- function(...) {dw(..., w. = "|")}

#' @rdname delim
#' @export
dwB <- function(...) {dw(..., w. = "¦")}

#' @rdname delim
#' @export
daw00 <- function(...) {daw(..., a. = "", w. = "")}

#' @rdname delim
#' @export
daw01 <- function(...) {daw(..., a. = "", w. = " ")}

#' @rdname delim
#' @export
daw0D <- function(...) {daw(..., a. = "", w. = ".")}

#' @rdname delim
#' @export
daw0P <- function(...) {daw(..., a. = "", w. = "|")}

#' @rdname delim
#' @export
daw0B <- function(...) {daw(..., a. = "", w. = "¦")}

#' @rdname delim
#' @export
daw10 <- function(...) {daw(..., a. = " ", w. = "")}

#' @rdname delim
#' @export
daw11 <- function(...) {daw(..., a. = " ", w. = " ")}

#' @rdname delim
#' @export
daw1D <- function(...) {daw(..., a. = " ", w. = ".")}

#' @rdname delim
#' @export
daw1P <- function(...) {daw(..., a. = " ", w. = "|")}

#' @rdname delim
#' @export
daw1B <- function(...) {daw(..., a. = " ", w. = "¦")}

#' @rdname delim
#' @export
dawD0 <- function(...) {daw(..., a. = ".", w. = "")}

#' @rdname delim
#' @export
dawD1 <- function(...) {daw(..., a. = ".", w. = " ")}

#' @rdname delim
#' @export
dawDD <- function(...) {daw(..., a. = ".", w. = ".")}

#' @rdname delim
#' @export
dawDP <- function(...) {daw(..., a. = ".", w. = "|")}

#' @rdname delim
#' @export
dawDB <- function(...) {daw(..., a. = ".", w. = "¦")}

#' @rdname delim
#' @export
dawP0 <- function(...) {daw(..., a. = "|", w. = "")}

#' @rdname delim
#' @export
dawP1 <- function(...) {daw(..., a. = "|", w. = " ")}

#' @rdname delim
#' @export
dawPD <- function(...) {daw(..., a. = "|", w. = ".")}

#' @rdname delim
#' @export
dawPP <- function(...) {daw(..., a. = "|", w. = "|")}

#' @rdname delim
#' @export
dawPB <- function(...) {daw(..., a. = "|", w. = "¦")}

#' @rdname delim
#' @export
dawB0 <- function(...) {daw(..., a. = "¦", w. = "")}

#' @rdname delim
#' @export
dawB1 <- function(...) {daw(..., a. = "¦", w. = " ")}

#' @rdname delim
#' @export
dawBD <- function(...) {daw(..., a. = "¦", w. = ".")}

#' @rdname delim
#' @export
dawBP <- function(...) {daw(..., a. = "¦", w. = "|")}

#' @rdname delim
#' @export
dawBB <- function(...) {daw(..., a. = "¦", w. = "¦")}

#' @rdname delim
#' @export
dwa00 <- function(...) {dwa(..., w. = "", a. = "")}

#' @rdname delim
#' @export
dwa01 <- function(...) {dwa(..., w. = "", a. = " ")}

#' @rdname delim
#' @export
dwa0D <- function(...) {dwa(..., w. = "", a. = ".")}

#' @rdname delim
#' @export
dwa0P <- function(...) {dwa(..., w. = "", a. = "|")}

#' @rdname delim
#' @export
dwa0B <- function(...) {dwa(..., w. = "", a. = "¦")}

#' @rdname delim
#' @export
dwa10 <- function(...) {dwa(..., w. = " ", a. = "")}

#' @rdname delim
#' @export
dwa11 <- function(...) {dwa(..., w. = " ", a. = " ")}

#' @rdname delim
#' @export
dwa1D <- function(...) {dwa(..., w. = " ", a. = ".")}

#' @rdname delim
#' @export
dwa1P <- function(...) {dwa(..., w. = " ", a. = "|")}

#' @rdname delim
#' @export
dwa1B <- function(...) {dwa(..., w. = " ", a. = "¦")}

#' @rdname delim
#' @export
dwaD0 <- function(...) {dwa(..., w. = ".", a. = "")}

#' @rdname delim
#' @export
dwaD1 <- function(...) {dwa(..., w. = ".", a. = " ")}

#' @rdname delim
#' @export
dwaDD <- function(...) {dwa(..., w. = ".", a. = ".")}

#' @rdname delim
#' @export
dwaDP <- function(...) {dwa(..., w. = ".", a. = "|")}

#' @rdname delim
#' @export
dwaDB <- function(...) {dwa(..., w. = ".", a. = "¦")}

#' @rdname delim
#' @export
dwaP0 <- function(...) {dwa(..., w. = "|", a. = "")}

#' @rdname delim
#' @export
dwaP1 <- function(...) {dwa(..., w. = "|", a. = " ")}

#' @rdname delim
#' @export
dwaPD <- function(...) {dwa(..., w. = "|", a. = ".")}

#' @rdname delim
#' @export
dwaPP <- function(...) {dwa(..., w. = "|", a. = "|")}

#' @rdname delim
#' @export
dwaPB <- function(...) {dwa(..., w. = "|", a. = "¦")}

#' @rdname delim
#' @export
dwaB0 <- function(...) {dwa(..., w. = "¦", a. = "")}

#' @rdname delim
#' @export
dwaB1 <- function(...) {dwa(..., w. = "¦", a. = " ")}

#' @rdname delim
#' @export
dwaBD <- function(...) {dwa(..., w. = "¦", a. = ".")}

#' @rdname delim
#' @export
dwaBP <- function(...) {dwa(..., w. = "¦", a. = "|")}

#' @rdname delim
#' @export
dwaBB <- function(...) {dwa(..., w. = "¦", a. = "¦")}
