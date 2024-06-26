% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hexdec.R
\encoding{UTF-8}
\name{hex_dec_help}
\alias{hex_dec_help}
\alias{to_dec}
\alias{to_hex}
\alias{todec}
\alias{tohex}
\title{Convert non-negative whole numbers between decimal and hexadecimal}
\usage{
hex_dec_help()

to_dec(x)

to_hex(x)

todec(x)

tohex(x)
}
\arguments{
\item{x}{A \link[=cmp_nnw]{complete non-negative whole-number object} (\code{to_hex} and \code{tohex}) or a \link[=cmp_chr]{complete character vec} containing only non-negative whole-number hexademical values (\code{to_dec} or \code{todec}).}
}
\description{
Convert non-negative whole numbers between decimal and hexadecimal
}
\section{Functions}{
\itemize{
\item \code{to_dec()}: Convert hexadecimal numerals to decimal numerals. Returns a non-negative whole-number object.

\item \code{to_hex()}: Convert decimal numerals to hexadecimal numerals (\emph{not} prefixed by \code{'#'}). Returns a character object of non-negative whole number hexadecimal values.

\item \code{todec()}: An alias for \code{to_dec.}

\item \code{tohex()}: An aliax for \code{to_hex.}

}}
\examples{
egHexDec <- function() {
  RGBA   <- col2rgb("orange", alpha = 0.5)
  HEX1   <- rgb(RGBA[1] / 255, RGBA[2] / 255, RGBA[3] / 255, RGBA[4] / 255)
  HEX1   <- c(substr(HEX1, 2, 3), substr(HEX1, 4, 5), substr(HEX1, 6, 7), substr(HEX1, 8, 9))
  DEC1   <- to_dec(HEX1)
  HEX2   <- to_hex(DEC1)
  HEX12A <- tohex(12)
  DEC12A <- todec(HEX12A)
  HEX12B <- tohex(DEC12A)
  list(RGBA = RGBA, HEX1 = HEX2, HEX12A = HEX12A, HEX12B = HEX12B, DEC1 = DEC1, DEC12A = DEC12A)
}
egHexDec <- egHexDec()
egHexDec
}
\seealso{
Other conversions: 
\code{\link{rng2lev}()},
\code{\link{tocase}()},
\code{\link{u2u_help}()}
}
\concept{conversions}
