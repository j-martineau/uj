% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mmm.R
\encoding{UTF-8}
\name{mmm}
\alias{mmm}
\alias{mmm_funs}
\alias{mmm_props}
\alias{is_mmm_spec}
\alias{MMM}
\alias{CH1}
\alias{CH3}
\alias{CHR}
\alias{CLR}
\alias{EVN}
\alias{FAC}
\alias{FRC}
\alias{IND}
\alias{LGL}
\alias{NEG}
\alias{NGW}
\alias{NNG}
\alias{NNW}
\alias{NPS}
\alias{NPW}
\alias{NST}
\alias{NUM}
\alias{ODD}
\alias{ORD}
\alias{PCT}
\alias{POS}
\alias{PPN}
\alias{PSW}
\alias{SRT}
\alias{STR}
\alias{UNO}
\alias{WHL}
\title{Extended mode (xmode) properties}
\usage{
mmm(x)

mmm_funs()

mmm_props()

is_mmm_spec(spec)

MMM(x, spec, ...)

CH1(x, ...)

CH3(x, ...)

CHR(x, ...)

CLR(x, ...)

EVN(x, ...)

FAC(x, ...)

FRC(x, ...)

IND(x, ...)

LGL(x, ...)

NEG(x, ...)

NGW(x, ...)

NNG(x, ...)

NNW(x, ...)

NPS(x, ...)

NPW(x, ...)

NST(x, ...)

NUM(x, ...)

ODD(x, ...)

ORD(x, ...)

PCT(x, ...)

POS(x, ...)

PPN(x, ...)

PSW(x, ...)

SRT(x, ...)

STR(x, ...)

UNO(x, ...)

WHL(x, ...)
}
\arguments{
\item{x}{An R object.}

\item{spec}{\code{NULL} or a \link[=cmp_chr_scl]{complete character vec} containing one or more xmode properties from \code{mmm_props()}. xmode specs may be pipe-delimited. If there are multiple properties in \code{spec}, \code{x} is inspected for a match to any of the specified properties.}

\item{...}{
  Arguments passed on to \code{\link[=meets]{meets}}
  \describe{
    \item{\code{}}{}
  }}
}
\value{
\strong{A character vector} \cr\cr \verb{mmm_props, mmm_funs, mmm}
\cr\cr  \strong{A logical scalar}   \cr\cr \verb{is_mmm_spec, \{MMM\}, MMM}
}
\description{
xmodes are defined for non-empty atomic objects. For all other objects, the xmode is \code{NULL}. These are not formally defined modes, but are evaluated dynamically based on the current characteristics of an object.
\cr\cr Atomic objects that contain only \code{NA} values are of every xmode (they can be coerced to any mode without introducing new \code{NA} values).
\cr\cr \strong{Generic xmode}
\tabular{ll}{  \verb{'atm', 'ATM'}   \tab atomic (both a \code{\link[=bbb]{basic}} and an xmode property).}
\strong{Character xmodes}
\tabular{ll}{  \verb{'chr', 'CHR'}   \tab character                          \cr
  \verb{'ch1', 'CH1'}   \tab onechar, or \code{all(nchar(x) == 1)}   \cr
  \verb{'ch3', 'CH3'}   \tab threechar, or \code{all(nchar(x) == 3)} \cr
  \verb{'clr', 'CLR'}   \tab color (valid color values)         \cr
  \verb{'str', 'STR'}   \tab string (no blanks)                   }
\strong{Categorical xmodes}
\tabular{ll}{  \verb{'fac', 'FAC'}   \tab factor         \cr
  \verb{'lgl', 'LGL'}   \tab logical        \cr
  \verb{'ord', 'ORD'}   \tab ordered factor \cr
  \verb{'uno', 'UNO'}   \tab unordered factor }
\strong{Basic numeric xmodes}
\tabular{ll}{  \verb{'num', 'NUM'}   \tab numeric    \cr
  \verb{'frc', 'FRC'}   \tab fractional \cr
  \verb{'whl', 'WHL'}   \tab whole number }
\strong{Value-restricted numeric xmodes}
\tabular{ll}{  \verb{'pct', 'PCT'}   \tab percentage (\code{0} to \code{100}) \cr
  \verb{'ppn', 'PPN'}   \tab proportion (\code{0-1})        \cr
  \verb{'pos', 'POS'}   \tab positive                  \cr
  \verb{'neg', 'NEG'}   \tab negative                  \cr
  \verb{'nng', 'NNG'}   \tab non-negative              \cr
  \verb{'nps', 'NPS'}   \tab non-positive                }
\strong{Whole-number value-restricted xmodes}
\tabular{ll}{  \verb{'evn', 'EVN'}   \tab even               \cr
  \verb{'odd', 'ODD'}   \tab odd                \cr
  \verb{'ngw', 'NGW'}   \tab negative whole     \cr
  \verb{'psw', 'PSW'}   \tab positive whole     \cr
  \verb{'npw', 'NPW'}   \tab non-positive whole \cr
  \verb{'nnw', 'NNW'}   \tab non-negative whole   }
\strong{Combination xmodes}
\tabular{ll}{  \verb{'ind', 'IND'}   \tab indexer (\code{'lgl'}, \code{'psw'})           \cr
  \verb{'srt', 'SRT'}   \tab sortable (\code{'chr'}, \code{'num'}, \code{'ord'}) \cr
  \verb{'nst', 'NST'}   \tab non-sortable atomic                    }
}
\details{
\tabular{ll}{  \code{is_mmm_spec}    \tab Is \code{spec} an xmode specification?                                                                                  \cr   \tab   \cr
  \code{mmm_props}      \tab What xmode properties are there?                                                                                   \cr   \tab   \cr
  \code{mmm_funs}       \tab What xmode property functions are there?                                                                           \cr   \tab   \cr
  \code{{MMM}}          \tab Is \code{x} a match to the single xmode property \code{'{MMM}'} where \code{{MMM}} is a placeholder for any given xmode property? \cr   \tab   \cr
  \code{MMM}            \tab Is \code{x} a match to the xmode property spec in \code{spec}?                                                               \cr   \tab   \cr
  \code{mmm}            \tab What are \code{x}'s xmode properties?                                                                                                  }
}
\section{Specifying count and value restrictions}{
 Specifying restrictions in \code{...} is optional. The full set of recognized arguments names are defined in the following table along with the properties each specifies:
\tabular{ll}{  \verb{.MAX, .MAXR, .MAXC}   \tab Scalar maximum valid numbers of elements, rows, and columns, respectively.                                                                 \cr   \tab   \cr
  \verb{.MIN, .MINR, .MINC}   \tab Scalar minimum valid numbers of elements, rows, and columns, respectively.                                                                 \cr   \tab   \cr
  \verb{.LT, .LE, .GE, .GT}   \tab \link[=cmp_srt_scl]{Complete sortable scalar} less-than, less-than-or-equal, greater-than-or-equal, and greater-than bounds, respectively. \cr   \tab   \cr
  \verb{.N, .NR, .NC}         \tab A vector of valid numbers of elements, rows, and columns, respectively.                                                                    \cr   \tab   \cr
  \code{.VALS}                \tab A vector of valid values.                                                                                                                                 }
}

\examples{
mmm_funs()
mmm_props()
mmm(letters)
mmm(1:10)
mmm(c(pi, log(10), exp(1)))
is_mmm_spec("invalid")
is_mmm_spec("psw|srt")
MMM(1:10, "psw|srt")
MMM(letters, "ch1")
MMM(letters, "srt")
MMM(1:10, "ch3")
CH1(letters)
SRT(letters)
CLR(1:10)
CLR("blue")
CLR("#1077ACFF")
}
\seealso{
Other properties: 
\code{\link{as_mmm}()},
\code{\link{bbb_mmm}()},
\code{\link{bbb}()},
\code{\link{cls}()},
\code{\link{cmp_ccc}()},
\code{\link{cmp_mmm}()},
\code{\link{comparable}()},
\code{\link{compatible}()},
\code{\link{ddd}()},
\code{\link{eee}()},
\code{\link{iii}()},
\code{\link{is_unique}()},
\code{\link{meets}()},
\code{\link{mmm_ccc}()},
\code{\link{ppp_fast}()},
\code{\link{ppp}()},
\code{\link{sss}()},
\code{\link{stopperr}()},
\code{\link{unq_ccc}()}
}
\concept{properties}
