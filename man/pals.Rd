% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pals.R
\encoding{UTF-8}
\name{pals}
\alias{pals}
\alias{line_types}
\alias{fill_shapes}
\alias{open_shapes}
\alias{punc_shapes}
\alias{empty_shapes}
\alias{solid_shapes}
\alias{digit_shapes}
\alias{digit_shapes0}
\alias{letter_shapes}
\alias{LETTER_shapes}
\alias{colors_dark}
\alias{colors_bright}
\alias{colors_sensitive}
\alias{build_palette}
\alias{blank_shape}
\title{Plotting palettes}
\usage{
pals()

line_types(tot, unq = min(9, tot))

fill_shapes(tot, unq = min(5, tot))

open_shapes(tot, unq = min(5, tot))

punc_shapes(tot, unq = min(7, tot))

empty_shapes(tot, unq = min(5, tot))

solid_shapes(tot, unq = min(4, tot))

digit_shapes(tot, unq = min(9, tot))

digit_shapes0(tot, unq = min(10, tot))

letter_shapes(tot, unq = min(26, tot))

LETTER_shapes(tot, unq = min(26, tot))

colors_dark(tot, unq = min(6, tot))

colors_bright(tot, unq = min(6, tot))

colors_sensitive(tot, unq = min(12, tot))

build_palette(type, tot, unq)

blank_shape()
}
\arguments{
\item{tot}{A \link[=cmp_psw_scl]{complete positive whole-number scalar} indicating the number of values to return.}

\item{unq}{A positive whole-number scalar less than or equal to \code{tot} giving the number of unique values to be repeated until there are \code{tot} total values.}

\item{type}{A character scalar from \code{c('line.types', 'open.shapes', 'punc.shapes', 'fill.shapes','empty.shapes', 'solid.shapes', 'digit.shapes', 'digit.shapes0', 'letter.shapes', 'LETTER.shapes', 'colors.standard', 'colors.sensitive')}.}
}
\value{
\strong{A positive whole number vector} \cr\cr \code{fill_shapes}, \code{open_shapes}, \code{punc_shapes}, \code{empty_shapes}, \code{solid_shapes}, \code{digit_shapes}, \code{digit_shapes0}, \code{letter_shapes}, \verb{LETTER_shapes,} \emph{and possibly} \code{build_palette}
\cr\cr  \strong{A character vector}             \cr\cr \code{line_types}, \code{colors_dark}, \code{colors_bright}, \code{colors_sensitive} \emph{and possibly} \code{build_palette}
\cr\cr  \strong{A positive whole number scalar} \cr\cr \code{blank_shape}
}
\description{
Build line type, shape, and color palettes.
}
\details{
The functions in this family create palettes of various types from values of source vectors as defined below:
\tabular{lll}{
\strong{Function}           \tab \strong{Palette type}                                  \tab \strong{Source}            \cr
\code{colors_sensitive}     \tab 6 dark + 6 bright colorblind sensitive colors     \tab \code{v(colors.sensitive)} \cr
\code{colors_bright}        \tab 6 colorblind sensitive bright colors              \tab \code{v(colors.bright)}    \cr
\code{colors_dark}          \tab 6 colorblind-sensitive dark colors                \tab \code{v(colors.dark)}      \cr
\code{line_types}           \tab line types                                        \tab \code{v(line.types)}       \cr
\code{open_shapes}          \tab \verb{+ x ~ ^ v < >} as shapes                         \tab \code{v(open.shapes)}      \cr
\code{punc_shapes}          \tab \verb{! @ # $ \% & ?} as shapes                         \tab \code{v(punc.shapes)}      \cr
\code{fill_shapes}          \tab The \code{5} R fillable shapes                         \tab \code{v(fill.shapes)}      \cr
\code{empty_shapes}         \tab The \code{5} R empty shapes                            \tab \code{v(empty.shapes)}     \cr
\code{solid_shapes}         \tab The \code{4} R solid shapes                            \tab \code{v(solid.shapes)}     \cr
\code{digit_shapes}         \tab \code{1} to \code{9} as shapes                              \tab \code{v(digit.shapes)}     \cr
\code{digit_shapes0}        \tab \code{0} to \code{9} as shapes                              \tab \code{v(digit.shapes0)}    \cr
\code{letter_shapes}        \tab \code{a} to \code{z} as shapes                              \tab \code{v(letter.shapes)}    \cr
\code{LETTER_shapes}        \tab \code{A} to \code{Z} as shapes                              \tab \code{v(LETTER.shapes)}    \cr
\code{build_palette}        \tab \emph{any of the above}                                \tab \emph{any of the above}    \cr
\code{blank_shape}          \tab Space as a shape                                  \tab \code{v(blank.shape)}        }
\emph{NOTE}: \code{unq} defaults to the smaller of \code{tot} or the number of unique values available for the palette. If \code{unq} is larger than the number of unique values available for the palette, an error is generated, except in \code{create_palette} where \code{unq} is reduced to the number of available unique values if necessary.
}
\seealso{
Other plots: 
\code{\link{color}},
\code{\link{ggp}()},
\code{\link{gr}},
\code{\link{markdown}()},
\code{\link{sca}()},
\code{\link{u2u}()}
}
\concept{plots}
