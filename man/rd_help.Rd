% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rd.R
\encoding{UTF-8}
\name{rd_help}
\alias{rd_help}
\alias{rd_xsv}
\alias{rd_csv}
\alias{rd_tsv}
\alias{rd_clip}
\title{Thin and extended functionality wrappers of \code{readr} functions.}
\usage{
rd_help()

rd_xsv(d = NULL, type = "data file", file = NULL, ...)

rd_csv(type = "data file", file = NULL, ...)

rd_tsv(type = "data file", file = NULL, ...)

rd_clip()
}
\arguments{
\item{d}{Either \code{NULL} or a \link[=cmp_ch1_scl]{complete onechar scalar} text delimiter.}

\item{type}{A \link[=cmp_str_scl]{complete string scalar} describing the type of file to be read.}

\item{file}{Either \code{NULL} or a \link[=cmp_chr_vec]{complete character vector} that resolves to a file path. When \code{file = NULL} the user is asked to select a file using a system dialog box.}
}
\description{
Read the clipboard and rectangular, delimited data files, prompting user to choose a file if no file name/path is provided.
}
\section{Functions}{
\itemize{
\item \code{rd_xsv()}: Extends \code{\link[readr]{read_delim}} by prompting the user to select a file. Returns a data frame.

\item \code{rd_csv()}: Extends \code{\link[readr]{read_csv}} by prompting the user to select a file. Returns a data frame.

\item \code{rd_tsv()}: Extends \code{\link[readr]{read_tsv}} by prompting the user to select a file. Returns a data frame.

\item \code{rd_clip()}: Thinly wraps \code{\link[readr]{clipboard}}. Return value depends on the contents of the clipboard.

}}
\seealso{
Other wraps: 
\code{\link{basics_help}()},
\code{\link{dp}()},
\code{\link{dt}()},
\code{\link{gg_help}()},
\code{\link{gr}},
\code{\link{tb_help}()}

Other files: 
\code{\link{paths_help}()},
\code{\link{read_rct_delim}()}
}
\concept{files}
\concept{wraps}
