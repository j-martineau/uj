% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/alert.R
\encoding{UTF-8}
\name{alert_utils}
\alias{alert_utils}
\alias{.fmt_errs}
\alias{.ok_fmt}
\alias{.choose_from}
\title{Validate Formatting Argument Values}
\usage{
alert_utils()

.fmt_errs(
  st = NULL,
  bg = NULL,
  fg = NULL,
  d = " ",
  nullst = TRUE,
  nullbg = TRUE,
  nullfg = TRUE
)

.ok_fmt(x)

.choose_from(
  opts,
  mssg,
  all,
  none,
  min,
  max,
  ft,
  fs,
  fun,
  stack,
  clear,
  cancel = T
)
}
\arguments{
\item{st}{Optional style spec.}

\item{bg}{Optional background color spec.}

\item{fg}{Optional foreground color spec.}

\item{d}{Character scalar text delimiter.}

\item{nullst}{Logical scalar indicating whether a \code{NULL} style spec is valid.}

\item{nullbg}{Logical scalar indicating whether a \code{NULL} background color spec is valid.}

\item{nullfg}{Logical scalar indicating whether a \code{NULL} foreground color spec is valid.}

\item{opts}{An atomic vector of options to choose from.}

\item{mssg}{Character scalar message to prompt the selection.}

\item{all}{Logical scalar indicating whether it is valid to select all options.}

\item{none}{Logical scalar indicating whether it is valid to select none of the options.}

\item{min}{Non-negative integer scalar minimum number of options to select.}

\item{max}{Positive integer scalar maximum numver of options to select.}

\item{ft}{Character scalar title formatting spec.}

\item{fs}{Character scalar subtitle formatting spec.}

\item{fun}{Character scalar name of calling function.}

\item{stack}{Character vector call stack.}

\item{clear}{Logical scalar indicating whether to clear the console before alerting the user to the selection.}

\item{cancel}{Logical scalar indicating whether cancellation of selection is valid.}
}
\description{
Validate Formatting Argument Values
}
\section{Functions}{
\itemize{
\item \code{.fmt_errs()}: Validate Formatting Argument Values

\item \code{.ok_fmt()}: Validate Formatting Argument Values

\item \code{.choose_from()}: Choose from a List of Options with a Given Message and a Variety of Options

}}
\examples{
egOkFmt <- function() {
  base::list(blank  = uj::.ok_fmt(""),
             null   = uj::.ok_fmt(NULL),
             bad.st = uj::.ok_fmt("q|r|b"),
             ok.all = uj::.ok_fmt("y|r|b"),
             padded = uj::.ok_fmt("  yellow | red | bold  "))
}
egOkFmt()
\dontrun{
  uj::.fmt_errs(),
  uj::.fmt_errs(d = 100),
  uj::.fmt_errs(st = "", bg = "", fg = ""),
  uj::.fmt_errs(st = "q", bg = "q", fg = "q"),
  uj::.fmt_errs(nullst = F, nullbg = F, nullfg = F))
  uj::.choose_from(letters, "What letter?", T, N, 0, 26, "b|s|u", "w|b|p", "console", "console", F)
  uj::.choose_from(0:9    , "What number?", F, F, 1, 1 , "r|y|b", "y|r|i", "console", "console", T, cancel = F)
}
}
\seealso{
Other console: 
\code{\link{console_help}()}
}
\concept{console}
