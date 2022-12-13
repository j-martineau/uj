% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rd.R
\name{rd}
\alias{rd}
\alias{rdcsv}
\alias{rdclip}
\alias{rdxsv}
\alias{rdtsv}
\title{Thin and extended functionality wrappers of \code{readr} functions.}
\usage{
rdcsv(
  file = NULL,
  col_names = TRUE,
  col_types = NULL,
  col_select = NULL,
  id = NULL,
  locale = readr::default_locale(),
  na = c("", "NA"),
  quoted_na = TRUE,
  quote = "\\"",
  comment = "",
  trim_ws = TRUE,
  skip = 0,
  n_max = Inf,
  guess_max = min(1000, n_max),
  name_repair = "unique",
  num_threads = readr::readr_threads(),
  progress = readr::show_progress(),
  show_col_types = readr::should_show_types(),
  skip_empty_rows = TRUE,
  lazy = readr::should_read_lazy()
)

rdclip()

rdxsv(
  file = NULL,
  delim = NULL,
  quote = "\\"",
  escape_backslash = FALSE,
  escape_double = TRUE,
  col_names = TRUE,
  col_types = NULL,
  col_select = NULL,
  id = NULL,
  locale = readr::default_locale(),
  na = c("", "NA"),
  quoted_na = TRUE,
  comment = "",
  trim_ws = FALSE,
  skip = 0,
  n_max = Inf,
  guess_max = min(1000, n_max),
  name_repair = "unique",
  num_threads = readr::readr_threads(),
  progress = readr::show_progress(),
  show_col_types = sreadr::should_show_types(),
  skip_empty_rows = TRUE,
  lazy = readr::should_read_lazy()
)

rdtsv(
  file = NULL,
  col_names = TRUE,
  col_types = NULL,
  col_select = NULL,
  id = NULL,
  locale = readr::default_locale(),
  na = c("", "NA"),
  quoted_na = TRUE,
  quote = "\\"",
  comment = "",
  trim_ws = TRUE,
  skip = 0,
  n_max = Inf,
  guess_max = min(1000, n_max),
  progress = readr::show_progress(),
  name_repair = "unique",
  num_threads = readr::readr_threads(),
  show_col_types = readr::should_show_types(),
  skip_empty_rows = TRUE,
  lazy = readr::should_read_lazy()
)
}
\arguments{
\item{file}{Either \code{NULL} or a \link[=chr_vec]{character vector} that resolves to a file path. When \code{file = NULL} the user is asked to select a file using a system dialog box.}

\item{col_names}{Either \code{TRUE}, \code{FALSE} or a character vector
of column names.

If \code{TRUE}, the first row of the input will be used as the column
names, and will not be included in the data frame. If \code{FALSE}, column
names will be generated automatically: X1, X2, X3 etc.

If \code{col_names} is a character vector, the values will be used as the
names of the columns, and the first row of the input will be read into
the first row of the output data frame.

Missing (\code{NA}) column names will generate a warning, and be filled
in with dummy names \code{...1}, \code{...2} etc. Duplicate column names
will generate a warning and be made unique, see \code{name_repair} to control
how this is done.}

\item{col_types}{One of \code{NULL}, a \code{\link[readr:cols]{cols()}} specification, or
a string. See \code{vignette("readr")} for more details.

If \code{NULL}, all column types will be imputed from \code{guess_max} rows
on the input interspersed throughout the file. This is convenient (and
fast), but not robust. If the imputation fails, you'll need to increase
the \code{guess_max} or supply the correct types yourself.

Column specifications created by \code{\link[=list]{list()}} or \code{\link[readr:cols]{cols()}} must contain
one column specification for each column. If you only want to read a
subset of the columns, use \code{\link[readr:cols_only]{cols_only()}}.

Alternatively, you can use a compact string representation where each
character represents one column:
\itemize{
\item c = character
\item i = integer
\item n = number
\item d = double
\item l = logical
\item f = factor
\item D = date
\item T = date time
\item t = time
\item ? = guess
\item _ or - = skip

By default, reading a file without a column specification will print a
message showing what \code{readr} guessed they were. To remove this message,
set \code{show_col_types = FALSE} or set `options(readr.show_col_types = FALSE).
}}

\item{col_select}{Columns to include in the results. You can use the same
mini-language as \code{dplyr::select()} to refer to the columns by name. Use
\code{c()} to use more than one selection expression. Although this
usage is less common, \code{col_select} also accepts a numeric column index. See
\code{\link[tidyselect:language]{?tidyselect::language}} for full details on the
selection language.}

\item{id}{The name of a column in which to store the file path. This is
useful when reading multiple input files and there is data in the file
paths, such as the data collection date. If \code{NULL} (the default) no extra
column is created.}

\item{locale}{The locale controls defaults that vary from place to place.
The default locale is US-centric (like R), but you can use
\code{\link[readr:locale]{locale()}} to create your own locale that controls things like
the default time zone, encoding, decimal mark, big mark, and day/month
names.}

\item{na}{Character vector of strings to interpret as missing values. Set this
option to \code{character()} to indicate no missing values.}

\item{quoted_na}{\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}} Should missing values
inside quotes be treated as missing values (the default) or strings. This
parameter is soft deprecated as of readr 2.0.0.}

\item{quote}{Single character used to quote strings.}

\item{comment}{A string used to identify comments. Any text after the
comment characters will be silently ignored.}

\item{trim_ws}{Should leading and trailing whitespace (ASCII spaces and tabs) be trimmed from
each field before parsing it?}

\item{skip}{Number of lines to skip before reading data. If \code{comment} is
supplied any commented lines are ignored \emph{after} skipping.}

\item{n_max}{Maximum number of lines to read.}

\item{guess_max}{Maximum number of lines to use for guessing column types.
See \code{vignette("column-types", package = "readr")} for more details.}

\item{name_repair}{Handling of column names. The default behaviour is to
ensure column names are \code{"unique"}. Various repair strategies are
supported:
\itemize{
\item \code{"minimal"}: No name repair or checks, beyond basic existence of names.
\item \code{"unique"} (default value): Make sure names are unique and not empty.
\item \code{"check_unique"}: no name repair, but check they are \code{unique}.
\item \code{"universal"}: Make the names \code{unique} and syntactic.
\item A function: apply custom name repair (e.g., \code{name_repair = make.names}
for names in the style of base R).
\item A purrr-style anonymous function, see \code{\link[rlang:as_function]{rlang::as_function()}}.
}

This argument is passed on as \code{repair} to \code{\link[vctrs:vec_as_names]{vctrs::vec_as_names()}}.
See there for more details on these terms and the strategies used
to enforce them.}

\item{num_threads}{The number of processing threads to use for initial
parsing and lazy reading of data. If your data contains newlines within
fields the parser should automatically detect this and fall back to using
one thread only. However if you know your file has newlines within quoted
fields it is safest to set \code{num_threads = 1} explicitly.}

\item{progress}{Display a progress bar? By default it will only display
in an interactive session and not while knitting a document. The automatic
progress bar can be disabled by setting option \code{readr.show_progress} to
\code{FALSE}.}

\item{show_col_types}{If \code{FALSE}, do not show the guessed column types. If
\code{TRUE} always show the column types, even if they are supplied. If \code{NULL}
(the default) only show the column types if they are not explicitly supplied
by the \code{col_types} argument.}

\item{skip_empty_rows}{Should blank rows be ignored altogether? i.e. If this
option is \code{TRUE} then blank rows will not be represented at all.  If it is
\code{FALSE} then they will be represented by \code{NA} values in all the columns.}

\item{lazy}{Read values lazily? By default, this is \code{FALSE}, because there
are special considerations when reading a file lazily that have tripped up
some users. Specifically, things get tricky when reading and then writing
back into the same file. But, in general, lazy reading (\code{lazy = TRUE}) has
many benefits, especially for interactive use and when your downstream work
only involves a subset of the rows or columns.

Learn more in \code{\link[readr:should_read_lazy]{should_read_lazy()}} and in the documentation for the
\code{altrep} argument of \code{\link[vroom:vroom]{vroom::vroom()}}.}

\item{delim}{Single character used to separate fields within a record.}

\item{escape_backslash}{Does the file use backslashes to escape special
characters? This is more general than \code{escape_double} as backslashes
can be used to escape the delimiter character, the quote character, or
to add special characters like \verb{\\\\n}.}

\item{escape_double}{Does the file escape quotes by doubling them?
i.e. If this option is \code{TRUE}, the value \verb{""""} represents
a single quote, \verb{\\"}.}
}
\value{
\tabular{rl}{
  \code{rdclip}   \tab Varies.
\cr           \tab   
\cr \code{rdcsv}   \tab A \code{data.frame}.
\cr \code{rdtsv}   \tab   
\cr \code{rdxsv}   \tab   
}
}
\description{
\tabular{rl}{
  \code{rdclip}   \tab Thinly wraps \code{\link[readr]{clipboard}}.
\cr \code{rdcsv}   \tab Extends \code{\link[readr]{read_csv}}\eqn{^1}.
\cr \code{rdtsv}   \tab Extends \code{\link[readr]{read_tsv}}\eqn{^1}.
\cr \code{rdxsv}   \tab Extends \code{\link[readr]{read_delim}}\eqn{^1}.
}
\eqn{^1} Prompts user to select a file if \code{file = NULL}.
}
\examples{
# Input sources -------------------------------------------------------------
# Read from a path
read_csv(readr_example("mtcars.csv"))
read_csv(readr_example("mtcars.csv.zip"))
read_csv(readr_example("mtcars.csv.bz2"))
\dontrun{
# Including remote paths
read_csv("https://github.com/tidyverse/readr/raw/main/inst/extdata/mtcars.csv")
}

# Read from multiple file paths at once
continents <- c("africa", "americas", "asia", "europe", "oceania")
filepaths <- vapply(
  paste0("mini-gapminder-", continents, ".csv"),
  FUN = readr_example,
  FUN.VALUE = character(1)
)
read_csv(filepaths, id = "file")

# Or directly from a string with `I()`
read_csv(I("x,y\n1,2\n3,4"))

# Column selection-----------------------------------------------------------
# Pass column names or indexes directly to select them
read_csv(readr_example("chickens.csv"), col_select = c(chicken, eggs_laid))
read_csv(readr_example("chickens.csv"), col_select = c(1, 3:4))

# Or use the selection helpers
read_csv(
  readr_example("chickens.csv"),
  col_select = c(starts_with("c"), last_col())
)

# You can also rename specific columns
read_csv(
  readr_example("chickens.csv"),
  col_select = c(egg_yield = eggs_laid, everything())
)

# Column types --------------------------------------------------------------
# By default, readr guesses the columns types, looking at `guess_max` rows.
# You can override with a compact specification:
read_csv(I("x,y\n1,2\n3,4"), col_types = "dc")

# Or with a list of column types:
read_csv(I("x,y\n1,2\n3,4"), col_types = list(col_double(), col_character()))

# If there are parsing problems, you get a warning, and can extract
# more details with problems()
y <- read_csv(I("x\n1\n2\nb"), col_types = list(col_double()))
y
problems(y)

# Column names --------------------------------------------------------------
# By default, readr duplicate name repair is noisy
read_csv(I("x,x\n1,2\n3,4"))

# To quiet, set the option that controls verbosity of name repair
withr::with_options(
  list(rlib_name_repair_verbosity = "quiet"),
  read_csv(I("x,x\n1,2\n3,4"))
)

# Or use "minimal" to turn off name repair
read_csv(I("x,x\n1,2\n3,4"), name_repair = "minimal")

# File types ----------------------------------------------------------------
read_csv(I("a,b\n1.0,2.0"))
read_csv2(I("a;b\n1,0;2,0"))
read_tsv(I("a\tb\n1.0\t2.0"))
read_delim(I("a|b\n1.0|2.0"), delim = "|")
}
\seealso{
Other wraps: 
\code{\link{basics}},
\code{\link{dp}},
\code{\link{dt}()},
\code{\link{ggp}()},
\code{\link{gr}},
\code{\link{sca}()},
\code{\link{tb}()}

Other files: 
\code{\link{paths}}
}
\concept{files}
\concept{wraps}