#' @name ccc
#' @family props
#' @title Extended Class (ccc) Property Family
#' @description Gets all valid extended class property values.
#' @details Extended classes are not formally defined as a new classes, but
#'   are dynamically evaluated for characteristics through a call to
#'   \code{xccc(x.)} where the function name (e.g., 'ccc') is the extended
#'   class name. \code{TRUE} or \code{FALSE} is always returned, and
#'   \code{FALSE} is always returned for the \code{NULL} object. In this
#'   package, the following types of objects are new and are defined as given in
#'   the following table.\tabular{lll}{
#'     EXTENDED CLASS \tab EXTENDED CLASS \tab CHARACTERISTICS               \cr
#'     PROPERTY VALUE \tab PROPERTY NAME  \tab QUALIFYING                    \cr
#'     \code{'vls'}   \tab vlist          \tab non-data.frame (vector) list  \cr
#'     \code{'gen'}   \tab generic        \tab any vector, vlist, or array   \cr
#'     \code{'scl'}   \tab scalar         \tab length-1 generic              \cr
#'     \code{'mvc'}   \tab multivec       \tab multiple-element vector,
#'                                        multiple-element vlist, or
#'                                        multiple-element array with 1
#'                                        \link[=eee]{effective dimension}   \cr
#'     \code{'tib'}   \tab tib            \tab \code{\link[tibble]{tibble}},
#'                                        \code{\link[data.table]{data.table}},
#'                                        or \code{\link[base]{data.frame}}  \cr
#'     \code{'vec'}   \tab vec            \tab scalar or mvec                \cr
#'     \code{'vtp'}   \tab vectype        \tab \code{link[=xemp]{empty}} or
#'                                        \code{link[=xpop]{populated}} scalar,
#'                                        vector, or vlist.                    }
#'   The following table defines extended class using existing ℝ concepts and
#'   the types just defined above. None of these newly defined types or extended
#'   classes are formally defined. They are evaluated dynamically for required
#'   properties. In the table, the first column indicates the structure of an
#'   object and column headings indicate restrictions on other properties.
#'   \tabular{lll}{
#'     ATOMIC EXTENDED \tab ATOMIC EXTENDED \tab CHARACTERISTICS OF          \cr
#'     CLASS VALUE     \tab CLASS NAME      \tab QUALIFYING OBJECTS          \cr
#'     \code{'arr'}    \tab atomic array    \tab non-recursive array         \cr
#'     \code{'agn'}    \tab atomic generic  \tab non-recursive generic       \cr
#'     \code{'atb'}    \tab atomic tib      \tab tib containing only atomic
#'                                               variables                   \cr
#'     \code{'avl'}    \tab atomic vlist    \tab vlist whose elements are all
#'                                               atomic                      \cr
#'     \code{'avt'}    \tab atomic vtype    \tab non-recursive vtype         \cr
#'     \code{'mat'}    \tab atomic matrix   \tab non-recursive matrix        \cr
#'     \code{'mvc'}    \tab atomic mvec     \tab non-recursive mvec          \cr
#'     \code{'scl'}    \tab atomic scalar   \tab non-recurvive scalar        \cr
#'     \code{'vec'}    \tab atomic vec      \tab non-recursive vec             }
#' @param x An object.
#' @param xxx A character scalar containing one or more values from
#'   \code{ccc_vals()} separated by pipes and/or underscores. Combinations of
#'   extended classes can be specified by separating them with underscores.
#'   Separating extended classes or combinations of extended classes with pipes
#'   will result in a value of \code{TRUE} if any of them applies to \code{x.}.
#' @param ... Additional arguments to \code{\link{meets}} containing value and
#'   element/row/column count restrictions.
#' @section Submitting additional arguments to \code{ccc} via \code{...}:
#'   Allows for checking not just the ccc but whether length, number of rows,
#'   number of columns, and element values meet flexible criteria.
#' @return \code{ccc_vals} returns a character vector containing all valid
#'   extended class property values. \code{ccc} returns a character scalar
#'   or vector containing all extended class properties from
#'   \code{ccc_vals()} applicable to \code{x.}. All others return either
#'   \code{TRUE} or \code{FALSE}.
#' @export
ccc_vals <- function() {
  x. <- c('arr', 'agn', 'atb', 'avl', 'avt', 'mat', 'mvc', 'scl', 'vec')
  names(x.) <- rep.int("ccc", length(x.))
  x.
}

#' @describeIn ccc Is \code{x.} an atomic array?
#' @export
iarr <- function(x.) {atm_arr(x.)}

#' @describeIn ccc Is \code{x.} an atomic generic?
#' @export
iagn <- function(x.) {atm_gen(x.)}

#' @describeIn ccc Is \code{x.} an atomic tibble?
#' @export
iatb <- function(x.) {atm_tbl(x.)}

#' @describeIn ccc Is \code{x.} an atomic vlist?
#' @export
iavl <- function(x.) {atm_vls(x.)}

#' @describeIn ccc Is \code{x.} an atomic vtype?
#' @export
iavt <- function(x.) {atm_vtp(x.)}

#' @describeIn ccc Is \code{x.} an atomic matrix?
#' @export
imat <- function(x.) {atm_mat(x.)}

#' @describeIn ccc Is \code{x.} an atomic mvect?
#' @export
imvc <- function(x.) {atm_mvc(x.)}

#' @describeIn ccc Is \code{x.} an atomic scalar?
#' @export
iscl <- function(x.) {atm_scl(x.)}

#' @describeIn ccc Is \code{x.} an atomic vect?
#' @export
ivec <- function(x.) {atm_vec(x.)}

#' @describeIn ccc Gets a vector of properties from \code{ccc_vals()} that are
#'   applicable to \code{x.}.
#' @export
ccc <- function(x.) {
  c(if (atm_arr(x.)) {'arr'} else {NULL},
    if (atm_gen(x.)) {'agn'} else {NULL},
    if (atm_tbl(x.)) {'atb'} else {NULL},
    if (atm_vls(x.)) {'avl'} else {NULL},
    if (atm_vtp(x.)) {'avt'} else {NULL},
    if (atm_mat(x.)) {'mat'} else {NULL},
    if (atm_mvc(x.)) {'mvc'} else {NULL},
    if (atm_scl(x.)) {'scl'} else {NULL},
    if (atm_vec(x.)) {'vec'} else {NULL})
}

#' @describeIn ccc Evaluates whether any (combination) property in \code{xxx.} is
#'   an extended class property applicable to \code{x.}.
#' @export
iccc <- function(x., xxx., ...) {
  if (!cmp_chr_scl(xxx.)) {stop("\n • [xxx.] must be a non-NA character scalar.")}
  valid. <- ccc_vals()
  combos. <- av(strsplit(xxx. , "|", fixed = T))
  newxxx. <- av(strsplit(combos., "_", fixed = T))
  valid. <- all(newxxx. %in% valid.)
  if (!valid.) {stop("\n • [xxx.] contains a value not in ccc_vals(), after splitting [xxx.] on pipes and underscores.")}
  ixxx(x., xxx., ...)
}
