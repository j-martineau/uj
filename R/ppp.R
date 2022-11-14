.drop_iprefix <- function(x) {
  i <- nchar(x) == 4 & substr(x, 1, 1) == "i"
  x[i] <- substr(x[i], 2, 4)
  x
}

#' @name ppp.
#' @family props
#' @title All purpose property checking
#' @description This set of functions provide utilities that bring together
#'   these seven families of properties defined by this package:\tabular{ll}{
#'   PROPERTY          \tab PROPERTY                                         \cr
#'   FAMILY            \tab TYPE                                             \cr
#'   \code{\link{ccc}} \tab Extended class                                   \cr
#'   \code{\link{ddd}} \tab Defined dimensionality                           \cr
#'   \code{\link{eee}} \tab Effective dimensionality                         \cr
#'   \code{\link{fff}} \tab Form                                             \cr
#'   \code{\link{mmm}} \tab Extended mode                                    \cr
#'   \code{\link{sss}} \tab State of completeness                            \cr
#'   \code{\link{ttt}} \tab Fundamental type                                   }
#'   \strong{Individual Property Specifications}
#'   \cr In this package, all individual property specifications are scalars
#'   containing exactly 3 characters (e.g., \code{'ord'}, \code{'dtf'},
#'   \code{'d1D'}, or \code{'rct'}).
#'   \cr\cr
#'   \strong{Combination/Conjunctive Property Specifications}
#'   \cr Specifications for properties that must co-occur are constructed by
#'   delimiting multiple individual properties with underscores (e.g., the
#'   properties \code{'ord'}, \code{'dtf'}, \code{'d1D'}, and \code{'rct'} could
#'   be specified to occur together using the combination/conjunctive property
#'   specification \code{'ord_dtf_d1D_rct'} or an equivalent
#'   underscore-delimited permutation.
#'   \cr\cr
#'   \strong{Alternative/Compensatory Property Specifications}
#'   \cr Specifications for alternative/compensatory properties give a way to
#'   indicate that if any one (individual or combination) property is satisfied
#'   the entire specification is satisfied. They are constructed by
#'   pipe-delimiting multiple individual or combination properties. For example,
#'   the specification \code{'ord|dtf_d1D||dtf_rct'} would be satisfied by an
#'   object with property \code{'ord'}, by an object with the combined property
#'   \code{'dtf_d1D'}, or by an object with the combined property
#'   \code{'dtf_rct'}.
#'   \cr\cr
#'   How functions are related to property families and property specifications
#'   is explained in the sections entitled \strong{Universal Property
#'   Functions}, \strong{Individual Property Functions}, and \strong{Combination
#'   Property Functions}.
#' @section Universal Property Functions: Property functions addressing
#'   \emph{all} property families are the following: \tabular{ll}{
#'     FUNCTION                 \tab WHAT THE                                \cr
#'     NAME                     \tab FUNCTION DOES                           \cr
#'     \code{ppp}               \tab Get all properties of all property families
#'                                   applicable to an object.                \cr
#'     \code{ippp}              \tab Evaluate whether an object satisfies the
#'                                   property specification in \code{ppp}
#'                                   subject to any additional restrictions
#'                                   supplied in \code{...}.                 \cr
#'     \code{nll_or}            \tab Evaluate whether an object is either
#'                                   \code{NULL} or satisfies the property
#'                                   specification in argument \code{ppp}
#'                                   subject to any additional restrictions in
#'                                   \code{...}.                             \cr
#'     \code{nas_or}            \tab Evaluate whether an object is either scalar
#'                                   \code{NA} or satisfies the property
#'                                   specification in argument \code{ppp}
#'                                   subject to any additional restrictions in
#'                                   \code{...}.                             \cr
#'     \code{ppp_vals}          \tab Get all
#'                                   possible individual properties from all
#'                                   property families and all possible
#'                                   combination/conjunctive properties as
#'                                   checked by combined property functions
#'                                   (see the \emph{combined property functions}
#'                                   section).                               \cr
#'     \code{ppp_all}           \tab Get all constituent individual properties
#'                                   from the property specification in argument
#'                                   \code{ppp}.                             \cr
#'     \code{ppp_funs}          \tab Gets the names of all property functions
#'                                   that check for a single property or a
#'                                   combined property without checking for any
#'                                   additional restrictions.                \cr
#'     \code{ppp_defs}          \tab Get a data frame defining all individual
#'                                   properties of all property families.    \cr
#'     \code{is_ppp_fun}        \tab Evaluates whether a character scalar is the
#'                                   name of a property function (those listed
#'                                   by \code{ppp_funs}).                    \cr
#'     \code{is_valid_ppp}      \tab Evaluates a character scalar property
#'                                   specification for validity (i.e., does it
#'                                   contain only single properties, valid
#'                                   combination properties, and/or valid
#'                                   alternate properties).                  \cr
#'     \code{ppp_from_combo}    \tab Extract each individual property value from
#'                                   a combination/conjunctive property
#'                                   specification. \cr
#'     \code{combos_from_ppp}   \tab Extracts each individual or combination
#'                                   property specification from an
#'                                   alternative/compensatory property
#'                                   specification.                          \cr
#'     \code{ppp_verbose}       \tab Get a verbose definition of an individual
#'                                   property specification.                 \cr
#'     \code{ppp_concise}       \tab Get a plain-language, concise definition of
#'                                   an individual or combination/conjunctive
#'                                   property specification.                 \cr
#'     \code{alt_ppp_concise}   \tab Get a plain-language, concise definition of
#'                                   an alternative/compensatory property
#'                                   specification.                            }
#' @section Individual Property Functions: Property functions associated with a
#'   single property family take the following forms where \code{xxx} is a
#'   placeholder for an individual property and \code{zzz} is a placeholder for
#'   a property family: \tabular{ll}{
#'   FUNCTION          \tab WHAT THE                                         \cr
#'   NAME FORMAT       \tab FUNCTION DOES                                    \cr
#'   \code{xxx}        \tab Get properties from the property family represented
#'                          by \code{xxx} applicable to an object.           \cr
#'   \code{izzz}       \tab Evaluate whether an object has one or more specific
#'                          properties from the property family represented by
#'                          \code{zzz}, subject to any additional restrictions
#'                          supplied in \code{...}.                          \cr
#'   \code{zzz_vals}   \tab Get all properties in the property family
#'                          represented by \code{zzz}.                         }
#' @section Combination Property Functions: Property functions associated with a
#'   single property family take the following forms where \code{ccc},
#'   \code{mmm}, and \code{ttt} are placeholders for properties from extended
#'   class, extended mode, and fundamental type property families:\tabular{ll}{
#'   FUNCTION      \tab WHAT THE                                             \cr
#'   NAME FORMAT   \tab FUNCTION DOES                                        \cr
#'   \code{\link[mmm_ccc.]{mmm_ccc}}
#'     \tab Evaluate whether an object is of the extended mode represented by
#'          \code{mmm} and of the extended class represented by \code{ccc}.  \cr
#'   \code{\link[ttt_ccc.]{ttt_ccc}}
#'     \tab Evaluate whether an object is of the fundamental type represented by
#'          \code{ttt} and of the extended class represented by \code{ccc}.  \cr
#'   \code{\link[ttt_mmm.]{ttt_mmm}}
#'     \tab Evaluate whether an object is of the fundamental type represented by
#'          \code{ttt} and of the extended mode represented by \code{mmm}.   \cr
#'   \code{\link[cmp_ccc.]{cmp_ccc}}
#'     \tab Evaluate whether an object is complete (non-empty, atomic,
#'          containing no \code{NA} values). and is of the extended class
#'          represented by \code{ccc}.                                       \cr
#'   \code{\link[cmp_mmm.]{cmp_mmm}}
#'     \tab Evaluate whether an object is complete (implying non-empty and
#'          atomic) and is of the extended mode represented by \code{mmm}.   \cr
#'   \code{\link[cmp_mmm_ccc.]{cmp_mmm_ccc}}
#'     \tab Evaluate whether an object is complete (implying non-empty and
#'          atomic), is of the extended mode represented by \code{mmm}, and is
#'          of the extended class represented by \code{ccc}.                 \cr
#'   \code{\link{mmm_ccc_vals}}
#'     \tab Get all combined extended mode + extended class properties.      \cr
#'   \code{\link{ttt_ccc_vals}}
#'     \tab Get all combined fundamental type + extended class properties.   \cr
#'   \code{\link{ttt_mmm_vals}}
#'     \tab Get combined fundamental type and extended mode properties.      \cr
#'   \code{\link{cmp_ccc_vals}}
#'     \tab Get all combined completeness
#'          and extended class properties.                                   \cr
#'   \code{\link{cmp_mmm_vals}}
#'     \tab Get all combined completeness + extended mode properties.        \cr
#'   \code{\link{cmp_mmm_ccc_vals}}   
#'     \tab Get all combined completeness + extended mode + extended class
#'          properties.                                                        }
#' @section Additional arguments in \code{...}: Arguments in the following table
#'   are optional. If used, they indicate additional property
#'   restrictions:\tabular{ll}{
#'     ARGUMENT    \tab USE AND MEANING                                      \cr
#'     \code{n}    \tab Set of valid lengths/numbers of elements.            \cr
#'     \code{nr}   \tab Set of valid numbers of rows.                        \cr
#'     \code{nc}   \tab Set of valid numbers of columns.                     \cr
#'     \code{min}  \tab Minimum valid length/number of elements.             \cr
#'     \code{minr} \tab Minimum valid number of rows.                        \cr
#'     \code{minc} \tab Minimum valid number of columns.                     \cr
#'     \code{max}  \tab Maximum valid length/number of element.              \cr
#'     \code{maxr} \tab Maximum valid number of rows.                        \cr
#'     \code{maxc} \tab Maximum valid number of columns.                     \cr
#'     \code{vals} \tab Set of valid values.                                 \cr
#'     \code{lt}   \tab Exclusive upper bound (x <) on elements of \code{x}. \cr
#'     \code{le}   \tab Inclusive upper bound (x ≤) on elements of \code{x}. \cr
#'     \code{ge}   \tab Inclusive lower bound (x ≥) on elements of \code{x}. \cr
#'     \code{gt}   \tab Exclusive lower bound (x >) on elements of \code{x}.   }
#' @param as.dtf \link[cmp_lgl_scl]{Complete logical scalar} indicating whether
#'   to return the result as a dtf with column 1 containing property values
#'   and column 2 containing the property families.
#' @param x An ℝ object.
#' @param ppp \link[chr_scl]{Character scalar} containing one or more values
#'   from \code{ppp_vals()} separated by pipes and/or underscores. Combinations
#'   of properties can be specified by separating them with underscores.
#'   Separating properties or combinations of properties with pipes will result
#'   in a value of \code{TRUE} if any of them applies to \code{x}.
#' @param valid \link[chr_vec]{Character vec} containing all properties
#'   considered valid.
#' @param print \link[lgl_scl]{Logical scalar} indicating whether to print the
#'   property definition to the console.
#' @param ... Additional arguments to \code{\link{meets}} containing value and
#'   element/row/column count restrictions.
#' @return \code{ppp}, \code{ppp_all}, \code{ppp_vals}, \code{pop_funs},
#'   \code{ppp_from_combo}, and \code{combos_from_ppp}, return a character
#'   vector.
#'   \cr\cr
#'   \code{ppp_concise} and \code{alt_ppp_concise} return a character scalar.
#'   \cr\cr
#'   \code{ippp}, \code{nll_or}, \code{nas_or}, \code{is_ppp_fun}, and
#'   \code{is_valid_ppp} return a logical scalar.
#' @export
ppp. <- function() {help("ppp.", package = "uj")}

#' @describeIn ppp. Get all single properties that apply to \code{x}.
#' @export
ppp <- function(x) {sort(c(sss(x), ttt(x), fff(x), ddd(x), eee(x), mmm(x), ccc(x)))}

#' @describeIn ppp. Evaluates whether any (combination) property in \code{ppp}
#'   is applicable to \code{x}, subject to any additional restrictions in
#'   \code{...}.
#' @export
ippp <- function(x, ppp, ...) {
  if (!is_valid_ppp(ppp)) {stop("\n • The property specification '", ppp, "' is not a character vector of values from ppp_vals() after splitting pipes and underscores.")}
  if (!meets(x, ...)) {return(F)}
  singles <- ppp_vals()
  combos <- combos_from_ppp(ppp)
  for (combo in combos) {
    is.one <- combo %in% singles
    is.fun <- is_ppp_fun(combo)
    if (!is.one & !is.fun) {
      new <- .drop_iprefix(ppp_from_combo(combo))
      good <- TRUE
      for (ppp in new) {if (good) {good <- good & eval(parse(text = paste0("i", ppp, "(x)")))}}
    }
    else if (is.one) {good <- eval(parse(text = paste0("i", combo, "(x)")))}
    else {good <- eval(parse(text = paste0(combo, "(x)")))}
    if (good) {return(TRUE)}
  }
  FALSE
}

#' @describeIn ppp. Extract unique property values from \code{ppp} by splitting
#'   along pipes (\code{"|"}) and underscores.
#' @export
ppp_all <- function(ppp, valid = ppp_vals()) {
  PPP <- ppp
  vx <- cmp_chr_scl(ppp)
  vv <- f0(cmp_chr_vec(valid), all(valid %in% ppp_vals()), F)
  err <- NULL
  if (!vx) {err <- c(err, "\n • [ppp] must be a complete character scalar (?cmp_chr_scl).")}
  if (!vv) {err <- c(err, "\n • [valid] must be a complete character vector (?cmp_chr_vec) containing only values from ppp_vals().")}
  if (idef(err)) {stop(err)}
  ppp <- av(strsplit(ppp, "|", fixed = T))
  ppp <- av(strsplit(ppp, "_", fixed = T))
  ppp <- trimws(ppp)
  ppp <- ppp[ppp != ""]
  ppp <- .drop_iprefix(ppp)
  if (length(ppp) == 0) {stop("\n • The property specification '", PPP, "' is empty after splitting on pipes and underscores.")}
  if (!all(ppp %in% valid)) {stop("\n • The property specification '", PPP, "' contains a property not in c(", paste0(paste0("'", valid, "'"), collapse = ", "), ").")}
  sort(unique(ppp))
}

#' @describeIn ppp. Get a vector of all possible single property values in all
#'   property families.
#' @export
ppp_vals <- function(as.dtf = F) {
  if (!isTF(as.dtf)) {stop("\n • [as.dtf] must be TRUE or FALSE.", call. = F)}
  ccc <- ccc_vals(); cfam <- rep("ccc", length(ccc)); clab <- c("c_", ccc)
  ddd <- ddd_vals(); dfam <- rep("ddd", length(ddd)); dlab <- c("d_", ddd)
  eee <- eee_vals(); efam <- rep("eee", length(eee)); elab <- c("c_", eee)
  fff <- fff_vals(); ffam <- rep("fff", length(fff)); flab <- c("f_", fff)
  mmm <- mmm_vals(); mfam <- rep("mmm", length(mmm)); mlab <- c("m_", mmm)
  sss <- sss_vals(); sfam <- rep("sss", length(sss)); slab <- c("s_", sss)
  ttt <- ttt_vals(); tfam <- rep("ttt", length(ttt)); tlab <- c("t_", ttt)
  ppp <- c(ccc, ddd, eee, fff, mmm, sss, ttt)
  fam <- c(cfam, dfam, efam, mfam, sfam, tfam)
  ord <- order(c(clab, dlab, elab, mlab, slab, tlab))
  if (!as.dtf) {return(ppp[ord])}
  tibble::tibble(family = fam[ord], ppp = ppp[ord])
}

#' @describeIn ppp. Evaluate whether \code{x} is \code{NULL} or matches one
#'   or more property (combos) specified in \code{ppp}.
#' @export
nll_or <- function(x, ppp, ...) {f0(inll(x), T, ippp(x, ppp, ...))}

#' @describeIn ppp. Evaluate whether \code{x} is an atomic scalar \code{NA}
#'   or matches one or more property (combos) specified in \code{ppp}.
#' @return Logical scalar.
#' @export
nas_or <- function(x, ppp, ...) {f0(inas(x), T, ippp(x., ppp, ...))}

#' @describeIn ppp. List all valid property function names.
#' @export
ppp_funs <- function(as.dtf = F) {
  if (!isTF(as.dtf)) {stop("\n • [as.dtf] must be TRUE or FALSE.")}
  x <- sort(c(paste0("i", ppp_vals()), cmp_mmm_vals(), cmp_mmm_ccc_vals(), cmp_ccc_vals(), mmm_ccc_vals(), ttt_ccc_vals(), ttt_mmm_vals()))
  if (!as.dtf) {return(x)}
  tibble::tibble(family = names(x), property = av(x))
}

#' @describeIn ppp. Get a dtf with 4 columns: property family (Family),
#'   property value (Value), short property definition (Short), and long
#'   property definition (Long).
#' @export
ppp_defs <- function() {
  tibble::tribble(
    ~Family, ~Value  , ~Short                              , ~Long,
    "ccc"  , "arr"   , "arrary+"                           , "An array or vector",
    "ccc"  , "dtf"   , "data.frame"                        , "A data.frame",
    "ccc"  , "gen"   , "generic"                           , "A generic (vector/array/vlist)",
    "ccc"  , "mat"   , "matrix"                            , "A atomic matrix",
    "ccc"  , "mvc"   , "multivec"                          , "A vector/vlist list of length 2+ or array of length 2+ with multiple index positions in exactly 1 dimension",
    "ccc"  , "scl"   , "scalar"                            , "A vector/vlist/array of length 1",
    "ccc"  , "vec"   , "vector+"                           , "A vector/1D array/multidimensional array with multiple index positions in 0 or 1 dimension",
    "ccc"  , "vls"   , "vlist (non-data.frame list)"       , "A vector-list (not a data.frame list)",
    "ddd"  , "d0D"   , "defined as 0-dimensional"          , "A 0D object (NULL)",
    "ddd"  , "d1D"   , "defined as 1-dimensional"          , "A 1D object (vector, non-data.frame list, or 1-dimensional array)",
    "ddd"  , "d2D"   , "defined as 2-dimensional"          , "A 2D object (matrix or data.frame)",
    "ddd"  , "dHD"   , "defined as hyper-dimensional"      , "A 3D+ object (array with 3+ dimensions)",
    "eee"  , "e0D"   , "effectively 0-dimensional"         , "An effectively 0D object (vector/vlist/array of length 1 or data.frame with 1 row and 1 column",
    "eee"  , "e1D"   , "effectively 1-dimensional"         , "An effectively 1D object (vector/vlist with 2+ elements, row/column matrix/data.frame, or populated array with more than one index position in exactly 1 dimension)",
    "eee"  , "e2D"   , "effectively 2-dimensional"         , "An effectively 2D object (matrix/data.frame with 2+ rows and 2+ columns or populated array with multiple index positions in exactly 2 dimensions)",
    "eee"  , "eHD"   , "effectively hyper-dimensional"     , "An effectively 3D+ object (populated array with multiple index positions in 3+ dimensions)",
    "eee"  , "eUD"   , "effectively non-dimensional"       , "An effectively non-dimensional object (of length 0)",
    "fff"  , "col"   , "column"                            , "A column object (2+ x 1 data.frame/matrix)",
    "fff"  , "emp"   , "empty"                             , "An empty object (of length 0, but not NULL)",
    "fff"  , "lin"   , "linear"                            , "A linear object (vector/vlist/1D array of length 2+, row/column matrix/data.frame, or populated array with multiple indexing positions in exactly 1 dimension)",
    "fff"  , "pnt"   , "point"                             , "A point object (length-1 vector/array/vlist or 1 x 1 data.frame/matrix)",
    "fff"  , "row"   , "row"                               , "A row object (1 x 2+ data.frame/matrix)",
    "fff"  , "rct"   , "rectangular"                       , "A rectangular object (2+ x 2+ data.frame/matrix, or a higher-dimensional array with multiple index positions in exactly 2 dimensions)",
    "fff"  , "sld"   , "solid"                             , "A solid object (array with multiple index positions in 3+ dimensions)",
    "fff"  , "sqr"   , "square"                            , "A square atomic matrix",
    "mmm"  , "ch1"   , "single-character"                  , "Any non-NA values are character scalars containing exactly 1 character",
    "mmm"  , "chr"   , "character"                         , "A character object",
    "mmm"  , "clr"   , "character color value"             , "A character object containing valid color values",
    "mmm"  , "evn"   , "even whole-number"                 , "An even whole-number object",
    "mmm"  , "fac"   , "factor"                            , "An ordered-factor or unordered-factor object",
    "mmm"  , "frc"   , "fractional numeric"                , "A fractional numeric object (containing 1+ non-whole-number values)",
    "mmm"  , "ind"   , "indexing"                          , "A indexing object (logical or positive whole number)",
    "mmm"  , "lgl"   , "logical"                           , "A logical object",
    "mmm"  , "neg"   , "negative numeric"                  , "A negative numeric object",
    "mmm"  , "ngw"   , "negative whole number"             , "A negative whole-number object",
    "mmm"  , "nng"   , "nonnegative numeric"               , "A non-negative numeric object",
    "mmm"  , "nnw"   , "nonnegative whole-number"          , "A non-negative whole-number object",
    "mmm"  , "nps"   , "nonpositive numeric"               , "A non-positive numeric object",
    "mmm"  , "npw"   , "nonpositive whole-number"          , "A non-positive whole-number object",
    "mmm"  , "nst"   , "non-sortable"                      , "A non-sortable object (atomic but not character, logical, numeric, or ordered factor)",
    "mmm"  , "num"   , "numeric"                           , "A numeric object",
    "mmm"  , "odd"   , "odd whole-number"                  , "An odd whole-number object",
    "mmm"  , "ord"   , "ordered-factor"                    , "An ordered-factor object",
    "mmm"  , "pct"   , "percentage numeric"                , "A numeric, percentage object (values in the interval [0, 100])",
    "mmm"  , "pos"   , "positive numeric"                  , "A positive numeric object",
    "mmm"  , "ppn"   , "proportion  numeric"               , "A numeric proportion object (values in the interval [0, 1])",
    "mmm"  , "psw"   , "positive whole-number"             , "A positive whole-number object",
    "mmm"  , "srt"   , "sortable"                          , "A sortable object (character, logical, numeric, or ordered factor)",
    "mmm"  , "uno"   , "unordered-factor"                  , "A unordered-factor object",
    "mmm"  , "whl"   , "whole-number"                      , "A whole-number object",
    "sss"  , "cmp"   , "atomic and complete"               , "A complete atomic object, complete atomic vlist, or complete atomic data.frame (without any NA values)",
    "sss"  , "mss"   , "atomic and missing"                , "A missing atomic object, missing atomic vlist, or missing atomic data.frame (without any non-NA values)",
    "sss"  , "nas"   , "atomic NA scalar"                  , "An atomic NA scalar object",
    "sss"  , "oks"   , "atomic non-NA scalar"              , "An atomic non-NA scalar object",
    "sss"  , "prt"   , "atomic and partial"                , "A partial atomic object, partial atomic vlist, or partial atomic data.frame (with both NA and non-NA values)",
    "ttt"  , "atm"   , "atomic"                            , "An atomic object",
    "ttt"  , "def"   , "defined"                           , "A defined object (not NULL)",
    "ttt"  , "fun"   , "function or function name"         , "A function object or a character scalar containing a function name",
    "ttt"  , "nil"   , "nil"                               , "A nil object (of length 0, including NULL)",
    "ttt"  , "nll"   , "NULL"                              , "The NULL object",
    "ttt"  , "pop"   , "populated"                         , "A populated object (not of length 0)",
    "ttt"  , "rcr"   , "recursive"                         , "A recursive object (a data.frame or vlist)"
  )
}

#' @describeIn ppp. Evaluate \code{x} to see if it the name of a property
#'   function.
#' @export
is_ppp_fun <- function(x) {
  if (!cmp_chr_scl(x)) {stop("\n • [x] must be a complete character scalar (?cmp_chr_scl).")}
  x %in% ppp_funs()
}

#' @describeIn ppp. Evaluate whether \code{ppp} is a character scalar of
#'   values from \code{ppp_vals} separated by pipes ('|') and/or underscores.
#'   Always returns either \code{TRUE} or \code{FALSE}.
#' @export
is_valid_ppp <- function(ppp) {
  if (!cmp_chr_scl(ppp)) {return(FALSE)}
  ppp <- av(strsplit(ppp, "_", TRUE))
  ppp <- av(strsplit(ppp, "|", TRUE))
  ppp <- trimws(ppp)
  ppp <- ppp[ppp != ""]
  ppp <- .drop_iprefix(ppp)
  if (length(ppp) == 0) {return(FALSE)}
  all(ppp %in% ppp_vals())
}

#' @describeIn ppp. Extract each single property value from a single combination
#'   property by splitting along underscores.
#' @export
ppp_from_combo <- function(ppp, valid = ppp_vals()) {
  PPP <- ppp
  vx <- cmp_chr_scl(ppp)
  vv <- f0(cmp_chr_vec(valid), all(valid %in% ppp_vals()), F)
  err <- NULL
  if (!vx) {err <- c(err, "\n • [ppp] must be a complete character scalar (?cmp_chr_scl).")}
  if (!vv) {err <- c(err, "\n • [valid] must be a complete character vec (?cpm_chr_vec) containing only values from ppp_vals().")}
  if (!is.null(err)) {stop(err)}
  ppp <- av(strsplit(ppp, "_", fixed = T))
  ppp <- trimws(ppp)
  ppp <- ppp[ppp != ""]
  ppp <- .drop_iprefix(ppp)
  if (length(ppp) == 0) {stop("\n • The property specification '", PPP, "' is empty after splitting on pipes and underscores.")}
  if (!all(ppp %in% valid)) {stop("\n • The property specification '", PPP, "' contains a property not in c(", paste0(paste0("'", valid, "'"), collapse = ", "), ").")}
  sort(unique(ppp))
}

#' @describeIn ppp. Extract each property combination from \code{ppp} by
#'   splitting along pipes (\code{"|"}).
#' @export
combos_from_ppp <- function(ppp, valid = ppp_vals()) {
  ppp_all(ppp, valid)
  ppp <- av(strsplit(ppp, "|", fixed = T))
  ppp <- trimws(ppp)
  ppp <- ppp[ppp != ""]
  ppp <- .drop_iprefix(ppp)
  sort(unique(ppp))
}

#' @describeIn ppp. Get the data.frame from \code{ppp_defs()}. If \code{ppp}
#'   contains a single valid property value (no combination values), extracts
#'   the associated row from the data.frame and creates a character scalar with
#'   the property family, property value, short property definition, and long
#'   property definition. If \code{print = TRUE}, prints the result (either the
#'   data.frame itself or the extracted row) to the console, otherwise, returns
#'   the result.
#' @export
ppp_verbose <- function(ppp = NULL, print = TRUE) {
  err <- NULL
  if (!f0(inll(ppp), T, is_valid_ppp(ppp))) {err <- c(err, "\n [ppp] must be NULL or a scalar valid property specification.")}
  if (!isTF(print)) {err <- c(err, "\n • [print] must be TRUE or FALSE.")}
  if (idef(err)) {stop(err)}
  if (!is.null(ppp)) {ppp <- ppp_all(ppp)}
  if (length(ppp) > 1) {stop("\n • '", ppp, "' contains more than 1 property.")}
  ppp <- .drop_iprefix(ppp)
  out <- ppp_defs()
  if (idef(ppp)) {
    fam <- av(out[out$value == ppp, 1])
    abb <- av(out[out$Value == ppp, 3])
    lab <- av(out[out$Value == ppp, 4])
    out <- paste0("\nFamily: '", fam, "'", "\nValue : '", ppp, "'", "\nShort : ", abb, "\nLong  : ", lab, ".\n\n")
  }
  if (!print) {return(out)}
  if (is.data.frame(out)) {print(out, n = nrow(out))} else {cat(out)}
  NULL
}

#' @describeIn ppp. Take an individual or combination/conjunctive property
#'   specification and expand it using plain, but concise, language. To get a
#'   verbose definition of any individual property, use \code{ppp_verbose}.
#' @export
ppp_concise <- function(ppp) {
  if (!is_valid_ppp(ppp)) {stop("\n • '", ppp, "' is not a valid property combination.")}
  if (length(combos_from_ppp(ppp)) != 1) {stop("\n • '", ppp, "' contains more than 1 property combination.")}
  ppp <- .drop_iprefix(combos_from_ppp(ppp))
  tbl <- ppp_defs()
  fam <- av(tbl$Family)
  val <- av(tbl$Value )
  abb <- av(tbl$Short )
  ccc <- abb[val %in% ppp & fam == "ccc"]
  ddd <- abb[val %in% ppp & fam == "ddd"]
  eee <- abb[val %in% ppp & fam == "eee"]
  fff <- abb[val %in% ppp & fam == "fff"]
  mmm <- abb[val %in% ppp & fam == "mmm"]
  sss <- abb[val %in% ppp & fam == "sss"]
  ttt <- abb[val %in% ppp & fam == "ttt"]
  obj <- length(ccc) == 0
  out <- paste0(c(ccc, ddd, eee, fff, mmm, sss, ttt), collapse = ", ")
  out <- av(strsplit(out, ", ", TRUE))
  out <- out[!duplicated(out)]
  out <- paste0(out, collapse = ", ")
  if (obj) {out <- paste0(out, " object")}
  if (substr(out, 1, 1) %in% c("a", "e", "i", "o", "u")) {pre <- "an "}
  else {pre <- "a "}
  out <- paste0(pre, out)
  return(out)
}

#' @describeIn ppp. Take one or more property combos separated by pipes (each
#'   combo may be a individual property) and expands each using plain, but
#'   concise, language, separating the multiple expansions with \code{'OR'}. To
#'   get a verbose definition of any individual property, use
#'   \code{ppp_verbose}.
#' @export
alt_ppp_concise <- function(ppp) {
  if (!is_valid_ppp(ppp)) {stop("\n • '", ppp, "' is not a valid property specification.")}
  ppp <- .drop_iprefix(combos_from_ppp(ppp))
  for (i in 1:length(ppp)) {ppp[i] <- ppp_concise(ppp[i])}
  return(paste0(ppp, collapse = " OR "))
}