#' @name xxx
#' @family props
#' @title All purpose property checking
#' @description Get all properties from \code{xxx_vals()} that apply to
#'   \code{x.}.
#' @details This set of functions provide utilities that bring together
#'   seven families of properties defined in this package as follows:
#'   \tabular{ll}{
#'     NAME OF           \tab PROPERTY                 \cr
#'     PROPERTY FAMILY   \tab TYPE                     \cr
#'     \code{\link{ttt}} \tab fundamental type         \cr
#'     \code{\link{sss}} \tab state of completeness    \cr
#'     \code{\link{ddd}} \tab defined dimensionality   \cr
#'     \code{\link{eee}} \tab effective dimensionality \cr
#'     \code{\link{fff}} \tab form                     \cr
#'     \code{\link{mmm}} \tab extended mode            \cr
#'     \code{\link{ccc}} \tab extended class           \cr
#'     \code{xxx}        \tab any property family        }
#'   There are associated property functions in the following forms:
#'   \tabular{lll}{
#'     FORM               \tab PROPERTY TYPE  \tab FUNCTION PURPOSE          \cr
#'     \code{xppp}        \tab \code{ppp} = any property value
#'                        \tab check an object for property \code{< xxx >}   \cr
#'     \code{ttt_ccc}     \tab \code{ttt} = a fundamental type property value,
#'                             \code{ccc} = an extended class property value
#'                        \tab check an object for a combination of fundamental
#'                             type and extended class.                      \cr
#'     \code{mmm_ccc}     \tab \code{mmm} = an extended mode property value,
#'                             \code{ccc} = an extended class property value
#'                        \tab check an object for a combination of extended
#'                             mode and extended class.                      \cr
#'     \code{cmp_mmm_ccc} \tab \code{cmp} = a complete object (i.e., a populated
#'                             object containing no \code{NA} values),
#'                             \code{mmm} = an extended mode property value,
#'                             \code{ccc} = an extended class property value
#'                        \tab check an object for completeness, extended mode,
#'                             and extended class simultaneously.            \cr
#'     \code{xxx_vals}    \tab \code{xxx} = the name of a property family
#'                        \tab get a character vector of all possible single
#'                             property values.                              \cr
#'     \code{xxx}         \tab \code{xxx} = name of a property family
#'                        \tab get a character vector of all properties from the
#'                             specified family that apply to an object.     \cr
#'     \code{is_xxx}      \tab \code{xxx} = name of a property family
#'                        \tab evaluate whether an object has one or more
#'                             properties within the specified property family
#'                             as well as whether it meets additional
#'                             restrictions as specified in \code{...}.        }
#'
#' @param atb. \code{TRUE} or \code{FALSE} indicating whether to return the
#'   result as a tibble with column 1 containing property values and
#'   column 2 containing the property families.
#' @param x. An object.
#' @param xxx. A character scalar containing one or more values from
#'   \code{xxx_vals()} separated by pipes and/or underscores. Combinations of
#'   properties can be specified by separating them with underscores. Separating
#'   properties or combinations of properties with pipes will result in a value
#'   of \code{TRUE} if any of them applies to \code{x.}.
#' @param valid. A character vector containing all properties considered valid.
#' @param ... Additional arguments to \code{\link{meets}} containing value and
#'   element/row/column count restrictions.
#' @section Additional arguments in \code{...}:
#'   \cr These arguments are optional. Possible arguments are as follows:
#'   \tabular{ll}{
#'     ARGUMENT   \tab USE AND MEANING                                       \cr
#'     \code{n}   \tab set of valid lengths/numbers of elements.             \cr
#'     \code{nr}  \tab set of valid numbers of rows.                         \cr
#'     \code{nc}  \tab set of valid numbers of columns.                      \cr
#'     \code{min} \tab minimum valid length/number of elements.              \cr
#'     \code{minr}\tab minimum valid number of rows.                         \cr
#'     \code{minc}\tab minimum valid number of columns.                      \cr
#'     \code{max} \tab maximum valid length/number of element.               \cr
#'     \code{maxr}\tab maximum valid number of rows.                         \cr
#'     \code{maxc}\tab maximum valid number of columns.                      \cr
#'     \code{vals}\tab set of valid values.                                  \cr
#'     \code{lt}  \tab value elements of \code{x.} must be less than.        \cr
#'     \code{le}  \tab upper bound (≤) on elements of \code{x.}              \cr
#'     \code{ge}  \tab lower bound (≥) on elements of \code{x.}              \cr
#'     \code{gt}  \tab value elements of \code{x.} must be greater than.       }
#' @return \code{xxx_vals}, \code{xxx_all}, \code{combos_from_xxx},
#'   \code{xxx_from_combo}, \code{xxx} and \code{xxx_funs} return a character
#'   vector. All others return \code{TRUE} or \code{FALSE}.
#' @export
xxx <- function(x.) {sort(c(sss(x.), ttt(x.), fff(x.), ddd(x.), eee(x.), mmm(x.), ccc(x.)))}

#' @describeIn xxx Evaluates whether any (combination) property in \code{xxx.}
#'   is applicable to \code{x.}, subject to any additional restrictions in
#'   \code{...}.
#' @export
ixxx <- function(x., xxx., ...) {
  if (!is_valid_xxx(xxx.)) {stop("\n • [xxx.] must be a character scalar with one or more values from xxx_vals() after separating [xxx.] by pipes and/or underscores.")}
  if (!meets(x., ...)) {return(F)}
  singles. <- xxx_vals()
  combos. <- combos_from_xxx(xxx.)
  for (combo. in combos.) {
    is. <- combo. %in% singles.
    ic. <- is_xxx_fun(combo.)
    if (!is. & !ic.) {
      new. <- xxx_from_combo(combo.)
      good.  <- TRUE
      for (xxx. in new.) {if (good.) {good. <- good. & eval(parse(text = paste0("x", xxx., "(x)")))}}
    }
    else if (is.) {good. <- eval(parse(text = paste0("x", combo., "(x)")))}
    else {good. <- eval(parse(text = paste0(combo., "(x)")))}
    if (good.) {return(TRUE)}
  }
  FALSE
}

#' @describeIn xxx Extract unique property values from \code{xxx.} by splitting
#'   along pipes (\code{"|"}) and underscores.
#' @export
xxx_all <- function(xxx., valid. = xxx_vals()) {
  vx. <- cmp_chr_scl(xxx.)
  vv. <- f0(cmp_chr_vec(valid.), all(valid. %in% xxx_vals()), F)
  err. <- NULL
  if (!vx.) {err. <- c(err., "\n • [xxx.] must be a non-NA character scalar.")}
  if (!vv.) {err. <- c(err., "\n • [valid.] must be a character vector containing only values from xxx_vals().")}
  if (idef(err.)) {stop(err.)}
  xxx. <- av(strsplit(xxx., "|", fixed = T))
  xxx. <- av(strsplit(xxx., "_", fixed = T))
  xxx. <- trimws(xxx.)
  xxx. <- xxx.[xxx. != ""]
  if (length(xxx.) == 0) {stop("\n • [xxx.] is empty after splitting on pipes and underscores.")}
  if (!all(xxx. %in% valid.)) {stop("\n • [xxx.] contains a property not in [valid.].")}
  sort(unique(xxx.))
}

#' @describeIn xxx Get a vector of all possible single property values in all
#'   property families.
#' @export
xxx_vals <- function(atb. = F) {
  if (!isTF(atb.)) {stop("\n • [atb.] must be TRUE or FALSE.", call. = F)}
  x. <- c(sss_vals(), ddd_vals(), eee_vals(), ttt_vals(), fff_vals(), ccc_vals(), mmm_vals())
  x. <- x.[order(paste0(names(x.), "_", x.))]
  if (!atb.) {return(x.)}
  tibble::tibble(family = names(x.), xxx = av(x.))
}

#' @describeIn xxx Evaluate whether \code{x.} is \code{NULL} or matches one or
#'   more property (combos) specified in \code{xxx.}.
#' @export
nll_or <- function(x., xxx., ...) {f0(inll(x.), T, ixxx(x., xxx., ...))}

#' @describeIn xxx Evaluate whether \code{x.} is an atomic scalar \code{NA} or
#'   matches one or more property (combos) specified in \code{xxx.}.
#' @export
nas_or <- function(x., xxx., ...) {f0(inas(x.), T, ixxx(x., xxx., ...))}

#' @describeIn xxx List all valid property function names.
#' @export
xxx_funs <- function(atb. = F) {
  if (!isTF(atb.)) {stop("\n • [atb.] must be TRUE or FALSE.")}
  x. <- c(paste0("x", xxx_vals()), cmp_mmm_arr_vals(), cmp_mmm_agn_vals(),
          mmm_arr_vals()         , mmm_agn_vals()    , cmp_mmm_atb_vals(),
          cmp_mmm_avl_vals()     , mmm_atb_vals()    , mmm_avl_vals()    ,
          cmp_mmm_avt_vals()     , cmp_mmm_mvc_vals(), mmm_avt_vals()    ,
          mmm_mat_vals()         , cmp_mmm_scl_vals(), cmp_mmm_vec_vals(),
          mmm_vec_vals()         , cmp_mmm_vals()    , mmm_mvc_vals()    ,
          mmm_scl_vals()         , ttt_ccc_vals()    )
  x. <- x.[order(paste0(names(x.), "_", av(x.)))]
  if (!atb.) {return(x.)}
  tibble::tibble(family = names(x.), property = av(x.))
}

#' @describeIn xxx Get a tibble with 4 columns: property family (Family),
#'   property value (Value), short property definition (Short), and long
#'   property definition (Long).
#' @export
xxx_table <- function() {
  tibble::tribble(
    ~Family, ~Value  , ~Short                                 , ~Long                                                                                                                                                                                                       ,
    "ccc"  , "agn"   , "populated, atomic generic"            , "A populated atomic generic (a vector or array)"                                                                                                                                                            ,
    "ccc"  , "arr"   , "populated, atomic array"              , "A populated atomic array"                                                                                                                                                                                  ,
    "ccc"  , "atb"   , "populated, atomic tibble"             , "A populated atomic tibble (containing only atomic columns)"                                                                                                                                                ,
    "ccc"  , "avl"   , "populated, atomic vlist"              , "A populated atomic vlist (a non-data.frame list whose elements are all populated and atomic)"                                                                                                              ,
    "ccc"  , "avt"   , "populated, atomic vtype"              , "A populated atomic vtype (a vector or array)"                                                                                                                                                              ,
    "ccc"  , "gen"   , "generic"                              , "A generic object (a vector, vlist, or array)."                                                                                                                                                             ,
    "ccc"  , "mat"   , "populated, atomic matrix"             , "A populated atomic matrix"                                                                                                                                                                                 ,
    "ccc"  , "mvc"   , "populated, atomic mvect"              , "An atomic mvect (i.e., a vect or vlist containing 2+ elements, including arrays with multiple index positions in exactly 1 dimension)"                                                                     ,
    "ccc"  , "scl"   , "populated, atomic scalar"             , "An atomic scalar"                                                                                                                                                                                          ,
    "ccc"  , "tib"   , "tibble"                               , "A tibble (a data.frame with properties from tibble)"                                                                                                                                                       ,
    "ccc"  , "vec"   , "populated, atomic vect"               , "An atomic vect (i.e., a vector or 1-dimensional array containing at least 1 element)"                                                                                                                      ,
    "ccc"  , "vls"   , "vlist"                                , "A vlist (vector-list, or a non-data.frame list)"                                                                                                                                                           ,
    "ccc"  , "vtp"   , "vtype"                                , "A vtype (vector-type, or an empty or populated vector, vlist, or array with multiple index positions in 0 or 1 dimension)"                                                                                 ,
    "ddd"  , "d0D"   , "0-dimensional"                        , "A 0-dimensional object (i.e., NULL)"                                                                                                                                                                       ,
    "ddd"  , "d1D"   , "1-dimensional"                        , "A 1-dimensional object (i.e., a vector, vlist, or 1-dimensional array)"                                                                                                                                    ,
    "ddd"  , "d2D"   , "2-dimensional"                        , "A 2-dimensional object (i.e., a matrix or tibble)"                                                                                                                                                         ,
    "ddd"  , "dHD"   , "hyper-dimensional"                    , "A hyper-dimensional object (i.e., an array with 3+ dimensions)"                                                                                                                                            ,
    "eee"  , "e0D"   , "effectively 0-dimensional"            , "An effectively 0-dimensional object (i.e., a vector, vlist, or array of length 2 or a tibble with 1 row and 1 column"                                                                                      ,
    "eee"  , "e1D"   , "effectively 1-dimensional"            , "An effectively 1-dimensional object (i.e., a vector or vlist with 2+ elements, a row or column matrix or tibble, or an populated array with more than one index position in exactly 1 dimension)"          ,
    "eee"  , "e2D"   , "effectively 2-dimensional"            , "An effectively 2-dimensional object (i.e., a matrix or tibble with 2+ rows and 2+ columns or a populated array with multiple index positions in exactly 2 dimensions)"                                     ,
    "eee"  , "eHD"   , "effectively hyper-dimensional"        , "An effectively hyper-dimensional object (i.e., a populated array with multiple index positions in 3+ dimensions)"                                                                                          ,
    "eee"  , "eUD"   , "effectively undimensional"            , "An effectively undimensional object (i.e., of length 0)"                                                                                                                                                   ,
    "fff"  , "c0l"   , "column"                               , "A column object (i.e., a column data.frame or a column matrix)"                                                                                                                                            ,
    "fff"  , "emp"   , "empty"                                , "An empty object (i.e., of length 0, but not NULL)"                                                                                                                                                         ,
    "fff"  , "lin"   , "linear"                               , "A linear object (i.e., a vector, vlist, or 1-dimensional array of length 2+, a row or column matrix, a row or column tibble, or a populated array with multiple indexing positions in exactly 1 dimension)",
    "fff"  , "pnt"   , "point"                                , "A point object (i.e., a length-1 vector, array, or vlist or a tibble with 1 row and 1 column)"                                                                                                             ,
    "fff"  , "r0w"   , "row"                                  , "A row object (i.e., a row data.frame or a row matrix)"                                                                                                                                                     ,
    "fff"  , "rct"   , "rectangular"                          , "A rectangular object (i.e., a 2+ by 2+ data.frame or matrix, or a higher-dimensional array with multiple index positions in exactly 2 dimensions)"                                                         ,
    "fff"  , "sld"   , "solid"                                , "A solid object (i.e., an array with multiple index positions in 3+ dimensions)"                                                                                                                            ,
    "fff"  , "sqr"   , "square"                               , "A square, atomic matrix"                                                                                                                                                                                   ,
    "mmm"  , "ch1"   , "populated, character, single-letter"  , "A populated, character object containing single-character elements"                                                                                                                                        ,
    "mmm"  , "chr"   , "populated, character"                 , "A populated, character object containing values of any character count"                                                                                                                                    ,
    "mmm"  , "clr"   , "populated, character, color value"    , "A populated, character object containing valid color values"                                                                                                                                               ,
    "mmm"  , "cnb"   , "populated, no-blank-string, character", "A populated, character object containing no blank strings"                                                                                                                                                 ,
    "mmm"  , "evn"   , "populated, even, whole-number"        , "A populated, even-number object"                                                                                                                                                                           ,
    "mmm"  , "fac"   , "populated, factor"                    , "A populated, factor object"                                                                                                                                                                                ,
    "mmm"  , "frc"   , "populated, numeric, fractional"       , "A populated, numeric, fractional object (i.e., containing 1 or more non-whole-number values)"                                                                                                              ,
    "mmm"  , "ind"   , "populated, indexing"                  , "A populated, indexing object (i.e., logical or positive whole number)"                                                                                                                                     ,
    "mmm"  , "lgl"   , "populated, logical"                   , "A populated, logical object"                                                                                                                                                                               ,
    "mmm"  , "neg"   , "populated, numeric, negative"         , "A populated, negative, numeric object"                                                                                                                                                                     ,
    "mmm"  , "ngw"   , "populated, negative, whole number"    , "A populated, negative, whole-number object"                                                                                                                                                                ,
    "mmm"  , "nng"   , "populated, numeric, nonnegative"      , "A populated, non-negative, numeric object"                                                                                                                                                                 ,
    "mmm"  , "nnw"   , "populated, nonnegative, whole-number" , "A populated, non-negative, whole-number object"                                                                                                                                                            ,
    "mmm"  , "nps"   , "populated, numeric, nonpositive"      , "A populated, non-positive, numeric object"                                                                                                                                                                 ,
    "mmm"  , "npw"   , "populated, nonpositive, whole-number" , "A populated, non-positive, whole-number object"                                                                                                                                                            ,
    "mmm"  , "nst"   , "populated, nonsortable"               , "A populated, non-sortable, atomic object (i.e., not character, logical, numeric, or ordered factor)"                                                                                                       ,
    "mmm"  , "num"   , "populated, numeric"                   , "A populated, numeric object"                                                                                                                                                                               ,
    "mmm"  , "odd"   , "populated, odd, whole-number"         , "A populated, odd-number object"                                                                                                                                                                            ,
    "mmm"  , "ord"   , "populated, ordered, factor"           , "A populated, ordered, factor object"                                                                                                                                                                       ,
    "mmm"  , "pct"   , "populated, numeric, percentage"       , "A populated, numeric, percentage object (i.e., containing only values in the interval [0, 100])"                                                                                                           ,
    "mmm"  , "pos"   , "populated, numeric, positive"         , "A populated, positive, numeric object"                                                                                                                                                                     ,
    "mmm"  , "ppn"   , "populated, numeric, proportion"       , "A populated, numeric, proportion object (i.e., containing only values in the interval [0, 1])"                                                                                                             ,
    "mmm"  , "psw"   , "populated, positive, whole-number"    , "A populated, positive, whole-number object"                                                                                                                                                                ,
    "mmm"  , "srt"   , "populated, sortable"                  , "A populated, sortable, atomic object (i.e., character, logical, numeric, or ordered factor)"                                                                                                               ,
    "mmm"  , "uno"   , "populated, unordered, factor"         , "A populated, unordered, factor object"                                                                                                                                                                     ,
    "mmm"  , "whl"   , "populated, whole-number"              , "A populated, whole-number object"                                                                                                                                                                          ,
    "sss"  , "cmp"   , "populated, atomic, complete"          , "A complete atomic object, atomic vlist, or atomic tibble (i.e., containing no NA values)"                                                                                                                  ,
    "sss"  , "mss"   , "populated, atomic, completely missing", "A completely missing atomic object, atomic vlist, or atomic tibble (i.e., containing only NA values)"                                                                                                      ,
    "sss"  , "nas"   , "atomic, NA scalar"                    , "An atomic, NA scalar"                                                                                                                                                                                      ,
    "sss"  , "oks"   , "atomic, non-NA scalar"                , "An atomic, non-NA scalar"                                                                                                                                                                                  ,
    "sss"  , "prt"   , "populated, atomic, partially missing" , "A partially missing atomic object, atomic vlist, or atomic tibble (i.e., containing both NA and non-NA values)"                                                                                            ,
    "ttt"  , "atm"   , "atomic"                               , "An atomic object"                                                                                                                                                                                          ,
    "ttt"  , "def"   , "defined"                              , "A defined object (i.e., not NULL)"                                                                                                                                                                         ,
    "ttt"  , "fun"   , "function or function name"            , "A function object or a character scalar containing a function name"                                                                                                                                        ,
    "ttt"  , "nil"   , "nil"                                  , "A nil object (i.e., of length 0, including NULL)"                                                                                                                                                          ,
    "ttt"  , "nll"   , "NULL"                                 , "The NULL object"                                                                                                                                                                                           ,
    "ttt"  , "pop"   , "populated"                            , "A populated object (i.e., not of length 0)"                                                                                                                                                                ,
    "ttt"  , "rcr"   , "recursive"                            , "A recursive object (i.e., a tibble or non-data.frame list)"
  )
}

#' @describeIn xxx Evaluate \code{x.} to see if it the name of a property
#'   function.
#' @export
is_xxx_fun <- function(x.) {
  if (!cmp_chr_scl(x.)) {stop("\n • [x.] must be a non-NA character scalar.")}
  x. %in% xxx_funs()
}

#' @describeIn xxx Evaluate whether \code{xxx.} is a character scalar of values
#'   from \code{xxx_vals} separated by pipes ('|') and/or underscores. Always
#'   returns either \code{TRUE} or \code{FALSE}.
#' @export
is_valid_xxx <- function(xxx.) {
  if (!cmp_chr_scl(xxx.)) {return(FALSE)}
  xxx. <- av(strsplit(xxx., "_", TRUE))
  xxx. <- av(strsplit(xxx., "|", TRUE))
  xxx. <- trimws(xxx.)
  xxx. <- xxx.[xxx. != ""]
  if (length(xxx.) == 0) {return(FALSE)}
  all(xxx. %in% xxx_vals())
}

#' @describeIn xxx Extract each single property value from a property
#'   combination by splitting along underscores.
#' @export
xxx_from_combo <- function(xxx., valid. = xxx_vals()) {
  vx. <- cmp_chr_scl(xxx.)
  vv. <- f0(cmp_chr_vec(valid.), all(valid. %in% xxx_vals()), F)
  err. <- NULL
  if (!vx.) {err. <- c(err., "\n • [xxx.] must be a non-NA character scalar.")}
  if (!vv.) {err. <- c(err., "\n • [valid.] must be a character vector containing only values from xxx_vals().")}
  if (!is.null(err.)) {stop(err.)}
  xxx. <- av(strsplit(xxx., "_", fixed = T))
  xxx. <- trimws(xxx.)
  xxx. <- xxx.[xxx. != ""]
  if (length(xxx.) == 0) {stop("\n • [xxx.] is empty after splitting on pipes and underscores.")}
  if (!all(xxx. %in% valid.)) {stop("\n • [xxx.] contains a property not in [valid].")}
  sort(unique(xxx.))
}

#' @describeIn xxx Extract each property combination from \code{xxx.} by
#'   splitting along pipes (\code{"|"}).
#' @export
combos_from_xxx <- function(xxx., valid. = xxx_vals()) {
  xxx_all(xxx., valid.)
  xxx. <- av(strsplit(xxx., "|", fixed = T))
  xxx. <- trimws(xxx.)
  xxx. <- xxx.[xxx. != ""]
  sort(unique(xxx.))
}



#' @describeIn xxx Get the tibble from \code{xxx_table()}. If
#'   \code{xxx.} contains a single valid property value (no combination values),
#'   extracts the associated row from the tibble and creates a character scalar
#'   with the property family, property value, short property definition, and
#'   long property definition. If \code{print. = TRUE}, prints the result (either
#'   the tibble or the extracted row) to the console, otherwise, returns the
#'   result.
#' @export
define_xxx <- function(xxx. = NULL, print. = TRUE) {
  err. <- NULL
  if (!f0(inll(xxx.), T, is_valid_xxx(xxx.))) {err. <- c(err., "\n [xxx.] must be NULL or a single valid property.")}
  if (!isTF(print.)) {err. <- c(err., "\n • [print.] must be TRUE or FALSE.")}
  if (idef(err.)) {stop(err.)}
  if (!is.null(xxx.)) {xxx. <- xxx_all(xxx.)}
  if (length(xxx.) > 1) {stop("\n • [xxx.] contains more than 1 property.")}
  out. <- xxx_table()
  if (idef(xxx.)) {
    fam. <- av(out.[out.$value == xxx., 1])
    abb. <- av(out.[out.$Value == xxx., 3])
    lab. <- av(out.[out.$Value == xxx., 4])
    out. <- paste0("\nFamily: '", fam., "'" ,
                   "\nValue : '", xxx., "'" ,
                   "\nShort : " , abb.,
                   "\nLong  : " , lab., ".\n\n")
  }
  if (!print.) {return(out.)}
  if (tibble::is_tibble(out.)) {print(out., n = nrow(out.))} else {cat(out.)}
  NULL
}

#' @describeIn xxx Take a single property combo (which may be a single property)
#'   and expands it using plain, but concise, language. To get a verbose
#'   definition of any single property, use \code{define_xxx}.
#' @export
define_xxx_combo <- function(xxx.) {
  if (!is_valid_xxx(xxx.)) {stop("\n • [xxx.] is not a valid property combination.")}
  if (length(combos_from_xxx(xxx.)) != 1) {stop("\n • [xxx.] contains more than 1 property combination.")}
  xxx. <- combos_from_xxx(xxx.)
  tbl. <- xxx_table()
  fam. <- as.vector(tbl.$Family)
  val. <- as.vector(tbl.$Value )
  abb. <- as.vector(tbl.$Short )
  ccc. <- abb.[val. %in% xxx. & fam. == "ccc"]
  ddd. <- abb.[val. %in% xxx. & fam. == "ddd"]
  eee. <- abb.[val. %in% xxx. & fam. == "eee"]
  fff. <- abb.[val. %in% xxx. & fam. == "fff"]
  mmm. <- abb.[val. %in% xxx. & fam. == "mmm"]
  sss. <- abb.[val. %in% xxx. & fam. == "sss"]
  ttt. <- abb.[val. %in% xxx. & fam. == "ttt"]
  obj. <- length(ccc.) == 0
  out. <- paste0(c(ccc., ddd., eee., fff., mmm., sss., ttt.), collapse = ", ")
  out. <- av(strsplit(out., ", ", TRUE))
  out. <- out.[!duplicated(out.)]
  out. <- paste0(out., collapse = ", ")
  if (obj.) {out. <- paste0(out., " object")}
  if (substr(out., 1, 1) %in% c("a", "e", "i", "o", "u")) {pre. <- "an "}
  else {pre. <- "a "}
  out. <- paste0(pre., out.)
  return(out.)
}

#' @describeIn xxx Take one or more property combos separated by pipes (each
#'   combo may be a single property) and expands each using plain, but concise,
#'   language, separating the multiple expansions with \code{'OR'}. To get a
#'   verbose definition of any single property, use \code{define_xxx}.
#' @export
define_xxx_combos <- function(xxx.) {
  if (!is_valid_xxx(xxx.)) {stop("\n • [xxx.] is not a valid property specification.")}
  xxx. <- combos_from_xxx(xxx.)
  for (i. in 1:length(xxx.)) {xxx.[i.] <- define_xxx_combo(xxx.[i.])}
  return(paste0(xxx., collapse = " OR "))
}
