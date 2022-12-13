#' @family props
#' @title All purpose property checking
#' @description This set of functions provide utilities that bring together seven families of object properties defined by this package:
#' \tabular{rl}{
#'       \code{\link{bbb}}   \tab basic.
#'   \cr \code{\link{ccc}}   \tab xclass.
#'   \cr \code{\link{ddd}}   \tab defined dimensionality.
#'   \cr \code{\link{eee}}   \tab effective dimensionality.
#'   \cr \code{\link{iii}}   \tab integrity.
#'   \cr \code{\link{mmm}}   \tab xmode.
#'   \cr \code{\link{sss}}   \tab shape.
#' }
#' **single property specs** are character scalars containing exactly `3` characters (e.g., `'ord', 'dtf', 'd1D', 'rct'`).
#' \cr\cr
#' **Combination (conjunctive) property specs** are for properties that must co-occur. They are constructed by delimiting multiple single properties with underscores (e.g., `'ord_dtf_d1D_rct'` or an equivalent underscore-delimited permutation).
#' \cr\cr
#' **Alternate (compensatory) property specs** are to indicate that if any one (single or combination) property spec is satisfied the entire spec is satisfied. They are constructed by pipe-delimiting multiple single or combination properties. For example, the spec `'ord|dtf_d1D||dtf_rct'` would be satisfied by an object with single property `'ord'`, combined property `'dtf_d1D'`, or combined property `'dtf_rct'`.
#' \cr\cr
#' **Property and property spec manipulation functions**
#' \tabular{rl}{
#'       `combos_from_spec`   \tab Convert spec to constituent combos.
#'   \cr `props_from_combo`   \tab Convert combo to constituent properties.
#'   \cr  `props_from_spec`   \tab Convert spec to constituent properties.
#' }
#' **Property and property spec validation functions**
#' \tabular{rl}{
#'      `is_prop_combo`   \tab Is `combo` a combo/conjunctive spec?
#'   \cr `is_prop_spec`   \tab Is `spec` a property spec of any type?
#'   \cr  `is_prop_fun`   \tab Is `fun` a property-function name?
#'   \cr      `is_prop`   \tab Is `prop` a value from `prop_vals()`?
#' }
#' **Comprehensive property and common property-specs**
#' \tabular{rl}{
#'      `prop_funs`   \tab Names of functions checking for specific single and combo properties matched to `x`.
#' }
#' **Family-specific object property listing functions**
#' \tabular{rl}{
#'       `ppp`   \tab gets all single properties matching `x`.
#'   \cr `bbb`   \tab   base
#'   \cr `ccc`   \tab   extended class
#'   \cr `ddd`   \tab   defined dimensionality
#'   \cr `eee`   \tab   effective dimensionality
#'   \cr `iii`   \tab   integrity
#'   \cr `mmm`   \tab   extended mode
#'   \cr `sss`   \tab   shape
#' }
#' **Property and property spec definition functions**
#' \tabular{rl}{
#'      `combo_concise`   \tab Concisely define a combo/conjunctive property.
#'   \cr `spec_concise`   \tab Concisely define a property spec.
#'   \cr `prop_verbose`   \tab Verbosely define an single property.
#'   \cr    `prop_defs`   \tab Table of single property definitions.
#' }
#' **Multi-family property spec matching functions**
#' \tabular{rl}{
#'       `nll_or`   \tab Is `x` `NULL` or a match to `spec`?
#'   \cr `nas_or`   \tab Is `x` scalar `NA` or a match to `spec`?
#'   \cr   `ippp`   \tab Is `x` a match to `spec`?
#' }
#' **Single-property  matching functions**
#' \tabular{rl}{
#'     `iPPP`   \tab does `x` match single property `'PPP'`.
#' }
#' **Single-family, single-property matching functions**
#' \tabular{rl}{
#'       `ibbb`   \tab Does  `x` match base property spec `spec`?
#'   \cr `iccc`   \tab   extended class
#'   \cr `iddd`   \tab   defined dimensionality
#'   \cr `ieee`   \tab   effective dimensionality
#'   \cr `iiii`   \tab   integrity
#'   \cr `immm`   \tab   extended mode
#'   \cr `isss`   \tab   shape
#' }
#' **Functions testing for combination/conjunctive properties**
#' \tabular{rl}{
#'     `cmp_MMM_CCC`   \tab Complete` + xmode 'MMM' + xclass 'CCC'`.
#'   \cr   `cmp_MMM`   \tab Complete` + xmode 'MMM'`.
#'   \cr   `cmp_CCC`   \tab Complete` + xclass 'CCC'`
#'   \cr   `BBB_CCC`   \tab Base property` 'BBB' + xclass 'CCC'`.
#'   \cr   `BBB_MMM`   \tab Base property` 'BBB' + xmode 'MMM'`.
#'   \cr   `MMM_CCC`   \tab `xmode 'MMM' + xclass 'CCC'`.
#' }
#' **Comprehensive lists of single and common combination properties**
#' \tabular{rl}{
#'     `cmp_mmm_ccc_props`   \tab Complete` emode + xclass `properties.
#'   \cr                     \tab   
#'   \cr   `cmp_ccc_props`   \tab Complete` xclass `properties.
#'   \cr   `cmp_mmm_props`   \tab Complete` xmode `properties.
#'   \cr   `bbb_ccc_props`   \tab Basic` + xclass `properties.
#'   \cr   `bbb_mmm_props`   \tab Basic` + xmode `properties.
#'   \cr   `mmm_ccc_props`   \tab `xmode + xclass` properties.
#'   \cr                     \tab   
#'   \cr       `bbb_props`   \tab Single basic properties.
#'   \cr       `ccc_props`   \tab Single `xclass` properties.
#'   \cr       `ddd_props`   \tab Single defined dimension properties.
#'   \cr       `eee_props`   \tab single effective dimension properties.
#'   \cr       `iii_props`   \tab single integrity properties.
#'   \cr       `mmm_props`   \tab single `xmode` properties.
#'   \cr       `sss_props`   \tab single shape properties.
#'   \cr                     \tab   
#'   \cr       `prop_vals`   \tab All possible single properties.
#' }
#' @param as.dtf A non-`NA` logical scalar indicating whether to return the result as a data.frame with column `1` containing property values and column `2` containing the property families.
#' @param x An R object.
#' @param ppp A \link[=cmp_chr_scl]{complete character scalar} containing one or more values from `ppp_vals()` separated by pipes and/or underscores. Combinations of properties can be specified by separating them with underscores. Separating properties or combinations of properties with pipes will result in a value of `TRUE` if any of them applies to `x`.
#' @param valid A \link[=cmp_chr_vec]{complete character vec} containing all properties considered valid.
#' @param print A non-`NA` logical scalar indicating whether to print the property definition to the console.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return \tabular{rl}{
#'       `combos_from_spec`   \tab A character
#'   \cr `props_from_combo`   \tab vector.
#'   \cr  `props_from_spec`   \tab   
#'   \cr        `prop_funs`   \tab   
#'   \cr        `prop_vals`   \tab   
#'   \cr              `ppp`   \tab   
#'   \cr                      \tab   
#'   \cr   `is_valid_combo`   \tab A logical
#'   \cr    `is_valid_spec`   \tab scalar.
#'   \cr    `is_valid_prop`   \tab   
#'   \cr      `is_prop_fun`   \tab   
#'   \cr           `nas_or`   \tab   
#'   \cr           `nll_or`   \tab   
#'   \cr             `ippp`   \tab   
#'   \cr                      \tab   
#'   \cr    `combo_concise`   \tab A character
#'   \cr     `spec_concise`   \tab scalar.
#'   \cr     `prop_verbose`   \tab   
#'   \cr                      \tab   
#'   \cr        `prop_defs`   \tab A \link[=tb]{tibble}.
#' }
#' @examples
#' prop_vals()
#' prop_funs()
#' prop_verbose("srt")
#' prop_verbose("nnw")
#' combo_concise("cmp_psw_vls")
#' spec_concise("nll|cmp_psw_vls|ch1_scl")
#' props_from_spec("nll|cmp_psw_vls|ch1_scl")
#' combos_from_spec("nll|cmp_psw_vls|ch1_scl")
#' props_from_combo("cmp_psw_vls")
#' nll_or("1", "ch1")
#' nll_or(NULL, "ch1")
#' nll_or(7, "ch1")
#' nas_or(NA, "ch1")
#' nas_or("5", "ch1")
#' ippp("7", "nll|cmp_psw_vls|ch1_scl")
#' ippp(NULL, "nll|cmp_psw_vls|ch1_scl")
#' ippp("35", "nll|cmp_psw_vls|ch1_scl")
#' is_prop_combo("letter")
#' is_prop_combo("cmp_psw_vls")
#' is_prop_spec("nll|cmp_psw_vls|ch1_scl")
#' is_prop_fun("18")
#' is_prop_fun("cmp_psw_vls")
#' is_prop("18")
#' is_prop("vls")
#' ppp(letters)
#' prop_defs()
#' @export
ppp <- function(x) {sort(c(bbb(x), ccc(x), ddd(x), eee(x), iii(x), mmm(x), sss(x)))}

#' @rdname ppp
#' @export
prop_vals <- function(as.dtf = F) {
  if (!isTF(as.dtf)) {stop(.errs("[as.dtf] must be TRUE or FALSE."))}
  bval <- bbb_props(); bfam <- rep("bbb", length(bval)); blab <- paste0("b_", bval)
  cval <- ccc_props(); cfam <- rep("ccc", length(cval)); clab <- paste0("c_", cval)
  dval <- ddd_props(); dfam <- rep("ddd", length(dval)); dlab <- paste0("d_", dval)
  eval <- eee_props(); efam <- rep("eee", length(eval)); elab <- paste0("e_", eval)
  ival <- iii_props(); ifam <- rep("iii", length(ival)); ilab <- paste0("i_", ival)
  mval <- mmm_props(); mfam <- rep("mmm", length(mval)); mlab <- paste0("m_", mval)
  sval <- sss_props(); sfam <- rep("sss", length(sval)); slab <- paste0("s_", sval)
  val <-       c(bval, cval, dval, eval, ival, mval, sval)
  fam <-       c(bfam, cfam, dfam, efam, ifam, mfam, sfam)
  ord <- order(c(blab, clab, dlab, elab, ilab, mlab, slab))
  if (!as.dtf) {return(val[ord])}
  tibble::tibble(family = fam[ord], ppp = val[ord])
}

#' @rdname ppp
#' @export
prop_funs <- function(as.dtf = F) {
  if (!isTF(as.dtf)) {stop(.errs("[as.dtf] must be TRUE or FALSE."))}
  b_fun <- paste0("i", bbb_props()); b_fam <- rep("bbb", length(b_fun)); b_lab <- paste0("1_", b_fam, bbb_props())
  c_fun <- paste0("i", ccc_props()); c_fam <- rep("ccc", length(c_fun)); c_lab <- paste0("1_", c_fam, ccc_props())
  d_fun <- paste0("i", ddd_props()); d_fam <- rep("ddd", length(d_fun)); d_lab <- paste0("1_", d_fam, ddd_props())
  e_fun <- paste0("i", eee_props()); e_fam <- rep("eee", length(e_fun)); e_lab <- paste0("1_", e_fam, eee_props())
  i_fun <- paste0("i", iii_props()); i_fam <- rep("iii", length(i_fun)); i_lab <- paste0("1_", i_fam, iii_props())
  m_fun <- paste0("i", mmm_props()); m_fam <- rep("mmm", length(m_fun)); m_lab <- paste0("1_", m_fam, mmm_props())
  s_fun <- paste0("i", sss_props()); s_fam <- rep("sss", length(s_fun)); s_lab <- paste0("1_", s_fam, sss_props())
  bc_fun <- bbb_ccc_props(); bc_fam <- rep("bbb_ccc", length(bc_fun)); bc_lab <- paste0("2_", bc_fam, "_", bc_fun)
  bm_fun <- bbb_mmm_props(); bm_fam <- rep("bbb_mmm", length(bm_fun)); bm_lab <- paste0("2_", bm_fam, "_", bm_fun)
  mc_fun <- mmm_ccc_props(); mc_fam <- rep("mmm_ccc", length(mc_fun)); mc_lab <- paste0("2_", mc_fam, "_", mc_fun)
  cc_fun <- cmp_ccc_props(); cc_fam <- rep("cmp_ccc", length(cc_fun)); cc_lab <- paste0("3_", cc_fam, "_", cc_fun)
  cm_fun <- cmp_mmm_props(); cm_fam <- rep("cmp_mmm", length(cc_fun)); cm_lab <- paste0("3_", cm_fam, "_", cm_fun)
  cmc_fun <- cmp_mmm_ccc_props(); cmc_fam <- rep("cmp_mmm_ccc", length(cmc_fun)); cmc_lab <- paste0("4_", cmc_fam, "_", cmc_fun)
  fun <-       c(b_fun, c_fun, d_fun, e_fun, i_fun, m_fun, s_fun, bc_fun, bm_fun, mc_fun, cc_fun, cm_fun, cmc_fun)
  fam <-       c(b_fam, c_fam, d_fam, e_fam, i_fam, m_fam, s_fam, bc_fam, bm_fam, mc_fam, cc_fam, cm_fam, cmc_fam)
  ord <- order(c(b_lab, c_lab, d_lab, e_lab, i_lab, m_lab, s_lab, bc_lab, bm_lab, mc_lab, cc_lab, cm_lab, cmc_lab))
  if (!as.dtf) {return(fun[ord])}
  tibble::tibble(family = fam[ord], fun = fun[ord])
}

#' @rdname ppp
#' @export
is_prop <- function(prop) {
  if (!cmp_ch3_scl(prop)) {return(FALSE)}
  prop %in% prop_vals()
}

#' @rdname ppp
#' @export
is_prop_fun <- function(fun) {
  if (!cmp_chr_scl(fun)) {stop(.errs("[fun] must be a complete character scalar (?cmp_chr_scl)."))}
  fun %in% prop_funs()
}

#' @rdname ppp
#' @export
is_prop_spec <- function(spec) {
  if (!cmp_chr_scl(spec)) {return(FALSE)}
  spec <- av(strsplit(spec, "_", TRUE))
  spec <- av(strsplit(spec, "|", TRUE))
  spec <- trimws(spec)
  spec <- spec[spec != ""]
  spec <- .drop_iprefix(spec)
  if (length(spec) == 0) {return(FALSE)}
  all(spec %in% prop_vals())
}

#' @rdname ppp
#' @export
is_prop_combo <- function(combo) {
  if (!cmp_chr_scl(combo)) {return(FALSE)}
  combo <- av(strsplit(combo, "_", TRUE))
  combo <- trimws(combo)
  combo <- combo[combo != ""]
  combo <- .drop_iprefix(combo)
  if (length(combo) == 0) {return(FALSE)}
  all(combo %in% prop_vals())
}

#' @rdname ppp
#' @export
props_from_spec <- function(spec, valid = prop_vals()) {
  ok.valid <- f0(cmp_chr_vec(valid), all(valid %in% prop_vals()), F)
  errs <- c(f0(cmp_chr_scl(spec), NULL, "[spec] must be a complete character scalar (?cmp_chr_scl)."),
            f0(ok.valid         , NULL, "[valid] must be a complete character vector (?cmp_chr_vec) containing only values from ppp_vals()."))
  if (!is.null(errs)) {stop(.errs(errs))}
  combos <- av(strsplit(spec, "|", fixed = T))
  singles <- trimws(av(strsplit(combos, "_", fixed = T)))
  singles <- singles[singles != ""]
  singles <- .drop_iprefix(singles)
  if (length(singles) == 0)     {stop(.errs("The property spec [spec] is empty after splitting on pipes and underscores."))}
  if (!all(singles %in% valid)) {stop(.errs("The property spec [spec] contains a property not in ppp_props()."))}
  sort(unique(singles))
}

#' @rdname ppp
#' @export
combos_from_spec <- function(spec, valid = prop_vals()) {
  ok.valid <- f0(cmp_chr_vec(valid), all(valid %in% prop_vals()), F)
  errs <- c(f0(cmp_chr_scl(spec), NULL, "[spec] must be a complete character scalar (?cmp_chr_scl)."),
            f0(ok.valid         , NULL, "[valid] must be a complete character vec (?cpm_chr_vec) containing only values from prop_vals()."))
  if (!is.null(errs)) {stop(.errs(errs))}
  ppp.singles <- props_from_spec(spec, valid)
  if (!all(ppp.singles %in% valid)) {stop(.errs(p0("[spec] contains a property not in c(", g(p0("'", valid, "'"), g = ", "), ").")))}
  out <- trimws(av(strsplit(spec, "|", fixed = T)))
  out <- out[out != ""]
  out <- .drop_iprefix(out)
  sort(unique(out))
}

#' @rdname ppp
#' @export
props_from_combo <- function(combo, valid = prop_vals()) {
  ok.valid <- f0(cmp_chr_vec(valid), all(valid %in% prop_vals()), F)
  errs <- c(f0(cmp_chr_scl(combo), NULL, "[combo] must be a complete character scalar (?cmp_chr_scl)."),
            f0(ok.valid          , NULL, "[valid] must be a complete character vec (?cpm_chr_vec) containing only values from prop_vals()."))
  if (!is.null(errs)) {stop(.errs(errs))}
  if (length(combo) != length(av(strsplit(combo, "|", fixed = T)))) {stop(.errs("[combo] contains multiple alternate property specs."))}
  out <- trimws(av(strsplit(combo, "_", fixed = T)))
  out <- out[out != ""]
  out <- .drop_iprefix(out)
  if (length(out) == 0    ) {stop(.errs("[combo] contains no property specs."))}
  if (!all(out %in% valid)) {stop(.errs(p0("[combo] contains a property not in c(", g(p0("'", valid, "'"), g = ", "), ").")))}
  sort(unique(out))
}

#' @rdname ppp
#' @export
ippp <- function(x, spec, ...) {
  if (!is_prop_spec(spec)) {stop(.errs("[spec] specifies a property not in ppp_props()."))}
  if (!meets(x, ...)) {return(F)}
  all.props <- prop_vals()
  combos <- combos_from_spec(spec)
  for (combo in combos) {
    is.one <- combo %in% all.props
    is.fun <- is_prop_fun(combo)
    if (!is.one & !is.fun) {
      singles <- .drop_iprefix(props_from_combo(combo))
      meets <- TRUE
      for (prop in singles) {if (meets) {meets <- meets & eval(parse(text = paste0("i", prop, "(x)")))}}
    }
    else if (is.one) {meets <- eval(parse(text = paste0("i", combo, "(x)")))}
    else {meets <- eval(parse(text = paste0(combo, "(x)")))}
    if (meets) {return(TRUE)}
  }
  FALSE
}

#' @rdname ppp
#' @export
nll_or <- function(x, spec, ...) {f0(inll(x), T, ippp(x, spec, ...))}

#' @rdname ppp
#' @export
nas_or <- function(x, spec, ...) {f0(inas(x), T, ippp(x, spec, ...))}

#' @rdname ppp
#' @export
prop_defs <- function() {
  tibble::tribble(
    ~family, ~value  , ~short                              , ~long,
    "bbb"  , "atm"   , "atomic"                            , "An atomic object",
    "bbb"  , "def"   , "defined"                           , "A defined object (not NULL)",
    "bbb"  , "fun"   , "function or function name"         , "A function object or a character scalar containing a function name",
    "bbb"  , "nil"   , "nil"                               , "A nil object (of length 0, including NULL)",
    "bbb"  , "nll"   , "NULL"                              , "The NULL object",
    "bbb"  , "pop"   , "populated"                         , "A populated object (not of length 0)",
    "bbb"  , "rcr"   , "recursive"                         , "A recursive object (a data.frame or vlist)",
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
    "iii"  , "cmp"   , "atomic and complete"               , "A complete atomic object, complete atomic vlist, or complete atomic data.frame (without any NA values)",
    "iii"  , "mss"   , "atomic and missing"                , "A missing atomic object, missing atomic vlist, or missing atomic data.frame (without any non-NA values)",
    "iii"  , "nas"   , "atomic NA scalar"                  , "An atomic NA scalar object",
    "iii"  , "oks"   , "atomic non-NA scalar"              , "An atomic non-NA scalar object",
    "iii"  , "prt"   , "atomic and partial"                , "A partial atomic object, partial atomic vlist, or partial atomic data.frame (with both NA and non-NA values)",
    "mmm"  , "ch1"   , "onechar"                           , "Any non-NA values are character scalars containing exactly 1 character",
    "mmm"  , "ch3"   , "threechar"                         , "Any non-NA values are character scalars containing exactly 3 characters",
    "mmm"  , "chr"   , "character"                         , "A character object",
    "mmm"  , "clr"   , "character color value"             , "A character object containing valid color values",
    "mmm"  , "evn"   , "even whole-number"                 , "An even whole-number object",
    "mmm"  , "fac"   , "factor"                            , "An ordered-factor or unordered-factor object",
    "mmm"  , "frc"   , "fractional numeric"                , "A fractional numeric object (having 1+ non-whole-number values)",
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
    "mmm"  , "str"   , "string"                            , "A string object (character without any blanks)",
    "mmm"  , "srt"   , "sortable"                          , "A sortable object (character, logical, numeric, or ordered factor)",
    "mmm"  , "uno"   , "unordered-factor"                  , "A unordered-factor object",
    "mmm"  , "whl"   , "whole-number"                      , "A whole-number object",
    "sss"  , "col"   , "column"                            , "A column object (2+ x 1 data.frame/matrix)",
    "sss"  , "emp"   , "empty"                             , "An empty object (of length 0, but not NULL)",
    "sss"  , "lin"   , "linear"                            , "A linear object (vector/vlist/1D array of length 2+, row/column matrix/data.frame, or populated array with multiple indexing positions in exactly 1 dimension)",
    "sss"  , "pnt"   , "point"                             , "A point object (length-1 vector/array/vlist or 1 x 1 data.frame/matrix)",
    "sss"  , "rct"   , "rectangular"                       , "A rectangular object (2+ x 2+ data.frame/matrix)",
    "sss"  , "row"   , "row"                               , "A row object (1 x 2+ data.frame/matrix)",
    "sss"  , "sld"   , "solid"                             , "A solid object (array with multiple index positions in 3+ dimensions)",
    "sss"  , "sqr"   , "square"                            , "A square atomic matrix"
  )
}

#' @rdname ppp
#' @export
prop_verbose <- function(prop, print = TRUE) {
  errs <- c(f0(is_prop(prop), NULL, "[prop] must be a character scalar containing a single property spec."),
            f0(isTF(print)   , NULL, "[print] must be TRUE or FALSE."))
  if (!is.null(errs)) {stop(.errs(errs))}
  if (!is.null(prop)) {prop <- props_from_spec(prop)}
  if (length(prop) > 1) {stop(.errs("[prop] contains more than 1 property value."))}
  prop <- .drop_iprefix(prop)
  out <- prop_defs()
  if (idef(prop)) {
    out <- paste0("\n family: '", av(out$family[out$value == prop]), "'",
                  "\n value:  '", prop, "'",
                  "\n short:   ", av(out$short[out$value == prop]),
                  "\n long:    ", av(out$long[out$value == prop]), ".\n\n")
  }
  if (!print) {return(out)}
  if (is.data.frame(out)) {print(out, n = nrow(out))} else {cat(out)}
}

#' @rdname ppp
#' @export
combo_concise <- function(combo) {
  if (!is_prop_combo(combo)) {stop(.errs("[combo] does not contain a valid property combo spec."))}
  combo <- .drop_iprefix(props_from_combo(combo))
  defs <- prop_defs()
  fam <- av(defs$family)
  val <- av(defs$value )
  abb <- av(defs$short )
  bbb <- abb[val %in% combo & fam == "bbb"]
  ccc <- abb[val %in% combo & fam == "ccc"]
  ddd <- abb[val %in% combo & fam == "ddd"]
  eee <- abb[val %in% combo & fam == "eee"]
  iii <- abb[val %in% combo & fam == "iii"]
  mmm <- abb[val %in% combo & fam == "mmm"]
  sss <- abb[val %in% combo & fam == "sss"]
  object <- length(ccc) == 0
  out <- paste0(c(bbb, ccc, ddd, eee, iii, mmm, sss), collapse = ", ")
  out <- av(strsplit(out, ", ", TRUE))
  out <- out[!duplicated(out)]
  out <- paste0(out, collapse = ", ")
  if (object) {out <- paste0(out, " object")}
  if (substr(out, 1, 1) %in% c("a", "e", "i", "o", "u")) {prefix <- "an "}
  else {prefix <- "a "}
  out <- paste0(prefix, out)
  return(out)
}

#' @rdname ppp
#' @export
spec_concise <- function(spec) {
  if (!is_prop_spec(spec)) {stop(.errs("[spec] is not a valid property spec."))}
  combos <- .drop_iprefix(combos_from_spec(spec))
  for (i in 1:length(combos)) {combos[i] <- combo_concise(combos[i])}
  return(paste0(combos, collapse = " OR "))
}
