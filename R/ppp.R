.ppp_props <- function() {sort(c(.bbbs(), .cccs(), .ddds(), .eees(), .iiis(), .mmms(), .ssss()))}
.spec_vals <- function(x) {if (!is.character(x)) {return(NULL)} else {x <- unname(unlist(strsplit(x, "|", fixed = T))); x[x != ""]}}
.drop_iprefix <- function(x) {i <- nchar(x) == 4 & substr(x, 1, 1) == "i"; x[i] <- substr(x[i], 2, 4); x}

#' @name ppp
#' @family props
#' @title All purpose property checking
#' @description This set of functions provide utilities that bring together seven families of object properties defined by this package:\itemize{
#'   \item **\code{\link{bbb}}**: base.
#'   \item **\code{\link{ccc}}**: extended class.
#'   \item **\code{\link{ddd}}**: defined dimensionality.
#'   \item **\code{\link{eee}}**: effective dimensionality.
#'   \item **\code{\link{iii}}**: integrity (state of completeness).
#'   \item **\code{\link{mmm}}**: extended mode.
#'   \item **\code{\link{sss}}**: shape.
#' }
#' **Individual property specifications** are character scalars containing exactly `3` characters (e.g., `'ord'`, `'dtf'`, `'d1D'`, `'rct'`).
#' \cr\cr
#' **Combination (or conjunctive property specifications** are for properties that must co-occur. They are constructed by delimiting multiple individual properties with underscores (e.g., `'ord_dtf_d1D_rct'` or an equivalent underscore-delimited permutation).
#' \cr\cr
#' **Alternate (compensatory) property specifications** give a way to indicate that if any one (individual or combination) property is satisfied the entire specification is satisfied. They are constructed by pipe-delimiting multiple individual or combination properties. For example, the specification `'ord|dtf_d1D||dtf_rct'` would be satisfied by an object with individual property `'ord'`, combined property `'dtf_d1D'`, or combined property `'dtf_rct'`.
#' \cr\cr
#' **Universal property functions** address all families.
#' \itemize{
#'   \item **`ppp`**: gets all properties of all property families applicable to `x`
#'   \item **`ippp`**: evaluates whether an object satisfies the property specification in `ppp` subject to any additional restrictions supplied in `...`.
#'   \item **`nll_or`**: evaluates whether an object is either `NULL` or satisfies the property specification in argument `ppp` subject to any additional restrictions in `...`.
#'   \item **`nas_or`**: evaluates whether an object is either scalar `NA` or satisfies the property specification in argument `ppp` subject to any additional restrictions in `...`.
#'   \item **`all_props`**: gets all possible individual properties from all property families and all possible combination/conjunctive properties as checked by combined property functions.
#'   \item **`ppp_all`**: gets all constituent individual properties from the property specification in argument `ppp`.
#'   \item **`ppp_funs`**: gets the names of all property functions that check for a single property or a combined property without checking for any additional restrictions.
#'   \item **`ppp_defs`**: gets a data frame defining all individual properties of all property families.
#'   \item **`is_ppp_fun`**: evaluates whether a character scalar is the name of a property function (as listed by `ppp_funs`).
#'   \item **`is_valid_ppp`**: evaluates a character scalar property specification for validity (i.e., does it contain only single properties, valid combination properties,and/or valid alternate properties).
#'   \item **`ppp_from_combo`**: extracts each individual property value from a combination/conjunctive property specification.
#'   \item **`combos_from_ppp`**: extracts each individual or combination property specification from an alternative/compensatory property specification.
#'   \item **`ppp_verbose`**: gets a verbose definition of an individual property specification.
#'   \item **`ppp_concise`**: gets a plain-language, concise definition of an individual or combination/conjunctive property specification.
#'   \item **`alt_ppp_concise`**: gets a plain-language, concise definition of an alternative/compensatory property specification.
#' }
#' **Individual property functions** associated with a single property family take the following forms where `xxx` is a placeholder for any given individual property and `yyy` is a placeholder for any given  property family.
#' \itemize{
#'   \item **`yyy`**: gets properties applicable to `x` from from property family `yyy`.
#'   \item **`ixxx`**: evaluates whether `x` has the property `xxx`.
#'   \item **`iyyy`**: evaluates whether `x` has one or more individual properties from property family `yyy`, subject to any restrictions supplied in `...`.
#'   \item **`yyy_props`**: gets all properties in the property family `yyy`.
#' }
#' **Combination property functions** associated with a multiple property families take the following forms where `xxx`, `yyy`, and `zzz` are placeholders for properties from base, extended mode, and extended class property families, respectively:
#' \itemize{
#'   \item **`yyy_zzz`**: evaluates whether an object is of extended mode `yyy` and extended class `zzz`.
#'   \item **`xxx_zzz`**: evaluates whether an object is of base property `xxx` and extended class `zzz`.
#'   \item **`xxx_yyy`**: evaluates whether an object is of base property `xxx` and extended mode `yyy`.
#'   \item **`cmp_yyy`**: evaluates whether an object is complete (non-empty, atomic, containing no `NA` values) and of extended mode `yyy`.
#'   \item **`cmp_zzz`**: evaluates whether an object is complete (implying non-empty and atomic) and extended class `zzz`.
#'   \item **`cmp_yyy_zzz`**: evaluates whether an object is complete (non-empty and atomic), extended mode `yyy`, and extended class `zzz`.
#'   \item **`mmm_ccc_props`**: gets all combined extended mode + extended class properties.
#'   \item **`bbb_ccc_props`**: gets all combined base + extended class properties.
#'   \item **`bbb_mmm_props`**: gets all combined base + extended mode properties.
#'   \item **`cmp_ccc_props`**: gets all combined completeness + extended class properties.
#'   \item **`cmp_mmm_props`**: gets all combined completeness + extended mode properties.
#'   \item **`cmp_mmm_ccc_props`**: gets all combined completeness + extended mode + extended class properties.
#' }
#' @param as.dtf A non-`NA` logical scalar indicating whether to return the result as a data.frame with column `1` containing property values and column `2` containing the property families.
#' @param x An R object.
#' @param ppp A \link[=cmp_chr_scl]{complete character scalar} containing one or more values from `ppp_vals()` separated by pipes and/or underscores. Combinations of properties can be specified by separating them with underscores. Separating properties or combinations of properties with pipes will result in a value of `TRUE` if any of them applies to `x`.
#' @param valid A \link[=cmp_chr_vec]{complete character vec} containing all properties considered valid.
#' @param print A non-`NA` logical scalar indicating whether to print the property definition to the console.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return \itemize{
#'   \item **`ppp_concise, alt_ppp_concise`**: a character scalar.
#'   \item **`ppp, ppp_all, ppp_vals, pop_funs, ppp_from_combo, combos_from_ppp`**: a character vector.
#'   \item **`All others`**: a logical scalar.
#' }
#' @export
ppp <- function(x) {sort(c(bbb(x), ccc(x), ddd(x), eee(x), iii(x), mmm(x), sss(x)))}

#' @rdname ppp
#' @export
ppp_props <- function(as.dtf = F) {
  if (!isTF(as.dtf)) {stop("\n \u2022 [as.dtf] must be TRUE or FALSE.", call. = F)}
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
is_valid_spec <- function(spec) {
  if (!cmp_chr_scl(spec)) {return(FALSE)}
  spec <- av(strsplit(spec, "_", TRUE))
  spec <- av(strsplit(spec, "|", TRUE))
  spec <- trimws(spec)
  spec <- spec[spec != ""]
  spec <- .drop_iprefix(spec)
  if (length(spec) == 0) {return(FALSE)}
  all(spec %in% ppp_props())
}

#' @rdname ppp
#' @export
is_valid_combo <- function(combo) {
  if (!cmp_chr_scl(combo)) {return(FALSE)}
  combo <- av(strsplit(combo, "_", TRUE))
  combo <- trimws(combo)
  combo <- combo[combo != ""]
  combo <- .drop_iprefix(combo)
  if (length(combo) == 0) {return(FALSE)}
  all(combo %in% ppp_props())
}

#' @rdname ppp
#' @export
is_valid_ppp <- function(ppp) {
  if (!cmp_chr_scl(ppp)) {return(FALSE)}
  ppp %in% ppp_props()
}

#' @rdname ppp
#' @export
ppp_from_combo <- function(combo, valid = ppp_props()) {
  ok.valid <- f0(cmp_chr_vec(valid), all(valid %in% ppp_props()), F)
  errs <- c(f0(cmp_chr_scl(combo), NULL, "\n \u2022 [combo] must be a complete character scalar (?cmp_chr_scl)."),
            f0(ok.valid          , NULL, "\n \u2022 [valid] must be a complete character vec (?cpm_chr_vec) containing only values from ppp_props()."))
  if (!is.null(errs)) {stop(errs)}
  if (length(combo) != av(strsplit(combo, "|", fixed = T))) {stop("\n \u2022 [combo] contains multiple alternate property specs.")}
  out <- trimws(av(strsplit(combo, "_", fixed = T)))
  out <- out[out != ""]
  out <- .drop_iprefix(out)
  if (length(out) == 0    ) {stop("\n \u2022 [combo] contains no property specs.")}
  if (!all(out %in% valid)) {stop("\n \u2022 [combo] contains a property not in c(", paste0(paste0("'", valid, "'"), collapse = ", "), ").")}
  sort(unique(out))
}

#' @rdname ppp
#' @export
combos_from_spec <- function(spec, valid = ppp_props()) {
  ok.valid <- f0(cmp_chr_vec(valid), all(valid %in% ppp_props()), F)
  errs <- c(f0(cmp_chr_scl(spec), NULL, "\n \u2022 [spec] must be a complete character scalar (?cmp_chr_scl)."),
            f0(ok.valid         , NULL, "\n \u2022 [valid] must be a complete character vec (?cpm_chr_vec) containing only values from ppp_props()."))
  if (!is.null(errs)) {stop(errs)}
  ppp.singles <- ppp_all(spec, valid)
  if (!all(ppp.singles %in% valid)) {stop("\n \u2022 [ppp] contains a property spec not in c(", paste0(paste0("'", valid, "'"), collapse = ", "), ").")}
  out <- trimws(av(strsplit(ppp, "|", fixed = T)))
  out <- out[out != ""]
  out <- .drop_iprefix(out)
  sort(unique(out))
}

#' @rdname ppp
#' @export
ppp_funs <- function(as.dtf = F) {
  if (!isTF(as.dtf)) {stop("\n \u2022 [as.dtf] must be TRUE or FALSE.")}
  b_fun <- paste0("i", bbb_props()); b_fam <- rep("bbb", length(b_fun)); b_lab <- paste0("1_", b_fam, bbb_props())
  c_fun <- paste0("i", ccc_props()); c_fam <- rep("ccc", length(c_fun)); c_lab <- paste0("1_", c_fam, ccc_props())
  d_fun <- paste0("i", ddd_props()); d_fam <- rep("ddd", length(d_fun)); d_lab <- paste0("1_", d_fam, ddd_props())
  e_fun <- paste0("i", eee_props()); e_fam <- rep("eee", length(e_fun)); e_lab <- paste0("1_", e_fam, eee_props())
  i_fun <- paste0("i", iii_props()); i_fam <- rep("iii", length(i_fun)); i_lab <- paste0("1_", i_fam, iii_props())
  m_fun <- paste0("i", mmm_props()); m_fam <- rep("mmm", length(m_fun)); m_lab <- paste0("1_", m_fam, mmm_props())
  s_fun <- paste0("i", sss_props()); s_fam <- rep("sss", length(s_fun)); s_lab <- paste0("1_", s_fam, sss_props())
  mc_fun <- mmm_ccc_props(); mc_fam <- rep("mmm_ccc", length(mc_fun)); mc_lab <- paste0("2_", mc_fam, "_", mc_fun)
  bc_fun <- bbb_ccc_props(); bc_fam <- rep("bbb_ccc", length(bc_fun)); bc_lab <- paste0("2_", bc_fam, "_", bc_fun)
  bm_fun <- bbb_mmm_props(); bm_fam <- rep("bbb_mmm", length(bm_fun)); bm_lab <- paste0("2_", bm_fam, "_", bm_fun)
  cc_fun <- cmp_ccc_props(); cc_fam <- rep("cmp_yyy", length(cc_fun)); cc_lab <- paste0("3_", cc_fam, "_", cc_fun)
  cmc_fun <- cmp_mmm_ccc_props(); cmc_fam <- rep("cmp_zzz_yyy", length(cmc_fun)); cmc_lab <- paste0("4_", cmc_fam, "_", cmc_fun)
  fun <- c(b_fun, c_fun, d_fun, e_fun, i_fun, m_fun, s_fun, mc_fun, bc_fun, bm_fun, cc_fun, cmc_fun)
  fam <- c(b_fam, c_fam, d_fam, e_fam, i_fam, m_fam, s_fam, mc_fam, bc_fam, bm_fam, cc_fam, cmc_fam)
  ord <- order(c(b_lab, c_lab, d_lab, e_lab, i_lab, m_lab, s_lab, mc_lab, bc_lab, bm_lab, cc_lab, cmc_lab))
  if (!as.dtf) {return(fun[ord])}
  tibble::tibble(family = fam[ord], fun = fun[ord])
}

#' @rdname ppp
#' @export
is_ppp_fun <- function(x) {
  if (!cmp_chr_scl(x)) {stop("\n \u2022 [x] must be a complete character scalar (?cmp_chr_scl).")}
  x %in% ppp_funs()
}

#' @rdname ppp
#' @export
ippp <- function(x, spec, ...) {
  if (!is_valid_spec(spec)) {stop("\n \u2022 [spec] specifies a property not in ppp_props().")}
  if (!meets(x, ...)) {return(F)}
  all.ppps <- ppp_props()
  combos <- combos_from_spec(spec)
  for (combo in combos) {
    is.one <- combo %in% all.ppps
    is.fun <- is_ppp_fun(combo)
    if (!is.one & !is.fun) {
      singles <- .drop_iprefix(ppp_from_combo(combo))
      meets <- TRUE
      for (ppp in singles) {if (meets) {
        meets <- meets & eval(parse(text = paste0("i", ppp, "(x)")))
    }}}
    else if (is.one) {meets <- eval(parse(text = paste0("i", combo, "(x)")))}
    else {meets <- eval(parse(text = paste0(combo, "(x)")))}
    if (meets) {return(TRUE)}
  }
  FALSE
}

#' @rdname ppp
#' @export
ppp_all <- function(spec, valid = ppp_props()) {
  ok.valid <- f0(cmp_chr_vec(valid), all(valid %in% ppp_props()), F)
  errs <- c(f0(cmp_chr_scl(spec), NULL, "\n \u2022 [spec] must be a complete character scalar (?cmp_chr_scl)."),
            f0(ok.valid         , NULL, "\n \u2022 [valid] must be a complete character vector (?cmp_chr_vec) containing only values from ppp_vals()."))
  if (!is.null(errs)) {stop(errs)}
  combos <- av(strsplit(spec, "|", fixed = T))
  singles <- trimws(av(strsplit(combos, "_", fixed = T)))
  singles <- singles[singles != ""]
  singles <- .drop_iprefix(singles)
  if (length(singles) == 0) {stop("\n \u2022 The property specification [spec] is empty after splitting on pipes and underscores.")}
  if (!all(singles %in% valid)) {stop("\n \u2022 The property specification [spec] contains a property not in ppp_props().")}
  sort(unique(singles))
}

#' @rdname ppp
#' @export
nll_or <- function(x, ppp, ...) {f0(inll(x), T, ippp(x, ppp, ...))}

#' @rdname ppp
#' @export
nas_or <- function(x, ppp, ...) {f0(inas(x), T, ippp(x, ppp, ...))}

#' @rdname ppp
#' @export
ppp_defs <- function() {
  tibble::tribble(
    ~Family, ~Value  , ~Short                              , ~Long,
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
ppp_verbose <- function(ppp = NULL, print = TRUE) {
  errs <- c(f0(inll(ppp) | !is_valid_ppp(ppp), NULL, "\n \u2022 [ppp] must be NULL or a scalar individual property specification."),
            f0(isTF(print)                   , NULL, "\n \u2022 [print] must be TRUE or FALSE."))
  if (!is.null(errs)) {stop(errs)}
  if (!is.null(ppp)) {ppp <- ppp_all(ppp)}
  if (length(ppp) > 1) {stop("\n \u2022 [ppp] contains more than 1 property value.")}
  ppp <- .drop_iprefix(ppp)
  out <- ppp_defs()
  if (idef(ppp)) {
    out <- paste0("\nFamily: '", av(out[out$value == ppp, 1]), "'",
                  "\nValue: '" , ppp                         , "'",
                  "\nShort: "  , av(out[out$Value == ppp, 3]),
                  "\nLong: "   , av(out[out$Value == ppp, 4]), ".\n\n")
  }
  if (!print) {return(out)}
  if (is.data.frame(out)) {print(out, n = nrow(out))} else {cat(out)}
  NULL
}

#' @rdname ppp
#' @export
ppp_concise <- function(combo) {
  if (!is_valid_combo(combo)) {stop("\n \u2022 [combo] does not contain a valid property combo specification.")}
  combo <- .drop_iprefix(ppp_from_combo(combo))
  defs <- ppp_defs()
  fam <- av(defs$Family)
  val <- av(defs$Value )
  abb <- av(defs$Short )
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
alt_ppp_concise <- function(spec) {
  if (!is_valid_spec(spec)) {stop("\n \u2022 [spec] is not a valid property specification.")}
  combos <- .drop_iprefix(combos_from_spec(spec))
  for (i in 1:length(combos)) {combos[i] <- ppp_concise(combos[i])}
  return(paste0(combos, collapse = " OR "))
}
