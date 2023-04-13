#' @encoding UTF-8
#' @family properties
#' @title All purpose property checking
#' @description This family of functions brings together a variety of families defined in this and gives users the ability to manage object properties in a granular and concise manner.
#' \cr\cr **Property families defined by this package**
#' \tabular{ll}{  \code{\link{bbb}}   \tab basic properties                       \cr
#'                \code{\link{ccc}}   \tab xclass (extended class)                \cr
#'                \code{\link{ddd}}   \tab defined.D (defined dimensionality)     \cr
#'                \code{\link{eee}}   \tab effective.D (effective dimensionality) \cr
#'                \code{\link{iii}}   \tab integrity (completeness, uniqueness)   \cr
#'                \code{\link{mmm}}   \tab xmode (extended modes)                 \cr
#'                \code{\link{sss}}   \tab shape (geometric shape)                  }
#' This family uses concise and flexible *property specs* as defined in the following table.
#' \tabular{ll}{  *single*   \tab \link[=CH3]{3-char} scalars (i.e., `all(nchar(.) == 3)` with all values in `all_props()`. For example, `'ord', 'dtf', 'd2d',` and `'rct'` are *single* property specs.                                                                                                                                                                 \cr   \tab   \cr
#'                *combo*    \tab Character scalars containing multiple underscore-delimited *single* properties, indicating that those *single* properties must co-occur. For example, the *combo* property spec `'ord_dtf_d2d_rct'` (or an equivalent underscore-delimited permutation) indicates that all four *single* properties must co-occur to satisfy the spec. \cr   \tab   \cr
#'                *option*   \tab Character scalars containing multiple pipe-delimited *combo* and/or *single* property specs. For example, the *option* property spec `'ord|dtf_d2d|dtf_rct'` would be satisfied by an object with *single* property `'ord'`, *combo* property `'dtf_d2d'`, **or** *combo* property `'dtf_rct'`.                                        \cr   \tab   \cr
#'                *flex*     \tab A *single*, *combo*, or *option* property spec as defined above.                                                                                                                                                                                                                                                                                      }
#' @details **Functions for property spec decomposition**
#' \tabular{ll}{  `props_from_combo`   \tab What are the constituent *single* properties of a *combo* property spec? \cr   \tab   \cr
#'                `combo_from_spec`    \tab What are the constituent *combo* properties in a *flex* property spec?   \cr   \tab   \cr
#'                `props_from_spec`    \tab What are the *unique* constituent *single* properties in a *flex* property spec?        }
#' **Functions to check whether a value is a valid property spec**
#' \tabular{ll}{  `is_prop_combo`   \tab Is `Combo` a valid *combo* property spec?                    \cr   \tab   \cr
#'                `is_prop_spec`    \tab Is `Spec` a valid *flex* property spec?                      \cr   \tab   \cr
#'                `is_prop_fun`     \tab Is `Fun` the name of a dedicated property checking function? \cr   \tab   \cr
#'                `is_prop`         \tab Is `Prop` a valid *single* property?                                        }
#' **Functions to define properties and property specs**
#' \tabular{ll}{  `combo_concise`   \tab How is a *combo* property spec (concisely) defined?                       \cr   \tab   \cr
#'                `spec_concise`    \tab How is an *options* property spec (concisely) defined?                    \cr   \tab   \cr
#'                `prop_verbose`    \tab How is a *single* property (verbosely) defined?                           \cr   \tab   \cr
#'                `prop_defs`       \tab What are the definitions of all possible *single* properties (returned as a data.frame)? }
#' **Functions to list names of dedicated property-checking functions**
#' \tabular{ll}{  `ppp_or_funs`   \tab What dedicated functions check `X` for either special values or a match to a *flex* property spec? \cr   \tab   \cr
#'                `prop_funs`     \tab What dedicated functions check `X` for a match to a specific *single* or *combo* property?         \cr   \tab   \cr
#'                `all_props`     \tab What is the complete set of all possible *single* properties?                                                     }
#' **Functions to check object against arbitrary property specs**
#' \tabular{ll}{  `nas_or`   \tab Is `X` either `NA` or a match to the *flex* property spec in `Spec`?  \cr   \tab   \cr
#'                `nll_or`   \tab Is `X` either `NULL` or a match to the *flex* property spec in `Spec` \cr   \tab   \cr
#'                `ppp`      \tab Is `X` a match to the *flex* property spec in `Spec`?                                }
#' **Function to list all of an object's** single **properties across property families**
#' \tabular{ll}{  `ppp`   \tab What are all of `X`'s *single* properties compiled from all property families? }
#' For convenience, property functions from other function families are also described below.
#' \cr\cr **Functions to list all** single **properties in a property family**
#' \tabular{ll}{  `bbb_props`   \tab \link[=bbb]{basic}       \cr
#'                `ccc_props`   \tab \link[=ccc]{xclass}      \cr
#'                `ddd_props`   \tab \link[=ddd]{defined.D}   \cr
#'                `eee_props`   \tab \link[=eee]{effective.D} \cr
#'                `iii_props`   \tab \link[=iii]{integrity}   \cr
#'                `mmm_props`   \tab \link[=mmm]{xmode}       \cr
#'                `sss_props`   \tab \link[=sss]{shape}         }
#' **Functions to list names of dedicated property-checking functions**
#' \tabular{ll}{  `cmp_mmm_ccc_funs`   \tab integrity = `'cmp'` + xmode + xclass \cr
#'                `unq_mmm_ccc_funs`   \tab integrity = `'unq'` + xmode + xclass \cr   \tab  }
#' \tabular{ll}{  `cmp_ccc_funs`   \tab integrity = `'cmp'` + xclass \cr
#'                `cmp_mmm_funs`   \tab integrity = `'cmp'` + xmode  \cr
#'                `unq_ccc_funs`   \tab integrity = `'unq'` + xclass \cr
#'                `unq_mmm_funs`   \tab integrity = `'unq'` + xmode  \cr
#'                `bbb_ccc_funs`   \tab basic + xclass               \cr
#'                `bbb_mmm_funs`   \tab basic + xmode                \cr
#'                `mmm_ccc_funs`   \tab xmode + xclass               \cr   \tab  }
#' \tabular{ll}{  `bbb_funs`   \tab \link[=bbb]{basic}       \cr
#'                `ccc_funs`   \tab \link[=ccc]{xclass}      \cr
#'                `ddd_funs`   \tab \link[=ddd]{defined.D}   \cr
#'                `eee_funs`   \tab \link[=eee]{effective.D} \cr
#'                `iii_funs`   \tab \link[=iii]{integrity}   \cr
#'                `mmm_funs`   \tab \link[=mmm]{xmode}       \cr
#'                `sss_funs`   \tab \link[=sss]{shape}         }
#' **Dedicated functions to check an object for a specific** combo **or** single **property**
#' \cr\cr For these functions, `{bbb}`, `{ccc}`, `{ddd}`, `{eee}`, `{iii}`, `{mmm}`, and `{sss}` are placeholders for any given basic, xclass, defined.D, effective.D, integrity, xmode, and shape properties, respectively.
#' \tabular{ll}{  `{bbb}`   \tab \link[=bbb]{basic} = `'{bbb}'`       \cr
#'                `{ccc}`   \tab \link[=ccc]{xclass} = `'{ccc}'`      \cr
#'                `{ddd}`   \tab \link[=ddd]{defined.D} = `'{ddd}'`   \cr
#'                `{eee}`   \tab \link[=eee]{effective.D} = `'{eee}'` \cr
#'                `{iii}`   \tab \link[=iii]{integrity} = `'{iii}'`   \cr
#'                `{mmm}`   \tab \link[=mmm]{xmode} = `'{mmm}'`       \cr
#'                `{sss}`   \tab \link[=sss]{shape} = `'{sss}'`       \cr   \tab  }
#' \tabular{ll}{  `cmp_{ccc}`   \tab integrity = `'cmp'` + xclass = `'{ccc}'` \cr
#'                `cmp_{mmm}`   \tab integrity = `'cmp'` + xmode = `'{mmm}'`  \cr
#'                `unq_{ccc}`   \tab integrity = `'unq'` + xclass = `'{ccc}'` \cr
#'                `unq_{mmm}`   \tab integrity = `'unq'` + xmode = `'{mmm}'`  \cr   \tab  }
#' \tabular{ll}{  `{bbb}_{ccc}`   \tab basic = `'{bbb}'` + xclass = `'{ccc}'` \cr
#'                `{bbb}_{mmm}`   \tab basic = `'{bbb}'` + xmode = `'{mmm}'`  \cr
#'                `{mmm}_{ccc}`   \tab xmode = `'{mmm}'` + xclass = `'{ccc}'` \cr
#'                `{sss}_{ccc}`   \tab shape = `'{sss}'` + xclass = `'{ccc}'` \cr   \tab  }
#' \tabular{ll}{  `cmp_{mmm}_{ccc}`   \tab integrity = `'cmp'` + xmode = `'{mmm}'` + xclass = `'{ccc}'` \cr
#'                `unq_{mmm}_{ccc}`   \tab integrity = `'unq'` + xmode = `'{mmm}'` + xclass = `'{ccc}'`   }
#' **Functions to check an object against an arbitrary* combo *property spec**
#' \cr\cr For these functions, an uppercase letter repeated three times is a placeholder for the value of an arbitrary single property from the associated property family.
#' \tabular{ll}{  `bbb_ccc`   \tab basic property in arg `BBB` + xclass property in arg `CCC` \cr
#'                `mmm_ccc`   \tab xmode property in arg `MMM` + xclass property in arg `CCC` \cr
#'                `cmp_ccc`   \tab integrity = `'cmp'` + xclass property in arg `CCC`         \cr
#'                `sss_ccc`   \tab shape property in arg `SSS` + xclass property in arg `CCC` \cr
#'                `unq_ccc`   \tab integrity = `'unq'` + xclass property in arg `CCC`         \cr
#'                `bbb_mmm`   \tab basic property in arg `BBB` + xmode property in arg `MMM`  \cr
#'                `cmp_mmm`   \tab integrity = `'cmp'` + xmode property in arg `MMM`          \cr
#'                `unq_mmm`   \tab integrity = `'unq'` + xmode property in arg `MMM`          \cr   \tab  }
#' \tabular{ll}{  `cmp_mmm_ccc`   \tab integrity = `'cmp'` + xmode property in arg `MMM` + xclass property in arg `CCC` \cr   \tab   \cr
#'                `unq_mmm_ccc`   \tab integrity = `'unq'` + xmode property in arg `MMM` + xclass property in arg `CCC` }
#' **Functions to check objects against* flex *property specs in a single family**
#' \tabular{ll}{  `BBB`   \tab \link[=bbb]{basic}       \cr
#'                `CCC`   \tab \link[=ccc]{xclass}      \cr
#'                `DDD`   \tab \link[=ddd]{defined.D}   \cr
#'                `EEE`   \tab \link[=eee]{effective.D} \cr
#'                `III`   \tab \link[=iii]{integrity}   \cr
#'                `MMM`   \tab \link[=mmm]{xmode}       \cr
#'                `SSS`   \tab \link[=sss]{shape}         }
#' **Functions to retrieve all of an object's* single *properties of a specific family**
#' \tabular{ll}{  `bbb`   \tab \link[=bbb]{basic}       \cr
#'                `ccc`   \tab \link[=ccc]{xclass}      \cr
#'                `ddd`   \tab \link[=ddd]{defined.D}   \cr
#'                `eee`   \tab \link[=eee]{effective.D} \cr
#'                `iii`   \tab \link[=iii]{integrity}   \cr
#'                `mmm`   \tab \link[=mmm]{xmode}       \cr
#'                `sss`   \tab \link[=sss]{shape}         }
#' @param asDTF `TRUE` or `FALSE` indicating whether to return the result as a data.frame with column `1` containing property values and column `2` containing the property families.
#' @param X An R object.
#' @param Spec A \link[=cmp_chr_scl]{complete character scalar} containing one or more values from `ppp_vals()` separated by pipes and/or underscores. Combinations of properties can be specified by separating them with underscores. Separating properties or combinations of properties with pipes will result in a value of `TRUE` if any of them applies to `X`.
#' @param Valid A \link[=cmp_chr_vec]{complete character vec} containing all properties considered valid.
#' @param Print `TRUE` or `FALSE` indicating whether to print the property definition to the console.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return **A character scalar** \cr\cr `combo_concise`    \cr `prop_verbose`     \cr `spec_concise`
#' \cr\cr  **A character vector** \cr\cr `combos_from_spec` \cr `props_from_combo` \cr `props_from_specs` \cr `ppp_or_funs` \cr `all_props` \cr `prop_funs` \cr `ppp`
#' \cr\cr  **A logical scalar**   \cr\cr `is_prop_combo`    \cr `is_prop`          \cr `is_prop_spec`     \cr `is_prop_fun` \cr `nas_or`    \cr `nll_or`    \cr `PPP`
#' @examples
#' na0_or("5", "ch1")
#' na0_or(NA, "ch1")
#'
#' nll_or(NULL, "ch1")
#' nll_or("1", "ch1")
#' nll_or(7, "ch1")
#'
#' all_props()
#' prop_funs()
#'
#' spec_concise("nll|cmp_psw_vls|ch1_scl")
#' combo_concise("cmp_psw_vls")
#' prop_verbose("srt")
#' prop_verbose("nnw")
#'
#' combos_from_spec("nll|cmp_psw_vls|ch1_scl")
#' props_from_spec("nll|cmp_psw_vls|ch1_scl")
#' props_from_combo("cmp_psw_vls")
#'
#' PPP("7", "nll|cmp_psw_vls|ch1_scl")
#' PPP(NULL, "nll|cmp_psw_vls|ch1_scl")
#' PPP("35", "nll|cmp_psw_vls|ch1_scl")
#'
#' is_prop_combo("letter")
#' is_prop_combo("cmp_psw_vls")
#' is_prop_spec("nll|cmp_psw_vls|ch1_scl")
#'
#' is_prop("vls")
#' is_prop("18")
#'
#' is_prop_fun("cmp_psw_vls")
#' is_prop_fun("18")
#'
#' ppp(letters)
#' prop_defs()
#' @export
ppp <- function(X) {base::sort(base::c(uj::bbb(X), uj::ccc(X), uj::ddd(X), uj::eee(X), uj::iii(X), uj::mmm(X), uj::sss(X)))}

#' @rdname ppp
#' @export
ppp_or_funs <- function() {base::c("na0_or", "nll_or")}

#' @rdname ppp
#' @export
all_props <- function(asDTF = F) {
  if (!uj:::.cmp_lgl_scl(asDTF)) {uj::stopperr("[asDTF] must be TRUE or FALSE.", PKG = "uj")}
  bval <- uj::bbb_props(); bfam <- base::rep("bbb", base::length(bval)); blab <- base::paste0("b_", bval)
  cval <- uj::ccc_props(); cfam <- base::rep("ccc", base::length(cval)); clab <- base::paste0("c_", cval)
  dval <- uj::ddd_props(); dfam <- base::rep("ddd", base::length(dval)); dlab <- base::paste0("d_", dval)
  eval <- uj::eee_props(); efam <- base::rep("eee", base::length(eval)); elab <- base::paste0("e_", eval)
  ival <- uj::iii_props(); ifam <- base::rep("iii", base::length(ival)); ilab <- base::paste0("i_", ival)
  mval <- uj::mmm_props(); mfam <- base::rep("mmm", base::length(mval)); mlab <- base::paste0("m_", mval)
  sval <- uj::sss_props(); sfam <- base::rep("sss", base::length(sval)); slab <- base::paste0("s_", sval)
  val <- base::c(bval, cval, dval, eval, ival, mval, sval)
  fam <- base::c(bfam, cfam, dfam, efam, ifam, mfam, sfam)
  ord <- base::order(base::c(blab, clab, dlab, elab, ilab, mlab, slab))
  if (!asDTF) {base::unique(val[ord])} else {tibble::tibble(family = fam[ord], ppp = val[ord])}
}

#' @rdname ppp
#' @export
prop_funs <- function(asDTF = F) {
  if (!uj:::.cmp_lgl_scl(asDTF)) {uj::stopperr("[asDTF] must be TRUE or FALSE.", PKG = "uj")}
    bFun <-         uj::bbb_funs();   bFam <- base::rep(        "bbb", base::length(  bFun));   bLab <- base::paste0("1_",   bFam, "_",   bFun)
    cFun <-         uj::ccc_funs();   cFam <- base::rep(        "ccc", base::length(  cFun));   cLab <- base::paste0("1_",   cFam, "_",   cFun)
    dFun <-         uj::ddd_funs();   dFam <- base::rep(        "ddd", base::length(  dFun));   dLab <- base::paste0("1_",   dFam, "_",   dFun)
    eFun <-         uj::eee_funs();   eFam <- base::rep(        "eee", base::length(  eFun));   eLab <- base::paste0("1_",   eFam, "_",   eFun)
    iFun <-         uj::iii_funs();   iFam <- base::rep(        "iii", base::length(  iFun));   iLab <- base::paste0("1_",   iFam, "_",   iFun)
    mFun <-         uj::mmm_funs();   mFam <- base::rep(        "mmm", base::length(  mFun));   mLab <- base::paste0("1_",   mFam, "_",   mFun)
    sFun <-         uj::sss_funs();   sFam <- base::rep(        "sss", base::length(  sFun));   sLab <- base::paste0("1_",   sFam, "_",   sFun)
   orFun <-      uj::ppp_or_funs();  orFam <- base::rep(     "ppp_or", base::length( orFun));  orLab <- base::paste0("2_",  orFam, "_",  orFun)
   bcFun <-     uj::bbb_ccc_funs();  bcFam <- base::rep(    "bbb_ccc", base::length( bcFun));  bcLab <- base::paste0("3_",  bcFam, "_",  bcFun)
   bmFun <-     uj::bbb_mmm_funs();  bmFam <- base::rep(    "bbb_mmm", base::length( bmFun));  bmLab <- base::paste0("3_",  bmFam, "_",  bmFun)
   mcFun <-     uj::mmm_ccc_funs();  mcFam <- base::rep(    "mmm_ccc", base::length( mcFun));  mcLab <- base::paste0("3_",  mcFam, "_",  mcFun)
   scFun <-     uj::sss_ccc_funs();  scFam <- base::rep(    "sss_ccc", base::length( scFun));  scLab <- base::paste0("3_",  scFam, "_",  scFun)
   ccFun <-     uj::cmp_ccc_funs();  ccFam <- base::rep(    "cmp_ccc", base::length( ccFun));  ccLab <- base::paste0("4_",  ccFam, "_",  ccFun)
   cmFun <-     uj::cmp_mmm_funs();  cmFam <- base::rep(    "cmp_mmm", base::length( cmFun));  cmLab <- base::paste0("4_",  cmFam, "_",  cmFun)
   ucFun <-     uj::unq_mmm_funs();  ucFam <- base::rep(    "unq_ccc", base::length( ucFun));  ucLab <- base::paste0("4_",  ucFam, "_",  ucFun)
   umFun <-     uj::unq_mmm_funs();  umFam <- base::rep(    "unq_mmm", base::length( umFun));  umLab <- base::paste0("4_",  umFam, "_",  umFun)
  cmcFun <- uj::cmp_mmm_ccc_funs(); cmcFam <- base::rep("cmp_mmm_ccc", base::length(cmcFun)); cmcLab <- base::paste0("5_", cmcFam, "_", cmcFun)
  umcFun <- uj::unq_mmm_ccc_funs(); umcFam <- base::rep("unq_mmm_ccc", base::length(umcFun)); umcLab <- base::paste0("5_", umcFam, "_", umcFun)
  Fun <- base::c(bFun, cFun, dFun, eFun, iFun, mFun, sFun, orFun, bcFun, bmFun, mcFun, scFun, ccFun, cmFun, ucFun, umFun, cmcFun, umcFun)
  Fam <- base::c(bFam, cFam, dFam, eFam, iFam, mFam, sFam, orFam, bcFam, bmFam, mcFam, scFam, ccFam, cmFam, ucFam, umFam, cmcFam, umcFam)
  Ord <- base::c(bLab, cLab, dLab, eLab, iLab, mLab, sLab, orLab, bcLab, bmLab, mcLab, scLab, ccLab, cmLab, ucLab, umLab, cmcLab, umcLab)
  Ord <- base::order(Ord)
  Fun <- Fun[Ord]
  Fam <- Fam[Ord]
  uj::f0(!asDTF, base::unique(Fun[Ord]), base::unique(tibble::tibble(Family = Fam[Ord], Fun = Fun[Ord])))
}

#' @rdname ppp
#' @export
is_prop_fun <- function(Fun) {
  if (!uj:::.cmp_chr_scl(Fun)) {uj::stopperr("[Fun] must be a complete character scalar (?cmp_chr_scl).", PKG = "uj")}
  Fun %in% uj::prop_funs()
}

#' @rdname ppp
#' @export
is_prop <- function(Prop) {if (uj:::.cmp_ch3_scl(Prop)) {base::tolower(Prop) %in% uj::v(ppp)} else {F}}

#' @rdname ppp
#' @export
is_prop_combo <- function(Combo) {
  if (uj:::.cmp_chr_scl(Combo)) {
    Combo <- uj::av(base::strsplit(Combo, "|", fixed = T))
    if (base::length(Combo) == 1) {
      Props <- uj::av(base::strsplit(Combo, "_", fixed = T))
      if (base::length(base::unique(Props)) == base::length(Props)) {base::all(Props %in% uj::v(ppp))}
      else {F}
    } else {F}
  } else {F}
}

#' @rdname ppp
#' @export
is_prop_spec <- function(Spec) {
  if (uj:::.cmp_chr_scl(Spec)) {
    Combos <- uj::av(base::strsplit(Spec, "|", fixed = T))
    if (base::length(base::unique(Combos)) == base::length(Combos)) {
      base::all(base::sapply(Combos, uj::is_prop_combo) | base::sapply(Combos, uj::is_prop))
    } else {F}
  } else {F}
}

#' @rdname ppp
#' @export
props_from_spec <- function(Spec, Valid = all_props()) {
  OkValid <- uj::f0(uj:::.cmp_chr_vec(Valid), base::all(Valid %in% uj::v(ppp)), F)
  OkSpec <- uj:::.cmp_chr_scl(Spec)
  Errors <- NULL
  if (!OkSpec) {Errors <- base::c(Errors, "[Spec] must be a complete character scalar (?cmp_chr_scl).")}
  if (!OkValid) {Errors <- base::c(Errors, "[Valid] must be a complete character vector (?cmp_chr_vec) containing only values from all_props().")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  Singles <- uj:::.spec2props(Spec)
  if (base::length(Singles) == 0) {uj::stopperr("[Spec] is empty.", PKG = "uj")}
  if (!base::all(Singles %in% Valid)) {uj::stopperr("The property spec [Spec] contains a property not in [Valid].", PKG = "uj")}
  base::sort(base::unique(Singles))
}

#' @rdname ppp
#' @export
combos_from_spec <- function(Spec, Valid = uj::all_props()) {base::sort(base::unique(uj:::.spec2combos(Spec)))}

#' @rdname ppp
#' @export
props_from_combo <- function(Combo, valid = uj::all_props()) {
  uj::f0(base::length(uj:::.spec2props(Combo)) == 1,
         base::sort(base::unique(uj:::.combo2props(Combo))),
         uj::stopperr("[Combo] contains more than one combination property.", PKG = "uj"))
}

#' @rdname ppp
#' @export
PPP <- function(X, Spec, ...) {
  if (!uj::is_prop_spec(Spec)) {uj::stopperr("[Spec] specifies a property not in all_props().", PKG = "uj")}
  if (uj::meets(X, ...)) {
    AllProps <- uj::all_props()
    Combos <- uj:::.spec2combos(Spec)
    for (Combo in Combos) {
      IsOne <- uj::f0(base::length(Combo) == 1, Combo %in% AllProps, F)
      IsFun <- uj::is_prop_fun(Combo)
      if (!IsOne & !IsFun) {
        Singles <- uj:::.combo2props(Combo)
        Meets <- T
        for (Prop in Singles) {if (Meets) {Meets <- Meets & base::eval(base::parse(text = base::paste0("uj:::.", base::toupper(Prop), "(X)")))}}
      } else if (IsOne) {Meets <- base::eval(base::parse(text = base::paste0("uj:::.", base::toupper(Combo), "(X)")))}
      else {Meets <- base::eval(base::parse(text = base::paste0("uj::", Combo, "(X)")))}
      if (Meets) {return(T)}
  }}
  F
}

#' @rdname ppp
#' @export
nll_or <- function(X, Spec, ...) {uj::f0(base::is.null(X), T, uj::PPP(X, Spec, ...))}

#' @rdname ppp
#' @export
na0_or <- function(X, Spec, ...) {uj::f0(uj:::.NA0(X), T, uj::PPP(X, Spec, ...))}

#' @rdname ppp
#' @export
prop_defs <- function() {
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
    "iii"  , "cmp"   , "atomic and complete"               , "An complete atomic object (containing no NA values)",
    "iii"  , "dup"   , "atomic, complete, with duplicates" , "A complete dup atomic object (containing no NA values, containing duplicate values)",
    "iii"  , "mss"   , "atomic and missing"                , "A missing atomic object (containing only non-NA values)",
    "iii"  , "na0"   , "atomic NA scalar"                  , "An atomic NA scalar object",
    "iii"  , "ok0"   , "atomic non-NA scalar"              , "An atomic non-NA scalar object",
    "iii"  , "prt"   , "atomic and partial"                , "A partial atomic object (containing both NA and non-NA values)",
    "iii"  , "unq"   , "atomic, complete, and unique"      , "A unique, complete atomic object (containing no NA or duplicate values)",
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
    "sss"  , "col"   , "column"                            , "A column object (2+ X 1 data.frame/matrix)",
    "sss"  , "emp"   , "empty"                             , "An empty object (of length 0, but not NULL)",
    "sss"  , "lin"   , "linear"                            , "A linear object (vector/vlist/1D array of length 2+, row/column matrix/data.frame, or populated array with multiple indexing positions in exactly 1 dimension)",
    "sss"  , "pnt"   , "point"                             , "A point object (length-1 vector/array/vlist or 1 X 1 data.frame/matrix)",
    "sss"  , "rct"   , "rectangular"                       , "A rectangular object (2+ X 2+ data.frame/matrix)",
    "sss"  , "row"   , "row"                               , "A row object (1 X 2+ data.frame/matrix)",
    "sss"  , "sld"   , "solid"                             , "A solid object (array with multiple index positions in 3+ dimensions)",
    "sss"  , "sqr"   , "square"                            , "A square atomic matrix"
  )
}

#' @rdname ppp
#' @export
prop_verbose <- function(Prop, Print = TRUE) {
  Errors <- NULL
  if (!uj::is_prop(Prop)) {Errors <- base::c(Errors, "[Prop] must be a character scalar containing a single property spec.")}
  Prop <- base::tolower(Prop)
  if (!uj:::.cmp_lgl_scl(Print)) {Errors <- base::c(Errors, "[Print] must be TRUE or FALSE.")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  Y <- uj::prop_defs()
  if (!base::is.null(Prop)) {Y <- base::paste0("\n Family: '", uj::av(Y$Family[Y$Value == Prop]), "'"  ,
                                               "\n Value:  '", Prop, "'"                               ,
                                               "\n Short:   ", uj::av(Y$Short[Y$Value == Prop])        ,
                                               "\n Long:    ", uj::av(Y$Long[Y$Value == Prop]), ".\n\n")}
  if (!Print) {Y} else if (base::is.data.frame(Y)) {base::print(Y, n = base::nrow(Y))} else {base::cat(Y)}
}

#' @rdname ppp
#' @export
combo_concise <- function(Combo) {
  if (!uj::is_prop_combo(Combo)) {uj::stopperr("[Combo] does not contain a valid property combo spec.", PKG = "uj")}
  Combo <- uj:::.combo2props(Combo)
  Defs <- uj::prop_defs()
  Family <- uj::av(Defs$Family)
  Value <- uj::av(Defs$Value)
  Short <- uj::av(Defs$Short)
  bbb <- Short[Value %in% Combo & Family == "bbb"]
  ccc <- Short[Value %in% Combo & Family == "ccc"]
  ddd <- Short[Value %in% Combo & Family == "ddd"]
  eee <- Short[Value %in% Combo & Family == "eee"]
  iii <- Short[Value %in% Combo & Family == "iii"]
  mmm <- Short[Value %in% Combo & Family == "mmm"]
  sss <- Short[Value %in% Combo & Family == "sss"]
  Object <- base::length(ccc) == 0
  Y <- base::paste0(base::c(bbb, ccc, ddd, eee, iii, mmm, sss), collapse = ", ")
  Y <- uj::av(base::strsplit(Y, ", ", fixed = T))
  Y <- Y[!base::duplicated(Y)]
  Y <- base::paste0(Y, collapse = ", ")
  if (Object) {Y <- base::paste0(Y, " object")}
  Prefix <- uj::f0(base::substr(Y, 1, 1) %in% base::c("a", "e", "i", "o", "u"), "an ", "a ")
  base::paste0(Prefix, Y)
}

#' @rdname ppp
#' @export
spec_concise <- function(Spec) {
  if (!uj::is_prop_spec(Spec)) {uj::stopperr("[Spec] is not a valid property spec.", PKG = "uj")}
  Combos <- uj:::.spec2combos(Spec)
  for (i in 1:base::length(Combos)) {Combos[i] <- uj::combo_concise(Combos[i])}
  base::paste0(Combos, collapse = " OR ")
}
