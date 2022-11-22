.is_atm <- function(x) {is.atomic(x) & !is.null(x)}
.is_def <- function(x) {!is.null(x)}
.is_fun <- function(x) {f0(is.function(x), T, f0(length(x) != 1 | !is.character(x), F, f0(is.na(x), F, !isERR(match.fun(x)))))}
.is_nil <- function(x) {length(x) == 0}
.is_nll <- function(x) {is.null(x)}
.is_pop <- function(x) {length(x) > 0}
.is_rcr <- function(x) {is.recursive(x)}
.bbb_props <- function() {c("atm", "def", "fun", "nil", "nll", "pop", "rcr")}

.is_arr <- function(x) {is.array(x)}
.is_dtf <- function(x) {is.data.frame(x)}
.is_gen <- function(x) {is.array(x) | is.vector(x)}
.is_mat <- function(x) {is.matrix(x)}
.is_mvc <- function(x) {f0(length(x) < 2, F, f0(is.vector(x), T, is.array(x) & length(which(dim(x) > 1)) == 1))}
.is_scl <- function(x) {f0(length(x) != 1, F, is.array(x) | is.vector(x))}
.is_vec <- function(x) {f0(length(x) == 0, F, f0(is.vector(x), T, is.array(x) & length(which(dim(x) > 1)) < 2))}
.is_vls <- function(x) {is.list(x) & !is.data.frame(x)}
.ccc_props <- function() {c("arr", "dtf", "gen", "mat", "mvc", "scl", "mvc", "vls")}

.is_d0D <- function(x) {is.null(x)}
.is_d1D <- function(x) {is.vector(x)}
.is_d2D <- function(x) {is.matrix(x) | is.data.frame(x)}
.is_dHD <- function(x) {length(dim(x)) > 2}
.ddd_props <- function() {c("d0D", "d1D", "d2D", "dHD")}

.is_e0D <- function(x) {NROW(x) * NCOL(x) == 1 & length(x) == 1}
.is_e1D <- function(x) {1 %in% c(NROW(x), NCOL(x)) & NROW(x) * NCOL(x) > 1}
.is_e2D <- function(x) {f0(!is.array(x) & !is.data.frame(x), F, length(which(dim(x) > 1)) == 2)}
.is_eHD <- function(x) {f0(!is.array(x), F, length(which(dim(x) > 1)) > 2)}
.is_eUD <- function(x) {length(x) == 0}
.eee_props <- function() {c("e0D", "e1D", "e2D", "eHD", "eUD")}

.is_cmp <- function(x) {f0(length(x) == 0 | !is.atomic(x), F, !any(is.na(x)))}
.is_mss <- function(x) {f0(length(x) == 0 | !is.atomic(x), F, all(is.na(x)))}
.is_nas <- function(x) {f0(length(x) != 1 | !is.atomic(x), F, is.na(x))}
.is_oks <- function(x) {f0(length(x) != 1 | !is.atomic(x), F, !is.na(x))}
.is_prt <- function(x) {f0(length(x) < 2 | !is.atomic(x), F, {x <- is.na(x); any(x) & !all(x)})}
.iii_props <- function() {c("cmp", "mss", "nas", "oks", "prt")}

.is_mmm <- function(x) {!is.null(x) | !is.atomic(x)}
.is_MMM <- function(x) {f0(length(x) == 0, T, all(is.na(x)))}
.is_ch1 <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, f0(!is.character(x), F, all(nchar(x[!is.na(x)]) == 1))))}
.is_ch3 <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, f0(!is.character(x), F, all(nchar(x[!is.na(x)]) == 3))))}
.is_chr <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, is.character(x)))}
.is_clr <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, f0(!is.character(x), F, !any(c("error", "simpleError") %in% class(failsafe(col2rgb(x[!is.na(x)])))))))}
.is_evn <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, f0(!is.numeric(x), F, {x <- x[!is.na(x)]; all(round(x / 2) == x / 2)})))}
.is_fac <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, is.factor(x)))}
.is_frc <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, f0(!is.numeric(x), F, {x <- x[!is.na(x)]; any(round(x) != round(x))})))}
.is_ind <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, is.logical(x) | all()))}
.is_lgl <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, is.logical(x)))}
.is_neg <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, f0(!is.numeric(x), F, all(x[!is.na(x)] < 0))))}
.is_ngw <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, f0(!is.numeric(x), F, {x <- x[!is.na(x)]; all(x < 0 & round(x) == x)})))}
.is_nng <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, f0(!is.numeric(x), F, !any(x[!is.na(x)] < 0))))}
.is_nnw <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, f0(!is.numeric(x), F, {x <- x[!is.na(x)]; all(x >= 0 & round(x) == x)})))}
.is_nps <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, f0(!is.numeric(x), F, !any(x[!is.na(x)] > 0))))}
.is_npw <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, f0(!is.numeric(x), F, {x <- x[!is.na(x)]; all(x <= 0 & round(x) == x)})))}
.is_nst <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, f0(is.factor(x), !is.ordered(x), !(is.character(x) | is.numeric(x) | is.logical(x)))))}
.is_num <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, is.numeric(x)))}
.is_odd <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, f0(!is.numeric(x), F, {x <- x[!is.na(x)] + 1; all(round(x / 2) == x / 2)})))}
.is_ord <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, is.ordered(x)))}
.is_pct <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, f0(!is.numeric(x), F, {x <- x[!is.na(x)]; all(x >= 0 & x <= 100)})))}
.is_pos <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, f0(!is.numeric(x), F, all(x[!is.na(x)] > 0))))}
.is_ppn <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, f0(!is.numeric(x), F, {x <- x[!is.na(x)]; all(x >= 0 & x <= 1)})))}
.is_psw <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, f0(!is.numeric(x), F, {x <- x[!is.na(x)]; all(x > 0 & round(x) == x)})))}
.is_srt <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, f0(is.factor(x), is.ordered(x), is.character(x) | is.numeric(x) | is.logical(x))))}
.is_str <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, f0(!is.character(x), F, !any(x[!is.na(x)] == ""))))}
.is_uno <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, is.factor(x) & !is.ordered(x)))}
.is_whl <- function(x) {f0(!.is_mmm(x), F, f0(.is_MMM(x), T, f0(!is.numeric(x), F, {x <- x[!is.na(x)]; all(round(x) == x)})))}
.mmm_props <- function() {c("ch1", "ch3", "chr", "clr", "evn", "fac", "frc", "ind", "lgl", "neg", "ngw", "nng", "nnw", "nps", "npw", "nst", "num", "odd", "ord", "pct", "pos", "ppn", "psw", "srt", "str", "uno", "whl")}

.is_col <- function(x) {f0(!(is.matrix(x) | is.data.frame(x)), F, nrow > 1 & ncol == 1)}
.is_emp <- function(x) {length(x) == 0 & !is.null(x)}
.is_lin <- function(x) {.is_e1D(x)}
.is_pnt <- function(x) {.is_e0D(x)}
.is_rct <- function(x) {f0(!.is_e2D(x), F, is.matrix(x) | is.data.frame(x))}
.is_row <- function(x) {f0(!(is.matrix(x) | is.data.frame(x)), F, nrow == 1 & ncol > 1)}
.is_sld <- function(x) {.is_eUD(x)}
.is_sqr <- function(x) {is.matrix(x) & NROW(x) > 1 & NCOL(x) > 1 & NROW(x) == NCOL(x)}
.sss_props <- function() {c("col", "emp", "lin", "pnt", "rct", "row", "sld", "sqr")}

.ppp_props <- function() {sort(c(.bbb_props(), .ccc_props(), .ddd_props(), .eee_props(), .iii_props(), .mmm_props(), .sss_props()))}
.spec_vals <- function(x) {if (!is.character(x)) {return(NULL)} else {x <- unname(unlist(strsplit(x, "|", fixed = T))); x[x != ""]}}

.drop_iprefix <- function(x) {
  i <- nchar(x) == 4 & substr(x, 1, 1) == "i"
  x[i] <- substr(x[i], 2, 4)
  x
}

#' @name ppp
#' @family props
#' @title All purpose property checking
#' @description This set of functions provide utilities that bring together
#'   these seven families of properties defined by this package:\tabular{ll}{
#'     PROPERTY         \tab PROPERTY                                        \cr
#'     FAMILY VALUE     \tab FAMILY NAME                                     \cr
#'     \code{\link{bbb}}\tab Base                                            \cr
#'     \code{\link{ccc}}\tab Extended class                                  \cr
#'     \code{\link{ddd}}\tab Defined dimensionality                          \cr
#'     \code{\link{eee}}\tab Effective dimensionality                        \cr
#'     \code{\link{iii}}\tab Integrity (state of completeness)               \cr
#'     \code{\link{mmm}}\tab Extended mode                                   \cr
#'     \code{\link{sss}}\tab Shape                                             }
#'   \strong{Individual Property Specifications}: In this package, all
#'   individual property specifications are scalars containing exactly 3
#'   characters (e.g., \code{'ord'}, \code{'dtf'}, \code{'d1D'}, or
#'   \code{'rct'}).
#'   \cr\cr
#'   \strong{Combination/Conjunctive Property Specifications}: Specifications
#'   for properties that must co-occur are constructed by delimiting multiple
#'   individual properties with underscores (e.g., the properties \code{'ord'},
#'   \code{'dtf'}, \code{'d1D'}, and \code{'rct'} could be specified to occur
#'   together using the combination/conjunctive property specification
#'   \code{'ord_dtf_d1D_rct'} or an equivalent underscore-delimited permutation.
#'   \cr\cr
#'   \strong{Alternative/Compensatory Property Specifications}: Specifications
#'   for alternative/compensatory properties give a way to indicate that if any
#'   one (individual or combination) property is satisfied the entire
#'   specification is satisfied. They are constructed by pipe-delimiting
#'   multiple individual or combination properties. For example, the
#'   specification \code{'ord|dtf_d1D||dtf_rct'} would be satisfied by an object
#'   with property \code{'ord'}, by an object with the combined property
#'   \code{'dtf_d1D'}, or by an object with the combined property
#'   \code{'dtf_rct'}.
#'   \cr\cr
#'   How functions are related to property families and property specifications
#'   is explained in the sections entitled \itemize{
#'     \item Universal Property Functions
#'     \item Individual Property Functions
#'     \item Combination Property Functions}
#' @section Universal Property Functions: These functions address all property
#'   families.
#'   \cr\cr
#'   \strong{\code{ppp}}
#'   \cr Get all properties of all property families applicable to an object.
#'   \cr\cr
#'   \strong{\code{ippp}}
#'   \cr Evaluate whether an object satisfies the property specification in
#'   \code{ppp} subject to any additional restrictions supplied in \code{...}.
#'   \cr\cr
#'   \strong{\code{nll_or}}
#'   \cr Evaluate whether an object is either \code{NULL} or satisfies the
#'   property specification in argument \code{ppp} subject to any additional
#'   restrictions in \code{...}.
#'   \cr\cr
#'   \strong{\code{nas_or}}
#'   \cr Evaluate whether an object is either scalar \code{NA} or satisfies the
#'   property specification in argument \code{ppp} subject to any additional
#'   restrictions in \code{...}.
#'   \cr\cr
#'   \strong{\code{all_props}}
#'   \cr Get all possible individual properties from all property families and
#'   all possible combination/conjunctive properties as checked by combined
#'   property functions (see the \emph{combined property functions} section).
#'   \cr\cr
#'   \strong{\code{ppp_all}}
#'   \cr Get all constituent individual properties from the property
#'   specification in argument \code{ppp}.
#'   \cr\cr
#'   \strong{\code{ppp_funs}}
#'   \cr Gets the names of all property functions that check for a single
#'   property or a combined property without checking for any additional
#'   restrictions.
#'   \cr\cr
#'   \strong{\code{ppp_defs}}
#'   \cr Get a data frame defining all individual properties of all property
#'   families.
#'   \cr\cr
#'   \strong{\code{is_ppp_fun}}
#'   \cr Evaluates whether a character scalar is the name of a property function
#'   (those listed by \code{\link{ppp_funs}}).
#'   \cr\cr
#'   \strong{\code{is_valid_ppp}}
#'   \cr Evaluates a character scalar property specification for validity (i.e.,
#'   does it contain only single properties, valid combination properties,
#'   and/or valid alternate properties).
#'   \cr\cr
#'   \strong{\code{ppp_from_combo}}
#'   \cr Extract each individual property value from a combination/conjunctive
#'   property specification.
#'   \cr\cr
#'   \strong{\code{combos_from_ppp}}
#'   \cr Extracts each individual or combination property specification from an
#'   alternative/compensatory property specification.
#'   \cr\cr
#'   \strong{\code{ppp_verbose}}
#'   \cr Get a verbose definition of an individual property specification.
#'   \cr\cr
#'   \strong{\code{ppp_concise}}
#'   \cr Get a plain-language, concise definition of an individual or
#'   combination/conjunctive property specification.
#'   \cr\cr
#'   \strong{\code{alt_ppp_concise}}
#'   \cr Get a plain-language, concise definition of an alternative/compensatory
#'   property specification.
#' @section Individual Property Functions: Property functions associated with a
#'   single property family take the following forms where \code{PPP} is a
#'   placeholder for any given individual property and \code{FFF} is a
#'   placeholder for any given  property family.
#'   \cr\cr
#'   \strong{\code{FFF}}
#'   \cr Get properties from the property family represented by \code{FFF}
#'   applicable to an object.
#'   \cr\cr
#'   \strong{\code{iPPP}}
#'   \cr Evaluate whether an object has the specific property represented by
#'   \code{PPP}.
#'   \cr\cr
#'   \strong{\code{iFFF}}
#'   \cr Evaluate whether an object has one or more individual properties from
#'   the property family represented by \code{FFF}, subject to any restrictions
#'   supplied in \code{...}.
#'   \cr\cr
#'   \strong{\code{FFF_props}}
#'   \cr Get all properties in the property family represented by \code{FFF}.
#' @section Combination Property Functions: Property functions associated with a
#'   single property family take the following forms where \code{CCC},
#'   \code{MMM}, and \code{BBB} are placeholders for properties from extended
#'   class, extended mode, and base property families.
#'   \cr\cr
#'   \strong{\code{MMM_CCC}}
#'   \cr Evaluate whether an object is of the extended mode represented by
#'   \code{MMM} and of the extended class represented by \code{CCC}.
#'   \cr\cr
#'   \strong{\code{BBB_CCC}}
#'   \cr Evaluate whether an object is of the fundamental type represented by
#'   \code{BBB} and of the extended class represented by \code{CCC}.
#'   \cr\cr
#'   \strong{\code{BBB_MMM}}
#'   \cr Evaluate whether an object is of the fundamental type represented by
#'   \code{BBB} and of the extended mode represented by \code{MMM}.
#'   \cr\cr
#'   \strong{\code{cmp_CCC}}
#'   \cr Evaluate whether an object is complete (non-empty, atomic, containing
#'   no \code{NA} values). and is of the extended class represented by
#'   \code{CCC}.
#'   \cr\cr
#'   \strong{\code{cmp_MMM}}
#'   \cr Evaluate whether an object is complete (implying non-empty and atomic)
#'   and is of the extended mode represented by \code{MMM}.
#'   \cr\cr
#'   \strong{\code{cmp_MMM_CCC}}
#'   \cr Evaluate whether an object is complete (implying non-empty and atomic),
#'   is of the extended mode represented by \code{MMM}, and is of the extended
#'   class represented by \code{CCC}.
#'   \cr\cr
#'   \strong{\code{mmm_ccc_props}}
#'   \cr Get all combined extended mode + extended class properties.
#'   \cr\cr
#'   \strong{\code{bbb_ccc_props}}
#'   \cr Get all combined base + extended class properties.
#'   \cr\cr
#'   \strong{\code{bbb_mmm_props}}
#'   \cr Get combined base + extended mode properties.
#'   \cr\cr
#'   \strong{\code{cmp_ccc_props}}
#'   \cr Get all combined completeness + extended class properties.
#'   \cr\cr
#'   \strong{\code{cmp_mmm_props}}
#'   \cr Get all combined completeness + extended mode properties.
#'   \cr\cr
#'   \strong{\code{cmp_mmm_ccc_props}}
#'   \cr Get all combined completeness + extended mode + extended class
#'   properties.
#' @param as.dtf \link[=cmp_lgl_scl]{Complete logical scalar} indicating whether
#'   to return the result as a dtf with column 1 containing property values and
#'   column 2 containing the property families.
#' @param x An R object.
#' @param ppp \link[=cmp_chr_scl]{Complete character scalar} containing one or
#'   more values from \code{ppp_vals()} separated by pipes and/or underscores.
#'   Combinations of properties can be specified by separating them with
#'   underscores. Separating properties or combinations of properties with pipes
#'   will result in a value of \code{TRUE} if any of them applies to \code{x}.
#' @param valid \link[=cmp_chr_vec]{Complete character vec} containing all
#'   properties considered valid.
#' @param print \link[=cmp_lgl_scl]{Complete logical scalar} indicating whether
#'   to print the property definition to the console.
#' @inheritDotParams meets
#' @inheritSection meets Specifying Count and Value Restrictions
#' @return \strong{\code{ppp, ppp_all, ppp_vals, pop_funs, ppp_from_combo,
#'   combos_from_ppp}}
#'   \cr A character vector.
#'   \cr\cr
#'   \strong{\code{ppp_concise, alt_ppp_concise}}
#'   \cr A character scalar.
#'   \cr\cr
#'   \strong{All others}
#'   \cr A logical scalar.
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
ippp <- function(x, spec, ...) {
  if (!is_valid_ppp(ppp)) {stop("\n \u2022 [spec] specifies a property not in ppp_props().")}
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
  cc_fun <- cmp_ccc_props(); cc_fam <- rep("cmp_ccc", length(cc_fun)); cc_lab <- paste0("3_", cc_fam, "_", cc_fun)
  cmc_fun <- cmp_mmm_ccc_props(); cmc_fam <- rep("cmp_mmm_ccc", length(cmc_fun)); cmc_lab <- paste0("4_", cmc_fam, "_", cmc_fun)
  fun <- c(b_fun, c_fun, d_fun, e_fun, i_fun, m_fun, s_fun, mc_fun, bc_fun, bm_fun, cc_fun, cmc_fun)
  fam <- c(b_fam, c_fam, d_fam, e_fam, i_fam, m_fam, s_fam, mc_fam, bc_fam, bm_fam, cc_fam, cmc_fam)
  ord <- order(c(b_lab, c_lab, d_lab, e_lab, i_lab, m_lab, s_lab, mc_lab, bc_lab, bm_lab, cc_lab, cmc_lab))
  if (!as.dtf) {return(fun[ord])}
  tibble::tibble(family = fam[ord], fun = fun[ord])
}

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
is_ppp_fun <- function(x) {
  if (!cmp_chr_scl(x)) {stop("\n \u2022 [x] must be a complete character scalar (?cmp_chr_scl).")}
  x %in% ppp_funs()
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
  ccc <- abb[val %in% combo & fam == "ccc"]
  ddd <- abb[val %in% combo & fam == "ddd"]
  eee <- abb[val %in% combo & fam == "eee"]
  fff <- abb[val %in% combo & fam == "fff"]
  mmm <- abb[val %in% combo & fam == "mmm"]
  sss <- abb[val %in% combo & fam == "sss"]
  ttt <- abb[val %in% combo & fam == "ttt"]
  object <- length(ccc) == 0
  out <- paste0(c(ccc, ddd, eee, fff, mmm, sss, ttt), collapse = ", ")
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
