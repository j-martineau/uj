#' @encoding UTF-8
#' @title Package `uj` atomic scalar and vector constants
#' @description Constants defined by package `uj` are provided in a number of package values with associated descriptions. Constants are organized into the following types:
#' \tabular{ll}{        Properties   \tab Object properties defined by \code{\link{ppp}()} functions;
#'   \cr   \tab   \cr   Crayons      \tab Synonyms for \code{\link[crayon:red]{crayon::colors}} and \code{\link[crayon:styles]{crayon::styles}} used by \code{\link[uj:st_vals]{uj::crayons}} functions;
#'   \cr   \tab   \cr   Markdown     \tab Basic, subscript, superscript, and bold, plain, and italic text color open tags;
#'   \cr   \tab   \cr   Colors       \tab Named color codes and color palettes;
#'   \cr   \tab   \cr   Linetypes    \tab Named line types (extending those available by name in base R), and
#'   \cr   \tab   \cr   Markers      \tab ASCII codes for plotting characters and base R symbols as value markers rather than as text including:
#'   \cr   \tab \itemize{
#'     \item Lowercase letters,
#'     \item Uppercase letters,
#'     \item Digits (including 0),
#'     \item Digits (excluding 0),
#'     \item Distinctive non-enclosing shapes,
#'     \item Distinctive punctuation characters,
#'     \item Base R empty/non-fillable shapes,
#'     \item Base R fillable shapes,
#'     \item Base R solid shapes, and
#'     \item A blank string shape.
#'   }
#'   \cr   Charsets    \tab Characters sets including:
#'   \cr   \tab \itemize{
#'     \item Plain-font UTF-8 upper and lower Greek in markdown-compatible configuration
#'     \item Plain-font UTF-8 upper and lower Greek in markdown-compatible configuration
#'     \item Escaped UTF-8 codes for a variety of special or functional characters.
#'     \item Characters used for enclosing text (e.g., quotes, brackets);
#'     \item Lowercase and/or uppercase letters,
#'     \item Digits as characters,
#'     \item Characters valid for file names,
#'     \item Upper and lowercase vowels,
#'     \item Characters for plotting labels without special formatting,
#'     \item Characters valid for R model or formulas,
#'     \item Characters valid for the right side of R models or formulas,
#'     \item Characters valid for R object names without back ticks,
#'     \item A restricted set for file names,
#'     \item A restricted set for R models and formulas,
#'     \item A restricted set for the right side of R models and formulas, and
#'     \item A restricted set for R object names.
#' }}
#' @return An object of class `list` containing atomic scalar or atomic vector elements (`.xxxVals` objects) or a character vector describing the constants (`.xxxDefs` objects).
#' @export
ujPkgConstants <- function() {utils::help("ujPkgConstants", package = "uj")}

#' @rdname ujPkgConstants
#' @export
.PkgPropFamilies <- base::list(
  bbb = base::c("atm", "def", "fun", "nil", "nll", "pop", "rcr"),
  BBB = base::c("ATM", "DEF", "FUN", "NIL", "NLL", "POP", "RCR"),
  ccc = base::c("arr", "dtf", "gen", "mat", "mvc", "scl", "vec", "vls"),
  CCC = base::c("ARR", "DTF", "GEN", "MAT", "MVC", "SCL", "VEC", "VLS"),
  ddd = base::c("d0d", "d1d", "d2d", "dhd"),
  DDD = base::c("D0D", "D1D", "D2D", "DHD"),
  eee = base::c("e0d", "e1d", "e2d", "ehd", "eud"),
  EEE = base::c("E0D", "E1D", "E2D", "EHD", "EUD"),
  iii = base::c("cmp", "dup", "mss", "na0", "ok0", "prt", "unq"),
  III = base::c("CMP", "DUP", "MSS", "NA0", "OK0", "PRT", "UNQ"),
  mmm = base::c("atm", "ch1", "ch3", "chr", "clr", "evn", "fac", "frc", "ind", "lgl", "neg", "ngw", "nng", "nnw", "nps", "npw", "nst", "num", "odd", "ord", "pct", "pos", "ppn", "psw", "srt", "str", "uno", "whl"),
  MMM = base::c("ATM", "CH1", "CH3", "CHR", "CLR", "EVN", "FAC", "FRC", "IND", "LGL", "NEG", "NGW", "NNG", "NNW", "NPS", "NPW", "NST", "NUM", "ODD", "ORD", "PCT", "POS", "PPN", "PSW", "SRT", "STR", "UNO", "WHL"),
  sss = base::c("col", "emp", "lin", "pnt", "rct", "row", "sld", "sqr"),
  SSS = base::c("COL", "EMP", "LIN", "PNT", "RCT", "ROW", "SLD", "SQR")
)

#' @rdname ujPkgConstants
#' @export
.PkgAllPropVals <- base::sort(base::unique(base::c(base::unlist(.PkgPropFamilies, T, F), base::toupper(base::unlist(.PkgPropFamilies, T, F)))))

#' @rdname ujPkgConstants
#' @export
.PkgCrayonSynonyms <- base::list(
  Bold = base::c("b", "bo", "bld", "bold", "bolded", "s", "st", "str", "strong"),
  Plain = base::c("p", "pl", "pln", "plain", "r", "re", "res", "reset"),
  Italic = base::c("i", "it", "itl", "ital", "italic", "italics", "italicized", "e", "em", "emp", "emph", "emphasis", "emphasized"),
  Default = base::c("d", "def", "default"),
  Underline = base::c("u", "un", "und", "under", "underline", "underlined"),
  Red = base::c("r", "red"),
  Blue = base::c("b", "blu", "blue"),
  Cyan = base::c("c", "cyn", "cyan"),
  Green = base::c("g", "grn", "green"),
  Black = base::c("k", "blk", "black"),
  White = base::c("w", "wht", "white"),
  Yellow = base::c("y", "yel", "ylw", "yellow"),
  Silver = base::c("s", "gry", "grey", "gray", "sil", "slv", "silver"),
  Magenta = base::c("m", "mag", "magenta")
)

#' @rdname ujPkgConstants
#' @export
.PkgCrayonFamilies <- base::list(
  Styles = base::sort(base::unlist(base::c(.PkgCrayonSynonyms$Bold, .PkgCrayonSynonyms$Default, .PkgCrayonSynonyms$Italic, .PkgCrayonSynonyms$Plain, .PkgCrayonSynonyms$Underline), T, F)),
  BackgroundColors = base::sort(base::unlist(base::c(.PkgCrayonSynonyms$Blue, .PkgCrayonSynonyms$Cyan, .PkgCrayonSynonyms$Default, .PkgCrayonSynonyms$Green, .PkgCrayonSynonyms$Black, .PkgCrayonSynonyms$Magenta, .PkgCrayonSynonyms$Red, .PkgCrayonSynonyms$White, .PkgCrayonSynonyms$Yellow, .PkgCrayonSynonyms$Silver), T, F)),
  ForegroundColors = base::sort(base::unlist(base::c(.PkgCrayonSynonyms$Blue, .PkgCrayonSynonyms$Cyan, .PkgCrayonSynonyms$Default, .PkgCrayonSynonyms$Green, .PkgCrayonSynonyms$Black, .PkgCrayonSynonyms$Magenta, .PkgCrayonSynonyms$Red, .PkgCrayonSynonyms$White, .PkgCrayonSynonyms$Yellow), T, F))
)

#' @rdname ujPkgConstants
#' @export
.BoldGreekVals   <- base::list(bAlpha = "**\U03B1**", bBeta = "**\U03B2**", bGamma = "**\U03B3**", bDelta = "**\U03B4**", bEpsilon = "**\U03B5**", bZeta = "**\U03B5**", bEta = "**\U03B7**", bTheta = "**\U03B8**", bIota = "**\U03B9**", bKappa = "**\U03BA**", bLambda = "**\U03BB**", bMu = "**\U03BC**", bNu = "**\U03BD**", bXi = "**\U03BE**", bOmicron = "**\U03BF**", bPi = "**\U03C0**", bRho = "**\U03C1**", bSigma = "**\U03C3**", bTau = "**\U03C4**", bUpsilon = "**\U03C5**", bPhi = "**\U03C6**", bChi = "**\U03C7**", bPsi = "**\U03C8**", bOmega = "**\U03C9**")

#' @rdname ujPkgConstants
#' @export
.BoldGreekDefs   <- base::c(alpha = "bold alpha"  , beta = "bold beta"  , gamma = "bold gamma"  , delta = "bold delta"  , epsilon = "bold epsilon"  , zeta = "bold zeta"  , eta = "bold eta"  , theta = "bold theta"  , iota = "bold iota"  , kappa = "bold kappa"  , lambda = "bold lambda"  , mu = "bold mu"  , nu = "bold nu"  , xi = "bold xi"  , omicron = "bold omicron"  , pi = "bold pi"  , rho = "bold rho"  , sigma = "bold sigma"  , tau = "bold tau"  , upsilon = "bold upsilon"  , phi = "bold phi"  , chi = "bold chi"  , psi = "bold psi"  , omega = "bold omega"   )

#' @rdname ujPkgConstants
#' @export
.BoldGREEKvals   <- base::list(bALPHA = "**\U0391**", bBETA = "**\U0392**", bgAMMA = "**\U0393**", bDELTA = "**\U0394**", bEPSILON = "**\U0395**", bZETA = "**\U0396**", bETA = "**\U0397**", bTHETA = "**\U0398**", bIOTA = "**\U0399**", bKAPPA = "**\U039A**", bLAMBDA = "**\U039B**", bMU = "**\U039C**", bNU = "**\U039D**", bXI = "**\U039E**", bOMICRON = "**\U039F**", bPI = "**\U03A0**", bRHO = "**\U03A1**", bSIGMA = "**\U03A3**", bTAU = "**\U03A4**", bUPSILON = "**\U03A5**", bPHI = "**\U03A6**", bCHI = "**\U03A7**", bPSI = "**\U03A8**", bOMEGA = "**\U03A9**")

#' @rdname ujPkgConstants
#' @export
.BoldGREEKdefs   <- base::c(ALPHA = "bold ALPHA"  , BETA = "bold BETA"  , GAMMA = "bold GAMMA"  , DELTA = "bold DELTA"  , EPSILON = "bold EPSILON"  , ZETA = "bold ZETA"  , ETA = "bold ETA"  , THETA = "bold THETA"  , IOTA = "bold IOTA"  , KAPPA = "bold KAPPA"  , LAMBDA = "bold LAMBDA"  , MU = "bold MU"  , NU = "bold NU"  , XI = "bold XI"  , OMICRON = "bold OMICRON"  , PI = "bold PI"  , RHO = "bold RHO"  , SIGMA = "bold SIGMA"  , TAU = "bold TAU"  , UPSILON = "bold UPSILON"  , PHI = "bold PHI"  , CHI = "bold CHI"  , PSI = "bold PSI"  , OMEGA = "bold OMEGA"   )

#' @rdname ujPkgConstants
#' @export
.ItalicGreekVals <- base::list(iAlpha =  "*\U03B1*" , iBeta =  "*\U03B2*" , iGamma =  "*\U03B3*" , iDelta =  "*\U03B4*" , iEpsilon =  "*\U03B5*" , iZeta =  "*\U03B5*" , iEta =  "*\U03B7*" , iTheta =  "*\U03B8*" , iIota =  "*\U03B9*" , iKappa =  "*\U03BA*" , iLambda =  "*\U03BB*" , iMu =  "*\U03BC*" , iNu =  "*\U03BD*" , iXi =  "*\U03BE*" , iOmicron =  "*\U03BF*" , iPi =  "*\U03C0*" , iRho =  "*\U03C1*" , iSigma =  "*\U03C3*" , iTau =  "*\U03C4*" , iUpsilon =  "*\U03C5*" , iPhi =  "*\U03C6*" , iChi =  "*\U03C7*" , iPsi =  "*\U03C8*" , iOmega =  "*\U03C9*" )

#' @rdname ujPkgConstants
#' @export
.ItalicGreekDefs <- base::c(alpha = "italic alpha", beta = "italic beta", gamma = "italic gamma", delta = "italic delta", epsilon = "italic epsilon", zeta = "italic zeta", eta = "italic eta", theta = "italic theta", iota = "italic iota", kappa = "italic kappa", lambda = "italic lambda", mu = "italic mu", nu = "italic nu", xi = "italic xi", omicron = "italic omicron", pi = "italic pi", rho = "italic rho", sigma = "italic sigma", tau = "italic tau", upsilon = "italic upsilon", phi = "italic phi", chi = "italic chi", psi = "italic psi", omega = "italic omega" )

#' @rdname ujPkgConstants
#' @export
.ItalicGREEKvals <- base::list(iALPHA =  "*\U0391*" , iBETA =  "*\U0392*" , iGAMMA =  "*\U0393*" , iDELTA =  "*\U0394*" , iEPSILON =  "*\U0395*" , iZETA =  "*\U0396*" , iETA =  "*\U0397*" , iTHETA =  "*\U0398*" , iIOTA =  "*\U0399*" , iKAPPA =  "*\U039A*" , iLAMBDA =  "*\U039B*" , iMU =  "*\U039C*" , iNU =  "*\U039D*",  iXI =  "*\U039E*" , iOMICRON =  "*\U039F*" , iPI =  "*\U03A0*" , iRHO =  "*\U03A1*" , iSIGMA =  "*\U03A3*" , iTAU =  "*\U03A4*" , iUPSILON =  "*\U03A5*" , iPHI =  "*\U03A6*" , iCHI =  "*\U03A7*" , iPSI =  "*\U03A8*" , iOMEGA =  "*\U03A9*" )

#' @rdname ujPkgConstants
#' @export
.ItalicGREEKdefs <- base::c(ALPHA = "italic ALPHA", BETA = "italic BETA", GAMMA = "italic GAMMA", DELTA = "italic DELTA", EPSILON = "italic EPSILON", ZETA = "italic ZETA", ETA = "italic ETA", THETA = "italic THETA", IOTA = "italic IOTA", KAPPA = "italic KAPPA", LAMBDA = "italic LAMBDA", MU = "italic MU", NU = "italic NU", XI = "italic XI", OMICRON = "italic OMICRON", PI = "italic PI", RHO = "italic RHO", SIGMA = "italic SIGMA", TAU = "italic TAU", UPSILON = "italic UPSILON", PHI = "italic PHI", CHI = "italic CHI", PSI = "italic PSI", OMEGA = "italic OMEGA" )

#' @rdname ujPkgConstants
#' @export
.PlainGreekVals  <- base::list( Alpha =   "\U03B1"  ,  Beta =   "\U03B2"  ,  Gamma =   "\U03B3"  ,  Delta =   "\U03B4"  ,  Epsilon =   "\U03B5"  ,  Zeta =   "\U03B5"  ,  Eta =   "\U03B7"  ,  Theta =   "\U03B8"  ,  Iota =   "\U03B9"  ,  Kappa =   "\U03BA"  ,  Lambda =   "\U03BB"  ,  Mu =   "\U03BC"  ,  Nu =   "\U03BD"  ,  Xi =   "\U03BE"  ,  Omicron =   "\U03BF"  ,  Pi =   "\U03C0"  ,  Rho =   "\U03C1"  ,  Sigma =   "\U03C3"  ,  Tau =   "\U03C4"  ,  Upsilon =   "\U03C5"  ,  Phi =   "\U03C6"  ,  Chi =   "\U03C7"  ,  Psi =   "\U03C8"  ,  Omega =   "\U03C9"  )

#' @rdname ujPkgConstants
#' @export
.PlainGreekDefs  <- base::c(alpha = "alpha", beta = "beta", gamma = "gamma", delta = "delta", epsilon = "epsilon", zeta = "zeta", eta = "eta", theta = "theta", iota = "iota", kappa = "kappa", lambda = "lambda", mu = "mu", nu = "nu", xi = "xi", omicron = "omicron"       , pi = "pi"       , rho = "rho"       , sigma = "sigma"       , tau = "tau"       , upsilon = "upsilon"       , phi = "phi"       , chi = "chi"       , psi = "psi"       , omega = "omega"        )

#' @rdname ujPkgConstants
#' @export
.PlainGREEKvals  <- base::list(ALPHA = "\U0391",  BETA = "\U0392",  GAMMA =   "\U0393",  DELTA =   "\U0394"  ,  EPSILON =   "\U0395"  ,  ZETA =   "\U0396"  ,  ETA =  " \U0397"  ,  THETA =   "\U0398"  ,  IOTA =   "\U0399"  ,  KAPPA =   "\U039A"  ,  LAMBDA =   "\U039B"  ,  MU =   "\U039C"  ,  NU =   "\U039D"  ,  XI =   "\U039E"  ,  OMICRON =   "\U039F"  ,  PI =   "\U03A0"  ,  RHO =   "\U03A1"  ,  SIGMA =   "\U03A3"  ,  TAU =   "\U03A4"  ,  UPSILON =   "\U03A5"  ,  PHI =   "\U03A6"  ,  CHI =   "\U03A7"  ,  PSI =   "\U03A8"  ,  OMEGA =   "\U03A9"  )

#' @rdname ujPkgConstants
#' @export
.PlainGREEKdefs  <- base::c(ALPHA = "ALPHA", BETA = "BETA", GAMMA = "GAMMA", DELTA = "DELTA", EPSILON = "EPSILON", ZETA = "ZETA", ETA = "ETA", THETA = "THETA", IOTA = "IOTA", KAPPA = "KAPPA", LAMBDA = "LAMBDA", MU = "MU", NU = "NU", XI = "XI", OMICRON = "OMICRON", PI = "PI", RHO = "RHO", SIGMA = "SIGMA", TAU = "TAU", UPSILON = "UPSILON", PHI = "PHI", CHI = "CHI", PSI = "PSI", OMEGA = "OMEGA")

#' @rdname ujPkgConstants
#' @export
.EnclosureVals <- base::c(tick = "\U0060", open = "\U003C", close = "\U003E", quote = "\U0027", quote2 = "\U0022", lbrace = "\U007B", rbrace = "\U007D", lparen = "\U0028", rparen = "\U0029", lquote = "\U2018", rquote = "\U2019", lquote2 = "\U201C", rquote2 = "\U201D", lbracket = "\U005B", rbracket = "\U005D", langle = "\U27E8", rangle = "\U27E9", lparen2 = "\U2E28", rparen2 = "\U2E29", langle2 = "\U300A", rangle2 = "\U300B")

#' @rdname ujPkgConstants
#' @export
.EnclosureDefs <- base::c(tick = "backtick", open = "markdown tag open", close = "markdown tag close", quote = "single straight quote", quote2 = "double straight quote", lbrace = "left curly brace", rbrace = "right curly brace", lparen = "left paren", rparen = "right paren", lquote = "left single quote", rquote = "non-escaped right single quote", lquote2 = "non-escaped left double quote", rquote2 = "non-escaped right double quote", lbracket = "left square bracket", rbracket = "right square bracket", langle = "left angle bracket", rangle = "right angle bracket", lparen2 = "double left paren", rparen2 = "double right paren", langle2 = "double left angle bracket", rangle2 = "double right angle bracket")

#' @rdname ujPkgConstants
#' @export
.BasicMarkdownVals <- base::list(brk = "<br />", sub1  = "<sub>", sub2 = "</sub>", sup1 = "<sup>", sup2 = "</sup>", close = ">", pclose = "</span>", bclose = "</strong>", iclose = "</em>")

#' @rdname ujPkgConstants
#' @export
.BasicMarkdownDefs <- base::c(brk = "markdown line break", sub1 = "markdown subscript open", sub2 = "markdown subscript close", sup1 = "markdown superscript open", sup2 = "markdown superscript close", close = "markdown generic close", pclose = "markdown span/plain close", bclose = "markdown bold span close", iclose = "markdown italic span close")

#' @rdname ujPkgConstants
#' @export
.OpenMarkdownColorVals <- base::list(
  iblk  = "<em style=\"color:#000000FF;\">", ig05  = "<em style=\"color:#0D0D0DFF;\">", ig10  = "<em style=\"color:#1A1A1AFF;\">", ig15  = "<em style=\"color:#262626FF;\">", ig20  = "<em style=\"color:#333333FF;\">",
  ig25  = "<em style=\"color:#404040FF;\">", ig30  = "<em style=\"color:#4D4D4DFF;\">", ig35  = "<em style=\"color:#595959FF;\">", ig40  = "<em style=\"color:#666666FF;\">", ig45  = "<em style=\"color:#737373FF;\">",
  ig50  = "<em style=\"color:#808080FF;\">", ig55  = "<em style=\"color:#8C8C8CFF;\">", ig60  = "<em style=\"color:#999999FF;\">", ig65  = "<em style=\"color:#A6A6A6FF;\">", ig70  = "<em style=\"color:#B3B3B3FF;\">",
  ig75  = "<em style=\"color:#BFBFBFFF;\">", ig80  = "<em style=\"color:#CCCCCCFF;\">", ig85  = "<em style=\"color:#D9D9D9FF;\">", ig90  = "<em style=\"color:#E6E6E6FF;\">", ig95  = "<em style=\"color:#F2F2F2FF;\">",
  iwht  = "<em style=\"color:#FFFFFFFF;\">", iinv  = "<em style=\"color:#FFFFFF01;\">", ifblu = "<em style=\"color:#0000FFFF;\">", ifcyn = "<em style=\"color:#00FFFFFF;\">", ifgrn = "<em style=\"color:#00FF00FF;\">",
  ifmag = "<em style=\"color:#FF00FFFF;\">", iforn = "<em style=\"color:#FF8800FF;\">", ifred = "<em style=\"color:#AA0000FF;\">", ifylw = "<em style=\"color:#FFFF00FF;\">", ilblu = "<em style=\"color:#DDDDFFFF;\">",
  ilcyn = "<em style=\"color:#FFDDFFFF;\">", ilgrn = "<em style=\"color:#DDFFDDFF;\">", ilmag = "<em style=\"color:#FFBBFFFF;\">", ilorn = "<em style=\"color:#FFEEAAFF;\">", ilred = "<em style=\"color:#FFDDDDFF;\">",
  ilylw = "<em style=\"color:#FFFFDDFF;\">", imblu = "<em style=\"color:#AAAAFFFF;\">", imcyn = "<em style=\"color:#66FFFFFF;\">", imgrn = "<em style=\"color:#FFFF66FF;\">", immag = "<em style=\"color:#FF66FFFF;\">",
  imorn = "<em style=\"color:#FFAA66FF;\">", imred = "<em style=\"color:#AFFAAAFF;\">", imylw = "<em style=\"color:#FFFF66FF;\">", idblu = "<em style=\"color:#000080FF;\">", idcyn = "<em style=\"color:#008B8BFF;\">",
  idgrn = "<em style=\"color:#008B00FF;\">", idmag = "<em style=\"color:#8B008BFF;\">", idorn = "<em style=\"color:#8B4500FF;\">", idred = "<em style=\"color:#800000FF;\">", idylw = "<em style=\"color:#8B8B00FF;\">" ,
  pblk  = "<span style=\"color:#000000FF;\">", pg05  = "<span style=\"color:#0D0D0DFF;\">", pg10  = "<span style=\"color:#1A1A1AFF;\">", pg15  = "<span style=\"color:#262626FF;\">", pg20  = "<span style=\"color:#333333FF;\">",
  pg25  = "<span style=\"color:#404040FF;\">", pg30  = "<span style=\"color:#4D4D4DFF;\">", pg35  = "<span style=\"color:#595959FF;\">", pg40  = "<span style=\"color:#666666FF;\">", pg45  = "<span style=\"color:#737373FF;\">",
  pg50  = "<span style=\"color:#808080FF;\">", pg55  = "<span style=\"color:#8C8C8CFF;\">", pg60  = "<span style=\"color:#999999FF;\">", pg65  = "<span style=\"color:#A6A6A6FF;\">", pg70  = "<span style=\"color:#B3B3B3FF;\">",
  pg75  = "<span style=\"color:#BFBFBFFF;\">", pg80  = "<span style=\"color:#CCCCCCFF;\">", pg85  = "<span style=\"color:#D9D9D9FF;\">", pg90  = "<span style=\"color:#E6E6E6FF;\">", pg95  = "<span style=\"color:#F2F2F2FF;\">",
  pwht  = "<span style=\"color:#FFFFFFFF;\">", pinv  = "<span style=\"color:#FFFFFF01;\">", pfblu = "<span style=\"color:#0000FFFF;\">", pfcyn = "<span style=\"color:#00FFFFFF;\">", pfgrn = "<span style=\"color:#00FF00FF;\">",
  pfmag = "<span style=\"color:#FF00FFFF;\">", pforn = "<span style=\"color:#FF8800FF;\">", pfred = "<span style=\"color:#AA0000FF;\">", pfylw = "<span style=\"color:#FFFF00FF;\">", plblu = "<span style=\"color:#DDDDFFFF;\">",
  plcyn = "<span style=\"color:#FFDDFFFF;\">", plgrn = "<span style=\"color:#DDFFDDFF;\">", plmag = "<span style=\"color:#FFBBFFFF;\">", plorn = "<span style=\"color:#FFEEAAFF;\">", plred = "<span style=\"color:#FFDDDDFF;\">",
  plylw = "<span style=\"color:#FFFFDDFF;\">", pmblu = "<span style=\"color:#AAAAFFFF;\">", pmcyn = "<span style=\"color:#66FFFFFF;\">", pmgrn = "<span style=\"color:#FFFF66FF;\">", pmmag = "<span style=\"color:#FF66FFFF;\">",
  pmorn = "<span style=\"color:#FFAA66FF;\">", pmred = "<span style=\"color:#AFFAAAFF;\">", pmylw = "<span style=\"color:#FFFF66FF;\">", pdblu = "<span style=\"color:#000080FF;\">", pdcyn = "<span style=\"color:#008B8BFF;\">",
  pdgrn = "<span style=\"color:#008B00FF;\">", pdmag = "<span style=\"color:#8B008BFF;\">", pdorn = "<span style=\"color:#8B4500FF;\">", pdred = "<span style=\"color:#800000FF;\">", pdylw = "<span style=\"color:#8B8B00FF;\">",
  bblk  = "<strong style=\"color:#000000FF;\">", bg05  = "<strong style=\"color:#0D0D0DFF;\">", bg10  = "<strong style=\"color:#1A1A1AFF;\">", bg15  = "<strong style=\"color:#262626FF;\">", bg20  = "<strong style=\"color:#333333FF;\">",
  bg25  = "<strong style=\"color:#404040FF;\">", bg30  = "<strong style=\"color:#4D4D4DFF;\">", bg35  = "<strong style=\"color:#595959FF;\">", bg40  = "<strong style=\"color:#666666FF;\">", bg45  = "<strong style=\"color:#737373FF;\">",
  bg50  = "<strong style=\"color:#808080FF;\">", bg55  = "<strong style=\"color:#8C8C8CFF;\">", bg60  = "<strong style=\"color:#999999FF;\">", bg65  = "<strong style=\"color:#A6A6A6FF;\">", bg70  = "<strong style=\"color:#B3B3B3FF;\">",
  bg75  = "<strong style=\"color:#BFBFBFFF;\">", bg80  = "<strong style=\"color:#CCCCCCFF;\">", bg85  = "<strong style=\"color:#D9D9D9FF;\">", bg90  = "<strong style=\"color:#E6E6E6FF;\">", bg95  = "<strong style=\"color:#F2F2F2FF;\">",
  bwht  = "<strong style=\"color:#FFFFFFFF;\">", binv  = "<strong style=\"color:#FFFFFF01;\">", bfblu = "<strong style=\"color:#0000FFFF;\">", bfcyn = "<strong style=\"color:#00FFFFFF;\">", bfgrn = "<strong style=\"color:#00FF00FF;\">",
  bfmag = "<strong style=\"color:#FF00FFFF;\">", bforn = "<strong style=\"color:#FF8800FF;\">", bfred = "<strong style=\"color:#AA0000FF;\">", bfylw = "<strong style=\"color:#FFFF00FF;\">", blblu = "<strong style=\"color:#DDDDFFFF;\">",
  blcyn = "<strong style=\"color:#FFDDFFFF;\">", blgrn = "<strong style=\"color:#DDFFDDFF;\">", blmag = "<strong style=\"color:#FFBBFFFF;\">", blorn = "<strong style=\"color:#FFEEAAFF;\">", blred = "<strong style=\"color:#FFDDDDFF;\">",
  blylw = "<strong style=\"color:#FFFFDDFF;\">", bmblu = "<strong style=\"color:#AAAAFFFF;\">", bmcyn = "<strong style=\"color:#66FFFFFF;\">", bmgrn = "<strong style=\"color:#FFFF66FF;\">", bmmag = "<strong style=\"color:#FF66FFFF;\">",
  bmorn = "<strong style=\"color:#FFAA66FF;\">", bmred = "<strong style=\"color:#AFFAAAFF;\">", bmylw = "<strong style=\"color:#FFFF66FF;\">", bdblu = "<strong style=\"color:#000080FF;\">", bdcyn = "<strong style=\"color:#008B8BFF;\">",
  bdgrn = "<strong style=\"color:#008B00FF;\">", bdmag = "<strong style=\"color:#8B008BFF;\">", bdorn = "<strong style=\"color:#8B4500FF;\">", bdred = "<strong style=\"color:#800000FF;\">", bdylw = "<strong style=\"color:#8B8B00FF;\">"
)

#' @rdname ujPkgConstants
#' @export
.OpenMarkdownColorDefs <- base::c(
  iblk  = "markdown italic hex black open"        , ig05  = "markdown italic hex 05% grey open"    , ig10  = "markdown italic hex 10% grey open"     , ig15  = "markdown italic hex 15% grey open"    , ig20  = "markdown italic hex 20% grey open"      ,
  ig25  = "markdown italic hex 25% grey open"     , ig30  = "markdown italic hex 30% grey open"    , ig35  = "markdown italic hex 35% grey open"     , ig40  = "markdown italic hex 40% grey open"    , ig45  = "markdown italic hex 45% grey open"      ,
  ig50  = "markdown italic hex 50% grey open"     , ig55  = "markdown italic hex 55% grey open"    , ig60  = "markdown italic hex 60% grey open"     , ig65  = "markdown italic hex 65% grey open"    , ig70  = "markdown italic hex 70% grey open"      ,
  ig75  = "markdown italic hex 75% grey open"     , ig80  = "markdown italic hex 80% grey open"    , ig85  = "markdown italic hex 85% grey open"     , ig90  = "markdown italic hex 90% grey open"    , ig95  = "markdown italic hex 95% grey open"      ,
  iwht  = "markdown italic hex white open"        , iinv  = "markdown italic hex invisible open"   , ifblu = "markdown italic hex full blue open"    , ifcyn = "markdown italic hex full cyan open"   , ifgrn = "markdown italic hex full green open"    ,
  ifmag = "markdown italic hex full magenta open" , iforn = "markdown italic hex full orange open" , ifred = "markdown italic hex full red open"     , ifylw = "markdown italic hex full yellow open" , ilblu = "markdown italic hex light blue open"    ,
  ilcyn = "markdown italic hex light cyan open"   , ilgrn = "markdown italic hex light green open" , ilmag = "markdown italic hex light magenta open", ilorn = "markdown italic hex light orange open", ilred = "markdown italic hex light red open"     ,
  ilylw = "markdown italic hex light yellow open" , imblu = "markdown italic hex medium blue open" , imcyn = "markdown italic hex medium cyan open"  , imgrn = "markdown italic hex medium green open", immag = "markdown italic hex medium magenta open",
  imorn = "markdown italic hex medium orange open", imred = "markdown italic hex medium red open"  , imylw = "markdown italic hex medium yellow open", idblu = "markdown italic hex dark blue open"   , idcyn = "markdown italic hex dark cyan open"     ,
  idgrn = "markdown italic hex dark green open"   , idmag = "markdown italic hex dark magenta open", idorm = "markdown italic hex dark orange open"  , idred = "markdown italic hex dark red open"    , idylw = "markdown italic hex dark yellow open"   ,  bblk  = "markdown bold hex black open"        , bg05  = "markdown bold hex 05% grey open"    , bg10  = "markdown bold hex 10% grey open"     , bg15  = "markdown bold hex 15% grey open"    , bg20  = "markdown bold hex 20% grey open"      ,
  pblk  = "markdown plain hex black open"        , pg05  = "markdown plain hex 05% grey open"    , pg10  = "markdown plain hex 10% grey open"     , pg15  = "markdown plain hex 15% grey open"    , pg20  = "markdown plain hex 20% grey open"      ,
  pg25  = "markdown plain hex 25% grey open"     , pg30  = "markdown plain hex 30% grey open"    , pg35  = "markdown plain hex 35% grey open"     , pg40  = "markdown plain hex 40% grey open"    , pg45  = "markdown plain hex 45% grey open"      ,
  pg50  = "markdown plain hex 50% grey open"     , pg55  = "markdown plain hex 55% grey open"    , pg60  = "markdown plain hex 60% grey open"     , pg65  = "markdown plain hex 65% grey open"    , pg70  = "markdown plain hex 70% grey open"      ,
  pg75  = "markdown plain hex 75% grey open"     , pg80  = "markdown plain hex 80% grey open"    , pg85  = "markdown plain hex 85% grey open"     , pg90  = "markdown plain hex 90% grey open"    , pg95  = "markdown plain hex 95% grey open"      ,
  pwht  = "markdown plain hex white open"        , pinv  = "markdown plain hex invisible open"   , pfblu = "markdown plain hex full blue open"    , pfcyn = "markdown plain hex full cyan open"   , pfgrn = "markdown plain hex full green open"    ,
  pfmag = "markdown plain hex full magenta open" , pforn = "markdown plain hex full orange open" , pfred = "markdown plain hex full red open"     , pfylw = "markdown plain hex full yellow open" , plblu = "markdown plain hex light blue open"    ,
  plcyn = "markdown plain hex light cyan open"   , plgrn = "markdown plain hex light green open" , plmag = "markdown plain hex light magenta open", plorn = "markdown plain hex light orange open", plred = "markdown plain hex light red open"     ,
  plylw = "markdown plain hex light yellow open" , pmblu = "markdown plain hex medium blue open" , pmcyn = "markdown plain hex medium cyan open"  , pmgrn = "markdown plain hex medium green open", pmmag = "markdown plain hex medium magenta open",
  pmorn = "markdown plain hex medium orange open", pmred = "markdown plain hex medium red open"  , pmylw = "markdown plain hex medium yellow open", pdblu = "markdown plain hex dark blue open"   , pdcyn = "markdown plain hex dark cyan open"     ,
  pdgrn = "markdown plain hex dark green open"   , pdmag = "markdown plain hex dark magenta open", pdorm = "markdown plain hex dark orange open"  , pdred = "markdown plain hex dark red open"    , pdylw = "markdown plain hex dark yellow open"   ,
  bg25  = "markdown bold hex 25% grey open"     , bg30  = "markdown bold hex 30% grey open"    , bg35  = "markdown bold hex 35% grey open"     , bg40  = "markdown bold hex 40% grey open"    , bg45  = "markdown bold hex 45% grey open"      ,
  bg50  = "markdown bold hex 50% grey open"     , bg55  = "markdown bold hex 55% grey open"    , bg60  = "markdown bold hex 60% grey open"     , bg65  = "markdown bold hex 65% grey open"    , bg70  = "markdown bold hex 70% grey open"      ,
  bg75  = "markdown bold hex 75% grey open"     , bg80  = "markdown bold hex 80% grey open"    , bg85  = "markdown bold hex 85% grey open"     , bg90  = "markdown bold hex 90% grey open"    , bg95  = "markdown bold hex 95% grey open"      ,
  bwht  = "markdown bold hex white open"        , binv  = "markdown bold hex invisible open"   , bfblu = "markdown bold hex full blue open"    , bfcyn = "markdown bold hex full cyan open"   , bfgrn = "markdown bold hex full green open"    ,
  bfmag = "markdown bold hex full magenta open" , bforn = "markdown bold hex full orange open" , bfred = "markdown bold hex full red open"     , bfylw = "markdown bold hex full yellow open" , blblu = "markdown bold hex light blue open"    ,
  blcyn = "markdown bold hex light cyan open"   , blgrn = "markdown bold hex light green open" , blmag = "markdown bold hex light magenta open", blorn = "markdown bold hex light orange open", blred = "markdown bold hex light red open"     ,
  blylw = "markdown bold hex light yellow open" , bmblu = "markdown bold hex medium blue open" , bmcyn = "markdown bold hex medium cyan open"  , bmgrn = "markdown bold hex medium green open", bmmag = "markdown bold hex medium magenta open",
  bmorn = "markdown bold hex medium orange open", bmred = "markdown bold hex medium red open"  , bmylw = "markdown bold hex medium yellow open", bdblu = "markdown bold hex dark blue open"   , bdcyn = "markdown bold hex dark cyan open"     ,
  bdgrn = "markdown bold hex dark green open"   , bdmag = "markdown bold hex dark magenta open", bdorm = "markdown bold hex dark orange open"  , bdred = "markdown bold hex dark red open"    , bdylw = "markdown bold hex dark yellow open"
)

#' @rdname ujPkgConstants
#' @export
.MiscVals <- base::list(nl = "\n", blank = "", yesno = "yesno", okx = "okcancel", and = "and", and1 = " and ", or = "or", or1 = " or ", int = "intercept", ref = "reference", saf = "stringsAsFactors = FALSE", pt = ggplot2::.pt, NAL = NA, NAR = NA_real_ , NAI = NA_integer_ , NAC = NA_character_ , one = 0.99999999, zero = 0.00000001)

#' @rdname ujPkgConstants
#' @export
.MiscDefs <- base::c(nl = "newline", blank = "blank string", yesno = "yes/no spec for dialog box buttons in dialogs functions", okx = "ok/cancel spec for dialog box buttons in ?dialogs functions", and = "and", and1 = "padded and", or = "or", or1 = "padded or", int = "intercept", ref = "reference", saf = "'stringsAsFactors = FALSE' argument for data.frame calls", pt = "ggplot point size multiplier", NAL = "logical NA", NAR = "real NA", NAI = "integer NA", NAC = "character NA", one = "nearly 1", zero = "nearly 0")

#' @rdname ujPkgConstants
#' @export
.UTF8vals <- base::list(back = "\U005C", back1 = " \U005C ", backs = "\U005C\U005C", backs1 = " \U005C\U005C ", bullet = "\u2022", bullet1 = " \u2022 ", by = "\u00D7", by1 = " \u00D7 ", chi2 = "*\U03C7*<sup>2</sup>", colon = "\U003A", colon1 = "\U003A ", comma = "\U002C", comma1 = "\U002C ", dagger = "\U2020", dagger2 = "\U2021", dash = "\U002D", dash1 = " \U002D ", dn = "\U2193", dn1 = " \U2193 ", em = "\U2014" , em1 = " \U2014 ", en0 = "\U2013" , en1 = " \U2013 ", eq = "\U003D", eq1 = " \U003D ", ge = "\u2265", ge1 = " \u2265 ", le = "\u2264", le1 = " \u2264 ", lft = "\u2190", lft1 = " \u2190 ", ne = "\u2260", ne1 = " \u2260 ", pipe = "\U007C", pipe1 = " \U007C ", plus = "\U002B", plus1 = " \U002B ", rgt = "\u2192", rgt1 = " \u2192 ", section = "\U00A7", semi = "\U003B", semi1 = "\U003B ", slash = "\U002F", slash1 = " \U002F ", slashes = "\U002F\U002F", slashes1 = " \U002F\U002F ", star = "\U002A", star1 = " \U002A ", tilde = "\U007E", tilde1 = " \U007E ", up = "\U2191", up1 = " \U2191 ", inf = "\U221E", pm = " \U00B1 ", pm0 = "\U00B1", le = " \U2264 ", le0 = "\U2264", ge = " \U2265 ", ge0 = "\U2265", ne = " \U2260", ne0 = "\U2260", approx = " \U2248 ", approx0 = "\U2248", dmd = " \U2B29 ", dmd0 = "\U2B29", . = "\U002E", dot = "\U002E", tick = "\U0060", nbsp = "\U0080", space = "\U0020", under = "\U005F")

#' @rdname ujPkgConstants
#' @export
.UTF8defs <- base::c(back = "backslash", back1 = "padded backslash", backs = "double backslash", backs1 = "padded double backslash", bullet = "bullet", bullet1 = "padded bullet", by = "by operator", by1 = "padded 'by' operator", chi2 = "(italic chi)squared" , colon = "colon" , colon1 = "padded colon (followed by space)", comma = "comma" , comma1 = "padded comma (followed by space)", dagger = "dagger sign", dagger2 = "double dagger sign", dash = "unpadded dash", dash1 = "padded dash", dn = "down arrow", dn1 = "padded down arrow", em = "em dash", em1 = "padded em dash", en0 = "en dash", en1 = "padded en dash", eq = "equals operator", eq1 = "padded equal sign", ge = "greater than or equal operator", ge1 = "padded greater than or equal", le = "less than or equal operator", le1 = "padded less than or equal", lft = "left arrow", lft1 = "padded left arrow", ne = "does not equal operator", ne1 = "padded does not equal", pipe = "pipe", pipe1 = "padded pipe", plus = "plus operator", plus1 = "padded plus operator", pm = "plus or minus operator", pm1 = "padded plus or minus sign", rgt = "right arrow", rgt1 = "padded right arrow", section = "section symbol", semi = "semicolon", semi1 = "padded semi-colon (followed by space)", slash = "slash" , slash1 = "padded slash", slashes = "unpadded double slash", slashes1 = "padded double slash", star = "asterisk", star1 = "padded asterisk", tilde = "tilde" , tilde1 = "padded tilde", up = "up arrow", up1 = "padded up arrow", inf = "unicode infinity", pm =  "padded plus or minus", pm0 = "unpadded plus or minus", le = "padded less than or equal", le0 = "unpadded less than or equal", ge = "padded greater than or equal", ge0 = "unpadded greater than or equal", ne = " padded not equal", ne0 = "unpadded not equal", approx = "padded approximately equal", approx0 = "unpadded approximately equal", dmd = "padded diamond", dmd0 = "unpadded diamond", . = "period", dot = "period", tick = "backtick", nbsp = "non-breaking space", space = "space" , under = "underscore")

#' @rdname ujPkgConstants
#' @export
.PkgColorVals <- base::list(
  inv  = "#FFFFFF01",
  blk  = "#000000FF", g05  = "#0D0D0DFF", g10  = "#1A1A1AFF", g15  = "#262626FF", g20  = "#333333FF", g25  = "#404040FF", g30  = "#4D4D4DFF",
  g35  = "#595959FF", g40  = "#666666FF", g45  = "#737373FF", g50  = "#808080FF", g55  = "#8C8C8CFF", g60  = "#999999FF", g65  = "#A6A6A6FF",
  g70  = "#B3B3B3FF", g75  = "#BFBFBFFF", g80  = "#CCCCCCFF", g85  = "#D9D9D9FF", g90  = "#E6E6E6FF", g95  = "#F2F2F2FF", wht  = "#FFFFFFFF",
  fblu = "#0000FFFF", fcyn = "#00FFFFFF", fgrn = "#00FF00FF", fmag = "#FF00FFFF", forn = "#FF8800FF", fred = "#AA0000FF", fylw = "#FFFF00FF",
  lblu = "#DDDDFFFF", lcyn = "#DDFFFFFF", lgrn = "#DDFFDDFF", lmag = "#FFBBFFFF", lorn = "#FFEEAAFF", lred = "#FFDDDDFF", lylw = "#FFFFDDFF",
  mblu = "#AAAAFFFF", mcyn = "#66FFFFFF", mgrn = "#FFFF66FF", mmag = "#FF66FFFF", morn = "#FFAA66FF", mred = "#FFAAAAFF", mylw = "#FFFF66FF",
  dblu = "#000080FF", dcyn = "#008B8BFF", dgrn = "#008B00FF", dmag = "#8B008BFF", dorn = "#8B4500FF", dred = "#800000FF", dylw = "#8B8B00FF",
  colors.sensitive = base::c('#7F2200FF', '#00007FFF'  , '#7F007FFF'  , '#333333FF', '#3F117FFF'    , '#FF4500FF', '#0000FFFF' , '#FF00FFFF', '#999999FF', '#7F22FFFF'),
  colors.standard  = base::c("darkblue" , "darkorange4", "darkmagenta", "darkred"  , "darkslategrey", "darkgreen", "steelblue4", "wheat4"   , "grey35"   , "orangered")
)

#' @rdname ujPkgConstants
#' @export
.PkgColorDefs <- base::c(
  inv = "invisible",
  blk = "black"       , g05  = "grey5"      , g10  = "grey10"      , g15  = "grey15"        , g20  = "grey20"       , g25  = "grey25"    , g30  = "grey30"       ,
  g35 = "grey35"      , g40  = "grey40"     , g45  = "grey45"      , g50  = "grey50"        , g55  = "grey55"       , g60  = "grey60"    , g65  = "grey65"       ,
  g70 = "grey70"      , g75  = "grey75"     , g80  = "grey80"      , g85  = "grey85"        , g90  = "grey90"       , g95  = "grey95"    , wht  = "white"        ,
  fblu = "full blue"  , fcyn = "full cyan"  , fgrn = "full green"  , fmag = "full magenta"  , forn = "full orange"  , fred = "full red"  , fylw = "full yellow"  ,
  lblu = "light blue" , lcyn = "light cyan" , lgrn = "light green" , lmag = "light magenta" , lorn = "light orange" , lred = "light red" , lylw = "light yellow" ,
  mblu = "medium blue", mcyn = "medium cyan", mgrn = "medium green", mmag = "medium magenda", morn = "medium orange", mred = "medium red", mylw = "medium yellow",
  dblu = "dark blue"  , dcyn = "dark cyan"  , dgrn = "dark green"  , dmag = "dark magenta"  , dorn = "dark orange"  , dred = "dark red"  , dylw = "dark yellow"  ,
  colors.sensitive    = "custom hex colorblind-sensitive color palette with 10 colors (5 dark followed by 5 bright of the same hue)",
  colors.standard     = "standard R named color palette (10 colors chosen for distinctiveness; not colorblind sensitive after the 3rd element)"
)

#' @rdname ujPkgConstants
#' @export
.PkgLinetypeVals <- base::list(linetypes = base::c('solid', '42', '22', '11', '4111', '2111', '1114', '1112', '1441'))

#' @rdname ujPkgConstants
#' @export
.PkgLinetypeDefs <- "Default linetypes (solid, 4on.2off, 2on.2off, 1on.1off, 4on.1off.1on.1off, 2on.1off.1on.1oof, 1on.1off.1on.4off, 1on.1off.1on.2off, 1on.4off.4on.1off"

#' @rdname ujPkgConstants
#' @export
.PkgShapeVals <- base::list(
  letter.shapes = 97:122                                    , #       a,      b,  c,    d,       e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z
  LETTER.shapes = 65:90                                     , #       A,      B,  C,    D,       E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z
  digit.shapes0 = 48:57                                     , #       0,      1,  2,    3,       4, 5, 6, 7, 8, 9
  digit.shapes  = 49:57                                     , #       1,      2,  3,    4,       5, 6, 7, 8, 9
  open.shapes   = base::c(  3,   4, 126,  94, 118,  60,  62), #       +,      x,  ~,    ^,       v, <, >
  punc.shapes   = base::c( 33,  64,  35,  36,  37,  38,  63), #       !,      @,  #,    $,       %, &, ?
  empty.shapes  = base::c(  1,   0,   2,   6,   5          ), # circle , square, up, down, diamond
  fill.shapes   = base::c( 21,  22,  24,  25,  23          ), # circle , square, up, down, diamond
  solid.shapes  = base::c( 16,  15,  17,  18               ), # circle , square, up, diamond
  blank.shape   = base::c( 32                              )  # <space>
)

#' @rdname ujPkgConstants
#' @export
.PkgShapeDefs <- base::c(
  letter.shapes = "lowercase letters drawn as plotting symbols (uses ASCII rather than character values)",
  LETTER.shapes = "uppercase letters drawn as plotting symbols (uses ASCII rather than character values)",
  digit.shapes0 = "digits from 0 to 9 drawn as plotting symbols (uses ASCII rather than character values)",
  digit.shapes  = "digits from 1 to 9 drawn as plotting symbols (uses ASCII rather than character values)",
  open.shapes   = "distinctive characters that have no enclosed areas (+, x, ~, ^, v, <, >; drawn as plotting symbols, using ASCII rather than character values)",
  punc.shapes   = "distinctive punctuation characters for use where overplotting as not problem (!, @, #, $, %, &, ?; drawn as plotting symbols, using ASCII rather than character values)",
  empty.shapes  = "standard R empty shapes ordered to maximize distinctiveness for smaller numbers of shapes needed (circle, square, up triangle, down triangle, diamond)",
  fill.shapes   = "standard R fillable shapes ordered to maximize distinctiveness for smaller numbers of shapes needed (circle, square, up triangle, down triangle, diamond)",
  solid.shapes  = "standard R soplid shapes ordered to maximize distinctiveness for smaller numbers of shapes needed (circle, square, up triangle, diamond)",
  blank.shape   = "space as a plotting symbol (uses ASCII = 31 rather than ' ')."
)

#' @rdname ujPkgConstants
#' @export
.PkgCharSetVals <- base::list(
  az      = letters,
  AZ      = LETTERS,
  aZ      = base::c(letters, LETTERS),
  digits  = base::as.character(0:9),
  files   = base::c(letters, LETTERS, 0:9, '(', ')', '_', '.', ' ', ',', '-'),
  labels  = base::c(letters, LETTERS, 0:9, '(', ')', '[', '_', ']', '{', '}', '.', ' ', ',', '-'),
  models  = base::c(letters, LETTERS, 0:9, ' ', '+', '*', '~', ':', '_', '.'),
  names   = base::c(letters, LETTERS, 0:9, '_', '.'),
  right   = base::c(letters, LETTERS, 0:9, ' ', '+' ,'*', ':', '_', '.'),
  vowels  = base::c('a', 'e', 'i', 'o', 'u', 'A', 'E', 'I', 'O', 'U'),
  files0  = base::c(letters, LETTERS, 0:9, '(', ')', '.', ' ', ',', '-'),
  models0 = base::c(letters, LETTERS, 0:9, ' ', '+', '*', '~'),
  names0  = base::c(letters, LETTERS, 0:9, '.'),
  right0  = base::c(letters, LETTERS, 0:9, ' ', '+', '*')
)

#' @rdname ujPkgConstants
#' @export
.PkgCharSetDefs <- base::c(
  az      = "lowercase letters",
  AZ      = "uppercase letters",
  aZ      = "upper and lowercase letters",
  digits  = "digits as character",
  files   = "characters valid for file names and paths",
  labels  = "characters valid for basic plot labeling",
  models  = "characters valid for specifying models/formulas",
  names   = "characters valid for R object names without backticks",
  right   = "characters valid for the right side of models/formulas",
  vowels  = "upper and lowercasre vowels",
  files0  = "valid chars for restricted file names >> c(letters, LETTERS, 0:9, ' ', '.')",
  models0 = "valid chars for restricted model specification >> c(letters, LETTERS, 0:9, ' ', '+', '*', '~')",
  names0  = "valid chars for restricted model element names >> c(letters, LETTERS, 0:9)",
  right0  = "valid chars for restricted right-side model spec >> c(letters, LETTERS, 0:9, ' ', '+', '*')"
)

#' @rdname ujPkgConstants
#' @export
.PkgPropVals <- base::list(
  bbb = .PkgPropFamilies$bbb,
  BBB = .PkgPropFamilies$BBB,
  ccc = .PkgPropFamilies$ccc,
  CCC = .PkgPropFamilies$CCC,
  ddd = .PkgPropFamilies$ddd,
  DDD = .PkgPropFamilies$DDD,
  eee = .PkgPropFamilies$eee,
  EEE = .PkgPropFamilies$EEE,
  iii = .PkgPropFamilies$iii,
  III = .PkgPropFamilies$III,
  mmm = .PkgPropFamilies$mmm,
  MMM = .PkgPropFamilies$MMM,
  sss = .PkgPropFamilies$sss,
  SSS = .PkgPropFamilies$SSS,
  ppp = base::sort(base::unique(base::tolower(.PkgAllPropVals))),
  PPP = base::sort(base::unique(base::tolower(.PkgAllPropVals))),
  XXX = .PkgAllPropVals
)

#' @rdname ujPkgConstants
#' @export
.PkgPropDefs <- base::c(
  bbb = "lowercase basic property values >> bbb_props() >> i.e., basic structural properties",
  BBB = "uppercase basic property values >> toupper(bbb_props()) >> i.e., uppercase basic structural properties",
  ccc = "lowercase xclass property values >> ccc_props() >> i.e., extended class properties",
  CCC = "uppercase xclass property values >> toupper(ccc_props()) >> i.e., uppercase extended class properties",
  ddd = "lowercase defined-D property values >> ddd_props() >> i.e., defined dimensionality properties",
  DDD = "uppercase defined-D property values >> toupper(ddd_props()) >> i.e., uppercase defined dimensionality properties",
  eee = "lowercase effective-D property values >> eee_props() >> i.e., effective dimensionality properties",
  EEE = "uppercase effective-D property values >> toupper(eee_props()) >> i.e., uppercase effective dimensionality properties",
  iii = "lowercase integrity property values >> iii_props() >> i.e., degree of completeness and uniqueness properties",
  III = "uppercase integrity property values >> toupper(iii_props()) >> i.e., uppercase completeness and uniqueness properties",
  mmm = "lowercase xmode property values >> mmm_props() >> i.e., extended mode properties",
  MMM = "uppercase xmode property values >> toupper(mmm_props()) >> i.e., uppercase extended mode properties",
  sss = "lowercase shape property values >> sss_props() >> i.e., geometric shape properties",
  SSS = "uppercase shape property values >> toupper(sss_props()) >> i.e., uppercase geometric shape properties",
  ppp = "all possible lowercase property values defined by package uj",
  PPP = "all possible uppercaes property values defined by package uj",
  XXX = "all possible upper and lowercase property values defined by package uj"
)

#' @rdname ujPkgConstants
#' @export
.PkgCrayonVals <- base::list(
  CrayonBold = .PkgCrayonSynonyms$Bold,
  CrayonPlain = .PkgCrayonSynonyms$Plain,
  CrayonItalic = .PkgCrayonSynonyms$Italic,
  CrayonDefault = .PkgCrayonSynonyms$Default,
  CrayonUnderline = .PkgCrayonSynonyms$Underline,
  CrayonRed = .PkgCrayonSynonyms$Red,
  CrayonBlue = .PkgCrayonSynonyms$Blue,
  CrayonCyan = .PkgCrayonSynonyms$Cyan,
  CrayonGreen = .PkgCrayonSynonyms$Green,
  CrayonBlack = .PkgCrayonSynonyms$Black,
  CrayonWhite = .PkgCrayonSynonyms$White,
  CrayonSilver = .PkgCrayonSynonyms$Silver,
  CrayonYellow = .PkgCrayonSynonyms$Yellow,
  CrayonMagenta = .PkgCrayonSynonyms$Magenta,
  CrayonStyles = .PkgCrayonSynonyms$Styles,
  CrayonBackgroundColors = .PkgCrayonFamilies$BackgroundColors,
  CrayonForegroundColors = .PkgCrayonFamilies$ForegroundColors
)

#' @rdname ujPkgConstants
#' @export
.PkgCrayonDefs <- base::c(
  CrayonBold = "synonymous names for bold text style (used by uj::crayons functions)",
  CrayonPlain = "synonymous names for plain text style (used by uj::crayons functions)",
  CrayonItalic = "synonymous names for italic text style (used by uj::crayons functions)",
  CrayonDefault = "synonymous names for default text style and color (used by uj::crayons functions)",
  CrayonUnderline = "synonymous names for underline text style (used by uj::crayons functions)",
  CrayonRed = "synonymous names for red (used by uj::crayons functions)",
  CrayonBlue = "synonymous names for blue (used by uj::crayons functions)",
  CrayonCyan = "synonymous names for cyan (used by uj::crayons functions)",
  CrayonGreen = "synonymous names for green (used by uj::crayons functions)",
  CrayonBlack = "synonymous names for black (used by uj::crayons functions)",
  CrayonWhite = "synonymous names for white (used by uj::crayons functions)",
  CrayonSilver = "synonymous names for silver/grey/gray (used by uj::crayons functions)",
  CrayonYellow = "synonymous names for yellow (used by uj::crayons functions)",
  CrayonMagenta = "synonymous names for magenta (used by uj::crayons functions)",
  CrayonStyles = "all possible names for text styles (used by uj::crayons functions)",
  CrayonBackgroundColors = "all possible names for background text colors (used by uj::crayons functions)",
  CrayonForegroundColors = "all possible names for foreground text colors (used by uj::crayons functions)"
)

#' @rdname ujPkgConstants
#' @export
.PkgVals <- base::c(
  .BoldGreekVals,
  .BoldGREEKvals,
  .ItalicGreekVals,
  .ItalicGREEKvals,
  .PlainGreekVals,
  .PlainGREEKvals,
  .EnclosureVals,
  .BasicMarkdownVals,
  .OpenMarkdownColorVals,
  .UTF8vals,
  .PkgColorVals,
  .PkgLinetypeVals,
  .PkgShapeVals,
  .PkgCharSetVals,
  .PkgPropVals,
  .PkgCrayonVals,
  .MiscVals
)

#' @rdname ujPkgConstants
#' @export
.PkgDefs <- base::c(
  .BoldGreekDefs,
  .BoldGREEKdefs,
  .ItalicGreekDefs,
  .ItalicGREEKdefs,
  .PlainGreekDefs,
  .PlainGREEKdefs,
  .EnclosureDefs,
  .BasicMarkdownDefs,
  .OpenMarkdownColorDefs,
  .UTF8defs,
  .PkgColorDefs,
  .PkgLinetypeDefs,
  .PkgShapeDefs,
  .PkgCharSetDefs,
  .PkgPropDefs,
  .PkgCrayonDefs,
  .MiscDefs
)

#' @encoding UTF-8
#' @family values
#' @title Named values (unique to package `uj`)
#' @description Get names, definitions, and values of special `uj` package named values.
#' @details
#' \tabular{ll}{  `value_names, val_names, vnames`   \tab Get names of all package values.                                                                             \cr   \tab   \cr
#'                `value_table, val_table, vtable`   \tab Get a data.frame of all `uj`-specific named values with three columns: `name`, `value`, and `description`,
#'                                                        where named values with multiple elements are pipe-delimited and all values are converted to mode character. \cr   \tab   \cr
#'                `values, values, vs`               \tab Get a named list where names are the arguments submitted to `v(...)`, element values are the values of
#'                                                        the contents of the named element, and the attribute `'descriptions'` gives associated verbose descriptions. \cr   \tab   \cr
#'                `value, val, v`                    \tab Gets package values by name.                                                                                                }
#' @param ... Unquoted, comma-separated list of names of values to return. If multiple values are specified, they are coerced into a single atomic vector result.
#' @return **A character vector**         \cr\cr `value_names, val_names, vnames`
#' \cr\cr  **A data.frame**               \cr\cr `value_table, val_table, vtable`
#' \cr\cr  **A \link[=VLS]{vlist}**       \cr\cr `values, values, vs`
#' \cr\cr  **An atomic scalar or vector** \cr\cr `value, val, v`
#' @examples
#' v(.)
#' v(pipe, pipe0, eq)
#' vnames()
#' values()
#' vtable()
#' @export
values <- function(Vec = FALSE) {
  Out <- uj::.PkgVals
  base::attr(Out, "Defs") <- uj::.PkgDefs
  uj::f0(Vec, uj::av(base::sapply(Out, base::paste0, collapse = "|")), Out)
}

#' @rdname values
#' @export
value_names <- function() {base::names(uj::values())}

#' @rdname values
#' @export
value_descriptions <- function() {base::attr(uj::values(), "Descriptions")}

#' @rdname values
#' @export
value_table <- function() {tibble::tibble(Name = uj::value_names(), Value = uj::values(Vec = TRUE), Description = uj::value_descriptions())}

#' @rdname values
#' @export
value <- function(...) {
  X <- base::as.character(base::match.call())
  X <- X[2:base::length(X)]
  uj::av(uj::values()[X])
}

#' @rdname values
#' @export
vals <- values

#' @rdname values
#' @export
val_names <- value_names

#' @rdname values
#' @export
val_table <- value_table

#' @rdname values
#' @export
val <- value

#' @rdname values
#' @export
vs <- values

#' @rdname values
#' @export
vnames <- value_names

#' @rdname values
#' @export
vtable <- value_table

#' @rdname values
#' @export
v <- value
