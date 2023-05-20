#' @encoding UTF-8
#' @title Package `uj` atomic scalar and vector constants
#' @description Lists of named scalars and/or vectors include the following lists and named character vectors defining elements of the associated lists:
#' \tabular{ll}{
#'                                    \tab **Name of Associated**
#'   \cr **Name of List**             \tab **Definitions Vector**
#'   \cr `.PkgVals`                   \tab `.PkgDefs`
#'   \cr `.MiscVals`                  \tab `.MiscDefs`
#'   \cr `.UTF8vals`                  \tab `.UTF8defs`
#'   \cr `.PropVals`                  \tab `.PropDefs`
#'   \cr `.ShapeVals`                 \tab `.ShapeDefs`
#'   \cr `.CrayonVals`                \tab `.CrayonDefs`
#'   \cr `.AllPropVals`               \tab `.AllPropDefs`
#'   \cr `.CharSetVals`               \tab `.CharsetDefs`
#'   \cr `.LineTypeVals`              \tab `.LineTypeDefs`
#'   \cr `.HexColorVals`              \tab `.HexColorDefs`
#'   \cr `.EnclosureVals`             \tab `.EnclosureDefs`
#'   \cr `.BoldGreekVals`             \tab `.BoldGreekDefs`
#'   \cr `.BoldGREEKvals`             \tab `.BoldGREEKdefs`
#'   \cr `.PlainGreekVals`            \tab `.PlainGreekDefs`
#'   \cr `.PlainGREEKvals`            \tab `.PlainGREEKdefs`
#'   \cr `.PropFamilyVals`            \tab `.PropFamilyDefs`
#'   \cr `.ItalicGreekVals`           \tab `.ItalicGreekDefs`
#'   \cr `.ItalicGREEKvals`           \tab `.ItalicGREEKdefs`
#'   \cr `.CrayonFamilyVals`          \tab `.CrayonFamilyDefs`
#'   \cr `.CrayonSynonymVals`         \tab `.CrayonSynonymDefs`
#'   \cr `.BasicMarkdownVals`         \tab `.BasicMarkdownDefs`
#'   \cr `.OpenMarkdownColorVals`     \tab `.OpenMarkdownColorDefs`
#' }
#' @return Either a named list or a named character vector.
#' @export
ujconstants <- function() {utils::help("ujconstants", package = "uj")}

#' @rdname ujconstants
#' @export
.PropFamilyVals <- base::list(
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

#' @rdname ujconstants
#' @export
.PropFamilyDefs <- base::c(
  bbb = "lowercase basic structural properties"        , BBB = "uppercase basic structural properties"        ,
  ccc = "lowercase extended class properties"          , CCC = "uppercase extended class properties"          ,
  ddd = "lowercase defined dimensionality properties"  , DDD = "uppercase defined dimensionality properties"  ,
  eee = "lowercase effective dimensionality properties", EEE = "uppercase effective dimensionality properties",
  iii = "lowercase integrity properties"               , III = "uppercase integrity properties"               ,
  mmm = "lowercase extended mode properties"           , MMM = "uppercase extended mode properties"           ,
  sss = "lowercase shape properties"                   , SSS = "uppercase shape properties"
)

#' @rdname ujconstants
#' @export
.AllPropVals <- base::list(AllPropVals = base::sort(base::unique(base::c(base::unlist(.PropFamilyVals, T, F), base::toupper(base::unlist(.PropFamilyVals, T, F))))))

#' @rdname ujconstants
#' @export
.AllPropDefs <- "All uppercase and lowercase properties defined by package uj"

#' @rdname ujconstants
#' @export
.CrayonSynonymVals <- base::list(
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

#' @rdname ujconstants
#' @export
.CrayonSynonymDefs <- base::c(Bold = "synonyms for 'bold'", Plain = "synonyms for 'plain'", Italic = "synonyms for 'italic'", Default = "synonyms for 'default'", Underline = "synonyms for 'underline'",
                              Red = "synonyms for 'red'", Blue = "synonyms for 'blue'", Cyan = "synonyms for 'cyan'", Green = "synonyms for 'green'", Black = "synonyms for 'black'",
                              White = "synonyms for 'white'", Yellow = "synonyms for 'yellow'", Silver = "synonyms for 'silver'", Magenta = "synonyms for 'magenta'")

#' @rdname ujconstants
#' @export
.CrayonFamilyVals <- base::list(
  CrayonStyles = base::sort(base::unlist(base::c(.CrayonSynonymVals$Bold, .CrayonSynonymVals$Default, .CrayonSynonymVals$Italic, .CrayonSynonymVals$Plain, .CrayonSynonymVals$Underline), T, F)),
  CrayonBackgroundColors = base::sort(base::unlist(base::c(.CrayonSynonymVals$Blue, .CrayonSynonymVals$Cyan, .CrayonSynonymVals$Default, .CrayonSynonymVals$Green, .CrayonSynonymVals$Black, .CrayonSynonymVals$Magenta, .CrayonSynonymVals$Red, .CrayonSynonymVals$White, .CrayonSynonymVals$Yellow, .CrayonSynonymVals$Silver), T, F)),
  CrayonForegroundColors = base::sort(base::unlist(base::c(.CrayonSynonymVals$Blue, .CrayonSynonymVals$Cyan, .CrayonSynonymVals$Default, .CrayonSynonymVals$Green, .CrayonSynonymVals$Black, .CrayonSynonymVals$Magenta, .CrayonSynonymVals$Red, .CrayonSynonymVals$White, .CrayonSynonymVals$Yellow), T, F))
)

#' @rdname ujconstants
#' @export
.CrayonFamilyDefs <- base::c(Styles = "all possible text style specifications", BackgroundColors = "all possible background color specifications", ForegroundColors = "all possible foreground color specifications")

#' @rdname ujconstants
#' @export
.BoldGreekVals <- base::list(bAlpha = "**\U03B1**", bBeta = "**\U03B2**", bGamma = "**\U03B3**", bDelta = "**\U03B4**", bEpsilon = "**\U03B5**", bZeta = "**\U03B5**", bEta = "**\U03B7**", bTheta = "**\U03B8**", bIota = "**\U03B9**", bKappa = "**\U03BA**", bLambda = "**\U03BB**", bMu = "**\U03BC**", bNu = "**\U03BD**", bXi = "**\U03BE**", bOmicron = "**\U03BF**", bPi = "**\U03C0**", bRho = "**\U03C1**", bSigma = "**\U03C3**", bTau = "**\U03C4**", bUpsilon = "**\U03C5**", bPhi = "**\U03C6**", bChi = "**\U03C7**", bPsi = "**\U03C8**", bOmega = "**\U03C9**")

#' @rdname ujconstants
#' @export
.BoldGreekDefs <- base::c(alpha = "bold alpha"  , beta = "bold beta"  , gamma = "bold gamma"  , delta = "bold delta"  , epsilon = "bold epsilon"  , zeta = "bold zeta"  , eta = "bold eta"  , theta = "bold theta"  , iota = "bold iota"  , kappa = "bold kappa"  , lambda = "bold lambda"  , mu = "bold mu"  , nu = "bold nu"  , xi = "bold xi"  , omicron = "bold omicron"  , pi = "bold pi"  , rho = "bold rho"  , sigma = "bold sigma"  , tau = "bold tau"  , upsilon = "bold upsilon"  , phi = "bold phi"  , chi = "bold chi"  , psi = "bold psi"  , omega = "bold omega"   )

#' @rdname ujconstants
#' @export
.BoldGREEKvals <- base::list(bALPHA = "**\U0391**", bBETA = "**\U0392**", bgAMMA = "**\U0393**", bDELTA = "**\U0394**", bEPSILON = "**\U0395**", bZETA = "**\U0396**", bETA = "**\U0397**", bTHETA = "**\U0398**", bIOTA = "**\U0399**", bKAPPA = "**\U039A**", bLAMBDA = "**\U039B**", bMU = "**\U039C**", bNU = "**\U039D**", bXI = "**\U039E**", bOMICRON = "**\U039F**", bPI = "**\U03A0**", bRHO = "**\U03A1**", bSIGMA = "**\U03A3**", bTAU = "**\U03A4**", bUPSILON = "**\U03A5**", bPHI = "**\U03A6**", bCHI = "**\U03A7**", bPSI = "**\U03A8**", bOMEGA = "**\U03A9**")

#' @rdname ujconstants
#' @export
.BoldGREEKdefs <- base::c(ALPHA = "bold ALPHA"  , BETA = "bold BETA"  , GAMMA = "bold GAMMA"  , DELTA = "bold DELTA"  , EPSILON = "bold EPSILON"  , ZETA = "bold ZETA"  , ETA = "bold ETA"  , THETA = "bold THETA"  , IOTA = "bold IOTA"  , KAPPA = "bold KAPPA"  , LAMBDA = "bold LAMBDA"  , MU = "bold MU"  , NU = "bold NU"  , XI = "bold XI"  , OMICRON = "bold OMICRON"  , PI = "bold PI"  , RHO = "bold RHO"  , SIGMA = "bold SIGMA"  , TAU = "bold TAU"  , UPSILON = "bold UPSILON"  , PHI = "bold PHI"  , CHI = "bold CHI"  , PSI = "bold PSI"  , OMEGA = "bold OMEGA"   )

#' @rdname ujconstants
#' @export
.ItalicGreekVals <- base::list(iAlpha =  "*\U03B1*" , iBeta =  "*\U03B2*" , iGamma =  "*\U03B3*" , iDelta =  "*\U03B4*" , iEpsilon =  "*\U03B5*" , iZeta =  "*\U03B5*" , iEta =  "*\U03B7*" , iTheta =  "*\U03B8*" , iIota =  "*\U03B9*" , iKappa =  "*\U03BA*" , iLambda =  "*\U03BB*" , iMu =  "*\U03BC*" , iNu =  "*\U03BD*" , iXi =  "*\U03BE*" , iOmicron =  "*\U03BF*" , iPi =  "*\U03C0*" , iRho =  "*\U03C1*" , iSigma =  "*\U03C3*" , iTau =  "*\U03C4*" , iUpsilon =  "*\U03C5*" , iPhi =  "*\U03C6*" , iChi =  "*\U03C7*" , iPsi =  "*\U03C8*" , iOmega =  "*\U03C9*" )

#' @rdname ujconstants
#' @export
.ItalicGreekDefs <- base::c(alpha = "italic alpha", beta = "italic beta", gamma = "italic gamma", delta = "italic delta", epsilon = "italic epsilon", zeta = "italic zeta", eta = "italic eta", theta = "italic theta", iota = "italic iota", kappa = "italic kappa", lambda = "italic lambda", mu = "italic mu", nu = "italic nu", xi = "italic xi", omicron = "italic omicron", pi = "italic pi", rho = "italic rho", sigma = "italic sigma", tau = "italic tau", upsilon = "italic upsilon", phi = "italic phi", chi = "italic chi", psi = "italic psi", omega = "italic omega" )

#' @rdname ujconstants
#' @export
.ItalicGREEKvals <- base::list(iALPHA =  "*\U0391*" , iBETA =  "*\U0392*" , iGAMMA =  "*\U0393*" , iDELTA =  "*\U0394*" , iEPSILON =  "*\U0395*" , iZETA =  "*\U0396*" , iETA =  "*\U0397*" , iTHETA =  "*\U0398*" , iIOTA =  "*\U0399*" , iKAPPA =  "*\U039A*" , iLAMBDA =  "*\U039B*" , iMU =  "*\U039C*" , iNU =  "*\U039D*",  iXI =  "*\U039E*" , iOMICRON =  "*\U039F*" , iPI =  "*\U03A0*" , iRHO =  "*\U03A1*" , iSIGMA =  "*\U03A3*" , iTAU =  "*\U03A4*" , iUPSILON =  "*\U03A5*" , iPHI =  "*\U03A6*" , iCHI =  "*\U03A7*" , iPSI =  "*\U03A8*" , iOMEGA =  "*\U03A9*" )

#' @rdname ujconstants
#' @export
.ItalicGREEKdefs <- base::c(ALPHA = "italic ALPHA", BETA = "italic BETA", GAMMA = "italic GAMMA", DELTA = "italic DELTA", EPSILON = "italic EPSILON", ZETA = "italic ZETA", ETA = "italic ETA", THETA = "italic THETA", IOTA = "italic IOTA", KAPPA = "italic KAPPA", LAMBDA = "italic LAMBDA", MU = "italic MU", NU = "italic NU", XI = "italic XI", OMICRON = "italic OMICRON", PI = "italic PI", RHO = "italic RHO", SIGMA = "italic SIGMA", TAU = "italic TAU", UPSILON = "italic UPSILON", PHI = "italic PHI", CHI = "italic CHI", PSI = "italic PSI", OMEGA = "italic OMEGA" )

#' @rdname ujconstants
#' @export
.PlainGreekVals <- base::list( Alpha =   "\U03B1"  ,  Beta =   "\U03B2"  ,  Gamma =   "\U03B3"  ,  Delta =   "\U03B4"  ,  Epsilon =   "\U03B5"  ,  Zeta =   "\U03B5"  ,  Eta =   "\U03B7"  ,  Theta =   "\U03B8"  ,  Iota =   "\U03B9"  ,  Kappa =   "\U03BA"  ,  Lambda =   "\U03BB"  ,  Mu =   "\U03BC"  ,  Nu =   "\U03BD"  ,  Xi =   "\U03BE"  ,  Omicron =   "\U03BF"  ,  Pi =   "\U03C0"  ,  Rho =   "\U03C1"  ,  Sigma =   "\U03C3"  ,  Tau =   "\U03C4"  ,  Upsilon =   "\U03C5"  ,  Phi =   "\U03C6"  ,  Chi =   "\U03C7"  ,  Psi =   "\U03C8"  ,  Omega =   "\U03C9"  )

#' @rdname ujconstants
#' @export
.PlainGreekDefs <- base::c(alpha = "alpha", beta = "beta", gamma = "gamma", delta = "delta", epsilon = "epsilon", zeta = "zeta", eta = "eta", theta = "theta", iota = "iota", kappa = "kappa", lambda = "lambda", mu = "mu", nu = "nu", xi = "xi", omicron = "omicron"       , pi = "pi"       , rho = "rho"       , sigma = "sigma"       , tau = "tau"       , upsilon = "upsilon"       , phi = "phi"       , chi = "chi"       , psi = "psi"       , omega = "omega"        )

#' @rdname ujconstants
#' @export
.PlainGREEKvals <- base::list(ALPHA = "\U0391",  BETA = "\U0392",  GAMMA =   "\U0393",  DELTA =   "\U0394"  ,  EPSILON =   "\U0395"  ,  ZETA =   "\U0396"  ,  ETA =  " \U0397"  ,  THETA =   "\U0398"  ,  IOTA =   "\U0399"  ,  KAPPA =   "\U039A"  ,  LAMBDA =   "\U039B"  ,  MU =   "\U039C"  ,  NU =   "\U039D"  ,  XI =   "\U039E"  ,  OMICRON =   "\U039F"  ,  PI =   "\U03A0"  ,  RHO =   "\U03A1"  ,  SIGMA =   "\U03A3"  ,  TAU =   "\U03A4"  ,  UPSILON =   "\U03A5"  ,  PHI =   "\U03A6"  ,  CHI =   "\U03A7"  ,  PSI =   "\U03A8"  ,  OMEGA =   "\U03A9"  )

#' @rdname ujconstants
#' @export
.PlainGREEKdefs <- base::c(ALPHA = "ALPHA", BETA = "BETA", GAMMA = "GAMMA", DELTA = "DELTA", EPSILON = "EPSILON", ZETA = "ZETA", ETA = "ETA", THETA = "THETA", IOTA = "IOTA", KAPPA = "KAPPA", LAMBDA = "LAMBDA", MU = "MU", NU = "NU", XI = "XI", OMICRON = "OMICRON", PI = "PI", RHO = "RHO", SIGMA = "SIGMA", TAU = "TAU", UPSILON = "UPSILON", PHI = "PHI", CHI = "CHI", PSI = "PSI", OMEGA = "OMEGA")

#' @rdname ujconstants
#' @export
.EnclosureVals <- base::list(tick = "\U0060", open = "\U003C", close = "\U003E", quote = "\U0027", quote2 = "\U0022", lbrace = "\U007B", rbrace = "\U007D", lparen = "\U0028", rparen = "\U0029", lquote = "\U2018", rquote = "\U2019", lquote2 = "\U201C", rquote2 = "\U201D", lbracket = "\U005B", rbracket = "\U005D", langle = "\U27E8", rangle = "\U27E9", lparen2 = "\U2E28", rparen2 = "\U2E29", langle2 = "\U300A", rangle2 = "\U300B")

#' @rdname ujconstants
#' @export
.EnclosureDefs <- base::c(tick = "backtick", open = "markdown tag open", close = "markdown tag close", quote = "single straight quote", quote2 = "double straight quote", lbrace = "left curly brace", rbrace = "right curly brace", lparen = "left paren", rparen = "right paren", lquote = "left single quote", rquote = "non-escaped right single quote", lquote2 = "non-escaped left double quote", rquote2 = "non-escaped right double quote", lbracket = "left square bracket", rbracket = "right square bracket", langle = "left angle bracket", rangle = "right angle bracket", lparen2 = "double left paren", rparen2 = "double right paren", langle2 = "double left angle bracket", rangle2 = "double right angle bracket")

#' @rdname ujconstants
#' @export
.BasicMarkdownVals <- base::list(brk = "<br />", sub1  = "<sub>", sub2 = "</sub>", sup1 = "<sup>", sup2 = "</sup>", close = ">", pclose = "</span>", bclose = "</strong>", iclose = "</em>")

#' @rdname ujconstants
#' @export
.BasicMarkdownDefs <- base::c(brk = "markdown line break", sub1 = "markdown subscript open", sub2 = "markdown subscript close", sup1 = "markdown superscript open", sup2 = "markdown superscript close", close = "markdown generic close", pclose = "markdown span/plain close", bclose = "markdown bold span close", iclose = "markdown italic span close")

#' @rdname ujconstants
#' @export
.HexColorVals <- base::list(
  inv  = "#FFFFFF01",
  blk  = "#000000FF", g05  = "#0D0D0DFF", g10  = "#1A1A1AFF", g15  = "#262626FF", g20  = "#333333FF", g25  = "#404040FF", g30  = "#4D4D4DFF",
  g35  = "#595959FF", g40  = "#666666FF", g45  = "#737373FF", g50  = "#808080FF", g55  = "#8C8C8CFF", g60  = "#999999FF", g65  = "#A6A6A6FF",
  g70  = "#B3B3B3FF", g75  = "#BFBFBFFF", g80  = "#CCCCCCFF", g85  = "#D9D9D9FF", g90  = "#E6E6E6FF", g95  = "#F2F2F2FF", wht  = "#FFFFFFFF",
  lblu = "#DDDDFFFF", lcyn = "#D7FFFFFF", lgrn = "#D7FFD7FF", lmag = "#FFDDFFFF", lorn = "#FFEECCFF", lred = "#FFD7D7FF", lylw = "#FFFFDDFF",
  mblu = "#AAAAFFFF", mcyn = "#AAFFFFFF", mgrn = "#AAFFAAFF", mmag = "#FFAAFFFF", morn = "#FFAA66FF", mred = "#FFAAAAFF", mylw = "#F7F7AAFF",
  fblu = "#4444FFFF", fcyn = "#00BBBBFF", fgrn = "#00FF00FF", fmag = "#FF00FFFF", forn = "#FF7700FF", fred = "#FF0000FF", fylw = "#FFFF00FF",
  dblu = "#000080FF", dcyn = "#006666FF", dgrn = "#006600FF", dmag = "#880088FF", dorn = "#773300FF", dred = "#770000FF", dylw = "#777700FF",
  colors.dark      = base::c("#000099FF", "#773300FF", "#880088FF", "#006666FF", "#880011FF", "#116600FF"),
  colors.sensitive = base::c("#000099FF", "#773300FF", "#880088FF", "#006666FF", "#880011FF", "#116600FF" ,
                             "#4444FFFF", "#FF7700FF", "#FF00FFFF", "#00AAAAFF", "#FF2244FF", "#22CC00FF"),
  colors.bright    = base::c("#4444FFFF", "#FF7700FF", "#FF00FFFF", "#00AAAAFF", "#FF2244FF", "#22CC00FF")
)

#' @rdname ujconstants
#' @export
.HexColorDefs <- base::c(
  inv = "invisible",
  blk = "black"       , g05  = "grey5"      , g10  = "grey10"      , g15  = "grey15"        , g20  = "grey20"       , g25  = "grey25"    , g30  = "grey30"       ,
  g35 = "grey35"      , g40  = "grey40"     , g45  = "grey45"      , g50  = "grey50"        , g55  = "grey55"       , g60  = "grey60"    , g65  = "grey65"       ,
  g70 = "grey70"      , g75  = "grey75"     , g80  = "grey80"      , g85  = "grey85"        , g90  = "grey90"       , g95  = "grey95"    , wht  = "white"        ,
  lblu = "light blue" , lcyn = "light cyan" , lgrn = "light green" , lmag = "light magenta" , lorn = "light orange" , lred = "light red" , lylw = "light yellow" ,
  mblu = "medium blue", mcyn = "medium cyan", mgrn = "medium green", mmag = "medium magenda", morn = "medium orange", mred = "medium red", mylw = "medium yellow",
  fblu = "full blue"  , fcyn = "full cyan"  , fgrn = "full green"  , fmag = "full magenta"  , forn = "full orange"  , fred = "full red"  , fylw = "full yellow"  ,
  dblu = "dark blue"  , dcyn = "dark cyan"  , dgrn = "dark green"  , dmag = "dark magenta"  , dorn = "dark orange"  , dred = "dark red"  , dylw = "dark yellow"  ,
  colors = "custom hex colorblind-sensitive color palette with 10 colors (5 dark followed by 5 bright of the same hue)"
)

#' @rdname ujconstants
#' @export
.OpenMarkdownColorVals <- base::list(
  bblk  = base::paste0("<strong style='color:", .HexColorVals$blk, ";'>" ), bg05  = base::paste0("<strong style='color:", .HexColorVals$g05, ";'>" ), bg10  = base::paste0("<strong style='color:", .HexColorVals$g10, ";'>" ), bg15  = base::paste0("<strong style='color:", .HexColorVals$g15, ";'>" ), bg20  = base::paste0("<strong style='color:", .HexColorVals$g20, ";'>" ), bg25  = base::paste0("<strong style='color:", .HexColorVals$g25, ";'>" ), bg30  = base::paste0("<strong style='color:", .HexColorVals$g30, ";'>" ), bg35  = base::paste0("<strong style='color:", .HexColorVals$g35, ";'>"), bg40  = base::paste0("<strong style='color:", .HexColorVals$g40, ";'>"), bg45  = base::paste0("<strong style='color:", .HexColorVals$g45, ";'>"), bg50  = base::paste0("<strong style='color:", .HexColorVals$g50, ";'>"), bg55  = base::paste0("<strong style='color:", .HexColorVals$g55, ";'>"), bg60  = base::paste0("<strong style='color:", .HexColorVals$g60, ";'>"), bg65  = base::paste0("<strong style='color:", .HexColorVals$g65, ";'>"), bg70  = base::paste0("<strong style='color:", .HexColorVals$g70, ";'>"), bg75  = base::paste0("<strong style='color:", .HexColorVals$g75, ";'>"), bg80  = base::paste0("<strong style='color:", .HexColorVals$g80, ";'>"), bg85  = base::paste0("<strong style='color:", .HexColorVals$g85, ";'>"), bg90  = base::paste0("<strong style='color:", .HexColorVals$g90, ";'>"), bg95  = base::paste0("<strong style='color:", .HexColorVals$g95, ";'>"), bwht  = base::paste0("<strong style='color:", .HexColorVals$wht, ";'>"), binv  = base::paste0("<strong style='color:", .HexColorVals$inv, ";'>"),
  pblk  = base::paste0("<span style='color:"  , .HexColorVals$blk, ";'>" ), pg05  = base::paste0("<span style='color:"  , .HexColorVals$g05, ";'>" ), pg10  = base::paste0("<span style='color:"  , .HexColorVals$g10, ";'>" ), pg15  = base::paste0("<span style='color:"  , .HexColorVals$g15, ";'>" ), pg20  = base::paste0("<span style='color:"  , .HexColorVals$g20, ";'>" ), pg25  = base::paste0("<span style='color:"  , .HexColorVals$g25, ";'>" ), pg30  = base::paste0("<span style='color:"  , .HexColorVals$g30, ";'>" ), pg35  = base::paste0("<span style='color:"  , .HexColorVals$g35, ";'>"), pg40  = base::paste0("<span style='color:"  , .HexColorVals$g40, ";'>"), pg45  = base::paste0("<span style='color:"  , .HexColorVals$g45, ";'>"), pg50  = base::paste0("<span style='color:"  , .HexColorVals$g50, ";'>"), pg55  = base::paste0("<span style='color:"  , .HexColorVals$g55, ";'>"), pg60  = base::paste0("<span style='color:"  , .HexColorVals$g60, ";'>"), pg65  = base::paste0("<span style='color:"  , .HexColorVals$g65, ";'>"), pg70  = base::paste0("<span style='color:"  , .HexColorVals$g70, ";'>"), pg75  = base::paste0("<span style='color:"  , .HexColorVals$g75, ";'>"), pg80  = base::paste0("<span style='color:"  , .HexColorVals$g80, ";'>"), pg85  = base::paste0("<span style='color:"  , .HexColorVals$g85, ";'>"), pg90  = base::paste0("<span style='color:"  , .HexColorVals$g90, ";'>"), pg95  = base::paste0("<span style='color:"  , .HexColorVals$g95, ";'>"), pwht  = base::paste0("<span style='color:"  , .HexColorVals$wht, ";'>"), pinv  = base::paste0("<span style='color:",   .HexColorVals$inv, ";'>"),
  iblk  = base::paste0("<em style='color:"    , .HexColorVals$blk, ";'>" ), ig05  = base::paste0("<em style='color:"    , .HexColorVals$g05, ";'>" ), ig10  = base::paste0("<em style='color:"    , .HexColorVals$g10, ";'>" ), ig15  = base::paste0("<em style='color:"    , .HexColorVals$g15, ";'>" ), ig20  = base::paste0("<em style='color:"    , .HexColorVals$g20, ";'>" ), ig25  = base::paste0("<em style='color:"    , .HexColorVals$g25, ";'>" ), ig30  = base::paste0("<em style='color:"    , .HexColorVals$g39, ";'>" ), ig35  = base::paste0("<em style='color:"    , .HexColorVals$g35, ";'>"), ig40  = base::paste0("<em style='color:"    , .HexColorVals$g40, ";'>"), ig45  = base::paste0("<em style='color:"    , .HexColorVals$g45, ";'>"), ig50  = base::paste0("<em style='color:"    , .HexColorVals$g50, ";'>"), ig55  = base::paste0("<em style='color:"    , .HexColorVals$g55, ";'>"), ig60  = base::paste0("<em style='color:"  ,   .HexColorVals$g60, ";'>"), ig65  = base::paste0("<em style='color:"    , .HexColorVals$g65, ";'>"), ig70  = base::paste0("<em style='color:"    , .HexColorVals$g70, ";'>"), ig75  = base::paste0("<em style='color:"    , .HexColorVals$g75, ";'>"), ig80  = base::paste0("<em style='color:"    , .HexColorVals$g80, ";'>"), ig85  = base::paste0("<em style='color:"    , .HexColorVals$g85, ";'>"), ig90  = base::paste0("<em style='color:"    , .HexColorVals$g90, ";'>"), ig95  = base::paste0("<em style='color:"    , .HexColorVals$g95, ";'>"), iwht  = base::paste0("<em style='color:"    , .HexColorVals$wht, ";'>"), iinv  = base::paste0("<em style='color:"  ,   .HexColorVals$inv, ";'>"),
  bfblu = base::paste0("<strong style='color:", .HexColorVals$fblu, ";'>"), bfcyn = base::paste0("<strong style='color:", .HexColorVals$fcyn, ";'>"), bfgrn = base::paste0("<strong style='color:", .HexColorVals$fgrn, ";'>"), bfmag = base::paste0("<strong style='color:", .HexColorVals$fmag, ";'>"), bforn = base::paste0("<strong style='color:", .HexColorVals$forn, ";'>"), bfred = base::paste0("<strong style='color:", .HexColorVals$fred, ";'>"), bfylw = base::paste0("<strong style='color:", .HexColorVals$fylw, ";'>"),
  blblu = base::paste0("<strong style='color:", .HexColorVals$lblu, ";'>"), blcyn = base::paste0("<strong style='color:", .HexColorVals$lcyn, ";'>"), blgrn = base::paste0("<strong style='color:", .HexColorVals$lgrn, ";'>"), blmag = base::paste0("<strong style='color:", .HexColorVals$lmag, ";'>"), blorn = base::paste0("<strong style='color:", .HexColorVals$lorn, ";'>"), blred = base::paste0("<strong style='color:", .HexColorVals$lred, ";'>"), blylw = base::paste0("<strong style='color:", .HexColorVals$lylw, ";'>"),
  bmblu = base::paste0("<strong style='color:", .HexColorVals$mblu, ";'>"), bmcyn = base::paste0("<strong style='color:", .HexColorVals$mcyn, ";'>"), bmgrn = base::paste0("<strong style='color:", .HexColorVals$mgrn, ";'>"), bmmag = base::paste0("<strong style='color:", .HexColorVals$mmag, ";'>"), bmorn = base::paste0("<strong style='color:", .HexColorVals$morn, ";'>"), bmred = base::paste0("<strong style='color:", .HexColorVals$mred, ";'>"), bmylw = base::paste0("<strong style='color:", .HexColorVals$mylw, ";'>"),
  bdblu = base::paste0("<strong style='color:", .HexColorVals$dblu, ";'>"), bdcyn = base::paste0("<strong style='color:", .HexColorVals$dcyn, ";'>"), bdgrn = base::paste0("<strong style='color:", .HexColorVals$dgrn, ";'>"), bdmag = base::paste0("<strong style='color:", .HexColorVals$dmag, ";'>"), bdorn = base::paste0("<strong style='color:", .HexColorVals$dorn, ";'>"), bdred = base::paste0("<strong style='color:", .HexColorVals$dred, ";'>"), bdylw = base::paste0("<strong style='color:", .HexColorVals$dylw, ";'>"),
  pfblu = base::paste0("<span style='color:", .HexColorVals$fblu, ";'>"), pfcyn = base::paste0("<span style='color:", .HexColorVals$fcyn, ";'>"), pfgrn = base::paste0("<span style='color:", .HexColorVals$fgrn, ";'>"), pfmag = base::paste0("<span style='color:", .HexColorVals$fmag, ";'>"), pforn = base::paste0("<span style='color:", .HexColorVals$forn, ";'>"), pfred = base::paste0("<span style='color:", .HexColorVals$fred, ";'>"), pfylw = base::paste0("<span style='color:", .HexColorVals$fylw, ";'>"),
  plblu = base::paste0("<span style='color:", .HexColorVals$lblu, ";'>"), plcyn = base::paste0("<span style='color:", .HexColorVals$lcyn, ";'>"), plgrn = base::paste0("<span style='color:", .HexColorVals$lgrn, ";'>"), plmag = base::paste0("<span style='color:", .HexColorVals$lmag, ";'>"), plorn = base::paste0("<span style='color:", .HexColorVals$lorn, ";'>"), plred = base::paste0("<span style='color:", .HexColorVals$lred, ";'>"), plylw = base::paste0("<span style='color:", .HexColorVals$lylw, ";'>"),
  pmblu = base::paste0("<span style='color:", .HexColorVals$mblu, ";'>"), pmcyn = base::paste0("<span style='color:", .HexColorVals$mcyn, ";'>"), pmgrn = base::paste0("<span style='color:", .HexColorVals$mgrn, ";'>"), pmmag = base::paste0("<span style='color:", .HexColorVals$mmag, ";'>"), pmorn = base::paste0("<span style='color:", .HexColorVals$morn, ";'>"), pmred = base::paste0("<span style='color:", .HexColorVals$mred, ";'>"), pmylw = base::paste0("<span style='color:", .HexColorVals$mylw, ";'>"),
  pdblu = base::paste0("<span style='color:", .HexColorVals$dblu, ";'>"), pdcyn = base::paste0("<span style='color:", .HexColorVals$dcyn, ";'>"), pdgrn = base::paste0("<span style='color:", .HexColorVals$dgrn, ";'>"), pdmag = base::paste0("<span style='color:", .HexColorVals$dmag, ";'>"), pdorn = base::paste0("<span style='color:", .HexColorVals$dorn, ";'>"), pdred = base::paste0("<span style='color:", .HexColorVals$dred, ";'>"), pdylw = base::paste0("<span style='color:", .HexColorVals$dylw, ";'>"),
  ifblu = base::paste0("<em style='color:", .HexColorVals$fblu, ";'>"), ifcyn = base::paste0("<em style='color:", .HexColorVals$fcyn, ";'>"), ifgrn = base::paste0("<em style='color:", .HexColorVals$fgrn, ";'>"), ifmag = base::paste0("<em style='color:", .HexColorVals$fmag, ";'>"), iforn = base::paste0("<em style='color:", .HexColorVals$forn, ";'>"), ifred = base::paste0("<em style='color:", .HexColorVals$fred, ";'>"), ifylw = base::paste0("<em style='color:", .HexColorVals$fylw, ";'>"),
  ilblu = base::paste0("<em style='color:", .HexColorVals$lblu, ";'>"), ilcyn = base::paste0("<em style='color:", .HexColorVals$lcyn, ";'>"), ilgrn = base::paste0("<em style='color:", .HexColorVals$lgrn, ";'>"), ilmag = base::paste0("<em style='color:", .HexColorVals$lmag, ";'>"), ilorn = base::paste0("<em style='color:", .HexColorVals$lorn, ";'>"), ilred = base::paste0("<em style='color:", .HexColorVals$lred, ";'>"), ilylw = base::paste0("<em style='color:", .HexColorVals$lylw, ";'>"),
  imblu = base::paste0("<em style='color:", .HexColorVals$mblu, ";'>"), imcyn = base::paste0("<em style='color:", .HexColorVals$mcyn, ";'>"), imgrn = base::paste0("<em style='color:", .HexColorVals$mgrn, ";'>"), immag = base::paste0("<em style='color:", .HexColorVals$mmag, ";'>"), imorn = base::paste0("<em style='color:", .HexColorVals$morn, ";'>"), imred = base::paste0("<em style='color:", .HexColorVals$mred, ";'>"), imylw = base::paste0("<em style='color:", .HexColorVals$mylw, ";'>"),
  idblu = base::paste0("<em style='color:", .HexColorVals$dblu, ";'>"), idcyn = base::paste0("<em style='color:", .HexColorVals$dcyn, ";'>"), idgrn = base::paste0("<em style='color:", .HexColorVals$dgrn, ";'>"), idmag = base::paste0("<em style='color:", .HexColorVals$dmag, ";'>"), idorn = base::paste0("<em style='color:", .HexColorVals$dorn, ";'>"), idred = base::paste0("<em style='color:", .HexColorVals$dred, ";'>"), idylw = base::paste0("<em style='color:", .HexColorVals$dylw, ";'>")
)

#' @rdname ujconstants
#' @export
.OpenMarkdownColorDefs <- base::c(
  bblk  = "markdown bold hex black open"  , bg05  = "markdown bold hex 05% grey open"  , bg10  = "markdown bold hex 10% grey open"  , bg15  = "markdown bold hex 15% grey open"  , bg20  = "markdown bold hex 20% grey open"  , bg25  = "markdown bold hex 25% grey open"  , bg30  = "markdown bold hex 30% grey open"  , bg35  = "markdown bold hex 35% grey open"  , bg40  = "markdown bold hex 40% grey open"  , bg45  = "markdown bold hex 45% grey open"  , bg50  = "markdown bold hex 50% grey open"  , bg55  = "markdown bold hex 55% grey open"  , bg60  = "markdown bold hex 60% grey open"  , bg65  = "markdown bold hex 65% grey open"  , bg70  = "markdown bold hex 70% grey open"  , bg75  = "markdown bold hex 75% grey open"  , bg80  = "markdown bold hex 80% grey open"  , bg85  = "markdown bold hex 85% grey open"  , bg90  = "markdown bold hex 90% grey open"  , bg95  = "markdown bold hex 95% grey open"  , bwht  = "markdown bold hex white open"  , binv  = "markdown bold hex invisible open"  ,
  pblk  = "markdown plain hex black open" , pg05  = "markdown plain hex 05% grey open" , pg10  = "markdown plain hex 10% grey open" , pg15  = "markdown plain hex 15% grey open" , pg20  = "markdown plain hex 20% grey open" , pg25  = "markdown plain hex 25% grey open" , pg30  = "markdown plain hex 30% grey open" , pg35  = "markdown plain hex 35% grey open" , pg40  = "markdown plain hex 40% grey open" , pg45  = "markdown plain hex 45% grey open" , pg50  = "markdown plain hex 50% grey open" , pg55  = "markdown plain hex 55% grey open" , pg60  = "markdown plain hex 60% grey open" , pg65  = "markdown plain hex 65% grey open" , pg70  = "markdown plain hex 70% grey open" , pg75  = "markdown plain hex 75% grey open" , pg80  = "markdown plain hex 80% grey open" , pg85  = "markdown plain hex 85% grey open" , pg90  = "markdown plain hex 90% grey open" , pg95  = "markdown plain hex 95% grey open" , pwht  = "markdown plain hex white open" , pinv  = "markdown plain hex invisible open" ,
  iblk  = "markdown italic hex black open", ig05  = "markdown italic hex 05% grey open", ig10  = "markdown italic hex 10% grey open", ig15  = "markdown italic hex 15% grey open", ig20  = "markdown italic hex 20% grey open", ig25  = "markdown italic hex 25% grey open", ig30  = "markdown italic hex 30% grey open", ig35  = "markdown italic hex 35% grey open", ig40  = "markdown italic hex 40% grey open", ig45  = "markdown italic hex 45% grey open", ig50  = "markdown italic hex 50% grey open", ig55  = "markdown italic hex 55% grey open", ig60  = "markdown italic hex 60% grey open", ig65  = "markdown italic hex 65% grey open", ig70  = "markdown italic hex 70% grey open", ig75  = "markdown italic hex 75% grey open", ig80  = "markdown italic hex 80% grey open", ig85  = "markdown italic hex 85% grey open", ig90  = "markdown italic hex 90% grey open", ig95  = "markdown italic hex 95% grey open", iwht  = "markdown italic hex white open", iinv  = "markdown italic hex invisible open",
  bfblu = "markdown bold hex full blue open"  , bfcyn = "markdown bold hex full cyan open"  , bfgrn = "markdown bold hex full green open"  , bfmag = "markdown bold hex full magenta open"  , bforn = "markdown bold hex full orange open"  , bfred = "markdown bold hex full red open"  , bfylw = "markdown bold hex full yellow open"  ,
  blblu = "markdown bold hex light blue open" , blcyn = "markdown bold hex light cyan open" , blgrn = "markdown bold hex light green open" , blmag = "markdown bold hex light magenta open" , blorn = "markdown bold hex light orange open" , blred = "markdown bold hex light red open" , blylw = "markdown bold hex light yellow open" ,
  bmblu = "markdown bold hex medium blue open", bmcyn = "markdown bold hex medium cyan open", bmgrn = "markdown bold hex medium green open", bmmag = "markdown bold hex medium magenta open", bmorn = "markdown bold hex medium orange open", bmred = "markdown bold hex medium red open", bmylw = "markdown bold hex medium yellow open",
  bdblu = "markdown bold hex dark blue open"  , bdcyn = "markdown bold hex dark cyan open"  , bdgrn = "markdown bold hex dark green open"  , bdmag = "markdown bold hex dark magenta open"  , bdorm = "markdown bold hex dark orange open"  , bdred = "markdown bold hex dark red open"  , bdylw = "markdown bold hex dark yellow open"  ,
  pfblu = "markdown plain hex full blue open"  , pfcyn = "markdown plain hex full cyan open"  , pfgrn = "markdown plain hex full green open"  , pfmag = "markdown plain hex full magenta open"  , pforn = "markdown plain hex full orange open"  , pfred = "markdown plain hex full red open"  , pfylw = "markdown plain hex full yellow open"  ,
  plblu = "markdown plain hex light blue open" , plcyn = "markdown plain hex light cyan open" , plgrn = "markdown plain hex light green open" , plmag = "markdown plain hex light magenta open" , plorn = "markdown plain hex light orange open" , plred = "markdown plain hex light red open" , plylw = "markdown plain hex light yellow open" ,
  pmblu = "markdown plain hex medium blue open", pmcyn = "markdown plain hex medium cyan open", pmgrn = "markdown plain hex medium green open", pmmag = "markdown plain hex medium magenta open", pmorn = "markdown plain hex medium orange open", pmred = "markdown plain hex medium red open", pmylw = "markdown plain hex medium yellow open",
  pdblu = "markdown plain hex dark blue open"  , pdcyn = "markdown plain hex dark cyan open"  , pdgrn = "markdown plain hex dark green open"  , pdmag = "markdown plain hex dark magenta open"  , pdorm = "markdown plain hex dark orange open"  , pdred = "markdown plain hex dark red open"  , pdylw = "markdown plain hex dark yellow open"  ,
  ifblu = "markdown italic hex full blue open"  , ifcyn = "markdown italic hex full cyan open"  , ifgrn = "markdown italic hex full green open"  , ifmag = "markdown italic hex full magenta open"  , iforn = "markdown italic hex full orange open"  , ifred = "markdown italic hex full red open"  , ifylw = "markdown italic hex full yellow open"  ,
  ilblu = "markdown italic hex light blue open" , ilcyn = "markdown italic hex light cyan open" , ilgrn = "markdown italic hex light green open" , ilmag = "markdown italic hex light magenta open" , ilorn = "markdown italic hex light orange open" , ilred = "markdown italic hex light red open" , ilylw = "markdown italic hex light yellow open" ,
  imblu = "markdown italic hex medium blue open", imcyn = "markdown italic hex medium cyan open", imgrn = "markdown italic hex medium green open", immag = "markdown italic hex medium magenta open", imorn = "markdown italic hex medium orange open", imred = "markdown italic hex medium red open", imylw = "markdown italic hex medium yellow open",
  idblu = "markdown italic hex dark blue open"  , idcyn = "markdown italic hex dark cyan open"  , idgrn = "markdown italic hex dark green open"  , idmag = "markdown italic hex dark magenta open"  , idorm = "markdown italic hex dark orange open"  , idred = "markdown italic hex dark red open"  , idylw = "markdown italic hex dark yellow open"
)

#' @rdname ujconstants
#' @export
.MiscVals <- base::list(nl = "\n", blank = "", yesno = "yesno", okx = "okcancel", and = "and", and1 = " and ", or = "or", or1 = " or ", int = "intercept", ref = "reference", saf = "stringsAsFactors = FALSE", pt = ggplot2::.pt, NAL = NA, NAR = NA_real_ , NAI = NA_integer_ , NAC = NA_character_ , one = 0.99999999, zero = 0.00000001)

#' @rdname ujconstants
#' @export
.MiscDefs <- base::c(nl = "newline", blank = "blank string", yesno = "yes/no spec for dialog box buttons in dialogs functions", okx = "ok/cancel spec for dialog box buttons in ?dialogs functions", and = "and", and1 = "padded and", or = "or", or1 = "padded or", int = "intercept", ref = "reference", saf = "'stringsAsFactors = FALSE' argument for data.frame calls", pt = "ggplot point size multiplier", NAL = "logical NA", NAR = "real NA", NAI = "integer NA", NAC = "character NA", one = "nearly 1", zero = "nearly 0")

#' @rdname ujconstants
#' @export
.UTF8vals <- base::list(back = "\U005C", back1 = " \U005C ", backs = "\U005C\U005C", backs1 = " \U005C\U005C ", bullet = "\u2022", bullet1 = " \u2022 ", by = "\u00D7", by1 = " \u00D7 ", chi2 = "*\U03C7*<sup>2</sup>", colon = "\U003A", colon1 = "\U003A ", comma = "\U002C", comma1 = "\U002C ", dagger = "\U2020", dagger2 = "\U2021", dash = "\U002D", dash1 = " \U002D ", dn = "\U2193", dn1 = " \U2193 ", em = "\U2014" , em1 = " \U2014 ", en0 = "\U2013" , en1 = " \U2013 ", eq = "\U003D", eq1 = " \U003D ", ge = "\u2265", ge1 = " \u2265 ", le = "\u2264", le1 = " \u2264 ", lft = "\u2190", lft1 = " \u2190 ", ne = "\u2260", ne1 = " \u2260 ", pipe = "\U007C", pipe1 = " \U007C ", plus = "\U002B", plus1 = " \U002B ", rgt = "\u2192", rgt1 = " \u2192 ", section = "\U00A7", semi = "\U003B", semi1 = "\U003B ", slash = "\U002F", slash1 = " \U002F ", slashes = "\U002F\U002F", slashes1 = " \U002F\U002F ", star = "\U002A", star1 = " \U002A ", tilde = "\U007E", tilde1 = " \U007E ", up = "\U2191", up1 = " \U2191 ", inf = "\U221E", pm = " \U00B1 ", pm0 = "\U00B1", le = " \U2264 ", le0 = "\U2264", ge = " \U2265 ", ge0 = "\U2265", ne = " \U2260", ne0 = "\U2260", approx = " \U2248 ", approx0 = "\U2248", dmd = " \U2B29 ", dmd0 = "\U2B29", . = "\U002E", dot = "\U002E", tick = "\U0060", nbsp = "\U0080", space = "\U0020", under = "\U005F")

#' @rdname ujconstants
#' @export
.UTF8defs <- base::c(back = "backslash", back1 = "padded backslash", backs = "double backslash", backs1 = "padded double backslash", bullet = "bullet", bullet1 = "padded bullet", by = "by operator", by1 = "padded 'by' operator", chi2 = "(italic chi)squared" , colon = "colon" , colon1 = "padded colon (followed by space)", comma = "comma" , comma1 = "padded comma (followed by space)", dagger = "dagger sign", dagger2 = "double dagger sign", dash = "unpadded dash", dash1 = "padded dash", dn = "down arrow", dn1 = "padded down arrow", em = "em dash", em1 = "padded em dash", en0 = "en dash", en1 = "padded en dash", eq = "equals operator", eq1 = "padded equal sign", ge = "greater than or equal operator", ge1 = "padded greater than or equal", le = "less than or equal operator", le1 = "padded less than or equal", lft = "left arrow", lft1 = "padded left arrow", ne = "does not equal operator", ne1 = "padded does not equal", pipe = "pipe", pipe1 = "padded pipe", plus = "plus operator", plus1 = "padded plus operator", pm = "plus or minus operator", pm1 = "padded plus or minus sign", rgt = "right arrow", rgt1 = "padded right arrow", section = "section symbol", semi = "semicolon", semi1 = "padded semi-colon (followed by space)", slash = "slash" , slash1 = "padded slash", slashes = "unpadded double slash", slashes1 = "padded double slash", star = "asterisk", star1 = "padded asterisk", tilde = "tilde" , tilde1 = "padded tilde", up = "up arrow", up1 = "padded up arrow", inf = "unicode infinity", pm =  "padded plus or minus", pm0 = "unpadded plus or minus", le = "padded less than or equal", le0 = "unpadded less than or equal", ge = "padded greater than or equal", ge0 = "unpadded greater than or equal", ne = " padded not equal", ne0 = "unpadded not equal", approx = "padded approximately equal", approx0 = "unpadded approximately equal", dmd = "padded diamond", dmd0 = "unpadded diamond", . = "period", dot = "period", tick = "backtick", nbsp = "non-breaking space", space = "space" , under = "underscore")

#' @rdname ujconstants
#' @export
.LineTypeVals <- base::list(line.types = base::c('solid', '42', '22', '11', '4111', '2111', '1114', '1112', '1441'))

#' @rdname ujconstants
#' @export
.LinetypeDefs <- "Default linetypes (solid, 4on.2off, 2on.2off, 1on.1off, 4on.1off.1on.1off, 2on.1off.1on.1oof, 1on.1off.1on.4off, 1on.1off.1on.2off, 1on.4off.4on.1off"

#' @rdname ujconstants
#' @export
.ShapeVals <- base::list(
  letter.shapes = 97:122                                    , #       a,      b,  c,    d,       e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z
  LETTER.shapes = 65:90                                     , #       A,      B,  C,    D,       E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z
  digit.shapes0 = 48:57                                     , #       0,      1,  2,    3,       4, 5, 6, 7, 8, 9
  digit.shapes  = 49:57                                     , #       1,      2,  3,    4,       5, 6, 7, 8, 9
  open.shapes   = base::c(  3,  94, 118,  60,  62)          , #       +,      ^,  v,    <,       >
  punc.shapes   = base::c( 33,  64,  35,  36,  37,  38,  63), #       !,      @,  #,    $,       %, &, ?
  empty.shapes  = base::c(  1,   0,   2,   6,   5          ), # circle , square, up, down, diamond
  fill.shapes   = base::c( 21,  22,  24,  25,  23          ), # circle , square, up, down, diamond
  solid.shapes  = base::c( 16,  15,  17,  18               ), # circle , square, up, diamond
  blank.shape   = base::c( 32                              )  # <space>
)

#' @rdname ujconstants
#' @export
.ShapeDefs <- base::c(
  letter.shapes = "lowercase letters drawn as plotting symbols (uses ASCII rather than character values)",
  LETTER.shapes = "uppercase letters drawn as plotting symbols (uses ASCII rather than character values)",
  digit.shapes0 = "digits from 0 to 9 drawn as plotting symbols (uses ASCII rather than character values)",
  digit.shapes  = "digits from 1 to 9 drawn as plotting symbols (uses ASCII rather than character values)",
  open.shapes   = "distinctive characters that have no enclosed areas (+, ^, v, <, >; drawn as plotting symbols, using ASCII rather than character values)",
  punc.shapes   = "distinctive punctuation characters for use where overplotting as not problem (!, @, #, $, %, &, ?; drawn as plotting symbols, using ASCII rather than character values)",
  empty.shapes  = "standard R empty shapes ordered to maximize distinctiveness for smaller numbers of shapes needed (circle, square, up triangle, down triangle, diamond)",
  fill.shapes   = "standard R fillable shapes ordered to maximize distinctiveness for smaller numbers of shapes needed (circle, square, up triangle, down triangle, diamond)",
  solid.shapes  = "standard R soplid shapes ordered to maximize distinctiveness for smaller numbers of shapes needed (circle, square, up triangle, diamond)",
  blank.shape   = "space as a plotting symbol (uses ASCII = 31 rather than ' ')."
)

#' @rdname ujconstants
#' @export
.CharSetVals <- base::list(
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

#' @rdname ujconstants
#' @export
.CharSetDefs <- base::c(
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

#' @rdname ujconstants
#' @export
.PropVals <- base::list(
  bbb = .PropFamilyVals$bbb,
  BBB = .PropFamilyVals$BBB,
  ccc = .PropFamilyVals$ccc,
  CCC = .PropFamilyVals$CCC,
  ddd = .PropFamilyVals$ddd,
  DDD = .PropFamilyVals$DDD,
  eee = .PropFamilyVals$eee,
  EEE = .PropFamilyVals$EEE,
  iii = .PropFamilyVals$iii,
  III = .PropFamilyVals$III,
  mmm = .PropFamilyVals$mmm,
  MMM = .PropFamilyVals$MMM,
  sss = .PropFamilyVals$sss,
  SSS = .PropFamilyVals$SSS,
  ppp = base::sort(base::unique(base::tolower(.AllPropVals[[1]]))),
  PPP = base::sort(base::unique(base::toupper(.AllPropVals[[1]]))),
  XXX = base::sort(.AllPropVals[[1]])
)

#' @rdname ujconstants
#' @export
.PropDefs <- base::c(
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

#' @rdname ujconstants
#' @export
.CrayonVals <- base::list(
  CrayonBold = .CrayonSynonymVals$Bold,
  CrayonPlain = .CrayonSynonymVals$Plain,
  CrayonItalic = .CrayonSynonymVals$Italic,
  CrayonDefault = .CrayonSynonymVals$Default,
  CrayonUnderline = .CrayonSynonymVals$Underline,
  CrayonRed = .CrayonSynonymVals$Red,
  CrayonBlue = .CrayonSynonymVals$Blue,
  CrayonCyan = .CrayonSynonymVals$Cyan,
  CrayonGreen = .CrayonSynonymVals$Green,
  CrayonBlack = .CrayonSynonymVals$Black,
  CrayonWhite = .CrayonSynonymVals$White,
  CrayonSilver = .CrayonSynonymVals$Silver,
  CrayonYellow = .CrayonSynonymVals$Yellow,
  CrayonMagenta = .CrayonSynonymVals$Magenta
)

#' @rdname ujconstants
#' @export
.CrayonDefs <- base::c(
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

#' @rdname ujconstants
#' @export
.PkgVals <- base::c(
  .MiscVals,
  .PropVals,
  .AllPropVals,
  .CharSetVals,
  .UTF8vals,
  .HexColorVals,
  .EnclosureVals,
  .ShapeVals,
  .LineTypeVals,
  .CrayonVals,
  .CrayonFamilyVals,
  .CrayonSynonymVals,
  .PropFamilyVals,
  .BoldGreekVals,
  .PlainGreekVals,
  .ItalicGreekVals,
  .BoldGREEKvals,
  .PlainGREEKvals,
  .ItalicGREEKvals,
  .BasicMarkdownVals,
  .OpenMarkdownColorVals
)

#' @rdname ujconstants
#' @export
.PkgDefs <- base::c(
  .MiscDefs,
  .PropDefs,
  .AllPropDefs,
  .CharSetDefs,
  .UTF8defs,
  .HexColorDefs,
  .EnclosureDefs,
  .ShapeDefs,
  .LinetypeDefs,
  .CrayonDefs,
  .CrayonFamilyDefs,
  .CrayonSynonymDefs,
  .PropFamilyDefs,
  .BoldGreekDefs,
  .PlainGreekDefs,
  .ItalicGreekDefs,
  .BoldGREEKdefs,
  .PlainGREEKdefs,
  .ItalicGREEKdefs,
  .BasicMarkdownDefs,
  .OpenMarkdownColorDefs
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
