#' @name uj_constants.
#' @title Package constants for \code{uj}
#' @param ... unquoted, comma-separated list of the names of constants to
#'   return. If multiple constants are specified, they are coerced into a single
#'   \link[is_vec]{atomic vector} result.
#' @return An atomic vector of length 1 or greater.
#' @export
uj_constants. <- function() {help("uj_constants.", package = "uj")}

#' @describeIn uj_constants. Get a named \link[is_vls]{vlist} containing all
#'   package \code{uj} constants with an associated attribute describing each.
#' @export
uj_constants <- function() {
  labs <- c(
    "yesno", "int", "okx", "ref", "saf", "by", "by0", "em", "em0", "en", "en0", "eq", "eq0", "or", "or0", "pm", "pm0", "and", "and0", "ge", "ge0", "le", "le0", "ne", "ne0", "pipe", "pipe0", "plus",
    "plus0", "semi", "semi0", "star", "star0", "colon", "colon0", "comma", "comma0", "slash", "slash0", "tilde", "tilde0", "bullet", "bullet0", "dash", "dash0", "slashes", "slashes0", "open", "close",
    "quote", "quote2", "lbrace", "rbrace", "lparen", "rparen", "lquote", "rquote", "lquote2", "rquote2", "lbracket", "rbracket", "inf", "pm", "pm0", "le", "le0", "ge", "ge0", "ne", "ne0", "approx",
    "approx0", "dmd", "dmd0", "nl", ".", "dot", "tick", "nbsp", "blank", "space", "under", "alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta", "theta", "iota", "kappa", "lambda", "mu", "nu",
    "xi", "omicron", "pi", "rho", "sigma", "tau", "upsilon", "phi", "chi", "psi", "omega", "ialpha", "ibeta", "igamma", "idelta", "iepsilon", "izeta", "ieta", "itheta", "iiota", "ikappa", "ilambda",
    "imu", "inu", "ixi", "iomicron", "ipi", "irho", "isigma", "itau", "iupsilon", "iphi", "ichi", "ipsi", "iomega", "balpha", "bbeta", "bgamma", "bdelta", "bepsilon", "bzeta", "beta", "btheta", "biota",
    "bkappa", "blambda", "bmu", "bnu", "bxi", "bomicron", "bpi", "brho", "bsigma", "btau", "bupsilon", "bphi", "bchi", "bpsi", "bomega", "ALPHA", "BETA", "GAMMA", "DELTA", "EPSILON", "ZETA", "ETA",
    "THETA", "IOTA", "KAPPA", "LAMBDA", "MU", "NU", "XI", "OMICRON", "PI", "RHO", "SIGMA", "TAU", "UPSILON", "PHI", "CHI", "PSI", "OMEGA", "iALPHA", "iBETA", "iGAMMA", "iDELTA", "iEPSILON", "iZETA",
    "iETA", "iTHETA", "iIOTA", "iKAPPA", "iLAMBDA", "iMU", "iNU", "iXI", "iOMICRON", "iPI", "iRHO", "iSIGMA", "iTAU", "iUPSILON", "iPHI", "iCHI", "iPSI", "iOMEGA", "bALPHA", "bBETA", "bGAMMA", "bDELTA",
    "bEPSILON", "bZETA", "bETA", "bTHETA", "bIOTA", "bKAPPA", "bLAMBDA", "bMU", "bNU", "bXI", "bOMICRON", "bPI", "bRHO", "bSIGMA", "bTAU", "bUPSILON", "bPHI", "bCHI", "bPSI", "bOMEGA", "chi2", "brk",
    "sub1", "sub2", "sup1", "sup2", "b1", "b2", "p1", "p2", "c2", "blk", "g10", "g20", "g30", "g40", "g50", "g60", "g70", "g80", "g85", "g90", "g95", "wht", "inv", "blu", "cyn", "grn", "mag", "orn",
    "red", "ylw", "lblu", "lcyn", "lgrn", "lmag", "lorn", "lred", "lylw", "mblu", "mcyn", "mgrn", "mmag", "morn", "mred", "mylw", "dblu", "dcyn", "dgrn", "dmag", "dorn", "dred", "dylw", "bblk", "bg10",
    "bg20", "bg30", "bg40", "bg50", "bg60", "bg70", "bg80", "bg85", "bg90", "bg95", "bwht", "binv", "bblu", "bcyn", "bgrn", "bmag", "born", "bred", "bylw", "blblu", "blcyn", "blgrn", "blmag", "blorn",
    "blred", "blylw", "bmblu", "bmcyn", "bmgrn", "bmmag", "bmorn", "bmred", "bmylw", "bdblu", "bdcyn", "bdgrn", "bdmag", "bdorn", "bdred", "bdylw", "pblk", "pg10", "pg20", "pg30", "pg40", "pg50", "pg60",
    "pg70", "pg80", "pg85", "pg90", "pg95", "pwht", "pinv", "pblu", "pcyn", "pgrn", "pmag", "porn", "pred", "pylw", "plblu", "plcyn", "plgrn", "plmag", "plorn", "plred", "plylw", "pmblu", "pmcyn", "pmgrn",
    "pmmag", "pmorn", "pmred", "pmylw", "pdblu", "pdcyn", "pdgrn", "pdmag", "pdorn", "pdred", "pdylw", "pt", "L", "R", "I", "C", "one", "zero", "az", "AZ", "colors", "lines", "ldots", "fdots", "edots",
    "sdots", "digits", "aAzZ", "names", "names0", "files", "files0", "models", "labels", "vowels"
  )
  values <- list(
    "yesno", "intercept", "okcancel", "reference", "stringsAsFactors = FALSE", " × ", "×", " — ", "—", " – ", "–", " = ", "=", " or ", "or", " ± ", "±", " and ", "and", " ≥ ", "≥", " ≤ ", "≤", " ≠ ",
    "≠", " | ", "|", " + ", "+", "; ", ";", " * ", "*", ": ", ":", ", ", ",", " / ", "/", " ~ ", "~", " • ", "•", " - ", "-", " // ", "//", "<", ">", "'", "\"", "{", "}", "(", ")", "‘", "‘", "“", "”",
    "[", "]", "\U221E", " \U00B1 ", "\U00B1", " \U2264 ", "\U2264", " \U2265 ", "\U2265", " \U2260", "\U2260", " \U2248 ", "\U2248", " \U2B29 ", "\U2B29", "\n", ".", ".", "`", " ", "", " ", "_",
    "\U03B1", "\U03B2", "\U03B3", "\U03B4", "\U03B5", "\U03B5", "\U03B7", "\U03B8", "\U03B9", "\U03BA", "\U03BB", "\U03BC", "\U03BD", "\U03BE", "\U03BF", "\U03C0", "\U03C1", "\U03C3", "\U03C4",
    "\U03C5", "\U03C6", "\U03C7", "\U03C8", "\U03C9", "*\U03B1*", "*\U03B2*", "*\U03B3*", "*\U03B4*", "*\U03B5*", "*\U03B5*", "*\U03B7*", "*\U03B8*", "*\U03B9*", "*\U03BA*", "*\U03BB*", "*\U03BC*",
    "*\U03BD*", "*\U03BE*", "*\U03BF*", "*\U03C0*", "*\U03C1*", "*\U03C3*", "*\U03C4*", "*\U03C5*", "*\U03C6*", "*\U03C7*", "*\U03C8*", "*\U03C9*", "**\U03B1**", "**\U03B2**", "**\U03B3**",
    "**\U03B4**", "**\U03B5**", "**\U03B5**", "**\U03B7**", "**\U03B8**", "**\U03B9**", "**\U03BA**", "**\U03BB**", "**\U03BC**", "**\U03BD**", "**\U03BE**", "**\U03BF**", "**\U03C0**", "**\U03C1**",
    "**\U03C3**", "**\U03C4**", "**\U03C5**", "**\U03C6**", "**\U03C7**", "**\U03C8**", "**\U03C9**", "\U0391", "\U0392", "\U0393", "\U0394", "\U0395", "\U0396", "\U0397", "\U0398", "\U0399",
    "\U039A", "\U039B", "\U039C", "\U039D", "\U039E", "\U039F", "\U03A0", "\U03A1", "\U03A3", "\U03A4", "\U03A5", "\U03A6", "\U03A7", "\U03A8", "\U03A9", "*\U0391*", "*\U0392*", "*\U0393*",
    "*\U0394*", "*\U0395*", "*\U0396*", "*\U0397*", "*\U0398*", "*\U0399*", "*\U039A*", "*\U039B*", "*\U039C*", "*\U039D*", "*\U039E*", "*\U039F*", "*\U03A0*", "*\U03A1*", "*\U03A3*", "*\U03A4*",
    "*\U03A5*", "*\U03A6*", "*\U03A7*", "*\U03A8*", "*\U03A9*", "**\U0391**", "**\U0392**", "**\U0393**", "**\U0394**", "**\U0395**", "**\U0396**", "**\U0397**", "**\U0398**", "**\U0399**",
    "**\U039A**", "**\U039B**", "**\U039C**", "**\U039D**", "**\U039E**", "**\U039F**", "**\U03A0**", "**\U03A1**", "**\U03A3**", "**\U03A4**", "**\U03A5**", "**\U03A6**", "**\U03A7**",
    "**\U03A8**", "**\U03A9**", "*\U03C7*<sup>2</sup>", "<br />", "<sub>", "</sub>", "<sup>", "</sup>", "<b style=color:'", "</b>", "<span style=color'", "</span>", ">", "#000000FF", "#1A1A1AFF",
    "#333333FF", "#4D4D4DFF", "#666666FF", "#808080FF", "#999999FF", "#B3B3B3FF", "#CCCCCCFF", "#D9D9D9FF", "#E6E6E6FF", "#F2F2F2FF", "#FFFFFFFF", "#FFFFFF01", "#0000FFFF", "#00FFFFFF", "#00FF00FF",
    "#FF00FFFF", "#FF8800FF", "#AA0000FF", "#FFFF00FF", "#DDDDFFFF", "#FFDDFFFF", "#DDFFDDFF", "#FFBBFFFF", "#FFEEAAFF", "#FFDDDDFF", "#FFFFDDFF", "#AAAAFFFF", "#66FFFFFF", "#FFFF66FF", "#FF66FFFF",
    "#FFAA66FF", "#AFFAAAFF", "#FFFF66FF", "#000080FF", "#008B8BFF", "#008B00FF", "#8B008BFF", "#8B4500FF", "#800000FF", "#8B8B00FF", "<b style=color:'#000000FF'>", "<b style=color:'#1A1A1AFF'>",
    "<b style=color:'#333333FF'>", "<b style=color:'#4D4D4DFF'>", "<b style=color:'#666666FF'>", "<b style=color:'#808080FF'>", "<b style=color:'#999999FF'>", "<b style=color:'#B3B3B3FF'>",
    "<b style=color:'#CCCCCCFF'>", "<b style=color:'#D9D9D9FF'>", "<b style=color:'#E6E6E6FF'>", "<b style=color:'#F2F2F2FF'>", "<b style=color:'#FFFFFFFF'>", "<b style=color:'#FFFFFF01'>",
    "<b style=color:'#0000FFFF'>", "<b style=color:'#00FFFFFF'>", "<b style=color:'#00FF00FF'>", "<b style=color:'#FF00FFFF'>", "<b style=color:'#FF8800FF'>", "<b style=color:'#AA0000FF'>",
    "<b style=color:'#FFFF00FF'>", "<b style=color:'#DDDDFFFF'>", "<b style=color:'#FFDDFFFF'>", "<b style=color:'#DDFFDDFF'>", "<b style=color:'#FFBBFFFF'>", "<b style=color:'#FFEEAAFF'>",
    "<b style=color:'#FFDDDDFF'>", "<b style=color:'#FFFFDDFF'>", "<b style=color:'#AAAAFFFF'>", "<b style=color:'#66FFFFFF'>", "<b style=color:'#FFFF66FF'>", "<b style=color:'#FF66FFFF'>",
    "<b style=color:'#FFAA66FF'>", "<b style=color:'#AFFAAAFF'>", "<b style=color:'#FFFF66FF'>", "<b style=color:'#000080FF'>", "<b style=color:'#008B8BFF'>", "<b style=color:'#008B00FF'>",
    "<b style=color:'#8B008BFF'>", "<b style=color:'#8B4500FF'>", "<b style=color:'#800000FF'>", "<b style=color:'#8B8B00FF'>", "<span style=color:'#000000FF'>", "<span style=color:'#1A1A1AFF'>",
    "<span style=color:'#333333FF'>", "<span style=color:'#4D4D4DFF'>", "<span style=color:'#666666FF'>", "<span style=color:'#808080FF'>", "<span style=color:'#999999FF'>",
    "<span style=color:'#B3B3B3FF'>", "<span style=color:'#CCCCCCFF'>", "<span style=color:'#D9D9D9FF'>", "<span style=color:'#E6E6E6FF'>", "<span style=color:'#F2F2F2FF'>",
    "<span style=color:'#FFFFFFFF'>", "<span style=color:'#FFFFFF01'>", "<span style=color:'#0000AAFF'>", "<span style=color:'#00FFFFFF'>", "<span style=color:'#00FF00FF'>",
    "<span style=color:'#FF00FFFF'>", "<span style=color:'#FF8800FF'>", "<span style=color:'#AA0000FF'>", "<span style=color:'#FFFF00FF'>", "<span style=color:'#DDDDFFFF'>",
    "<span style=color:'#FFDDFFFF'>", "<span style=color:'#DDFFDDFF'>", "<span style=color:'#FFBBFFFF'>", "<span style=color:'#FFEEAAFF'>", "<span style=color:'#FFDDDDFF'>",
    "<span style=color:'#FFFFDDFF'>", "<span style=color:'#AAAAFFFF'>", "<span style=color:'#66FFFFFF'>", "<span style=color:'#FFFF66FF'>", "<span style=color:'#FF66FFFF'>",
    "<span style=color:'#FFAA66FF'>", "<span style=color:'#AFFAAAFF'>", "<span style=color:'#FFFF66FF'>", "<span style=color:'#000080FF'>", "<span style=color:'#008B8BFF'>",
    "<span style=color:'#008B00FF'>", "<span style=color:'#8B008BFF'>", "<span style=color:'#8B4500FF'>", "<span style=color:'#800000FF'>", "<span style=color:'#8B8B00FF'>",
    ggplot2::.pt, NA, NA_real_, NA_integer_, NA_character_, 0.99999999, 0.00000001, letters, LETTERS,
    c('#800000FF', '#000080FF', '#8B008BFF', '#000000FF', '#8B4500FF', '#FF0000FF', '#0000FFFF', '#FF00FFFF', '#666666FF', '#FF8800FF'),
    c('solid', '42', '22', '11', '4111', '2111', '1114', '1112', '1441'),
    c(3, 4, 8, 2, 6, 5, 0, 1), c(21, 23, 24, 22, 25), c(5, 0, 2, 6, 1),
    c(16, 18, 17, 15),
    as.character(0:9),
    c(letters, LETTERS),
    c(letters, LETTERS, 0:9, "_", "."),
    c(letters, LETTERS, 0:9, "."),
    c(letters, LETTERS, 0:9, "_", ".", " ", ",", "-", "(", ")"),
    c(letters, LETTERS, 0:9, ".", " ", ",", "-", "(", ")"),
    c(letters, LETTERS, 0:9, ".", " ", "×", "+", "*", ":"),
    c(letters, LETTERS, 0:9, ".", " ", ",", "-", "(", ")"),
    c("a", "e", "i", "o", "u", "A", "E", "I", "O", "U")
  )
  descriptions <- c(
    "yes and no buttons", "intercept", "ok and cancel buttons", "reference", "strings as character", "non-escaped padded 'by' operator", "non-escaped unpadded 'by' operator", "padded em dash",
    "unpadded em dash", "padded en dash", "unpadded en dash", "padded equal sign", "unpadded equal sign", "padded or", "unpadded or", "non-escaped padded plus or minus sign",
    "non-escaped unpadded plus or minus sign", "padded and", "unpadded and", "non-escaped padded greater than or equal", "non-escaped unpadded greater than or equal",
    "non-escaped padded less than or equal", "non-escaped unpadded less than or equal", "non-escaped padded does not equal", "non-escaped unpadded does not equal", "padded pipe",
    "unpadded pipe", "padded plus sign", "unpadded plus sign", "padded semi-colon (followed by space)", "unpadded semicolon", "padded asterisk", "unpadded asterisk",
    "padded colon (followed by space)", "unpadded colon", "padded comma (followed by space)", "unpadded comma", "padded slash", "unpadded slash", "padded tilde", "unpadded tilde",
    "non-escaped padded bullet", "non-escaped unpadded bullet", "padded dash", "unpadded dash", "padded double slash", "unpadded double slash", "markdown tag open", "markdown tag close",
    "single straight quote", "double straight quote", "left curly brace", "right curly brace", "left paren", "right paren", "non-escaped left single quote", "non-escaped right single quote",
    "non-escaped left double quote", "non-escaped right double quote", "left square bracket", "right square bracket", "unicode infinity", "unicode padded plus or minus",
    "unicode unpadded plus or minus", "unicode padded less than or equal", "unicode unpadded less than or equal", "unicode padded greater than or equal", "unicode unpadded greater than or equal",
    " unicode padded not equal", "unicode unpadded not equal", "unicode padded approximately equal", "unicode unpadded approximately equal", "unicode padded diamond", "unicode unpadded diamond",
    "new line", "period", "period", "backtick", "non-breaking space", "blank string (0 characters)", "space", "underscore", "alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta", "theta",
    "iota", "kappa", "lambda", "mu", "nu", "xi", "omicron", "pi", "rho", "sigma", "tau", "upsilon", "phi", "chi", "psi", "omega", "italic alpha", "italic beta", "italic gamma", "italic delta",
    "italic epsilon", "italic zeta", "italic eta", "italic theta", "italic iota", "italic kappa", "italic lambda", "italic mu", "italic nu", "italic xi", "italic omicron", "italic pi",
    "italic rho", "italic sigma", "italic tau", "italic upsilon", "italic phi", "italic chi", "italic psi", "italic omega", "bold alpha", "bold beta", "bold gamma", "bold delta", "bold epsilon",
    "bold zeta", "bold eta", "bold theta", "bold iota", "bold kappa", "bold lambda", "bold mu", "bold nu", "bold xi", "bold omicron", "bold pi", "bold rho", "bold sigma", "bold tau",
    "bold upsilon", "bold phi", "bold chi", "bold psi", "bold omega", "ALPHA", "BETA", "GAMMA", "DELTA", "EPSILON", "ZETA", "ETA", "THETA", "IOTA", "KAPPA", "LAMBDA", "MU", "NU", "XI",
    "OMICRON", "PI", "RHO", "SIGMA", "TAU", "UPSILON", "PHI", "CHI", "PSI", "OMEGA", "italic ALPHA", "italic BETA", "italic GAMMA", "italic DELTA", "italic EPSILON", "italic ZETA",
    "italic ETA", "italic THETA", "italic IOTA", "italic KAPPA", "italic LAMBDA", "italic MU", "italic NU", "italic XI", "italic OMICRON", "italic PI", "italic RHO", "italic SIGMA",
    "italic TAU", "italic UPSILON", "italic PHI", "italic CHI", "italic PSI", "italic OMEGA", "bold ALPHA", "bold BETA", "bold GAMMA", "bold DELTA", "bold EPSILON", "bold ZETA", "bold ETA",
    "bold THETA", "bold IOTA", "bold KAPPA", "bold LAMBDA", "bold MU", "bold NU", "bold XI", "bold OMICRON", "bold PI", "bold RHO", "bold SIGMA", "bold TAU", "bold UPSILON", "bold PHI",
    "bold CHI", "bold PSI", "bold OMEGA", "chi squared", "markdown line break", "markdown subscript open", "markdown subscript close", "markdown superscript open", "markdown superscript close",
    "markdown (bold + color) open", "markdown bold close", "markdown (plain + color) open", "markdown plain close", "markdown generic close", "hex black", "hex 10% grey", "hex 20% grey",
    "hex 30% grey", "hex 40% grey", "hex 50% grey", "hex 60% grey", "hex 70% grey", "hex 80% grey", "hex 85% grey", "hex 90% grey", "hex 95% grey", "hex white", "hex invisible (1/256th alpha)",
    "hex full blue", "hex full cyan", "hex full green", "hex full magenta", "hex full orange", "hex full red", "hex full yellow", "hex light blue", "hex light cyan", "hex light green",
    "hex light magenta", "hex light orange", "hex light red", "hex light yellow", "hex medium blue", "hex medium cyan", "hex medium green", "hex medium magenta", "hex medium orange",
    "hex medium red", "hex medium yellow", "hex dark blue", "hex dark cyan", "hex dark green", "hex dark magenta", "hex dark orange", "hex dark red", "hex dark yellow",
    "markdown bold hex black open", "markdown bold hex 10% grey open", "markdown bold hex 20% grey open", "markdown bold hex 30% grey open", "markdown bold hex 40% grey open",
    "markdown bold hex 50% grey open", "markdown bold hex 60% grey open", "markdown bold hex 70% grey open", "markdown bold hex 80% grey open", "markdown bold hex 85% grey open",
    "markdown bold hex 90% grey open", "markdown bold hex 95% grey open", "markdown bold hex white open", "markdown bold hex invisible (1/256th alpha) open", "markdown bold hex full blue open",
    "markdown bold hex full cyan open", "markdown bold hex full green open", "markdown bold hex full magenta open", "markdown bold hex full orange open", "markdown bold hex full red open",
    "markdown bold hex full yellow open", "markdown bold hex light blue open", "markdown bold hex light cyan open", "markdown bold hex light green open", "markdown bold hex light magenta open",
    "markdown bold hex light orange open", "markdown bold hex light red open", "markdown bold hex light yellow open", "markdown bold hex medium blue open", "markdown bold hex medium cyan open",
    "markdown bold hex medium green open", "markdown bold hex medium magenta open", "markdown bold hex medium orange open", "markdown bold hex medium red open",
    "markdown bold hex medium yellow open", "markdown bold hex dark blue open", "markdown bold hex dark cyan open", "markdown bold hex dark green open", "markdown bold hex dark magenta open",
    "markdown bold hex dark orange open", "markdown bold hex dark red open", "markdown bold hex dark yellow open", "markdown plain hex black open", "markdown plain hex 10% grey open",
    "markdown plain hex 20% grey open", "markdown plain hex 30% grey open", "markdown plain hex 40% grey open", "markdown plain hex 50% grey open", "markdown plain hex 60% grey open",
    "markdown plain hex 70% grey open", "markdown plain hex 80% grey open", "markdown plain hex 85% grey open", "markdown plain hex 90% grey open", "markdown plain hex 95% grey open",
    "markdown plain hex white open", "markdown plain hex invisible (1/256th alpha) open", "markdown plain hex full blue open", "markdown plain hex full cyan open",
    "markdown plain hex full green open", "markdown plain hex full magenta open", "markdown plain hex full orange open", "markdown plain hex full red open", "markdown plain hex full yellow open",
    "markdown plain hex light blue open", "markdown plain hex light cyan open", "markdown plain hex light green open", "markdown plain hex light magenta open",
    "markdown plain hex light orange open", "markdown plain hex light red open", "markdown plain hex light yellow open", "markdown plain hex medium blue open",
    "markdown plain hex medium cyan open", "markdown plain hex medium green open", "markdown plain hex medium magenta open", "markdown plain hex medium orange open",
    "markdown plain hex medium red open", "markdown plain hex medium yellow open", "markdown plain hex dark blue open", "markdown plain hex dark cyan open",
    "markdown plain hex dark green open", "markdown plain hex dark magenta open", "markdown plain hex dark orange open", "markdown plain hex dark red open",
    "markdown plain hex dark yellow open", "the size of a single point (ggplot constant)", "logical NA", "real-valued NA", "integer NA", "character NA", "almost one", "almost zero",
    "lower-case letters", "upper-case letters", "standard color palette (dark red, dark blue, dark magenta, black, dark orange, full red, full blue, full magenta, grey40, full orange)",
    "standard line type palette", "standard line-based dot shapes palette", "standard fillable dot shapes palette", "standard empty (outlined) dot shapes palette",
    "standard solid dot shapes palette", "digits as character", "English letters", "valid for element names names (includes underscore)", "valid for element names names (excludes underscore)",
    "valid for file names (includes underscore)", "valid for file names (excludes underscore)", "valid for model specification", "valid for plotting labels", "English vowels"
  )
  names(values) <- labs
  attr(values, "descriptions") <- descriptions
  values
}

#' @describeIn uj_constants. Get one or more \code{uj} package constants.
#' @export
v <- function(...) {
  x <- as.character(match.call())
  x <- x[2:length(x)]
  av(uj_constants()[x])
}
