#' @title Map Variables in a Data Frame to Specified Roles
#' @description Utility to map variables in a x frame to specific roles, with options to trim variables not assigned to roles, to rename variables to match variable roles (vs. attaching a variable map as an attribute), and to clear the console with each step in variable mapping.
#' @details Variable mapping is conducted interactively via the console for all variable roles in `roles` and `.ROLES` that do not have matching variable names in `x`. For variable roles in `roles` and `.ROLES` that have matching variable names in `x`, matching is done automatically.
#' @param x A \link[uj:rct_atm_dtf]{rectangular atomic x.frame} whose variables are to be mapped.
#' @param roles \link[uj::unq_str_mvc]{Unique string multivec} containing labels for *required* variable roles (that are also valid object names without back-quoting).
#' @param defs \link[uj::unq_str_mvc]{Unique string multivec} with `length(defs) == length(roles)` containing definitions/descriptions associated with corresponding values of `roles`.
#' @param mmms \link[uj::cmp_str_mvc]{Complete string multivec} with `length(mmms) == length(roles)` containing \link[uj::mmm]{extended modes} associated with corresponding values of `roles`.
#' @param .ROLES Optional \link[uj::unq_str_mvc]{unique string multivec} containing labels for *optional* variable roles (that are also valid object names without back-quoting).
#' @param .DEFS Optional \link[uj::unq_str_mvc]{unique string multivec} with `length(.DEFS) == length(.ROLES)` containing definitions/descriptions associated with corresponding values of `.ROLES`.
#' @param .MMMS Optional \link[uj::cmp_str_mvc]{Complete string multivec} with `length(.MMMS) == length(.ROLES)` containing \link[uj::mmm]{extended modes} associated with corresponding values of `.ROLES`.
#' @param .TRIM,.RENAME,.CLEAR Logical scalar indicating, respectively, whether to trim un-mapped variables from `x`, whether to rename mapped variables using the associated values of `roles` and `.ROLES`, and whether to clear the console with each stop in the variable mapping process.
#' @return A \link[uj:rct_atm_dtf]{rectangular atomic x.frame}.
#' @examples
#' x <- x.frame(order = 1:26,
#'                    letters = letters,
#'                    LETTERS = LETTERS,
#'                    fac.letters = factor(letters),
#'                    ord.LETTERS = ordered(LETTERS),
#'                    lgl.sampled = sample(c(TRUE, FALSE), 26, replace = TRUE),
#'                    stringsAsFactors = FALSE)
#' roles  <- c("order"     , "letters"          , "LETTERS"          )
#' defs   <- c("case order", "lowercase letters", "uppercase letters")
#' mmms   <- c("psw"       , "ch1"              , "ch1"              )
#' .ROLES <- c("fac.letters"                          , "ord.LETTERS"                        )
#' .DEFS  <- c("lowercase letters as unordered factor", "uppercase letters as ordered factor")
#' .MMMS  <- c("uno"                                  , "ord"                                )
#' map_variables(x, roles, defs, mmms)
#' map_variables(x, roles, defs, mmms, .TRIM = FALSE)
#' map_variables(x, roles, defs, mmms, .TRIM = FALSE, .RENAME = FALSE)
#' map_variables(x, roles, defs, mmms, .TRIM = FALSE, .RENAME = FALSE, .CLEAR = FALSE)
#' map_variables(x, roles, defs, mmms, .ROLES = .ROLES, .DEFS = .DEFS, .MMMS = .MMMS)
#' map_variables(x, roles, defs, mmms, .ROLES = .ROLES, .DEFS = .DEFS, .MMMS = .MMMS, .TRIM = FALSE)
#' map_variables(x, roles, defs, mmms, .ROLES = .ROLES, .DEFS = .DEFS, .MMMS = .MMMS, .TRIM = FALSE, .RENAME = FALSE)
#' map_variables(x, roles, defs, mmms, .ROLES = .ROLES, .DEFS = .DEFS, .MMMS = .MMMS, .TRIM = FALSE, .RENAME = FALSE, .CLEAR = FALSE)
#' @export
map_variables <- function(x, roles, defs, mmms, .ROLES = NULL, .DEFS = NULL, .MMMS = NULL, .TRIM = TRUE, .RENAME = TRUE, .CLEAR = TRUE) {
  uj::check_funs("rct_atm_dtf", x = x)
  uj::check_funs("unq_str_vec", roles = roles, defs = defs)
  uj::check_funs("cmp_str_vec", mmms = mmms, VALS = uj::mmm_props())
  uj::check_nll_or("unq_str_vec", .ROLES = .ROLES, .DEFS = .DEFS)
  uj::check_nll_or("cmp_str_vec", .MMMS = .MMMS, VALS = uj::mmm_props())
  uj::check_tf(.TRIM = .TRIM, .RENAME = .RENAME, .CLEAR = .CLEAR)
  uj::checkerr(PKG = "uj")
  available <- uj::cnames(x)
  if (!uj::unq_str_vec(available)) {uj::bankerr("Names of variables in [x] must be unique and non-blank.")}
  if (uj::N(roles) > uj::N(available)) {uj::bankerr("length(roles) > ncol(x).")}
  if (uj::N(roles) != uj::N(defs)) {uj::bankerr("length(defs) != length(roles).")}
  if (uj::N(roles) != uj::N(mmms)) {uj::bankerr("length(mmms) != length(roles).")}
  if (uj::N(.ROLES) != uj::N(.DEFS)) {uj::bankerr("length(.DEFS) != length(.ROLES).")}
  if (uj::N(.ROLES) != uj::N(.MMMS)) {uj::bankerr("length(.MMMS) != length(.ROLES).")}
  uj::checkerr(PKG = "uj")
  map <- uj::tb(role = roles, name = NA)
  for (i in 1:uj::N(roles)) {
    role <- roles[i]
    if (uj::not_IN(role, available)) {
      def <- defs[i]
      name <- uj::choose1(available, "What variable of [x] contains ", def, " x?", .CLEAR = .CLEAR)
      available <- available[available != name]
    } else {name <- role}
    map$name[i] <- name
  }
  for (i in 1:uj::N(.ROLES)) {
    if (uj::n1p(available)) {
      .ROLE <- .ROLES[i]
      if  (uj::not_IN(.ROLE, available)) {
        .DEF <- .DEFS[i]
        if (uj::YES("Does this dataset contain a variable filling the role ", .ROLE, " >> ", .DEF, "?", .CLEAR = .CLEAR)) {
          .NAME <- uj::choose1(available, "What variable of [x] contains ", .DEF, " x?", .CLEAR = .CLEAR)
          available <- available[available != .NAME]
        } else {.NAME <- NULL}
      } else {.NAME <- .ROLE}
      if (uj::DEF(.NAME)) {map <- base::rbind(map, uj::tb(role = .ROLE, name = .NAME))}
    }
  }
  if (.TRIM) {x <- x[ , map$name]}
  if (.RENAME) {
    names <- uj::cnames(x)
    for (i in 1:uj::nr(map)) {
      role <- map$role[i]
      name <- map$name[i]
      if (uj::is_IN(name, names)) {names[names == name] <- role}
    }
    x <- uj::name_cols(x, names)
  } else {x <- uj::set_at(x, map = map)}
  x
}
