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
  Errs <- NULL
  if (!uj::.atm_rct_dtf(x)) {Errs <- base::c(Errs, "[x] must be a data.frame.")}
  if (!uj::.unq_str_vec(roles)) {Errs <- base::c(Errs, "[roles] must be a unique string vec (?uj::unq_str_vec).")}
  if (!uj::.unq_str_vec(defs)) {Errs <- base::c(Errs, "[defs] must be a unique string vec (?uj::unq_str_vec).")}
  if (!uj::.cmp_str_vec(mmms)) {Errs <- base::c(Errs, "[mmms] must be a complete string vec (?uj::cmp_str_vec).")}
  if (!base::is.null(.ROLES)) {if (!uj::.unq_str_vec(.ROLES)) {Errs <- base::c(Errs, "[.ROLES] must be NULL or a unique string vec (?uj::unq_str_vec).")}}
  if (!base::is.null(.DEFS)) {if (!uj::.unq_str_vec(.DEFS)) {Errs <- base::c(Errs, "[.DEFS] must be NULL or a unique string vec (?uj::unq_str_vec).")}}
  if (!base::is.null(.MMMS)) {if (!uj::.cmp_str_vec(.MMMS)) {Errs <- base::c(Errs, "[.MMMS] must be NULL or a complete string vec (?uj::cmp_str_vec).")}}
  if (!base::isTRUE(.TRIM) & !base::isFALSE(.TRIM)) {Errs <- base::c(Errs, "[.TRIM] must be TRUE or FALSE.")}
  if (!base::isTRUE(.RENAME) & !base::isFALSE(.RENAME)) {Errs <- base::c(Errs, "[.RENAME] must be TRUE or FALSE.")}
  if (!base::isTRUE(.CLEAR) & !base::isFALSE(.CLEAR)) {Errs <- base::c(Errs, "[.CLEAR] must be TRUE or FALSE.")}
  if (!base::is.null(Errs)) {uj::stopperr(Errs, .PKG = "uj")}
  if (!base::all(mmms %in% uj::mmm_props())) {Errs <- base::c(Errs, "[mmms] contains a value not in uj::mmm_props().")}
  if (!base::is.null(.MMMS)) {if (!base::all(.MMMS %in% uj::mmm_props())) {Errs <- base::c(Errs, "[.MMMS] contains a value not in uj::mmm_props().")}}
  if (!base::is.null(Errs)) {uj::stopperr(Errs, .PKG = "uj")}
  available <- uj::cnames(x)
  if (!uj::unq_str_vec(available)) {Errs <- base::c(Errs, "Names of variables in [x] must be unique and non-blank.")}
  if (uj::N(roles) > uj::N(available)) {Errs <- base::c(Errs, "length(roles) > ncol(x).")}
  if (uj::N(roles) != uj::N(defs)) {Errs <- base::c(Errs, "length(defs) != length(roles).")}
  if (uj::N(roles) != uj::N(mmms)) {Errs <- base::c(Errs, "length(mmms) != length(roles).")}
  if (uj::N(.ROLES) != uj::N(.DEFS)) {Errs <- base::c(Errs, "length(.DEFS) != length(.ROLES).")}
  if (uj::N(.ROLES) != uj::N(.MMMS)) {Errs <- base::c(Errs, "length(.MMMS) != length(.ROLES).")}
  if (!base::is.null(Errs)) {uj::stopperr(Errs, .PKG = "uj")}
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
