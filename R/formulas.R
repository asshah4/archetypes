# Formula class ----------------------------------------------------------------

#' Formula Archetype
#' @name formula
#' @export
formula_archetype <- function(x = unspecified(),
                              role = list(),
                              tier = list(),
                              label = list(),
                              family = character(),
                              source = character(),
                              pattern = character(),
                              ...) {

  # Early Break if needed
  if (length(x) == 0) {
    return(new_formula())
  }

  # Usable classes
  acceptable_classes <-
    c("character",
      "formula",
      "term_archetype",
      "formula_archetype",
      "script")

  if (!any(acceptable_classes %in% class(x))) {
    stop("`formula_archetype()` is not defined for a `",
         class(x)[1],
         "` object.",
         call. = FALSE)
  }


  # Generate rough-draft of terms
  if ("character" %in% class(x)) {
    y <- tm(stats::formula(x))
  } else if ("formula" %in% class(x)) {
    y <- tm(x)
  } else if ("script" %in% class(x)) {
    y <- tm(x)
    pattern <- field(x, "pattern")
  } else if ("term_archetype" %in% class(x)) {
    y <- x
  }

  # Create terms
  t <-
    y |>
    set_roles(roles = formula_args_to_list(role)) |>
    set_tiers(tiers = formula_args_to_list(tier)) |>
    set_labels(labels = formula_args_to_list(label))

  # Formulas
  f <- deparse1(stats::formula(t))

  # Underlying terms and their roles
  rls <- roles(t)
  outcomes <- names(rls[rls == "outcome"])
  exposures <- names(rls[rls == "exposure"])
  predictors <- names(rls[rls == "predictor"])
  confounders <- names(rls[rls == "confounder"])
  mediators <- names(rls[rls == "mediator"])
  unknowns <- names(rls[rls == "unknown"])
  strata <- names(rls[rls == "strata"])

  # Family
  family <- f

  # Check patterns
  if (length(pattern) == 0) {
    pattern <- NA_character_
  }

  new_formula(
    formula = f,
    left = list(lhs(t)),
    right = list(rhs(t)),
    outcomes = list(outcomes),
    predictors = list(predictors),
    exposures = list(exposures),
    confounders = list(confounders),
    mediators = list(mediators),
    unknowns = list(unknowns),
    strata = ifelse(length(strata) == 0, NA_character_, strata),
    family = family,
    source = class(x)[1],
    pattern = pattern,
    status = check_complexity(t)
  )
}

#' @rdname formula
#' @export
fmls = formula_archetype

# Record definition ------------------------------------------------------------

#' Record of formula archetypes
#' @keywords internal
#' @noRd
new_formula <- function(formula = character(),
                        left = list(),
                        right = list(),
                        outcomes = list(),
                        predictors = list(),
                        exposures = list(),
                        confounders = list(),
                        mediators = list(),
                        unknowns = list(),
                        strata = character(),
                        family = character(),
                        source = character(),
                        pattern = character(),
                        status = character()) {

  # Validation
  vec_assert(formula, ptype = character())
  vec_assert(left, ptype = list()) # A string, unabridged
  vec_assert(right, ptype = list()) # A string, unabridged
  vec_assert(outcomes, ptype = list())
  vec_assert(predictors, ptype = list())
  vec_assert(exposures, ptype = list())
  vec_assert(mediators, ptype = list())
  vec_assert(confounders, ptype = list())
  vec_assert(unknowns, ptype = list())
  vec_assert(strata, ptype = character())
  vec_assert(family, ptype = character())
  vec_assert(source, ptype = character())
  vec_assert(pattern, ptype = character())
  vec_assert(status, ptype = character())

  new_rcrd(
    fields = list(
      "formula" = formula,
      "left" = left,
      "right" = right,
      "outcomes" = outcomes,
      "predictors" = predictors,
      "exposures" = exposures,
      "confounders" = confounders,
      "mediators" = mediators,
      "unknowns" = unknowns,
      "strata" = strata,
      "family" = family,
      "source" = source,
      "pattern" = pattern,
      "status" = status
    ),
    class = "formula_archetype"
  )
}

#' @keywords internal
#' @noRd
methods::setOldClass(c("formula_archetype", "rcrds_rcrd"))

# Output -----------------------------------------------------------------------

#' @export
format.formula_archetype <- function(x, ...) {
  field(x, "formula")
}


#' @export
obj_print_data.formula_archetype <- function(x, ...) {

  # Colorful printing
  if (vec_size(x) == 0) {
    fmt <- new_script()
  } else {

    # Depending on length
    if (length(x) > 1) {
      cat(format(x), sep = "\n")
    } else {
      cat(format(x))
    }
  }
}

#' @export
vec_ptype_full.formula_archetype <- function(x, ...) {
  "formula_archetype"
}

#' @export
vec_ptype_abbr.formula_archetype <- function(x, ...) {
  "fmls"
}

# Casting and coercion ---------------------------------------------------------

#' @export
vec_ptype2.formula_archetype.formula_archetype <- function(x, y, ...) {
  x
}

#' @export
vec_cast.formula_archetype.formula_archetype <- function(x, to, ...) {
  x
}


#' @export
vec_ptype2.formula_archetype.character <- function(x, y, ...) {
  y
}

#' @export
vec_ptype2.character.formula_archetype <- function(x, y, ...) {
  x
}

#' @export
vec_cast.character.formula_archetype <- function(x, to, ...) {
  format(x) # Returns a character class by default
}

#' @export
vec_ptype2.formula_archetype.term_archetype <- function(x, y, ...) {
  y
}

#' @export
vec_ptype2.term_archetype.formula_archetype <- function(x, y, ...) {
  x
}

#' @export
vec_cast.term_archetype.formula_archetype <- function(x, to, ...) {
  term_archetype.formula_archetype(x)
}

#' @export
vec_ptype2.formula_archetype.script <- function(x, y, ...) {
  x
}

#' @export
vec_ptype2.script.formula_archetype <- function(x, y, ...) {
  y
}

#' @export
vec_cast.formula_archetype.script <- function(x, to, ...) {
  format(x) |>
    stats::as.formula() |>
    formula_archetype.formula()
}

#' @export
formula.formula_archetype <- function(x, ...) {
  lapply(x, FUN = function(.x) {
    format(.x) |>
      stats::as.formula()
  })
}
