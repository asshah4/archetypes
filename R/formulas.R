# Formula class ----------------------------------------------------------------

#' Formula Archetype
#' @name formula
#' @export
formula_archetype <- function(x = unspecified(), ...) {
  UseMethod("formula_archetype", object = x)
}

#' @rdname formula
#' @export
formula_archetype.character <- function(x = character(),
                                        outcomes = character(),
                                        predictors = character(),
                                        exposures = character(),
                                        confounders = character(),
                                        mediators = character(),
                                        unknowns = character(),
                                        parent = character(),
                                        origin = character(),
                                        pattern = character(),
                                        stage = character(),
                                        ...) {

  # Early Break if needed
  if (validate_empty(x)) {
    return(new_formula())
  }

  # Recreate formula
  f <- stats::as.formula(x)
  left <- paste(lhs(f), collapse = " + ")
  right <- paste(rhs(f), collapse = " + ")
  f <- paste(left, right, sep = " ~ ")

  new_formula(
    formula = f,
    left = left,
    right = right,
    outcomes = list(outcomes),
    predictors = list(predictors),
    exposures = list(exposures),
    confounders = list(confounders),
    mediators = list(mediators),
    unknowns = list(unknowns),
    parent = parent,
    origin = origin,
    pattern = pattern,
    stage = stage
  )
}

#' @rdname formula
#' @export
formula_archetype.term_archetype <- function(x, ...) {

  # Early Break if needed
  if (validate_empty(x)) {
    return(new_formula())
  }

  # Basic sides of a formula
  left <- paste(lhs(x), collapse = " + ")
  right <- paste(rhs(x), collapse = " + ")
  f <- paste(left, right, sep = " ~ ")

  # Underlying terms and their roles
  rls <- roles(x)

  # Roles
  outcomes <- names(rls[rls == "outcome"])
  predictors <- names(rls[rls == "predictor"])
  exposures <- names(rls[rls == "exposure"])
  confounders <- names(rls[rls == "confounder"])
  mediators <- names(rls[rls == "mediator"])
  unknowns <- names(rls[rls == "unknown"])

  # Parent of this archetype (as is from terms)
  parent <- f

  if (length(outcomes) > 0 | length(exposures) > 0) {
    stage <- "complex"
  } else {
    stage <- "simple"
  }

  # Return
  new_formula(
    formula = f,
    left = left,
    right = right,
    outcomes = list(outcomes),
    predictors = list(predictors),
    exposures = list(exposures),
    confounders = list(confounders),
    mediators = list(mediators),
    unknowns = list(unknowns),
    parent = f,
    origin = class(x)[1],
    pattern = "none",
    stage = stage
  )
}


#' @rdname formula
#' @export
formula_archetype.formula <- function(x, ...) {


  # Early Break if needed
  if (validate_empty(x)) {
    return(new_formula())
  }

  # Underlying terms and their roles
  t <- tm(x)
  left <- paste(lhs(t), collapse = " + ")
  right <- paste(rhs(t), collapse = " + ")
  f <- paste(left, right, sep = " ~ ")

  # Roles
  rls <- roles(t)
  outcomes <- names(rls[rls == "outcome"])
  exposures <- names(rls[rls == "exposure"])
  predictors <- names(rls[rls == "predictor"])
  confounders <- names(rls[rls == "confounder"])
  mediators <- names(rls[rls == "mediator"])
  unknowns <- names(rls[rls == "unknown"])

  # Parent of this archetype (as is from terms)
  parent <- deparse1(x)

  # Stage
  if (length(outcomes) > 0 | length(exposures) > 0) {
    stage <- "complex"
  } else {
    stage <- "simple"
  }

  # Return
  new_formula(
    formula = f,
    left = left,
    right = right,
    outcomes = list(outcomes),
    predictors = list(predictors),
    exposures = list(exposures),
    confounders = list(confounders),
    mediators = list(mediators),
    unknowns = list(unknowns),
    parent = f,
    origin = class(x)[1],
    pattern = "none",
    stage = stage
  )
}

#' @rdname formula
#' @export
formula_archetype.default <- function(x = unspecified(), ...) {
  # Early break
  if (length(x) == 0) {
    return(new_formula())
  }

  stop("`formula_archetype()` is not defined for a `",
    class(x)[1],
    "` object.",
    call. = FALSE
  )
}


#' @rdname formula
#' @export
fmls <- formula_archetype

# Record definition ------------------------------------------------------------

#' Record of formula archetypes
#' @keywords internal
#' @noRd
new_formula <- function(formula = character(),
                        left = character(),
                        right = character(),
                        outcomes = list(),
                        predictors = list(),
                        exposures = list(),
                        confounders = list(),
                        mediators = list(),
                        unknowns = list(),
                        parent = character(),
                        origin = character(),
                        pattern = character(),
                        stage = character()) {

  # Validation
  vec_assert(formula, ptype = character())
  vec_assert(left, ptype = character()) # A string, unabridged
  vec_assert(right, ptype = character()) # A string, unabridged
  vec_assert(outcomes, ptype = list())
  vec_assert(predictors, ptype = list())
  vec_assert(exposures, ptype = list())
  vec_assert(mediators, ptype = list())
  vec_assert(confounders, ptype = list())
  vec_assert(unknowns, ptype = list())
  vec_assert(parent, ptype = character())
  vec_assert(origin, ptype = character())
  vec_assert(pattern, ptype = character())

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
      "parent" = parent,
      "origin" = origin,
      "pattern" = pattern,
      "stage" = stage
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
