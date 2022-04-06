# Formula class ----------------------------------------------------------------

#' Formula Archetype
#' @name formula
#' @export
formula_archetype <- function(x = unspecified(),
                              role = list(),
                              tier = list(),
                              label = list(),
                              strata = character(),
                              pattern = character(),
                              lineage = list(),
                              order = integer(),
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
    if (length(strata) > 0) {
      y <- add_strata(y, strata)
    }
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

  # Strata
  if (length(strata) == 0) {
    strata <- NA_character_
  }

  # Pattern
  if (length(pattern) == 0) {
    pattern <- NA_character_
  }

  #############
  ### ORDER ###
  #############

    # ZEROETH
      # Only single term object
    # FIRST
      # Does not follow rules of roles
      # LHS = 1
      # RHS = 1
    # SECOND
      # Follows rules of roles
      # LHS = 1
      # RHS = exposure + confounder
      # RHS = mediator (no confounders allowed)
      # RHS =/= outcome
    # THIRD
      # Does not follow rules of roles
      # LHS = 1
      # RHS > 1 exposure
      # RHS > 1 mediator
      # RHS = exposure + mediator
    # FOURTH
      # LHS > 1
  vt <- vec_data(t)
  rls <- roles(t)
  outcome <- names(rls[rls == "outcome"])
  predictor <- names(rls[rls == "predictor"])
  exposure <- names(rls[rls == "exposure"])
  confounder <- names(rls[rls == "confounder"])
  mediator <- names(rls[rls == "mediator"])
  unknown <- names(rls[rls == "unknown"])
  strata <- names(rls[rls == "strata"])
  out <- length(outcome)
  exp <- length(exposure)
  prd <- length(c(confounder, predictor))
  med <- length(mediator)
  unk <- length(unknown)

  # This will check the complexity and break down of the formulas
  # Rules are...


  # Lineage

  new_formula(
    formula = f,
    outcome = outcome,
    exposure = list(exposure),
    confounder = list(confounder),
    mediator = list(mediator),
    unknown = list(unknown),
    strata = strata,
    order = order,
    lineage = lineage
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
                        outcome = character(),
                        exposure = list(),
                        confounder = list(),
                        mediator = list(),
                        unknown = list(),
                        strata = character(),
                        order = integer(),
                        lineage = list()) {

  # Validation will depend
  vec_assert(formula, ptype = character())
  vec_assert(outcome, ptype = character())
  vec_assert(exposure, ptype = list())
  vec_assert(confounder, ptype = list())
  vec_assert(mediator, ptype = list())
  vec_assert(unknown, ptype = list())
  vec_assert(strata, ptype = character())
  vec_assert(order, ptype = integer())
  vec_assert(lineage, ptype = list())

  new_rcrd(
    fields = list(
      "formula" = formula,
      "outcome" = outcome,
      "exposure" = exposure,
      "confounder" = confounder,
      "mediator" = mediator,
      "unknown" = unknown,
      "strata" = strata,
      "order" = order,
      "lineage" = lineage
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