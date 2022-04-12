# Formula class ----------------------------------------------------------------

#' Formula Archetype
#'
#' @param order Describes the requested order that formulas should be decomposed
#'   into. The default is to return ALL formula decompositions. Options include
#'   any integer vector between _1_ and _4_. The default is `2L:4L`, which
#'   includes every functional formula and its parent.
#'   are returned.
#' @name formula
#' @export
formula_archetype <- function(x = unspecified(),
                              role = list(),
                              tier = list(),
                              label = list(),
                              strata = character(),
                              pattern = character(),
                              lineage = list(),
                              order = 2L:4L,
                              ...) {

  # Early break and validation
  if (length(x) == 0) {
    return(new_formula())
  }

  if (!any(
    c(
      "script",
      "term_archetype",
      "formula_archetype",
      "character",
      "formula"
    ) %in% class(x)
  )) {
    stop("`formula_archetype()` is not defined for a `",
      class(x)[1],
      "` object.",
      call. = FALSE
    )
  }

  if (is.numeric(order) & !all(order %in% 1:4)) {
    stop("The order should be an integer range between 1 to 4.")
  }

  if ("script" %in% class(x) & length(x) > 1) {
    stop("`formula_archetype()` can only accept 1 `script` object at a time.")
  }

  # Extract and/or generate terms
  if ("character" %in% class(x)) {
    t <- tm(stats::formula(x))
  } else if ("formula" %in% class(x)) {
    t <- tm(x)
  } else if ("script" %in% class(x)) {
    t <- tm(x)
    pattern <- field(x, "pattern")
  } else if ("term_archetype" %in% class(x)) {
    t <- x
  }

  # Add strata back in (if not already available)
  if (length(strata) > 0) {
    t <- add_strata(t, strata)
  } else {
    strata <- names(roles(t)[roles(t) == "strata"])
    if (length(strata) == 0) {
      strata <- NA_character_
    }
  }

  # Update terms
  t <-
    t |>
    set_roles(roles = formula_args_to_list(role)) |>
    set_tiers(tiers = formula_args_to_list(tier)) |>
    set_labels(labels = formula_args_to_list(label))

  # Generate or re-generate script, should be only a single script to start
  # Filter out just the second order formulas for now
  ancestor <- deparse1(stats::formula(t))
  n <- decipher(t)
  s <- rx(t, pattern = pattern)
  for (i in 1:(n - min(order))) {
    s <- recompose_roles(s)
  }

  # Turn each of these into formulas
  fa <- formula_archetype()
  for (i in seq_along(s)) {

    # From all of the scripts, obtain the expansion patterns if possible
    fl <- decompose_patterns(s[i])
    tl <- field(s[i], "terms")[[1]]

    for (j in seq_along(fl)) {
      tms <- match_terms(tl, stats::formula(fl[[j]]))
      rls <- roles(tms)

      f <- new_formula(
        formula = fl[[j]],
        left = list(lhs(t)),
        right = list(rhs(t)),
        outcome = list(names(rls[rls == "outcome"])),
        exposure = list(names(rls[rls == "exposure"])),
        confounder = list(names(rls[rls == "confounder"])),
        mediator = list(names(rls[rls == "mediator"])),
        unknown = list(names(rls[rls == "unknown"])),
        strata = strata,
        pattern = field(s[i], "pattern"),
        ancestor = ancestor,
        order = field(s[i], "order")
      )

      fa <- append(fa, f)
    }
  }

  # Return formulas
  fa |>
    vec_data() |>
    {
      \(.x) {.x[.x$order %in% order, ]}
    }() |>
    vec_restore(to = fmls()) |>
    unique()
}

#' @rdname formula
#' @export
fmls <- formula_archetype

# Record definition ------------------------------------------------------------

#' Record of formula archetypes
#' @keywords internal
#' @noRd
new_formula <- function(formula = character(),
                        left = list(),
                        right = list(),
                        outcome = list(),
                        exposure = list(),
                        confounder = list(),
                        mediator = list(),
                        unknown = list(),
                        strata = character(),
                        pattern = character(),
                        ancestor = character(),
                        order = integer()) {

  # Validation will depend
  vec_assert(formula, ptype = character())
  vec_assert(left, ptype = list())
  vec_assert(right, ptype = list())
  vec_assert(outcome, ptype = list())
  vec_assert(exposure, ptype = list())
  vec_assert(confounder, ptype = list())
  vec_assert(mediator, ptype = list())
  vec_assert(unknown, ptype = list())
  vec_assert(strata, ptype = character())
  vec_assert(pattern, ptype = character())
  vec_assert(ancestor, ptype = character())
  vec_assert(order, ptype = integer())

  new_rcrd(
    fields = list(
      "formula" = formula,
      "left" = left,
      "right" = right,
      "outcome" = outcome,
      "exposure" = exposure,
      "confounder" = confounder,
      "mediator" = mediator,
      "unknown" = unknown,
      "strata" = strata,
      "pattern" = pattern,
      "ancestor" = ancestor,
      "order" = order
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
