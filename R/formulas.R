# Formula class ----------------------------------------------------------------

#' Formula Archetype
#'
#' @param order Requested order that formulas are decomposed
#'   into. The default is to return ALL formula decompositions. Options include
#'   any integer inclusive from _1_ to _4_. The default is `2L:4L`, which
#'   includes every functional formula and its parent.
#' @name formula
#' @export
formula_archetype <- function(x = unspecified(),
                              role = list(),
                              tier = list(),
                              label = list(),
                              strata = character(),
                              pattern = character(),
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
  }

  # Update terms
  t <-
    t |>
    set_roles(roles = formula_to_named_list(role)) |>
    set_tiers(tiers = formula_to_named_list(tier)) |>
    set_labels(labels = formula_to_named_list(label))

  # Generate or re-generate script, should be only a single script to start
  # Filter out just the second order formulas for now
  ancestor <- deparse1(stats::formula(t))
  n <- decipher(t)
  s <- rx(t, pattern = pattern)
  o <- min(order)
  while (n >= o) {
    s <- recompose_roles(s)
    n <- n - 1
  }

  # Turn each of these into formulas
  fa <- formula_archetype()
  for (i in seq_along(s)) {

    # From all of the scripts, obtain the expansion patterns if possible
    if (field(s[i], "order") == 1) {
      fl <- field(s[i], "formula")
    } else {
      fl <- decompose_patterns(s[i])
    }
    tl <- field(s[i], "terms")[[1]]

    for (j in seq_along(fl)) {
      fx <- stats::formula(fl[[j]])
      tms <-
        match_terms(tl, fx) |>
        add(get_terms(tl, "role", "strata"))

      f <- new_formula(
        formula = fl[[j]],

          n = length(tms),
        left = list(lhs(fx)),
        right = list(rhs(fx)),
        outcome = list(get_terms(tms, "role", "outcome")),
        predictor = list(get_terms(tms, "role", "predictor")),
        exposure = list(get_terms(tms, "role", "exposure")),
        confounder = list(get_terms(tms, "role", "confounder")),
        mediator = list(get_terms(tms, "role", "mediator")),
        unknown = list(get_terms(tms, "role", "unknown")),
        # Strata may get lost unless brought in from above
        strata = list(get_terms(tl, "role", "strata")),
        pattern = field(s[i], "pattern"),
        ancestor = ancestor,
        order = decipher(tms), # Only program lis if there are strata...
        source = class(x)[1]
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
                        n = integer(),
                        left = list(),
                        right = list(),
                        outcome = list(),
                        predictor = list(),
                        exposure = list(),
                        confounder = list(),
                        mediator = list(),
                        unknown = list(),
                        strata = list(),
                        pattern = character(),
                        ancestor = character(),
                        source = character(),
                        order = integer()) {

  # Validation
  vec_assert(formula, ptype = character())
  vec_assert(n, ptype = integer())

  # Character vectors based on sides
  vec_assert(left, ptype = list())
  vec_assert(right, ptype = list())

  # Roles (in term_archetype() format)
  vec_assert(outcome, ptype = list())
  vec_assert(predictor, ptype = list())
  vec_assert(exposure, ptype = list())
  vec_assert(confounder, ptype = list())
  vec_assert(mediator, ptype = list())
  vec_assert(unknown, ptype = list())
  vec_assert(strata, ptype = list())

  # Specification information
  vec_assert(pattern, ptype = character())
  vec_assert(ancestor, ptype = character())
  vec_assert(source, ptype = character())
  vec_assert(order, ptype = integer())

  new_rcrd(
    fields = list(
      "formula" = formula,
      "n" = n,
      "left" = left,
      "right" = right,
      "outcome" = outcome,
      "predictor" = predictor,
      "exposure" = exposure,
      "confounder" = confounder,
      "mediator" = mediator,
      "unknown" = unknown,
      "strata" = strata,
      "pattern" = pattern,
      "ancestor" = ancestor,
      "order" = order,
      "source" = source
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
