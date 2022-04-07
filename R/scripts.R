# Formula Prescription ---------------------------------------------------------

#' Prescriptions
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' This function defines a new modified `formula` class that has been
#' vectorized. It expands upon the functionality of formulas.
#'
#' @param x Objects of the following types can be used as inputs
#'
#'   * `term_archetype`
#'
#' @inheritParams terms
#'
#' @param pattern This is the expansion pattern used to decide how the
#'   covariates will incorporated into the formulas. The options are
#'   `c("direct", "sequential", "parallel")`. See the details for further
#'   explanation.
#'
#'   * __direct__: the covariates will all be included in each formula
#'
#'   * __sequential__: the covariates will be added sequentially, one by one, or
#'   by tiers, as indicated
#'
#'   * __parallel__: the covariates or tiers of covariates will be placed in
#'   parallel

#' @param ... Arguments to be passed to or from other methods
#'
#' @section Roles:
#'
#' Specific roles the variable plays within the formula. These are of particular
#' importance, as they serve as special terms that can effect how a formula is
#' interpreted. The specialized options for roles are as below:
#'
#' * __exposure__ or `X(...)`
#'
#' * __outcome__ or `O(...)` or placement of variable on LHS of formula
#'
#' * __confounder__ or `C(...)`
#'
#' * __mediator__ or `M(...)`
#'
#' * __strata__ or `S(...)`
#'
#' Formulas can be condensed by applying their specific role to individual terms
#' as a function/wrapper. For example, `y ~ X(x1) + x2 + x3`. This would signify
#' that `x1` has the specific role of an exposure.
#'
#' @inheritSection terms Pluralized Arguments
#'
#' @section Patterns:
#'
#' The expansion pattern allows for instructions on how the covariates should be
#' included in different formulas. Below, assuming that _x1_, _x2_, and _x3_ are
#' covariates...
#'
#' \deqn{y ~ x1 + x2 + x3}
#'
#' __Direct__:
#'
#' \deqn{y ~ x1 + x2 + x3}
#'
#' __Seqential__:
#'
#' \deqn{y ~ x1}
#' \deqn{y ~ x1 + x2}
#' \deqn{y ~ x1 + x2 + x3}
#'
#' __Parallel__:
#'
#' \deqn{y ~ x1}
#' \deqn{y ~ x2}
#' \deqn{y ~ x3}
#'
#' @return An object of class `script`
#' @name script
#' @export
prescribe <- function(x = unspecified(),
                      role = list(),
                      tier = list(),
                      label = list(),
                      pattern = character(),
                      ...) {

  # Validate early and if empty, break early
  if (validate_empty(x)) {
    return(new_script())
  }
  validate_class(x, c("term_archetype", "formula"))

  # Check pattern
  if (length(pattern) == 0) {
    pattern <- "direct"
  }
  if (!pattern %in% c("direct", "sequential", "parallel")) {
    stop("The pattern ",
      deparse(pattern),
      " is not yet supported.",
      call. = FALSE
    )
  }

  # Terms list (nested for field length equivalence)
  # Updated attributes/components internally
  t <-
    term_archetype(x) |>
    set_roles(roles = formula_args_to_list(role)) |>
    set_tiers(tiers = formula_args_to_list(tier)) |>
    set_labels(labels = formula_args_to_list(label))

  # Look at composition of terms
  order <- decipher(t)

  # Formula
  f <- deparse1(stats::formula(t))

  # Return
  new_script(
    formula = f,
    terms = t,
    pattern = pattern,
    order = order
  )
}


#' @rdname script
#' @export
rx <- prescribe

# Vector Creation --------------------------------------------------------------

#' Formula vector
#' @keywords internal
#' @noRd
new_script <- function(terms = term_archetype(),
                       formula = character(),
                       pattern = character(),
                       order = integer()) {

  # Validation of types
  vec_assert(terms, ptype = term_archetype())
  vec_assert(formula, ptype = character())
  vec_assert(pattern, ptype = character())
  vec_assert(order, ptype = integer())

  # Bend terms into a list
  if (vec_size(terms) == 0) {
    terms <- term_archetype()
  } else {
    terms <- list(terms)
  }

  # Everything needs to be the same length
  new_rcrd(
    fields = list(
      "formula" = formula,
      "terms" = terms,
      "pattern" = pattern,
      "order" = order
    ),
    class = "script"
  )
}

#' @keywords internal
#' @noRd
methods::setOldClass(c("script", "vctrs_vctr"))

# Output -----------------------------------------------------------------------

#' @export
format.script <- function(x, ...) {


  # Character representation of formula
  if (vec_size(x) == 0) {
    return()
  } else {
    fmt <-
      sapply(x, FUN = function(.x) {
        field(.x, "formula") |>
          format()
      })
  }
  # Return
  fmt
}

#' @export
obj_print_data.script <- function(x, ...) {

  # Colorful printing
  if (vec_size(x) == 0) {
    fmt <- new_script()
  } else {
    fmt <-
      sapply(
        x,
        FUN = function(.x) {
          t <- field(.x, "terms")[[1]]
          f <- stats::formula(field(.x, "formula"))
          left <- match_terms(t, lhs(f))
          right <- match_terms(t, rhs(f))

          f <-
            paste(format(left), collapse = " + ") |>
            paste(paste(format(right), collapse = " + "), sep = " ~ ")

          f
        }
      )

  }

  if (length(fmt) > 1) {
    cat(format(fmt), sep = "\n")
  } else {
    cat(format(fmt))
  }
}

#' @export
vec_ptype_full.script <- function(x, ...) {
  "script"
}

#' @export
vec_ptype_abbr.script <- function(x, ...) {
  "rx"
}

# Casting and coercion ---------------------------------------------------------

# Arithmetic
vec_arith.script <- function(op, x, y, ...) {
  UseMethod("vec_arith.script", y)
}

vec_arith.script.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}


### self

#' @export
vec_ptype2.script.script <- function(x, y, ...) {
  x
}

#' @export
vec_cast.script.script <- function(x, to, ...) {
  x
}

### characters

#' @export
vec_ptype2.script.character <- function(x, y, ...) {
  y
}

#' @export
vec_ptype2.character.script <- function(x, y, ...) {
  x
}

#' @export
vec_cast.character.script <- function(x, to, ...) {
  format(x) # Returns a character class by default
}

### term_archetype

#' @export
vec_ptype2.script.term_archetype <- function(x, y, ...) {
  y
}

#' @export
vec_ptype2.term_archetype.script <- function(x, y, ...) {
  x
}

#' @export
vec_cast.term_archetype.script <- function(x, to, ...) {
  term_archetype.script(x)
}

### base formula

#' @export
formula.script <- function(x, ...) {
  format(x) |>
    stats::as.formula()
}
