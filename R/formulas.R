# Formula vector ----

#' Formula vector
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
#'   * `term_rx`
#'
#' @param ... Arguments to be passed to or from other methods
#'
#' @name prescriptions
#' @export
formula_rx <- function(x = term_rx(), ...) {
	UseMethod("formula_rx", object = x)
}

#' @rdname prescriptions
#'
#' @param roles Specific roles the variable plays within the formula. These are
#'   of particular importance, as they serve as special terms that can effect
#'   how a formula is interpreted. The options for roles are as below:
#'
#'   * __exposure__ or `X(...)`: a predictor variable that serves as a primary
#'   or key variable in the \eqn{Exposure ~ Outcome} relationship
#'
#'   Formulas can be condensed by applying their specific role to individual
#'   terms as a function/wrapper. For example, `y ~ X(x1) + x2 + x3`. This would
#'   signify that `x1` has the specific role of an exposure.
#'
#' @param groups List of formulas that have the term (or terms) on the LHS and
#'   the group name on the RHS (quotations to indicate character value not
#'   necessary). E.g. `list(c(x1, x2) ~ grp)`
#' @export
formula_rx.term_rx <- function(x = term_rx(),
																	 roles = list(),
																	 groups = list(),
																	 pattern = character(),
																	 ...) {

	# Early break if not viable method dispatch
	if (length(x) == 0) {
		return(new_formula_rx())
	}

	# Check pattern
	if (length(pattern) == 0) {
		pattern <- "direct"
	}
	if (!pattern %in% c("direct", "sequential", "parallel")) {
		stop(
			"The pattern ", deparse(pattern), " is not yet supported.",
			call. = FALSE
		)
	}

	# Create simplified formula
	t <- vec_data(x)
	left <- lhs(x)
	right <- rhs(x)
	formulas <- paste(paste(left, collapse = " + "),
										paste(right, collapse = " + "),
										sep = " ~ ")
	formulas <- vec_cast(formulas, character())

	# Update groups
	grps <- formula_args_to_list(groups)
	x <- setGroups(x, groups = grps)

	# Formula level operations, should return a list
	ops <- identify_ops(x, pattern)

	# Term list (nested for field length equivalence)
	terms <- x

	# If ready to be transformed into a normal formula
	state <- if (length(ops$dependent_variables) > 1) {
		FALSE
	} else {
		TRUE
	}

	# Return
	new_formula_rx(
			formulas = formulas,
			operations = ops,
			terms = terms,
			state = state
	)
}

#' @rdname prescriptions
#' @export
formula_rx.default <- function(x, ...) {
	stop(
		"`formula_rx()` is not defined for a `", class(x)[1], "` object.",
		call. = FALSE
	)
}

#' @rdname prescriptions
#' @export
frx = formula_rx

# Vectors ----

#' Formula vector
#' @keywords internal
#' @noRd
new_formula_rx <- function(formulas = character(),
														 operations = list(),
														 terms = term_rx(),
														 state = logical()) {

	vec_assert(formulas, ptype = character())
	vec_assert(operations, ptype = list())
	vec_assert(terms, ptype = term_rx())
	vec_assert(state, ptype = logical())

	new_vctr(
		formulas,
		operations = operations,
		terms = terms,
		state = state,
		class = "formula_rx"
	)

}

#' @keywords internal
#' @noRd
methods::setOldClass(c("formula_rx", "vctrs_vctr"))

#' @export
format.formula_rx <- function(x, ...) {

	f <- vec_data(x)
	info <- f

	# Pasting
	if (length(f) > 0) {
		paste(info, sep = "\n")
	} else {
		paste(info)
	}

}

#' @export
obj_print_data.formula_rx <- function(x, ...) {
	if (vec_size(x) > 0) {
		cat(format(x), sep = "\n")
	} else {
		cat(format(x))
	}
}

#' @export
vec_ptype_abbr.formula_rx <- function(x, ...) {
	"fx"
}

# Casting and coercion ----

### self

#' @export
vec_ptype2.formula_rx.formula_rx <- function(x, y, ...) {
	x
}

#' @export
vec_cast.formula_rx.formula_rx <- function(x, to, ...) {
	x
}

### characters

#' @export
vec_ptype2.formula_rx.character <- function(x, y, ...) {
	y
}

#' @export
vec_ptype2.character.formula_rx <- function(x, y, ...) {
	x
}

#' @export
vec_cast.character.formula_rx <- function(x, to, ...) {
	attributes(x) <- NULL
	as.character(x)
}

