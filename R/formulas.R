# formula vector ----

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
#'
#' @param pattern This is the expansion pattern used to decide how the
#'   covariates will incorporated into the formulas. The options are
#'   `c("direct", "sequential", "parallel")`. See the details for further
#'   explanation.
#'
#'   * __direct__: the covariates will all be included in each formula
#'
#'   * __sequential__: the covariates will be added sequentially, one by one, or
#'   by groups, as indicated
#'
#'   * __parallel__: the covariates or groups of covariates will be placed in
#'   parallel
#'
#' @param ... Arguments to be passed to or from other methods
#'
#' @details
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
#' @return An object of class `formula_rx`
#' @name frx
#' @export
formula_rx <- function(x = term_rx(), ...) {
	UseMethod("formula_rx", object = x)
}

#' @rdname frx
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

	# Add roles as needed
	t$term[!is.na(t$role) & t$role == "exposure"] <-
		paste0("X(", t$term[!is.na(t$role) & t$role == "exposure"], ")")
	t$term[!is.na(t$role) & t$role == "mediator"] <-
		paste0("M(", t$term[!is.na(t$role) & t$role == "mediator"], ")")
	left <- t$term[t$side == "left"]
	right <- t$term[t$side == "right"]


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

	# Return
	new_formula_rx(
			formulas = formulas,
			operations = ops,
			terms = terms
	)
}

#' @rdname frx
#' @export
formula_rx.formula <- function(x = formula(),
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

	f <- x
	x <- term_rx.formula(x)

	# Create simplified formula
	tm <- vec_data(x)
	left <- lhs(f)
	right <- rhs(f)
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

	# Return
	new_formula_rx(
			formulas = formulas,
			operations = ops,
			terms = terms
	)
}

#' @rdname frx
#' @export
formula_rx.default <- function(x, ...) {
	stop(
		"`formula_rx()` is not defined for a `", class(x)[1], "` object.",
		call. = FALSE
	)
}

#' @rdname frx
#' @export
frx = formula_rx


# vctrs ----

#' Formula vector
#' @keywords internal
#' @noRd
new_formula_rx <- function(formulas = character(),
													 operations = list(),
													 terms = term_rx()) {

	vec_assert(formulas, ptype = character())
	vec_assert(operations, ptype = list())
	vec_assert(terms, ptype = term_rx())

	new_vctr(
		formulas,
		operations = operations,
		terms = terms,
		class = "formula_rx"
	)

}

#' @keywords internal
#' @noRd
methods::setOldClass(c("formula_rx", "vctrs_vctr"))

# casting and coercion ----

# Arithmetic
vec_arith.formula_rx <- function(op, x, y, ...) {
	UseMethod("vec_arith.formula_rx", y)
}

vec_arith.formula_rx.default <- function(op, x, y, ...) {
	stop_incompatible_op(op, x, y)
}


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

### term_rx

#' @export
vec_ptype2.formula_rx.term_rx <- function(x, y, ...) {
	y
}

#' @export
vec_ptype2.term_rx.formula_rx <- function(x, y, ...) {
	x
}

#' @export
vec_cast.term_rx.formula_rx <- function(x, to, ...) {

	attr(x, "terms")

}




