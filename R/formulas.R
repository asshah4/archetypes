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
#'   * `term_rcrd`
#'
#' @param ... Arguments to be passed to or from other methods
#'
#' @name formula_vector
formula_vctr <- function(x = term_rcrd(), ...) {
	UseMethod("formula_vctr", object = x)
}

#' @rdname formula_vector
#' @param groups List of formulas that have the term (or terms) on the LHS and
#'   the group name on the RHS (quotations to indicate character value not
#'   necessary). E.g. `list(c(x1, x2) ~ grp)`
#' @export
formula_vctr.term_rcrd <- function(x = term_rcrd(),
																	 groups = list(),
																	 pattern = character(),
																	 ...) {

	# Early break if not viable method dispatch
	if (length(x) == 0) {
		return(new_formula_vctr())
	}

	# Check pattern
	if (length(pattern) == 0) {
		pattern <- "default"
	}
	if (!pattern %in% c("default", "sequential", "parallel")) {
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
	new_formula_vctr(
			formulas = formulas,
			operations = ops,
			terms = terms,
			state = state
	)
}

#' @rdname formula_vector
#' @export
formula_vctr.default <- function(x, ...) {
	stop(
		"`formula_vctr()` is not defined for a `", class(x)[1], "` object.",
		call. = FALSE
	)
}

# Vectors ----

#' Formula vector
#' @keywords internal
#' @noRd
new_formula_vctr <- function(formulas = character(),
														 operations = list(),
														 terms = term_rcrd(),
														 state = logical()) {

	vec_assert(formulas, ptype = character())
	vec_assert(operations, ptype = list())
	vec_assert(terms, ptype = term_rcrd())
	vec_assert(state, ptype = logical())

	new_vctr(
		formulas,
		operations = operations,
		terms = terms,
		state = state,
		class = "formula_vctr"
	)

}

#' @keywords internal
#' @noRd
methods::setOldClass(c("formula_vctr", "vctrs_vctr"))

#' @export
format.formula_vctr <- function(x, ...) {

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
obj_print_data.formula_vctr <- function(x, ...) {
	if (vec_size(x) > 0) {
		cat(format(x), sep = "\n")
	} else {
		cat(format(x))
	}
}

#' @export
vec_ptype_abbr.formula_vctr <- function(x, ...) {
	"f_vctr"
}

# Casting and coercion ----

### self

#' @export
vec_ptype2.formula_vctr.formula_vctr <- function(x, y, ...) {
	x
}

#' @export
vec_cast.formula_vctr.formula_vctr <- function(x, to, ...) {
	x
}

### characters

#' @export
vec_ptype2.formula_vctr.character <- function(x, y, ...) {
	y
}

#' @export
vec_ptype2.character.formula_vctr <- function(x, y, ...) {
	x
}

#' @export
vec_cast.character.formula_vctr <- function(x, to, ...) {
	attributes(x) <- NULL
	as.character(x)
}

