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
#' @param ... Arguments to be passed to or from other methods
#'
#' @section Roles:
#'
#' Specific roles the variable plays within the formula. These are of particular
#' importance, as they serve as special terms that can effect how a formula is
#' interpreted. The options for roles are as below:
#'
#' * __exposure__ or `X(...)`: a predictor variable that serves as a primary or
#' key variable in the \eqn{Exposure ~ Outcome} relationship
#'
#' Formulas can be condensed by applying their specific role to individual terms
#' as a function/wrapper. For example, `y ~ X(x1) + x2 + x3`. This would signify
#' that `x1` has the specific role of an exposure.
#'
#' @inheritSection terms Pluralized Arguments
#'
#' @return An object of class `script`
#' @name script
#' @export
prescribe <- function(x = unspecified(), ...) {
	UseMethod("prescribe", object = x)
}

#' @rdname script
#' @export
prescribe.formula <- function(x,
							  role = list(),
							  group = list(),
							  label = list(),
							  pattern = character(),
							  ...) {

	# Early Break if needed
	mc <- match.call()
	if (validate_empty(x, mc)) {
		return(new_term())
	}

	# Check pattern
	if (length(pattern) == 0) {
		pattern <- "direct"
	}
	if (!pattern %in% c("direct", "sequential", "parallel")) {
		stop("The pattern ",
			 deparse(pattern),
			 " is not yet supported.",
			 call. = FALSE)
	}


	# terms list (nested for field length equivalence)
	# Updated attributes/components internally
	t <-
		term_archetype(x) |>
		set_roles(roles = formula_args_to_list(role)) |>
		set_groups(groups = formula_args_to_list(group)) |>
		set_labels(labels = formula_args_to_list(label))

	# Formula
	f <- formula_archetype(t)

	# Return
	new_script(
		formula = f,
		terms = t,
		pattern = pattern
	)

}

#' @rdname script
#' @export
prescribe.term_archetype <- function(x,
									 role = list(),
									 group = list(),
									 label = list(),
									 pattern = character(),
									 ...) {

	# Early Break if needed
	mc <- match.call()
	if (validate_empty(x, mc)) {
		return(new_term())
	}

	# Updated attributes/components internally
	t <-
		x |>
		set_roles(roles = formula_args_to_list(role)) |>
		set_groups(groups = formula_args_to_list(group)) |>
		set_labels(labels = formula_args_to_list(label))

	# Check pattern
	if (length(pattern) == 0) {
		pattern <- "direct"
	}
	if (!pattern %in% c("direct", "sequential", "parallel")) {
		stop("The pattern ",
			 deparse(pattern),
			 " is not yet supported.",
			 call. = FALSE)
	}

	# Formula
	f <- formula_archetype(t)


	# Return
	new_script(
		formula = f,
		terms = t,
		pattern = pattern
	)
}


#' @rdname script
#' @export
prescribe.default <- function(x = unspecified(), ...) {
	# Early break if not viable method dispatch
	if (length(x) == 0) {
		return(new_script())
	} else {
		stop("`prescribe()` is not defined for a `",
			 class(x)[1],
			 "` object.",
			 call. = FALSE)
	}
}

#' @rdname script
#' @export
rx = prescribe

# Vector Creation --------------------------------------------------------------

#' Formula vector
#' @keywords internal
#' @noRd
new_script <- function(formula = formula_archetype(),
					   terms = term_archetype(),
					   pattern = character()) {

	# Validation of types
	vec_assert(terms, ptype = term_archetype())
	vec_assert(formula, ptype = formula_archetype())
	vec_assert(pattern, ptype = character())

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
			"pattern" = pattern
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
		fmt <- new_script()
	} else {
		fmt <-
			field(x, "formula") |>
			format()
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

		t <- field(x, "terms")[[1]]
		tm <- vec_data(t)
		left <- vec_restore(tm[tm$side == "left", ], tm())
		right <- vec_restore(tm[tm$side == "right", ], tm())

		fmt <-
			paste(format(left), collapse = " + ") |>
			paste(paste(format(right), collapse = " + "), sep = " ~ ")

		# Depending on length
		if (length(x) > 1) {
			cat(fmt, sep = "\n")
		} else {
			cat(fmt)
		}

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