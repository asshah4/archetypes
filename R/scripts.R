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
#' @return An object of class `formula_script`
#' @name formula_script
#' @export
prescribe <- function(x = unspecified(), ...) {
	UseMethod("prescribe", object = x)
}

#' @rdname formula_script
#' @export
prescribe.formula <- function(x,
							  role = list(),
							  group = list(),
							  label = list(),
							  ...) {

	# Break early if zero length
	if (length(x) == 0) {
		return(new_script())
	}

	# terms list (nested for field length equivalence)
	# Updated attributes/components internally
	t <-
		term_archetype(x) |>
		set_roles(roles = formula_args_to_list(role)) |>
		set_groups(groups = formula_args_to_list(group)) |>
		set_labels(labels = formula_args_to_list(label))

	# Create simplified formula
	tm <- vec_data(t)

	# Obtain sides for formula
	left <-
		tm[tm$side == "left", ] |>
		vec_restore(to = term_archetype()) |>
		list()
	right <-
		tm[tm$side == "right", ] |>
		vec_restore(to = term_archetype()) |>
		list()
	meta <-
		tm[tm$side == "meta", ] |>
		vec_restore(to = term_archetype()) |>
		list()

	# Return
	new_script(
		left = left,
		right = right,
		meta = meta
	)
}

#' @rdname formula_script
#' @export
prescribe.term_archetype <- function(x,
									 role = list(),
									 group = list(),
									 label = list(),
									 ...) {


	# Early break
	if (length(x) == 0) {
		message(
			paste0(
				"No `",
				class(x)[1],
				"` object was provided, resulting in a [0] length ",
				"`prescribe` object."
			)
		)
		return(new_script())
	}

	# Updated attributes/components internally
	t <-
		x |>
		set_roles(roles = formula_args_to_list(role)) |>
		set_groups(groups = formula_args_to_list(group)) |>
		set_labels(labels = formula_args_to_list(label))

	# Create simplified formula
	tm <- vec_data(t)

	# Obtain sides for formula
	left <-
		tm[tm$side == "left", ] |>
		vec_restore(to = term_archetype()) |>
		list()
	right <-
		tm[tm$side == "right", ] |>
		vec_restore(to = term_archetype()) |>
		list()
	meta <-
		tm[tm$side == "meta", ] |>
		vec_restore(to = term_archetype()) |>
		list()

	# Return
	new_script(
		left = left,
		right = right,
		meta = meta
	)
}


#' @rdname formula_script
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

#' @rdname formula_script
#' @export
rx = prescribe

# Vector Creation --------------------------------------------------------------

#' Formula vector
#' @keywords internal
#' @noRd
new_script <- function(left = list(),
						right = list(),
						meta = list()) {

	# Validation of types
	vec_assert(left, ptype = list())
	vec_assert(right, ptype = list())
	vec_assert(meta, ptype = list())

	# Each of these should be wrapped in a list to allow equal lengths
	new_rcrd(
		fields = list(
			"left" = left,
			"right" = right,
			"meta" = meta
		),
		class = "formula_script"
	)
}

#' @keywords internal
#' @noRd
methods::setOldClass(c("formula_script", "vctrs_vctr"))

# Output -----------------------------------------------------------------------

#' @export
format.formula_script <- function(x, ...) {


	# Character representation of formula
	if (vec_size(x) == 0) {
		fmt <- new_script()
	} else {

		fd <- vec_data(x)
		left <- fd$left[[1]]
		right <- fd$right[[1]]

		fmt <-
			paste(left, collapse = " + ") |>
			paste(paste(right, collapse = " + "), sep = " ~ ")

	}

	# Return
	fmt

}

#' @export
obj_print_data.formula_script <- function(x, ...) {

	# Colorful printing
	if (vec_size(x) == 0) {
		fmt <- new_script()
	} else {

		fd <- vec_data(x)
		left <- fd$left[[1]]
		right <- fd$right[[1]]

		fmt <-
			paste(format(left), collapse = " + ") |>
			paste(paste(format(right), collapse = " + "), sep = " ~ ") |>
			vec_cast(character())

		# Depending on length
		if (length(x) > 1) {
			cat(fmt, sep = "\n")
		} else {
			cat(fmt)
		}

	}
}

#' @export
vec_ptype_full.formula_script <- function(x, ...) {
	"script"
}

#' @export
vec_ptype_abbr.formula_script <- function(x, ...) {
	"rx"
}

# Casting and coercion ---------------------------------------------------------

# Arithmetic
vec_arith.formula_script <- function(op, x, y, ...) {
	UseMethod("vec_arith.formula_script", y)
}

vec_arith.formula_script.default <- function(op, x, y, ...) {
	stop_incompatible_op(op, x, y)
}


### self

#' @export
vec_ptype2.formula_script.formula_script <- function(x, y, ...) {
	x
}

#' @export
vec_cast.formula_script.formula_script <- function(x, to, ...) {
	x
}

### characters

#' @export
vec_ptype2.formula_script.character <- function(x, y, ...) {
	y
}

#' @export
vec_ptype2.character.formula_script <- function(x, y, ...) {
	x
}

#' @export
vec_cast.character.formula_script <- function(x, to, ...) {
	format(x) # Returns a character class by default
}

### term_archetype

#' @export
vec_ptype2.formula_script.term_archetype <- function(x, y, ...) {
	y
}

#' @export
vec_ptype2.term_archetype.formula_script <- function(x, y, ...) {
	x
}

#' @export
vec_cast.term_archetype.formula_script <- function(x, to, ...) {
	term_archetype.formula_script(x)
}

### base formula

#' @export
formula.formula_script <- function(x, ...) {
	format(x) |>
		as.formula()
}