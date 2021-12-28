### Vectors ----

#' Formula frame
#' @keywords internal
#' @noRd
new_formula_frame <- function(formulas = character(),
															operations = list(),
															pattern = character(),
															term_list = list()) {

	# Validity
	vec_assert(formulas, ptype = character(), size = 1)
	vec_assert(operations, ptype = list(), size = 1)
	vec_assert(pattern, ptype = character(), size = 1)
	vec_assert(term_list, ptype = list(), size = 1)

	# Record type
	new_rcrd(
		list(
			"formulas" = formulas, # Simplified formulas
			"operations" = operations, # Formula level emergent transformations
			"pattern" = pattern, # Pattern
			"term_list" = term_list # List of term data
		),
		class = "formula_frame"
	)
}

#' @keywords internal
#' @noRd
methods::setOldClass(c("formula_frame", "vctrs_vctr"))

#' Vectorized formulas using a `formula_frame`
#'
#' @param x

#' * For `formula_frame()` or `rx()`: A `term_vctr` object
#'
#' @param pattern User-dependent methods to apply at the level of the formula at large
#'
#' @name formula
#' @export
rx <- function(x = term(), ...) {
	UseMethod("formula_frame", object = x)
}

#' @rdname formula
#' @export
formula_frame <- function(x = term(), ...) {
	UseMethod("formula_frame", object = x)
}

#' @rdname formula
#' @export
formula_frame.term_vctr <- function(x = term(),
																		pattern = character(),
																		...) {

	# Create simplified formula
	t <- vec_data(x)
	left <- get_terms(x, "left")
	right <- get_terms(x, "right")
	formulas <- paste(paste(left, collapse = " + "),
										paste(right, collapse = " + "),
										sep = " ~ ")
	formulas <- vec_cast(formulas, character())

	# Pattern
	if (length(pattern) == 0) pattern <- "default"
	pattern <- vec_cast(pattern, character())

	# Formula level operations
	ops <- list(data.frame(
		outcomes = length(get_terms(x, "left")),
		exposures = length(t$roles[t$roles == "exposure" & !is.na(t$roles)])
	))

	# Term list (nested for field length equivalence)
	term_list <- list(t)

	# Return
	new_formula_frame(
			formulas = formulas,
			operations = ops,
			pattern = pattern,
			term_list = term_list
	)
}

#' @rdname formula
#' @export
formula_frame.default <- function(x, ...) {
	stop(
		"`formula_frame()` is not defined for a `", class(x)[1], "` object.",
		call. = FALSE
	)
}

### Formating and printing ----

#' @export
format.formula_frame <- function(x, ...) {

	f <- field(x, "formulas")
	info <- f

	# Pasting
	if (length(f) > 0) {
		paste(info, sep = "\n")
	} else {
		paste(info)
	}

}

#' @export
obj_print_data.formula_frame <- function(x, ...) {
	if (vec_size(x) > 0) {
		cat(format(x), sep = "\n")
	} else {
		cat(format(x))
	}
}

#' @export
obj_print_footer.formula_frame <- function(x, ...) {
	# Terms
	t <-
		field(x, "term_list")[[1]] |>
		vec_restore(to = term()) |>
		vec_size()

	# Operations
	ops <-
		field(x, "operations")[[1]] |>
		rowSums()

	# Footer
	cat("# Terms:", t)
	cat("\n")
	cat("# Combinations:", ops)
}

#' @export
vec_ptype_abbr.formula_frame <- function(x, ...) {
	"frmls"
}

### Casting and coercion ---

### self

#' @export
vec_ptype2.formula_frame.formula_frame <- function(x, y, ...) {
	x
}

#' @export
vec_cast.formula_frame.formula_frame <- function(x, to, ...) {
	x
}

