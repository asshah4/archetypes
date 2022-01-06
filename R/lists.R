# List of Formulas ----

#' Prescribed Formula Lists
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' This function introduces a super class that combines both the `list` class (and its derivative `list_of`) and the `formula` class.
#'
#' @name list_of_formulas
#' @export
list_of_formulas <- function(x, ...) {
	UseMethod("list_of_formulas", object = x)
}

#' @rdname list_of_formulas
#' @export
list_of_formulas.formula_rcrd <- function(x = formula_vctr(),
																					pattern = character(),
																					...) {

	# Early break if not viable method dispatch
	if (length(x) == 0) {
		return(new_formula_vctr())
	}

	# Get components from formula
	cl <- as.character(x)
	ops <- attr(x, "operations")
	t <- attr(x, "terms")
	tm <- vec_data(t)

	# Get labels
	labs <- getComponent(t, "label")

	# Get roles
	rls <- getComponent(t, "role")

	# Expansion of formulas
	fl <- perform_ops(t, ops)


	new_list_of_formulas(
		formula_list = fl,
		labels = labs,
		roles = rls
	)
}

#' @rdname list_of_formulas
#' @export
list_of_formulas.default <- function(x, ...) {
	stop(
		"`list_of_formulas()` is not defined for a `", class(x)[1], "` object.",
		call. = FALSE
	)
}

# Vectors ----

#' Formula list
#' @keywords internal
#' @noRd
new_list_of_formulas <- function(formula_list = list(),
																 labels = list(),
																 roles = list()) {

	new_list_of(
		x = formula_list,
		ptype = character(),
		class = "list_of_formulas",
		labels = list(),
		roles = list()
	)

}

#' @keywords internal
#' @noRd
methods::setOldClass(c("list_of_formulas", "vctrs_vctr"))

#' @export
vec_ptype_full.list_of_formulas <- function(x, ...) {
	"list_of_formulas"
}

#' @export
vec_ptype_abbr.list_of_formulas <- function(x, ...) {
	"frmls"
}

# Casting and coercion ----

#' @export
vec_ptype2.vctrs_list_of.character <- function(x, y, ...) {
	x
}

#' @export
vec_ptype2.character.vctrs_list_of <- function(x, y, ...) {
	y
}

#' @export
vec_cast.vctrs_list_of.character <- function(x, to, ...) {
	cl <- as.list(x) # Make list of characters
	loc <- new_list_of(cl, ptype = character()) # Turn into list_of class
	loc # Return list of characters
}

#' @export
vec_cast.character.vctrs_list_of <- function(x, to, ...) {
	cv <- unlist(x) # Flatten list of characters
	cv # Return character vector (named)
}
