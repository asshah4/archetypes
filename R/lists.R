# list of formulas ----

#' Prescribed Formula Lists
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' This function introduces a super  that combines both the `list` class (and its derivative `list_of`) and the `formula` class.
#'
#' @name list_of_formulas
#' @export
list_of_formulas <- function(x, ...) {
	UseMethod("list_of_formulas", object = x)
}

#' @rdname list_of_formulas
#' @export
list_of_formulas.formula_rx <- function(x = formula_rx(),
																				pattern = character(),
																				name = deparse(substitute(x)),
																				...) {


	# Early break if not viable method dispatch
	if (length(x) == 0) {
		return(new_formula_rx())
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
	lof <- perform_ops(ops)
	names(lof) <-
		sapply(names(lof),
					 function(x) {
					 	paste(name, sep = "_", x)
					 },
					 USE.NAMES = FALSE)

	new_list_of_formulas(
		formula_list = lof,
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

# list_of vctr ----

#' Formula list
#' @keywords internal
#' @noRd
new_list_of_formulas <- function(formula_list = list(),
																 labels = list(),
																 roles = list()) {

	new_list_of(
		x = formula_list,
		ptype = list(),
		class = "list_of_formulas",
		labels = labels,
		roles = roles
	)

}

#' @keywords internal
#' @noRd
methods::setOldClass(c("list_of_formulas", "vctrs_vctr"))


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


# formating and printing ----

#' @export
vec_ptype_full.list_of_formulas <- function(x, ...) {
	"list_of_formulas"
}

#' @export
vec_ptype_abbr.list_of_formulas <- function(x, ...) {
	"fmls"
}

# fitting ----

#' Fitting a list of formulas
#'
#' @return A list of model fits
#'
#' @param object A `list_of_formulas` that can be fit by a modeling function,
#'   such as [stats::lm()]
#'
#' @rdname fit
#' @export
fit.list_of_formulas <- function(object, .f, ..., data) {
	validate_class(data, c("tbl_df", "data.frame"))
	args <- list(...)
	args$data <- quote(data)

	y <- lapply(object, function(.x) {
		f <- .x
		do.call(".f", args = c(formula = f, args))
	})

	y
}
