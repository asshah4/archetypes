# list of formulas ----

#' Prescribed Formula Lists
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' This function introduces a super  that combines both the `list` class (and
#' its derivative `list_of`) and the `formula` class.

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
#' @name list_of_formulas
#' @export
list_of_formulas <- function(x = list_of(), ...) {
	UseMethod("list_of_formulas", object = x)
}

#' @rdname list_of_formulas
#' @export
list_of_formulas.formula_rx <- function(x,
										name = deparse1(substitute(x)),
										pattern = character(),
										...) {

	# Early break if not viable method dispatch
	if (length(x) == 0) {
		return(new_list_of_formulas())
	}

	# Check pattern
	ptrn <- attr(x, "pattern")

	if (length(pattern) == 0) {
		pattern <- ptrn
	} else {
		# Default option
		pattern <- "direct"
	}

	if (!pattern %in% c("direct", "sequential", "parallel")) {
		stop(
			"The pattern ", deparse(pattern), " is not yet supported.",
			call. = FALSE
		)
	}

	# Get components from formula
	cl <- as.character(x)
	t <- attr(x, "terms")
	ops <- identify_ops(t, pattern)

	# Get attributes of labels, roles, groups
	labs <- labels.term_rx(t)
	rls <- roles.term_rx(t)
	grps <- groups.term_rx(t)

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
		roles = rls,
		groups = grps
	)
}

#' @rdname list_of_formulas
#' @export
list_of_formulas.default <- function(x = list_of(), ...) {

	# Early break if not viable method dispatch
	if (length(x) == 0) {
		return(new_list_of_formulas())
	} else {
		stop("`list_of_formulas()` is not defined for a `",
			 class(x)[1],
			 "` object.",
			 call. = FALSE)
	}
}

#' @rdname list_of_formulas
#' @export
fmls = list_of_formulas

# vctrs ----

#' Formula list
#' @keywords internal
#' @noRd
new_list_of_formulas <- function(formula_list = list(),
								 labels = list(),
								 roles = list(),
								 groups = list()) {
	new_list_of(x = formula_list,
		ptype = list(),
		class = "list_of_formulas",
		labels = labels,
		roles = roles,
		groups = groups
	)

}

#' @keywords internal
#' @noRd
methods::setOldClass(c("list_of_formulas", "vctrs_vctr"))

# casting and coercion ----

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


