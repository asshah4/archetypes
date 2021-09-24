#' Prescribing a Formula
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' This function defines a new modified `formula` class of type `rx`. It expands
#' upon the functionality of formulas.
#'
#' @return An `rx` object, which has the classes of `c("rx", "formula")`
#'
#' @param f An object of class `formula`
#'
#'
#' @param ... Further arguments passed to or from other methods
#'
#' @details
#'
#' When supplying a `formula` object, each RHS term of the formula is
#' considered an outcome variable, and is analyzed as a single outcome. Each
#' LHS term is consider a predictor, and can be modified as below:
#'
#' TODO an explanation below
#'
#' @export
rx <- function(f, ...) {
	UseMethod("rx", object = f)
}

#' @rdname rx
#' @export
rx.formula <- function(f, ...) {

	# Deparse the formula in search for current roles via special terms
	role_list <- deparse_formula(f)
	validate_roles(f, role_list)
	role_table <- list_to_table(role_list)

	# Left and right hand sides
	lhs <-
		getOption("rx.lhs") %>%
		role_list[[.]] %>%
		paste(., collapse = " + ")
	rhs <-
		getOption("rx.rhs") %>%
		role_list[[.]]

	# The formula should be simplified with removal of special characters
	new_formula <- stats::reformulate(rhs, lhs)


	# Return new class
	new_rx(
		new_formula,
		roles = role_table
	)

}

#' @rdname rx
#' @export
rx.default <- function(f, ...) {
	stop(
		"`rx()` is not defined for a `", class(f)[1], "` object.",
		call. = FALSE
	)
}

#' Constructor for `rx` class
#' @noRd
new_rx <- function(f, roles) {

	structure(
		f,
		roles = roles,
		class = c("rx", class(f))
	)

}

