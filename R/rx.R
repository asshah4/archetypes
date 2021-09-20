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
#' @param roles A named list pair that places each term into a label, such as
#'   `roles = list(outcome = "mpg", exposure = c("wt", "hp")`.
#'
#' @param ... Further arguments passed to or from other methods
#'
#' @details
#'
#' When supplying a `formula` object, each RHS term of the formula is
#' considered an outcome variable, and is analyzed as a single outcome. Each
#' LHS term is consider a predictor, and can be modified as below:
#'
#' * `X()` is placed around a term to define as an independent exposure, which
#' will be placed in separate formulae from any other term marked as an
#' exposure
#'
#' * `F()` is placed around a term for any predictors that should be
#' maintained/fixed in all models, which can include complex terms, such as
#' mixed effects
#'
#' * `C()` is placed around a term for any predictors that are a potential
#' confounders
#'
#' For example, the equation below describes two independent exposures "x1"
#' and "x2" that should be conditionally tested for every level of "z"
#'
#' \deqn{y ~ X(x1) + X(x2) + x3 + x4 + F((1 | z))}
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

