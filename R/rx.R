#' Prescribing a Formula
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' This function defines a new modified `formula` class of type `fx`. It expands
#' upon the functionality of formulas.
#'
#' @return An `fx` object, which has the classes of `c("fx", "formula")`
#'
#' @param f An object of class `formula`
#'
#' @param labels A list of formulas specifying variable labels, which can later
#'   be extracted for presentation. e.g. `list(y ~ "Primary Outcome")`. It
#'   defaults to `NULL`, which will use the variable names itself.
#'
#' @details
#'
#' When supplying a `formula` object, each RHS term of the formula is
#' considered an outcome variable, and is analyzed as a single outcome. Each
#' LHS term is consider a predictor, and can be modified as below:
#'
#' TODO an explanation below
#' @export
fx <- function(f, ...) {
	UseMethod("fx", object = f)
}

#' @rdname fx
#' @export
fx.formula <- function(f, labels = NULL, ...) {

	# Deparse the formula in search for current roles via special terms
	role_list <- deparse_formula(f)
	validate_roles(f, role_list)
	role_table <- list_to_table(role_list)

	# Left and right hand sides
	lhs <-
		getOption("fx.lhs") %>%
		role_list[[.]] %>%
		paste(., collapse = " + ")
	rhs <-
		getOption("fx.rhs") %>%
		role_list[[.]]

	# The formula should be simplified with removal of special characters
	new_formula <- stats::reformulate(rhs, lhs)

	# Create a list of labels for extraction later
	all_roles <- role_table$terms
	label_list <- list()
	if (!is.null(labels)) {
		for (i in 1:length(labels)) {
			f <- labels[[i]]
			label_list[as.character(f[[2]])] <- f[[3]]
		}
		# Set named variables
		overlap <- intersect(names(label_list), all_roles)
		label_list <- label_list[overlap]
		# Set unnamed variables
		diffs <- setdiff(all_roles, names(label_list))
		label_list[diffs] <- diffs
	} else if (is.null(labels)) {
		label_list[all_roles] <- all_roles
	}

	# Return new class
	new_fx(
		new_formula,
		roles = role_table,
		labels = label_list
	)

}

#' @rdname fx
#' @export
fx.default <- function(f, ...) {
	stop(
		"`fx()` is not defined for a `", class(f)[1], "` object.",
		call. = FALSE
	)
}

#' Constructor for `fx` class
#' @noRd
new_fx <- function(f, roles, labels) {

	structure(
		f,
		roles = roles,
		labels = labels,
		class = c("fx", class(f))
	)

}




