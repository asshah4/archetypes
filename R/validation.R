#' Validate the roles argument
#' @noRd
validate_roles <- function(f, roles) {

	# Requires named list
	if (!is.list(roles) || !is.named(roles)) {
		stop(
			"The object passed to the `roles` argument needs to be a named list pair.",
			call. = FALSE
		)
	}

	# Variables from both formula and what is specified in the roles
	original_vars <- all.vars(f)
	role_vars <-
		roles %>%
		unlist() %>%
		unname() %>%
		unique() %>%
		paste(., collapse = " + ") %>%
		paste("~", .) %>%
		stats::formula() %>%
		all.vars()

	# Requires roles to have appropriate terms
	if (!all(role_vars %in% original_vars)) {
		stop(
			"The variables in the `roles` argument are not terms within the formula.",
			call. = FALSE
		)
	}

	invisible(TRUE)

}

#' Validate class of objects
#' @noRd
validate_class <- function(x, what) {

	if (!inherits(x, what)) {
		stop(
			deparse(substitute(x)),
			" needs to inherit from `",
			paste("c(", paste(what, collapse = ", "),
						")", sep = ""),
			"`, but is of class `",
			class(x),
			"`.",
			call. = FALSE
		)
	}

	invisible(TRUE)

}
