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

	# Requires roles to have appropriate terms
	user_vars <- unname(unlist(roles))
	form_vars <- all.vars(f)

	if (!all(user_vars %in% form_vars)) {
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
