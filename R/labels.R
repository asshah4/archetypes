#' Find Labels from Object
#'
#' Retrieve the term labels from the `rx` object if available. At minimum, will
#' have the names of the variables themselves.
#'
#' @return A character vector or list of such vectors from an object of the `rx`
#'   class.
#'
#' @param object An object of `rx` class
#'
#' @inheritParams base::labels
#'
#' @export
labels.rx <- function(object, ...) {

	attributes(object)$labels

}

#' Find Roles from Formula
#'
#' Retrieve the term roles from a statistical object if available.
#'
#' @return A character vector or list of such vectors from an object
#'
#' @param object A generic object that contains a formula that has been given
#'   roles
#'
#' @inheritParams base::labels
#'
#' @export
roles <- function(object) {
	UseMethod("roles")
}

#' @rdname roles
#' @export
roles.rx <- function(object, ...) {

	attributes(object)$roles %>%
		table_to_list()

}

#' Set Custom Roles
#'
#' @description
#'
#' This function allows for setting global options for how `rx` should label
#' individual terms in a formula. The default roles are `lhs` for the response
#' or dependent variables, and the `rhs` are the terms or independent variables,
#' which require no shorthand or wrappers to work. This will exist for the
#' reminder of the working session.
#'
#' @details
#'
#' To modify the defaults, simply provide a named list pair in the argument
#' __roles__ that renames `lhs` and `rhs`. For example, the default roles can
#' be revised using `list(lhs = "outcomes", rhs = "predictors")`.
#'
#' To add new roles, add additional named/paired arguments. The name serves as
#' the wrapper for the formula, creating a shorthand for adding roles. For
#' example, using `list(X = "exposure")` will label any component of a formula
#' as an "exposure" variable. To use the wrapping feature, simply use the
#' __name__ of the list argument in parentheses around the individual term, as
#' below.
#'
#' \deqn{mpg ~ X(wt) + hp + cyl}
#'
#' @return Changes to global options within the working environment.
#'
#' @param roles A named list pair of character vectors that should be used to
#'   "wrap" formula terms, and change their roles. The default options roles
#'   the independent variables as `rhs` and the dependent variables as `lhs`.
#'
#'
#' @name options
#' @export
set_rx_roles <- function(roles) {

	# Current roles should be appended with new roles, overwriting old names
	old_labs <- getOption("rx.roles")
	dupes <- names(old_labs) %in% names(roles)
	old_labs[dupes] <- NULL
	new_labs <- append(old_labs, roles)

	# Primary roles should be preserved and placed into a special category
	new_lhs <- getOption("rx.lhs")
	new_rhs <- getOption("rx.rhs")

	for (i in names(new_labs)) {

		if (i == new_lhs) {
			new_lhs <- new_labs[[i]]
			new_labs[i] <- NULL
		}

		if (i == new_rhs) {
			new_rhs <- new_labs[[i]]
			new_labs[i] <- NULL
		}

	}

	# Options should be updated
	op.rx <- list(
		rx.lhs = new_lhs,
		rx.rhs = new_rhs,
		rx.roles = new_labs
	)

	options(op.rx)
	invisible()

}

#' @rdname options
#' @param theme A preset list of roles customized for specific packages. These
#'   can be expanded via [pull request](https://github.com/asshah4/rx/pulls).
#'
#'   Currently supported themes:
#'
#'   | package | lhs | rhs | roles |
#'   | :--- | --- | --- | --- |
#'   | `murmur` | outcomes | predictors | X = exposure; F = fixed |
#'   | `ggdag` | lhs | rhs | X = exposure; Y = "outcome", L = "latent" |
#'
#' @export
set_rx_theme <- function(theme) {

	switch(
		theme,
		murmur = {
			options(list(
				rx.lhs = "outcomes",
				rx.rhs = "predictors",
				rx.roles = list(X = "exposures", F = "fixed")
			))
		},
		ggdag = {
			options(list(
				rx.roles = list(X = "exposure", Y = "outcome", L = "latent")
			))
		},
		warning("No matching theme was found, thus no theme was set.")
	)

	invisible()

}

#' @rdname options
#' @export
reset_rx_roles <- function() {

	op.rx <- list(
		rx.default = list(
			rx.lhs = "lhs",
			rx.rhs = "rhs"
		),
		rx.lhs = "lhs",
		rx.rhs = "rhs",
		rx.roles = list()
	)

	options(op.rx)
	invisible()

}
