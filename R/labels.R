#' Find Labels from Object
#'
#' Retrieve the term labels from the `rx` object if available. At minimum, will have an _outcomes_ and _predictors_.
#'
#' @return A character vector or list of such vectors from an object of the `rx` class.
#'
#' @param object An object of `rx` class
#'
#' @inheritParams base::labels
#'
#' @export
labels.rx <- function(object, ...) {

	validate_class(object, "rx")

	attributes(object)$roles %>%
		table_to_list()

}

#' Set Custom Labels
#'
#' @description
#'
#' This function allows for setting global options for how `rx` should label
#' individual terms in a formula. The default labels are `lhs` for the response
#' or dependent variables, and the `rhs` are the terms or independent variables,
#' which require no shorthand or wrappers to work. This will exist for the
#' reminder of the working session.
#'
#' @details
#'
#' To modify the defaults, simply provide a named list pair in the argument
#' __labels__ that renames `lhs` and `rhs`. For example, the default labels can
#' be revised using `list(lhs = "outcomes", rhs = "predictors")`.
#'
#' To add new labels, add additional named/paired arguments. The name serves as
#' the wrapper for the formula, creating a shorthand for adding labels. For
#' example, using `list(X = "exposure")` will label any component of a formula
#' as an "exposure" variable. To use the wrapping feature, simply use the
#' __name__ of the list argument in parentheses around the individual term, as
#' below.
#'
#' \deqn{mpg ~ X(wt) + hp + cyl}
#'
#' @return Changes to global options within the working environment.
#'
#' @param labels A named list pair of character vectors that should be used to
#'   "wrap" formula terms, and change their labels. The default options labels
#'   the independent variables as `rhs` and the dependent variables as `lhs`.
#'
#'
#' @name options
#' @export
set_rx_labels <- function(labels) {

	# Current labels should be appended with new labels, overwriting old names
	old_labs <- getOption("rx.labels")
	dupes <- names(old_labs) %in% names(labels)
	old_labs[dupes] <- NULL
	new_labs <- append(old_labs, labels)

	# Primary labels should be preserved and placed into a special category
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
		rx.labels = new_labs
	)

	options(op.rx)
	invisible()

}

#' @rdname options
#' @param theme A preset list of labels customized for specific packages. These
#'   can be expanded via [pull request](https://github.com/asshah4/rx/pulls).
#'
#'   Currently supported themes:
#'
#'   | package | lhs | rhs | labels |
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
				rx.labels = list(X = "exposures", F = "fixed")
			))
		},
		ggdag = {
			options(list(
				rx.labels = list(X = "exposure", Y = "outcome", L = "latent")
			))
		},
		warning("No matching theme was found, thus no theme was set.")
	)

	invisible()

}

#' @rdname options
#' @export
reset_rx_labels <- function() {

	op.rx <- list(
		rx.default = list(
			rx.lhs = "lhs",
			rx.rhs = "rhs"
		),
		rx.lhs = "lhs",
		rx.rhs = "rhs",
		rx.labels = list()
	)

	options(op.rx)
	invisible()

}
