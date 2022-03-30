#' Validate class of objects
#' @keywords internal
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

#' Validate if an empty object is given to a function
#' @keywords internal
#' @noRd
validate_empty <- function(x, fn) {
	# x is the primary argument of the parent function
	n <- length(x)

	# Print message if needed
	if (n == 0) {
		message(
			"An empty `",
			class(x)[1],
			"` was provided, resulting in a [0] length object."
		)
		return(TRUE)
	} else {
		return(FALSE)
	}
}

#' Identification of formula and formula-adjacent objects
#'
#' @param x Confirmation of an object being of the following classes:
#'
#'   * `term`
#'   * `rx`
#'
#' @name check
#' @export
is_term <- function(x) {
	inherits(x, "term_archetype")
}

#' @rdname check
#' @export
is_script <- function(x) {
	inherits(x, "script")
}

#' @rdname check
#' @export
is_formula <- function(x) {
	inherits(x, "formula_archetype")
}
