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

#' Identification of objects in the `forks` package
#'
#' @param x Confirmation of an object being of the following classes:
#'
#'   * `term_rx`
#'   * `formula_rx`
#'
#' @name checkers
#' @export
is_term <- function(x) {
	inherits(x, "term_rx")
}

#' @rdname checkers
#' @export
is_tx <- function(x) {
	inherits(x, "term_rx")
}

