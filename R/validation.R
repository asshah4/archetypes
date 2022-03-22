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
	inherits(x, "term")
}

#' @rdname check
#' @export
is_script <- function(x) {
	inherits(x, "rx")
}

#' @rdname check
#' @export
is_formula <- function(x) {
	inherits(x, "formula")
}
