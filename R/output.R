#' @export
has_cli <- function() {
	isTRUE(requireNamespace("cli", quietly = TRUE))
}

