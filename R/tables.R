#' Rebuild a formula-based object as a data frame
#'
#' @return A `data.frame` object that is an explosion of the underlying object,
#'   giving out the underlying terms, patterns, etc, corresponding to the
#'   initial properties of the object formula
#'
#' @name rebuild
#' @export
rebuild <- function(x, ...) {
	UseMethod("rebuild", object = x)
}

