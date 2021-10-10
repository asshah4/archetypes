#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL

#' Identify if an Object has been Named
#'
#' A simple function to help identify if an object has names. If it is a `list`
#' or `vector`, then each element should be named.
#'
#' @return Returns `TRUE` or `FALSE` depending on if the object has been named
#'
#' @param x An object to be tested
#'
#' @param ... Further arguments passed to or from other methods
#'
#' @export
is.named <- function(x, ...) {

	classes <- class(x)
	n <- length(x)
	nms <- names(x)

	# Exclude NULL first
	if (is.null(nms)) {
		return(FALSE)
	}

	nms[nms == ""] <- NA
	nms <- stats::na.omit(nms)

	# By length
	if (length(nms) == n) {
		return(TRUE)
	} else {
		return(FALSE)
	}

}

#' Convert a List into a Table
#'
#' Expands a list of character vectors into a tidy, logic table.
#'
#' @return A `data.frame` object
#'
#' @param named_list A `list` object that is named
#'
#' @param id Name of column that contains terms
#'
#' @param ... Further arguments passed to or from other methods
#'
#' @export
list_to_table <- function(named_list, id = "terms", ...) {

	# Create table/matrix of roles
	list_table <- suppressWarnings(utils::stack(named_list))
	names(list_table) <- c(id, "roles")
	list_table$.id <- TRUE
	list_table <- stats::reshape(list_table, direction = "wide", idvar = id, timevar = "roles")
	names(list_table) <- gsub("\\.id\\.", "", names(list_table))
	list_table[is.na(list_table)] <- FALSE

	# Return
	list_table

}

#' Convert a Logical Table into a List
#'
#' Takes a `data.frame` and uses the columns to generate a named list.
#'
#' @param df A `data.frame` object with logical values for each term
#'
#' @param ... Further arguments passed to or from other methods
#'
#' @export
table_to_list <- function(df, ...) {

	named_list <-
		tidyr::pivot_longer(df, -1) %>%
		.[.$value == TRUE, ] %>%
		.[-3] %>%
		utils::unstack(.)

	if (is.data.frame(named_list)) {
		named_list <- as.list(named_list)
	}

	# Return
	named_list

}

#' Get left hand side terms
#' @keywords internal
get_lhs <- function(f) {

	f[[2]] %>%
		deparse(.) %>%
		strsplit(., "\ \\+\ ") %>%
		unlist(.) %>%
		gsub(" ", "", .)

}

#' Get right hand side terms. If tidy = TRUE, then return without wrappers.
#' @keywords internal
get_rhs <- function(f, tidy = TRUE) {

	if (tidy) {
		rhs <-
			paste(f[2], f[1], f[3], collapse = "") %>%
			strsplit(., " ~ ") %>%
			unlist(.) %>%
			.[2] %>%
			paste("~", .) %>%
			stats::formula(.) %>%
			all.vars()
	} else {
		rhs <-
			labels(stats::terms(f))
	}

	# Return
	rhs

}

#' Add parent environment back to formula
#' @keywords internal
give_env <- function(x, env = parent.frame()) {
	environment(x) <- env
	x
}

#' Obtain environment of original formula
#' @keywords internal
get_env <- function(x) {
	env <- environment(x)
	env
}
