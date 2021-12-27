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
#' @param x A named `list` object
#'
#' @param id Name of column that contains terms
#'
#' @param val Name of column that contains specific values
#'
#' @param ... Further arguments passed to or from other methods
#'
#' @export
list_to_table <- function(x, id = "terms", val = "ops", ...) {

	tbl <- as.data.frame(cbind(names(x), unlist(unname(x))))
	colnames(tbl) <- c(id, val)
	tbl

}

#' Convert a Logical Table into a List
#'
#' Takes a `data.frame` and uses the columns to generate a named list. This removes the original column names, as it assumes that the data is contained within the frame itself. It defaults to using the first column as the names of the list.
#'
#' @param x A `data.frame` object with values for each term

#' @param id Name of column that contains terms
#'
#' @param ... Further arguments passed to or from other methods
#'
#' @export
table_to_list <- function(x, id = "terms", ...) {

	nms <- tbl[[id]]
	val <- tbl[[which(!colnames(tbl) %in% id)]]
	names(val) <- nms
	as.list(val)

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

