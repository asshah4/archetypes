#' Convert between lists and tables
#'
#' @return A `data.frame` or `list` object
#'
#' @param x
#'
#' * For `list_to_table()`: A named `list` object
#'
#' * For `table_to_list()`: A `data.frame` object
#'
#' @param id Name of column that contains terms
#'
#' @param val Name of column that contains specific values
#'
#' @param ... Further arguments passed to or from other methods
#'
#' @details
#'
#' For `table_to_list()`:
#'
#' Takes a `data.frame` and uses the columns to generate a named list. This
#' removes the original column names, as it assumes that the data is contained
#' within the frame itself. It defaults to using the first column as the names
#' of the list.
#'
#' @name lists_tbls
#' @export
list_to_table <- function(x, id = "terms", val = "ops", ...) {

	tbl <- as.data.frame(cbind(names(x), unlist(unname(x))))
	colnames(tbl) <- c(id, val)
	tbl

}

#' @rdname lists_tbls
#' @export
table_to_list <- function(x, id = "terms", ...) {

	validate_class(x, "data.frame")
	tbl <- x
	nms <- tbl[[id]]
	val <- tbl[[which(!colnames(tbl) %in% id)]]
	names(val) <- nms
	as.list(val)

}

# Formulas ----

#' Add parent environment back to formula
#' @keywords internal
#' @noRd
give_env <- function(x, env = parent.frame()) {
	environment(x) <- env
	x
}

#' Obtain environment of original formula
#' @keywords internal
#' @noRd
get_env <- function(x) {
	env <- environment(x)
	env
}

# Terms ----

#' Get terms from prescribed formulas
#' @name getters
#' @export
get_terms <- function(x, side = "both", ...) {
	if ("term_rcrd" %in% class(x)) {
		switch(side,
					 left = {
					 	tm <- vec_data(x)
					 	t <- tm$terms[tm$sides == "left"]
					 	t
					 },
					 right = {
					 	tm <- vec_data(x)
					 	t <- tm$terms[tm$sides == "right"]
					 	t
					 },
					 both = {
					 	tm <- vec_data(x)
					 	t <- tm$terms[!is.na(tm$sides)]
					 	t
					 })

		return(t)
	}
}


#' @rdname getters
#' @export
get_roles <- function(x, role = "all", ...) {
	if ("term_rcrd" %in% class(x)) {
		switch(role,
					 all = {
					 	tm <- vec_data(x)
					 	rls <-
					 		tm[!is.na(tm$roles), c("terms", "roles")] |>
					 		table_to_list()
					 })

		return(rls)
	}
}
