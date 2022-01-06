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
list_to_table <- function(x, id = "term", val = "ops", ...) {

	tbl <- as.data.frame(cbind(names(x), unlist(unname(x))))
	colnames(tbl) <- c(id, val)
	tbl

}

#' @rdname lists_tbls
#' @export
table_to_list <- function(x, id = "term", ...) {

	validate_class(x, "data.frame")

	if (ncol(x) == 2) {
		tbl <- x
		nms <- tbl[[id]]
		val <- tbl[[which(!colnames(tbl) %in% id)]]
		names(val) <- nms
		return(as.list(val))
	} else if (ncol(x) == 1) {
		tbl <- x
		return(as.list(tbl))
	} else {
		stop("table_to_list() requires there to be a data.frame of either 1 or 2 columns.",
				 call. = FALSE)
	}

}

#' Handling of list-formula arguments
#' Stylistic choice to make arguments entered in the form of a list, with each
#' entry being a formula. The LHS will always be the terms, and the RHS will
#' always be the non-term item (e.g. group, label, role, etc).
#' @keywords internal
#' @noRd
formula_args_to_list <- function(x, ...) {

	validate_class(x, "list")

	pl <- list()

	for (i in seq_along(x)) {

		# Terms (left)
		f <- x[[i]]

		if (class(f[[2]]) == "character" | class(f[[2]]) == "name") {
			t <- as.character(f[[2]])
		} else if (class(f[[2]]) == "call") {
			t <- as.character(f[[2]])[-1]
		}

		# Descriptor (right)
		if (class(f[[3]]) == "character" | class(f[[3]]) == "name") {
			d <- as.character(f[[3]])
		} else if (class(f[[3]]) == "call") {
			d <- as.character(f[[3]])[-1]
		}

		y <- rep(d, length(t))
		names(y) <- t
		pl <- append(pl, y)

	}

	# Return paired/named list
	pl
}

#' Add parent environment back to formula
#' @keywords internal
#' @noRd
setEnv <- function(x, env = parent.frame()) {
	environment(x) <- env
	x
}

#' Obtain environment of original formula
#' @keywords internal
#' @noRd
getEnv <- function(x) {
	env <- environment(x)
	env
}

# Getters ----

#' Get terms from prescribed formulas
#' @name getters
#' @export
lhs <- function(x, ...) {
	UseMethod("lhs", object = x)
}

#' @rdname getters
#' @export
rhs <- function(x, ...) {
	UseMethod("rhs", object = x)
}

#' @rdname getters
#' @export
rhs.term_rcrd <- function(x, ...) {
	tm <- vec_data(x)
	tm$term[tm$side == "right"]
}

#' @rdname getters
#' @export
lhs.term_rcrd <- function(x, ...) {
	tm <- vec_data(x)
	tm$term[tm$side == "left"]
}

#' @rdname getters
#' @export
rhs.formula <- function(x, tidy = FALSE, ...) {
	if (tidy) {
		all <- all.vars(x, functions = FALSE)
		left <- x[[2]] |>
			deparse() |>
			strsplit("\ \\+\ ") |>
			unlist()

		setdiff(all, left)
	} else {
		labels(stats::terms(x))
	}
}

#' @rdname getters
#' @export
lhs.formula <- function(x, ...) {
	x[[2]] |>
		deparse() |>
		strsplit("\ \\+\ ") |>
		unlist()
}

#' @rdname getters
#' @export
getComponent <- function(x, ...) {
	UseMethod("getComponent", object = x)
}


#' @rdname getters
#' @export
getComponent.term_rcrd <- function(x,
																		part,
																		filter_id = NULL,
																		filter_val = NULL,
																		...) {
	tm <- vec_data(x)

	if (is.null(filter_id)) {
		y <-
			tm[!is.na(tm[part]), unique(c("term", part)), drop = FALSE] |>
			table_to_list()

		return(y)
	} else if (!is.null(filter_id) & !is.null(filter_val)) {
		y <-
			tm[!is.na(tm[part]) & tm[filter_id] == filter_val, part] |>
			table_to_list()

		return(y)
	} else {
		stop("Filtering inputs have been set incorrectly.")
	}
}

# Setters ----

#' Set components of terms and formulas
#' @return A modified
#' @name setters
#' @export
setGroups <- function(x, ...) {
	UseMethod("setGroups", object = x)
}

#' @rdname setters
#' @export
setGroups.term_rcrd <- function(x, groups, ...) {

	validate_class(groups, "list")

	# Append groups
	grps <-
		getComponent(x, "group") |>
		append(groups)

	tm <- vec_data(x)

	for (i in seq_along(grps)) {
		tm$group[tm$term == names(grps[i])] <- grps[[i]]
	}

	vec_restore(tm, to = term_rcrd())

}
