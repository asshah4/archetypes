# Conversion ----

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

# formula tools ----

#' Tools for working with formula-like objects
#' @name sides
#' @export
lhs <- function(x, ...) {
	UseMethod("lhs", object = x)
}

#' @rdname sides
#' @export
rhs <- function(x, ...) {
	UseMethod("rhs", object = x)
}

#' @rdname sides
#' @export
rhs.term_rx <- function(x, ...) {
	tm <- vec_data(x)
	tm$term[tm$side == "right"]
}

#' @rdname sides
#' @export
lhs.term_rx <- function(x, ...) {
	tm <- vec_data(x)
	tm$term[tm$side == "left"]
}

#' @rdname sides
#' @param tidy Logical value to decide if operations should be removed from the terms. If `FALSE`, then the operations will remain included.
#' @export
rhs.formula <- function(x, tidy = FALSE, ...) {

	if (length(x) == 2) {pos <- 2}
	if (length(x) == 3) {pos <- 3}

	if (tidy) {
		x[[pos]] |>
			deparse1() |>
			{\(.x) paste("~", .x)}() |>
			stats::as.formula() |>
			all.vars(functions = FALSE, unique = FALSE)
	} else {
		labels(stats::terms(x))
	}
}

#' @rdname sides
#' @export
lhs.formula <- function(x, tidy = FALSE, ...) {

	if (length(x) == 2) {
		return(character())
	}

	# Shift over to simplify evaluation
	y <-
		x[[2]] |>
		deparse1() |>
		{\(.x) paste("~", .x)}() |>
		stats::as.formula()

	if (tidy) {
		left <- all.vars(y, functions = FALSE, unique = FALSE)
	} else {
		left <- labels(stats::terms(y))
	}

	# Return
	left

}

#' @rdname sides
#' @export
rhs.formula_rx <- function(x, tidy = FALSE, ...) {

	if (tidy) {
		x |>
			as.formula() |>
			{\(.x) .x[[3]]}() |>
			deparse1() |>
			strsplit("\ \\+\ ") |>
			unlist()
	} else {
		x |>
			as.formula() |>
			stats::terms() |>
			labels()
	}
}

#' @rdname sides
#' @export
lhs.formula_rx <- function(x, ...) {

	x |>
		as.formula() |>
		{\(.x) .x[[2]]}() |>
		deparse1() |>
		strsplit("\ \\+\ ") |>
		unlist()
}

# Getters ----


#' Retrieval functions for `forks` classes
#' @name getters
#' @export
getComponent <- function(x, ...) {
	UseMethod("getComponent", object = x)
}


#' @rdname getters
#' @export
getComponent.term_rx <- function(x,
																 part,
																 filter_id = NULL,
																 filter_val = NULL,
																 ...) {

	# Clean up "part"
	part <- sub("(.*)s$", '\\1', part)

	tm <- vec_data(x)

	if (is.null(filter_id)) {
		y <-
			tm[!is.na(tm[part]), unique(c("term", part)), drop = FALSE] |>
			table_to_list()

		# For consistency
		if ("term" %in% names(y)) {
			names(y) <- "terms"
		}

	} else if (!is.null(filter_id) & !is.null(filter_val)) {
		y <-
			tm[!is.na(tm[part]) & tm[filter_id] == filter_val, part] |>
			table_to_list()

	} else {
		stop("Filtering inputs have been set incorrectly.")
	}

	y
}

#' @rdname getters
#' @export
getComponent.formula_rx <- function(x,
																		part,
																		...) {

	# Clean up "part" by removing last S if exists, and appending if need be
	parts <-
		sub("(.*)s$", '\\1', part) |>
		paste0("s")

	a <- names(attributes(x))
	if (parts %in% a) {

		if (parts %in% c("operations", "ops", "operation")) {
			y <- attr(x, "operations")
		}

		if (parts %in% c("terms", "term")) {
			y <-
				attr(x, parts) |>
				as.character() |>
				list()
			names(y) <- parts
		}
	} else {
		t <- attr(x, "term")
		y <- getComponent.term_rx(t, parts)
	}

	y

}


#' @rdname getters
#' @export
roles <- function(x, ...) {
	UseMethod("roles", object = x)
}

#' @rdname getters
roles.term_rx <- function(x, ...) {
	getComponent(x, "role")
}

#' @rdname getters
roles.formula_rx <- function(x, ...) {
	attr(x, "terms") |>
		roles.term_rx(t)
}

#' @rdname getters
#' @export
roles.list_of_formulas <- function(x, ...) {
	attr(x, "roles")
}

#' @rdname getters
#' @export
roles.list_of_models <- function(x, ...) {
	attr(x, "roles")
}

#' @rdname getters
#' @export
labels.list_of_formulas <- function(x, ...) {
	attr(x, "labels")
}

#' @rdname getters
#' @export
labels.list_of_models <- function(x, ...) {
	attr(x, "labels")
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
setGroups.term_rx <- function(x, groups, ...) {

	validate_class(groups, "list")

	# Append groups
	grps <-
		getComponent(x, "group") |>
		append(groups)

	tm <- vec_data(x)

	for (i in seq_along(grps)) {
		tm$group[tm$term == names(grps[i])] <- grps[[i]]
	}

	vec_restore(tm, to = term_rx())

}

# updates ----

#' Updating prescribed formulas
#' @name updates
#' @export
update.formula_rx <- function(object, parameters, ...) {

	t <- term_rx(object)

	if (class(parameters) == "formula") {

		### LHS
		all_left <- lhs(parameters, tidy = TRUE)
		plus_left <- lhs(parameters, tidy = FALSE)

		# Add
		if (length(plus_left) > 0) {
			for (i in seq_along(plus_left)) {
				.t <- term_rx(x = plus_left[i], role = "outcome", side = "left")
				t <- c(t, .t)
			}
		}

		# Subtract
		minus_left <- setdiff(all_left, plus_left)

		tm <- vec_data(t)
		left <-
			tm[tm$side == "left" & !(tm$term %in% minus_left), ] |>
			vec_restore(term_rx())

		### RHS
		all_right <- rhs(parameters, tidy = TRUE)
		plus_right <- rhs(parameters, tidy = FALSE)

		# Add
		if (length(plus_right) > 0) {
			.t <-
				paste(plus_right, collapse = " + ") |>
				{\(.x) paste("~", .x)}() |>
				stats::as.formula() |>
				term_rx()

			t <- c(t, .t)
		}

		# Subtract
		minus_right <- setdiff(all_right, plus_right)

		tm <- vec_data(t)
		right <-
			tm[tm$side == "right" & !(tm$term %in% minus_right),] |>
			vec_restore(term_rx())

		# Combine both sides
		t <- c(left, right)

	}

	# Return
	formula_rx(t)
}

#' @rdname updates
#' @return An object of class `formula_rx`
#' @export
add <- function(object, parameters, ...) {

	validate_class(object, "formula_rx")

	obj <- term_rx(object)

	switch(
		class(parameters)[1],
		term_rx = {
			f <-
				obj |>
				{\(.x) c(.x, parameters)}() |>
				formula_rx()
		},
		formula = {
			f <-
				term_rx(parameters) |>
				{\(.x) c(obj, .x)}() |>
				formula_rx()
		}
	)

	# Return
	f
}


