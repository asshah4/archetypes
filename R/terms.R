# Terms ----

#' Term records
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' @param x An object of the following types that can be coerced to a `term_rcrd` object
#'
#'   * `character`
#'
#'   * `formula`
#'
#' @param sides Left or right hand side of the equation
#'
#' @param roles Specific role the variable plays within the formula
#'
#' @param operations Modification of the term to be applied when combining with
#'   data
#'
#' @param labels Display-quality label describing the variable
#'
#' @name term_record
#' @export
term_rcrd <- function(x = character(), ...) {
	UseMethod("term_rcrd", object = x)
}

#' @rdname term_record
#' @export
term_rcrd.character <- function(x = character(),
																side = character(),
																role = character(),
																group = character(),
																type = character(),
																operation = character(),
																label = character(),
																...) {

	# Break early if need be
	if (length(x) == 0) {
		return(new_term())
	}

	# Missing values
	if (length(x) == 0) x <- NA
	if (length(side) == 0) side <- NA
	if (length(role) == 0) role <- NA
	if (length(group) == 0) group <- NA
	if (length(type) == 0) type <- NA
	if (length(operation) == 0) operation <- NA
	if (length(label) == 0) label <- NA

	# Casting
	x <- vec_cast(x, character())
	side <- vec_cast(side, character())
	role <- vec_cast(role, character())
	group <- vec_cast(group, character())
	type <- vec_cast(type, character())
	operation <- vec_cast(operation, character())
	label <- vec_cast(label, character())

	new_term(
		term = x,
		side = side,
		role = role,
		group = group,
		type = type,
		operation = operation,
		label = label
	)

}

#' @rdname term_record
#' @export
term_rcrd.formula <- function(x = formula(),
															roles = list(),
															groups = list(),
															types = list(),
															labels = list(),
															...) {

	# Break early if need be
	if (length(x) == 0) {
		return(new_term())
	}

	# Validate
	validate_class(roles, "list")
	validate_class(groups, "list")
	validate_class(types, "list")
	validate_class(labels, "list")

	# All terms are needed to build term record
	n <- length(all.vars(x))
	all <- all.vars(x, functions = TRUE, unique = FALSE)
	all_terms <- all.vars(x, functions = FALSE)
	left <- lhs(x)
	right <- rhs(x, tidy = TRUE)

	# The roles and operations need to be identified (upon which term they apply)
	all_ops <-
		all |>
		{\(.x) grep("~", .x, value = TRUE, invert = TRUE)}() |>
		{\(.x) grep("\\+", .x, value = TRUE, invert = TRUE)}() |>
		{\(.x) .x[!(.x %in% left)]}() |>
		{\(.x) {
			.y <- as.list(.x[!(.x %in% right)])
			names(.y) <- .x[which(!.x %in% right) + 1]
			.y
		}}()

	# Check to see if it is a "role" or a data transformation
	which_ops <- vapply(all_ops, FUN.VALUE = TRUE, FUN = function(.x) {
		.y <- try(getFromNamespace(.x, c("base", "stats", "utils", "methods")), silent = TRUE)
		if (class(.y) == "try-error") {
			.y <- FALSE
		} else if (class(.y) == "function") {
			.y <- TRUE
		}
	})
	data_ops <- all_ops[which_ops]
	role_ops <- all_ops[!which_ops]
	role_ops <- c(roles, role_ops)

	# Clean up groups
	grps <- list()
	for (i in seq_along(groups)) {
		g <- as.character(groups[[i]][[2]])
		t <- as.character(groups[[i]][[3]])[-1]

		grps <- rep(g, length(t))
		names(grps) <- t
	}

	# Create terms
	term_list <- list()

	for (i in 1:n) {
		# Make parameters
		t <- all_terms[i]
		side <- if (t %in% left) {
			"left"
		} else if (t %in% right) {
			"right"
		}

		# Data transforms
		op <- if (t %in% names(data_ops)) {
			data_ops[[t]]
		} else {
			NA
		}

		# Roles
		role <- if (t %in% names(role_ops)) {
			role_ops[[t]]
		} else {
			NA
		}

		# Groups
		grp <- if (t %in% names(groups)) {
			groups[[t]]
		} else {
			NA
		}

		# Groups
		typ <- if (t %in% names(types)) {
			groups[[t]]
		} else {
			NA
		}

		# Labels
		lab <- if (t %in% names(labels)) {
			labels[[t]]
		} else {
			NA
		}

		# Casting
		x <- vec_cast(t, character())
		side <- vec_cast(side, character())
		role <- vec_cast(role, character())
		grp <- vec_cast(grp, character())
		typ <- vec_cast(typ, character())
		op <- vec_cast(op, character())
		lab <- vec_cast(lab, character())


		# Place into term list
		term_list[[i]] <- term_rcrd.character(
			x = x,
			side = side,
			role = role,
			group = grp,
			type = typ,
			operation = op,
			label = lab
		)

	}

	# Return as a record of terms
	term_list |>
		vec_list_cast(to = term_rcrd())

}

#' @rdname term_record
#' @export
term_rcrd.default <- function(x, ...) {

	stop("`term()` is not defined for a `", class(x)[1], "` object.",
			 call. = FALSE)
}


# records ----

#' record of formula terms
#' @keywords internal
#' @noRd
new_term <- function(term = character(),
										 side = character(),
										 role = character(),
										 group = character(),
										 type = character(),
										 operation = character(),
										 label = character()) {

	vec_assert(term, ptype = character())
	vec_assert(side, ptype = character())
	vec_assert(role, ptype = character())
	vec_assert(group, ptype = character())
	vec_assert(type, ptype = character())
	vec_assert(operation, ptype = character())
	vec_assert(label, ptype = character())

	new_rcrd(list(
		"term" = term,
		"side" = side,
		"role" = role,
		"group" = group,
		"type" = type,
		"operation" = operation,
		"label" = label
	),
	class = "term_rcrd")

}

#' @keywords internal
#' @noRd
methods::setOldClass(c("term_rcrd", "rcrds_rcrd"))

### term() ###

#' @export
vec_ptype2.term_rcrd.term_rcrd <- function(x, y, ...) {
	x
}

#' @export
vec_cast.term_rcrd.term_rcrd <- function(x, to, ...) {
	x
}

### character() ###

#' @export
vec_ptype2.term_rcrd.character <- function(x, y, ...) {
	# `x` is term
	# `y` is character
	y
}

#' @export
vec_ptype2.character.term_rcrd <- function(x, y, ...) {
	# `x` is character
	# `y` is term
	x
}

#' @export
vec_cast.term_rcrd.character <- function(x, to, ...) {
	# Order is flipped, such that `x` is character
	attributes(x) <- NULL
	x[[1]]
}

#' @export
vec_cast.character.term_rcrd <- function(x, to, ...) {
	# Order is flipped, such that `x` is term
	attributes(x) <- NULL
	x[[1]]
}

### list_of() ###

#' @export
vec_ptype2.rcrds_list_of.term_rcrd <- function(x, y, ...) {
	x
}

#' @export
vec_ptype2.term_rcrd.rcrds_list_of <- function(x, y, ...) {
	y
}

#' @export
vec_cast.rcrds_list_of.term_rcrd <- function(x, to, ...) {
	tl <- as.list(x) # Convert to list
	lot <- new_list_of(tl, ptype = term_rcrd()) # make new list of
	lot # return list of terms
}

#' @export
vec_cast.term_rcrd.rcrds_list_of <- function(x, to, ...) {
	t <- vec_list_cast(x, term_rcrd()) # Convert to a flattened record
	t # Return record of terms
}

# Formating and printing ----

#' @export
format.term_rcrd <- function(x, ...) {

	# Formatting
	t <- field(x, "term")
	info <- t

	# Pasting
	if (length(t) > 0) {
		paste(info, sep = "\n")
	} else {
		paste(info)
	}

}

#' @export
obj_print_data.term_rcrd <- function(x) {
	if (vec_size(x) > 0) {
		cat(format(x), sep = "\n")
	} else {
		cat(format(x))
	}
}

#' @export
vec_ptype_abbr.term_rcrd <- function(x, ...) {
	"t_rcrd"
}
