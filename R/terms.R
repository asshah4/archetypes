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
																sides = character(),
																roles = character(),
																operations = character(),
																labels = character(),
																...) {

	# Early break if not viable method dispatch
	if (length(x) == 0) {
		return(new_term())
	}

	# Finding missing value
	if (length(sides) == 0) sides <- NA
	if (length(roles) == 0) roles <- NA
	if (length(operations) == 0) operations <- NA
	if (length(labels) == 0) labels <- NA

	# Casting
	x <- vec_cast(x, character())
	sides <- vec_cast(sides, character())
	roles <- vec_cast(roles, character())
	operations <- vec_cast(operations, character())
	labels <- vec_cast(labels, character())

	new_term(
		term = x,
		sides = sides,
		roles = roles,
		operations = operations,
		labels = labels
	)

}

#' @rdname term_record
#' @export
term_rcrd.formula <- function(x,
												 roles = list(),
												 labels = list(),
												 ...) {

	# All terms are needed to build term record
	n <- length(all.vars(x))
	all <- all.vars(x, functions = TRUE, unique = FALSE)
	all_terms <- all.vars(x, functions = FALSE)
	left_terms <-
		x[[2]] |>
		deparse() |>
		strsplit("\ \\+\ ") |>
		unlist()
	left_n <- length(left_terms)
	right_terms <-
		all.vars(x) |>
		setdiff(left_terms)
	right_n <- length(right_terms)

	# The roles and operations need to be identified (upon which term they apply)
	all_ops <-
		all |>
		{\(.x) grep("~", .x, value = TRUE, invert = TRUE)}() |>
		{\(.x) grep("\\+", .x, value = TRUE, invert = TRUE)}() |>
		{\(.x) .x[!(.x %in% left_terms)]}() |>
		{\(.x) {
			.y <- as.list(.x[!(.x %in% right_terms)])
			names(.y) <- .x[which(!.x %in% right_terms) + 1]
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

	# Create and confirm roles
	validate_class(roles, "list")
	role_ops <- c(roles, role_ops)

	# Confirm list
	validate_class(labels, "list")

	# Create terms
	term_list <- list()
	for (i in 1:n) {
		# Make parameters
		t <- all_terms[i]
		side <- if (t %in% left_terms) {
			"left"
		} else if (t %in% right_terms) {
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
		op <- vec_cast(op, character())
		lab <- vec_cast(lab, character())

		# Place into term list
		term_list[[i]] <- new_term(
			term = x,
			sides = side,
			roles = role,
			operations = op,
			labels = lab
		)

	}

	# Return as a record of terms
	term_list |>
		vec_list_cast(to = term_rcrd())

}

#' @rdname term_record
#' @export
term_rcrd.default <- function(x, ...) {
	stop(
		"`term()` is not defined for a `", class(x)[1], "` object.",
		call. = FALSE
	)
}



# records ----

#' record of formula terms
#' @keywords internal
#' @noRd
new_term <- function(terms = character(),
										 sides = character(),
										 roles = character(),
										 operations = character(),
										 labels = character()) {

	vec_assert(terms, ptype = character())
	vec_assert(sides, ptype = character())
	vec_assert(roles, ptype = character())
	vec_assert(operations, ptype = character())
	vec_assert(labels, ptype = character())

	new_rcrd(list(
		"terms" = terms,
		"sides" = sides,
		"roles" = roles,
		"operations" = operations,
		"labels" = labels
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
	t <- field(x, "terms")
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
