#' Vector of formula terms
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
	class = "term_vctr")

}

#' @keywords internal
#' @noRd
methods::setOldClass(c("term_vctr", "vctrs_vctr"))

#' `term` vector
#'
#' @param x
#' * For `term()` or `tx()`: A character vector
#' * For `is_term()`: An object to test
#' @name term
#' @export
tx <- function(x, ...) {
	UseMethod("term", object = x)
}

#' @rdname term
#' @export
term <- function(x, ...) {
	UseMethod("term", object = x)
}

#' @rdname term
#' @export
term.character <- function(x = character(),
													 sides = character(),
													 roles = character(),
													 operations = character(),
													 labels = character()) {


	# Finding missing values
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
		terms = x,
		sides = sides,
		roles = roles,
		operations = operations,
		labels = labels
	)

}

#' @rdname term
#' @export
term.formula <- function(x,
												 roles = list(),
												 labels = list()) {


	# All terms are needed to build term vector
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
	ops <-
		all |>
		{\(x) grep("~", x, value = TRUE, invert = TRUE)}() |>
		{\(x) grep("\\+", x, value = TRUE, invert = TRUE)}() |>
		{\(x) setdiff(x, left_terms)}() |>
		{\(x) {
			y <- as.list(x[!(x %in% right_terms)])
			names(y) <- x[which(!x %in% right_terms) + 1]
			y
		}}()

	# Confirm identity of labels and roles
	validate_class(labels, "list")
	validate_class(roles, "list")

	# Create terms
	term_list <- list()
	for (i in 1:n) {
		# Make parameters
		t <- all_terms[i]
		side <- if (t %in% left_terms) {
			"lhs"
		} else if (t %in% right_terms) {
			"rhs"
		}
		op <- if (t %in% names(ops)) {
			ops[[t]]
		} else {
			NA
		}
		lab <- if (t %in% names(labels)) {
			labels[[t]]
		} else {
			NA
		}


		# Casting
		x <- vec_cast(t, character())
		side <- vec_cast(side, character())
		if (length(roles) == 0) roles <- NA
		role <- vec_cast(roles, character())
		op <- vec_cast(op, character())
		lab <- vec_cast(lab, character())

		# Place into term list
		term_list[[i]] <- new_term(
			terms = x,
			sides = side,
			roles = role,
			operations = op,
			labels = lab
		)

	}

	# Return as a vector of terms
	term_list |>
		vec_list_cast(to = new_term())

}

#' @rdname term
#' @export
term.default <- function(x, ...) {
	stop(
		"`term()` is not defined for a `", class(x)[1], "` object.",
		call. = FALSE
	)
}


#' @export
#' @rdname term
is_term <- function(x) {
	inherits(x, "term_vctr")
}

### Formating and printing ----

#' @export
format.term_vctr <- function(x, ...) {

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
obj_print_data.term_vctr <- function(x) {
	if (vec_size(x) > 0) {
		cat(format(x), sep = "\n")
	} else {
		cat(format(x))
	}
}

#' @export
vec_ptype_abbr.term_vctr <- function(x, ...) {
	"tm"
}

### Casting and Coercion ----

### Self coercion
#' @export
vec_ptype2.term_vctr.term_vctr <- function(x, y, ...) {
	x
}

#' @export
vec_cast.term_vctr.term_vctr <- function(x, to, ...) {
	x
}

### Character coercion
#' @export
vec_ptype2.term_vctr.character <- function(x, y, ...) {
	# `x` is term
	# `y` is character
	y
}

#' @export
vec_ptype2.character.term_vctr <- function(x, y, ...) {
	# `x` is character
	# `y` is term
	x
}

#' @export
vec_cast.term_vctr.character <- function(x, to, ...) {
	# Order is flipped, such that `x` is character
	attributes(x) <- NULL
	x[[1]]
}

#' @export
vec_cast.character.term_vctr <- function(x, to, ...) {
	# Order is flipped, such that `x` is term
	attributes(x) <- NULL
	x[[1]]
}
