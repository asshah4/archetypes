# Formula Prescription ---------------------------------------------------------

#' Formula vector
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' This function defines a new modified `formula` class that has been
#' vectorized. It expands upon the functionality of formulas.
#'
#' @param x Objects of the following types can be used as inputs
#'
#'   * `term_rx`
#'
#' @inheritParams tx
#'
#' @param ... Arguments to be passed to or from other methods
#'
#' @section Roles:
#'
#' Specific roles the variable plays within the formula. These are of particular
#' importance, as they serve as special terms that can effect how a formula is
#' interpreted. The options for roles are as below:
#'
#' * __exposure__ or `X(...)`: a predictor variable that serves as a primary or
#' key variable in the \eqn{Exposure ~ Outcome} relationship
#'
#' Formulas can be condensed by applying their specific role to individual terms
#' as a function/wrapper. For example, `y ~ X(x1) + x2 + x3`. This would signify
#' that `x1` has the specific role of an exposure.
#'
#' @inheritSection tx Pluralized Arguments
#'
#' @return An object of class `formula_rx`
#' @name fx
#' @export
formula_rx <- function(x = unspecified(), ...) {
	UseMethod("formula_rx", object = x)
}

#' @rdname fx
#' @export
formula_rx.formula <- function(x,
							   role = list(),
							   group = list(),
							   label = list(),
							   description = list(),
							   distribution = list(),
							   type = list(),
							   subtype = list(),
							   tag = deparse1(substitute(x)),
							   ...) {
	# Break early if zero length
	if (length(x) == 0) {
		return(new_formula())
	}

	f <- x
	x <- term_rx.formula(x)

	# Create simplified formula
	tm <- vec_data(x)
	left <- lhs(f)
	right <- rhs(f)
	formula_string <-
		paste(left, collapse = " + ") |>
		paste(paste(right, collapse = " + "), sep = " ~ ") |>
		vec_cast(character())

	# Term list (nested for field length equivalence)
	# Updated attributes/components internally
	trms <-
		x |>
		setRoles(roles = formula_args_to_list(role)) |>
		setGroups(groups = formula_args_to_list(group)) |>
		setLabels(labels = formula_args_to_list(label))

	# Return
	new_formula(formula = formula_string,
				terms = trms)
}

#' @rdname fx
#' @export
formula_rx.term_rx <- function(x,
							   role = list(),
							   group = list(),
							   label = list(),
							   description = list(),
							   distribution = list(),
							   type = list(),
							   subtype = list(),
							   tag = deparse1(substitute(x)),
							   ...) {
	# Early break
	if (length(x) == 0) {
		message(
			paste0(
				"No `",
				class(x)[1],
				"` object was provided, resulting in a [0] length ",
				"`formula_rx` object."
			)
		)
		return(new_formula())
	}

	# Create simplified formula
	t <- vec_data(x)

	# Update roles to prepare for color and display of formula
	t$term[!is.na(t$role) & t$role == "exposure"] <-
		paste0("X(", t$term[!is.na(t$role) & t$role == "exposure"], ")")
	t$term[!is.na(t$role) & t$role == "mediator"] <-
		paste0("M(", t$term[!is.na(t$role) & t$role == "mediator"], ")")
	left <- t$term[t$side == "left"]
	right <- t$term[t$side == "right"]

	# Character representation of formula
	formula_string <-
		paste(left, collapse = " + ") |>
		paste(paste(right, collapse = " + "), sep = " ~ ") |>
		vec_cast(character())

	# Term list (nested for field length equivalence)
	# Updated attributes/components internally
	trms <-
		x |>
		setRoles(roles = formula_args_to_list(role)) |>
		setGroups(groups = formula_args_to_list(group)) |>
		setLabels(labels = formula_args_to_list(label))

	# Return
	new_formula(formula = formula_string,
				terms = trms)
}


#' @rdname fx
#' @export
formula_rx.default <- function(x = unspecified(), ...) {
	# Early break if not viable method dispatch
	if (length(x) == 0) {
		return(new_formula())
	} else {
		stop("`formula_rx()` is not defined for a `",
			 class(x)[1],
			 "` object.",
			 call. = FALSE)
	}
}

#' @rdname fx
#' @export
fx = formula_rx



# Vector Creation --------------------------------------------------------------

#' Formula vector
#' @keywords internal
#' @noRd
new_formula <- function(formula = character(),
						terms = term_rx()) {

	# Validation of types
	vec_assert(formula, ptype = character())
	vec_assert(terms, ptype = term_rx())

	# Terms should contain all the additional information
	new_vctr(formula,
			 terms = terms,
			 class = "formula_rx")
}

#' @keywords internal
#' @noRd
methods::setOldClass(c("formula_rx", "vctrs_vctr"))

# casting and coercion ----

# Arithmetic
vec_arith.formula_rx <- function(op, x, y, ...) {
	UseMethod("vec_arith.formula_rx", y)
}

vec_arith.formula_rx.default <- function(op, x, y, ...) {
	stop_incompatible_op(op, x, y)
}


### self

#' @export
vec_ptype2.formula_rx.formula_rx <- function(x, y, ...) {
	x
}

#' @export
vec_cast.formula_rx.formula_rx <- function(x, to, ...) {
	x
}

### characters

#' @export
vec_ptype2.formula_rx.character <- function(x, y, ...) {
	y
}

#' @export
vec_ptype2.character.formula_rx <- function(x, y, ...) {
	x
}

#' @export
vec_cast.character.formula_rx <- function(x, to, ...) {
	attributes(x) <- NULL
	as.character(x)
}

### term_rx

#' @export
vec_ptype2.formula_rx.term_rx <- function(x, y, ...) {
	y
}

#' @export
vec_ptype2.term_rx.formula_rx <- function(x, y, ...) {
	x
}

#' @export
vec_cast.term_rx.formula_rx <- function(x, to, ...) {
	attr(x, "terms")

}

# Output -----------------------------------------------------------------------


#' @export
format.formula_rx <- function(x, ...) {
	f <- vec_data(x)
	t <- attr(x, "terms")
	tm <- vec_data(t)
	fmts <- format(t)

	out <- fmts[which(tm$role == "outcome")]
	exp <- fmts[which(tm$role == "exposure")]
	med <- fmts[which(tm$role == "mediator")]
	cov <- fmts[which(tm$role == "covariate")]

	left <- paste(out, collapse = " + ")
	right <- paste(c(exp, med, cov), collapse = " + ")
	both <- paste(left, right, sep = " ~ ")

	# Return
	both

}

#' @export
obj_print_data.formula_rx <- function(x, ...) {
	if (vec_size(x) == 0) {
		new_term()
	} else if (vec_size(x) > 1) {
		cat(format(x), sep = "\n")
	} else {
		cat(format(x))
	}
}

#' @export
vec_ptype_abbr.formula_rx <- function(x, ...) {
	"fx"
}
