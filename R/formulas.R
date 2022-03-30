# Formula class ----------------------------------------------------------------

#' Formula Archetype
#' @name formula
#' @export
formula_archetype <- function(x = unspecified(), ...) {
	UseMethod("formula_archetype", object = x)
}

#' @rdname formula
#' @export
formula_archetype.formula <- function(x,
									  tag = character(),
									  ...) {


	# Early Break if needed
	mc <- match.call()
	if (validate_empty(x, mc)) {
		return(new_term())
	}

	f <- deparse1(x)
	t <- terms(x)
	left <- lhs(x)
	right <- rhs(x)

	# Tag
	if (length(tag) == 0) {
		tag <-
			paste0(
				"F_DV",
				length(attr(t, "response")),
				"_IV",
				length(attr(t, "term.labels")),
				collapse = ""
			)
	}

	new_formula(
		fx = f,
		left = list(left),
		right = list(right),
		tag = tag
	)

}

#' @rdname formula
#' @export
formula_archetype.term_archetype <- function(x, ...) {

	# Early Break if needed
	mc <- match.call()
	if (validate_empty(x, mc)) {
		return(new_term())
	}

	f <-
		paste(lhs(x), collapse = " + ") |>
		paste(paste(rhs(x), collapse = " + "), sep = " ~ ") |>
		stats::as.formula()

	# Return
	formula_archetype.formula(f)
}

#' @rdname formula
#' @export
formula_archetype.default <- function(x = unspecified(), ...) {
	# Early break
	if (length(x) == 0) {
		return(new_formula())
	}

	stop("`formula_archetype()` is not defined for a `",
		 class(x)[1],
		 "` object.",
		 call. = FALSE)

}


#' @rdname formula
#' @export
fm = formula_archetype

# Record definition ------------------------------------------------------------

#' Record of formula archetypes
#' @keywords internal
#' @noRd
new_formula <- function(fx = character(),
						left = list(),
						right = list(),
						tag = character()) {
	# Validation
	vec_assert(fx, ptype = character())
	vec_assert(tag, ptype = character())
	vec_assert(left, ptype = list())
	vec_assert(right, ptype = list())

	new_rcrd(
		fields = list(
			"formula" = fx,
			"left" = left,
			"right" = right,
			"tag" = tag
		),
		class = "formula_archetype"
	)
}

#' @keywords internal
#' @noRd
methods::setOldClass(c("formula_archetype", "rcrds_rcrd"))

# Output -----------------------------------------------------------------------

#' @export
format.formula_archetype <- function(x, ...) {
	field(x, "formula")
}


#' @export
obj_print_data.formula_archetype <- function(x, ...) {

	# Colorful printing
	if (vec_size(x) == 0) {
		fmt <- new_script()
	} else {

		# Depending on length
		if (length(x) > 1) {
			cat(format(x), sep = "\n")
		} else {
			cat(format(x))
		}

	}
}

#' @export
vec_ptype_full.formula_archetype <- function(x, ...) {
	"formula_archetype"
}

#' @export
vec_ptype_abbr.formula_archetype <- function(x, ...) {
	"fm"
}

# Casting and coercion ---------------------------------------------------------

#' @export
vec_ptype2.formula_archetype.formula_archetype <- function(x, y, ...) {
  x
}

#' @export
vec_cast.formula_archetype.formula_archetype <- function(x, to, ...) {
  x
}


#' @export
vec_ptype2.formula_archetype.character <- function(x, y, ...) {
	y
}

#' @export
vec_ptype2.character.formula_archetype <- function(x, y, ...) {
	x
}

#' @export
vec_cast.character.formula_archetype <- function(x, to, ...) {
	format(x) # Returns a character class by default
}

#' @export
vec_ptype2.formula_archetype.term_archetype <- function(x, y, ...) {
	y
}

#' @export
vec_ptype2.term_archetype.formula_archetype <- function(x, y, ...) {
	x
}

#' @export
vec_cast.term_archetype.formula_archetype <- function(x, to, ...) {
	term_archetype.formula_archetype(x)
}

#' @export
vec_ptype2.formula_archetype.formula_script <- function(x, y, ...) {
	x
}

#' @export
vec_ptype2.formula_script.formula_archetype <- function(x, y, ...) {
	y
}

#' @export
vec_cast.formula_archetype.formula_script <- function(x, to, ...) {
	format(x) |>
		stats::as.formula() |>
		formula_archetype.formula()
}


#' @export
formula.formula_archetype <- function(x, ...) {
	format(x) |>
		as.formula()
}