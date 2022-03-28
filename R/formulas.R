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
									  ...) {

	# early break
	if (length(x) == 0) {
		message(
			paste0(
				"no `",
				class(x)[1],
				"` object was provided, resulting in a [0] length `formula_archetype` object."
			)
		)
		return(new_formula())
	}

	f <- deparse1(x)

	new_formula(
		fx = f
	)

}

#' @rdname formula
#' @export
formula_archetype.default <- function(x = unspecified(), ...) {
	# early break
	if (length(x) == 0) {
		return(new_formula())
	}

	stop("`paths()` are not defined for a `",
		 class(x)[1],
		 "` object.",
		 call. = FALSE)

}


#' @rdname formula
#' @export
fx = formula_archetype

# Record definition ------------------------------------------------------------

#' Record of formula archetypes
#' @keywords internal
#' @noRd
new_formula <- function(fx = character(),
						tag = character()) {

	  new_rcrd(
	  	fields = list(
	  		"formula" = fx
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
	"fx"
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
formula.formula_archetype <- function(x, ...) {
	format(x) |>
		as.formula()
}