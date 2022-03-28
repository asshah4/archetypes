# list of formulas ----

#' Prescribed Formula Lists
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' This function introduces a super  that combines both the `list` class (and
#' its derivative `list_of`) and the `formula` class.

#' @param pattern This is the expansion pattern used to decide how the
#'   covariates will incorporated into the formulas. The options are
#'   `c("direct", "sequential", "parallel")`. See the details for further
#'   explanation.
#'
#'   * __direct__: the covariates will all be included in each formula
#'
#'   * __sequential__: the covariates will be added sequentially, one by one, or
#'   by groups, as indicated
#'
#'   * __parallel__: the covariates or groups of covariates will be placed in
#'   parallel
#'
#' @section Patterns:
#'
#' The expansion pattern allows for instructions on how the covariates should be
#' included in different formulas. Below, assuming that _x1_, _x2_, and _x3_ are
#' covariates...
#'
#' \deqn{y ~ x1 + x2 + x3}
#'
#' __Direct__:
#'
#' \deqn{y ~ x1 + x2 + x3}
#'
#' __Seqential__:
#'
#' \deqn{y ~ x1}
#' \deqn{y ~ x1 + x2}
#' \deqn{y ~ x1 + x2 + x3}
#'
#' __Parallel__:
#'
#' \deqn{y ~ x1}
#' \deqn{y ~ x2}
#' \deqn{y ~ x3}
#'
#'
#' @name list_of_formulas
#' @export
list_of_formulas <- function(x = unspecified(), ...) {
	UseMethod("list_of_formulas", object = x)
}

#' @rdname list_of_formulas
#' @export
list_of_formulas.formula_script <- function(x,
											tag = deparse1(substitute(x)),
											pattern = character(),
											strata = character(),
											...) {

	# Early break if not viable method dispatch
	if (length(x) == 0) {
		return(new_list_of_formulas())
	}

	# Check pattern
	if (length(pattern) == 0) {
		pattern <- "direct"
	}
	if (!pattern %in% c("direct", "sequential", "parallel")) {
		stop("The pattern ",
			 deparse(pattern),
			 " is not yet supported.",
			 call. = FALSE)
	}

	# Get components from formula
	cl <- as.character(x)
	t <- tx(x)
	ops <- identify_ops(t, pattern)

	# Update with strata if needed
	strata_terms <-
		term_archetype(x = strata, side = "meta", role = "strata") |>
		suppressMessages()

	t <- add(t, strata_terms)

	# Expansion of formulas
	lof <- perform_ops(ops)
	names(lof) <-
		sapply(names(lof),
			   function(.x) {
			   	paste(tag, sep = "_", .x)
			   },
			   USE.NAMES = FALSE)

	# Use special formula holder here
	f <-
		lof |>
		lapply(FUN = function(.x) {
			formula_archetype(.x)
		})

	new_list_of_formulas(
		formulas = f,
	)
}

#' @rdname list_of_formulas
#' @export
list_of_formulas.list <- function(x,
								  tag = deparse1(substitute(x)),
								  pattern = character(),
								  strata = character(),
								  ...) {

	# Each member of a list has to be of the same class (that of formula())
	class_check <- sapply(x, is_formula)
	lof <- x[class_check]
	if (length(x) != length(lof)) {
		stop(
			"The list items: `",
			paste0(x[!class_check], collapse = ", "),
			"` are not of the `formula` class",
			call. = FALSE
		)
	}

	# Generate terms from the formulas
	t <- term_archetype()
	for (i in lof) {
		t <- unique(append(t, term_archetype(i)))
	}

	# Generate a full formula
	x <- formula_script(t)

	# Add names
	new_names <- character()
	for (i in seq_along(lof)) {
		if (is.null(names(lof[i]))) {
			new_names <- append(new_names, paste0(tag, "_", i))
		} else {
			new_names <- append(new_names, names(lof[i]))
		}
	}
	names(lof) <- new_names

	# Return
	new_list_of_formulas(
		list_of_formulas = lof,
		formula_script = x,
		terms = t
	)

}

#' @rdname list_of_formulas
#' @export
list_of_formulas.default <- function(x = list(), ...) {
	# Early break if not viable method dispatch
	if (length(x) == 0) {
		return(new_list_of_formulas())
	} else {
		stop("`list_of_formulas()` is not defined for a `",
			 class(x)[1],
			 "` object.",
			 call. = FALSE)
	}
}

#' @rdname list_of_formulas
#' @export
fmls = list_of_formulas

# Vector definition ------------------------------------------------------------

#' Formula list
#' @keywords internal
#' @noRd
new_list_of_formulas <- function(formulas = list()) {

	# Each item in a ist of formulas should be the LHS and RHS of a formula
	vec_assert(formulas, ptype = list())

	new_list_of(
		x = formulas,
		ptype = formula_archetype(),
		class = "list_of_formulas"
	)

}

#' @keywords internal
#' @noRd
methods::setOldClass(c("list_of_formulas", "vctrs_vctr"))

# Output -----------------------------------------------------------------------

#' @export
format.list_of_formulas <- function(x, ...) {

	if (vec_size(x) == 0) {
		new_list_of_formulas()
	} else {
		fmt <-
			lapply(vec_data(x), function(.x) {
				format(.x) |>
					stats::as.formula()
			})
	}

	# Return
	fmt

}

#' @export
obj_print_data.list_of_formulas <- function(x, ...) {
	if (length(x) == 0) {
		new_list_of_formulas()
	}

	if (length(x) >= 1) {
		cat(as.character(format(x)), sep = "\n")
	} else {
		cat(as.character(format(x)))
	}
}

#' @export
vec_ptype_full.list_of_formulas <- function(x, ...) {
	"list_of_formulas"
}

#' @export
vec_ptype_abbr.list_of_formulas <- function(x, ...) {
	"fmls"
}

# Casting and coercion ---------------------------------------------------------

#' @export
vec_ptype2.list_of_formulas.list_of_formulas <- function(x, y, ...) {x}

#' @export
vec_cast.list_of_formulas.list_of_formulas <- function(x, y, ...) {x}


