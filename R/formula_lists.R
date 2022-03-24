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
#' @name formula_list
#' @export
formula_list <- function(x = unspecified(), ...) {
	UseMethod("formula_list", object = x)
}

#' @rdname formula_list
#' @export
formula_list.rx <- function(x,
							tag = deparse1(substitute(x)),
							pattern = character(),
							strata = character(),
							...) {

	# Early break if not viable method dispatch
	if (length(x) == 0) {
		return(new_formula_list())
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
	t <- attr(x, "terms")
	ops <- identify_ops(t, pattern)

	# Update with strata if needed
	strata_term <-
		suppressMessages(term(x = strata, side = "meta", role = "strata"))

	t <- add(t, strata_term)

	# Expansion of formulas
	lof <- perform_ops(ops)
	names(lof) <-
		sapply(names(lof),
			   function(x) {
			   	paste(tag, sep = "_", x)
			   },
			   USE.NAMES = FALSE)

	new_formula_list(
		formula_list = lof,
		script = x,
		terms = t
	)
}

#' @rdname formula_list
#' @export
formula_list.list <- function(x,
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
	t <- term()
	for (i in lof) {
		t <- unique(append(t, term(i)))
	}

	# Generate a full formula
	x <- rx(t)

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
	new_formula_list(
		formula_list = lof,
		script = x,
		terms = t
	)

}

#' @rdname formula_list
#' @export
formula_list.default <- function(x = list(), ...) {
	# Early break if not viable method dispatch
	if (length(x) == 0) {
		return(new_formula_list())
	} else {
		stop("`formula_list()` is not defined for a `",
			 class(x)[1],
			 "` object.",
			 call. = FALSE)
	}
}

#' @rdname formula_list
#' @export
fmls = formula_list

# Vector definition ------------------------------------------------------------

#' Formula list
#' @keywords internal
#' @noRd
new_formula_list <- function(formula_list = list(),
							 script = prescribe(),
							 terms = term()) {

	new_list_of(
		x = formula_list,
		ptype = list(),
		class = "formula_list",
		script = script,
		terms = terms
	)

}

#' @keywords internal
#' @noRd
methods::setOldClass(c("formula_list", "vctrs_vctr"))

# Casting and coercion ---------------------------------------------------------

#' @export
vec_ptype2.formula_list.formula_list <- function(x, y, ...) {x}

#' @export
vec_cast.formula_list.formula_list <- function(x, y, ...) {x}

#' @export
vec_proxy.formula_list <- function(x, ...) {
	unclass(x)
}


# Output -----------------------------------------------------------------------

#' @export
format.formula_list <- function(x, ...) {
	f <- lapply(vec_data(x), function(.x) {
		attributes(.x) <- NULL
		.x
	})
	f <- unname(f)
	f <- as.character(f)
	f

}

#' @export
obj_print_data.formula_list <- function(x, ...) {
	if (length(x) == 0) {
		return()
	}

	if (length(x) >= 1) {
		cat(format(x), sep = "\n")
	} else {
		cat(format(x))
	}
}

#' @export
vec_ptype_full.formula_list <- function(x, ...) {
	"formula_list"
}

#' @export
vec_ptype_abbr.formula_list <- function(x, ...) {
	"fmls"
}
