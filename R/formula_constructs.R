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
#' @name formula_construct
#' @export
formula_construct <- function(x = unspecified(), ...) {
	UseMethod("formula_construct", object = x)
}

#' @rdname formula_construct
#' @export
formula_construct.script <- function(x,
									 origin = deparse(substitute(x)),
									 pattern = character(),
									...) {

	# Early Break if needed
	mc <- match.call()
	if (validate_empty(x, mc)) {
		return(new_term())
	}

	# Get terms from script
	t <- field(x, "terms")[[1]]

	# Get pattern and list of formulas back into a table
	if (length(pattern) == 0) {
		ptrn <- field(x, "pattern")
	} else {
		ptrn <- pattern
	}
	lof <- deconstruct_patterns(x, ptrn)
	tbl <- reconstruct_patterns(lof, t)
	tbl$origin <- origin

	# Return
	new_formula_construct(
		formulas = tbl$formula,
		outcomes = tbl$outcome,
		exposures = tbl$exposure,
		mediators = tbl$mediator,
		covariates = tbl$covariate,
		tags = tbl$tag,
		origin = vec_rep(origin, nrow(tbl)),
		pattern = vec_rep(ptrn, nrow(tbl))
	)
}

#' @rdname formula_construct
#' @export
formula_construct.default <- function(x = unspecified(), ...) {
	# Early break if not viable method dispatch
	if (length(x) == 0) {
		return(new_formula_construct())
	} else {
		stop("`formula_construct()` is not defined for a `",
			 class(x)[1],
			 "` object.",
			 call. = FALSE)
	}
}

#' @rdname formula_construct
#' @export
fmls = formula_construct

# Vector definition ------------------------------------------------------------

#' Formula list
#' @keywords internal
#' @noRd
new_formula_construct <- function(formulas = character(),
								  outcomes = character(),
								  exposures = character(),
								  mediators = character(),
								  covariates = character(),
								  tags = character(),
								  origin = character(),
								  pattern = character()) {

	# Validation
	vec_assert(formulas, ptype = character())
	vec_assert(outcomes, ptype = character())
	vec_assert(exposures, ptype = character())
	vec_assert(mediators, ptype = character())
	vec_assert(covariates, ptype = character())
	vec_assert(tags, ptype = character())
	vec_assert(origin, ptype = character())
	vec_assert(pattern, ptype = character())

	new_rcrd(
		fields = list(
			"formula" = formulas,
			"outcome" = outcomes,
			"exposure" = exposures,
			"mediator" = mediators,
			"covariate" = covariates,
			"tag" = tags,
			"origin" = origin,
			"pattern" = pattern
		),
		class = "formula_construct"
	)

}

#' @keywords internal
#' @noRd
methods::setOldClass(c("formula_construct", "vctrs_vctr"))

# Output -----------------------------------------------------------------------

#' @export
format.formula_construct <- function(x, ...) {
	field(x, "formula")
}

#' @export
obj_print_data.formula_construct <- function(x, ...) {
	if (vec_size(x) == 0) {
		new_formula_construct()
	}

	if (vec_size(x) >= 1) {
		cat(as.character(format(x)), sep = "\n")
	} else {
		cat(as.character(format(x)))
	}
}

#' @export
vec_ptype_full.formula_construct <- function(x, ...) {
	"formula_construct"
}

#' @export
vec_ptype_abbr.formula_construct <- function(x, ...) {
	"fmls"
}

# Casting and coercion ---------------------------------------------------------

#' @export
vec_ptype2.formula_construct.formula_construct <- function(x, y, ...) {
	x
}

#' @export
vec_cast.formula_construct.formula_construct <- function(x, y, ...) {
	x
}


