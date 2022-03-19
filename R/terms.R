# Terms ------------------------------------------------------------------------

#' Term records
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' @param x An object of the following types that can be coerced to a `term_rex`
#'   object. If it is an object that contains multiple terms, such as `formula`,
#'   the parameters are pluralized and should be contained via a list of
#'   formulas. See details for further explanation.
#'
#'   * `character`
#'
#'   * `formula`
#'
#'   * `lm`
#'
#'   * `data.frame`
#'
#' @param side States the side of the formula the variable belongs on:
#'
#'   * __left__: For variables that are intended to be dependent
#'
#'   * __right__: For variables that are intended to be independent
#'
#'   * __meta__: For variables that are intended to explain relationships
#'   between other variables, e.g. _strata_ or _conditioning_ variables
#'
#' @param role Specific roles the variable plays within the formula. These are
#'   of particular importance, as they serve as special terms that can effect
#'   how a formula is interpreted. The options for roles are as below:
#'
#'   * __exposure__: a predictor variable that serves as a primary or key
#'   variable in the \eqn{Exposure ~ Outcome} relationship
#'
#'   * __outcome__: a outcome/dependent variable that serves as an individual
#'   variable in the \eqn{Exposure ~ Outcome} relationship
#'
#'   * __covariate__: a predictor variable that is used to adjust/control for an
#'   additional primary variable
#'
#'   * __mediator__: a predictor variable that is thought to be a causal
#'   intermediary in the \eqn{Exposure -> Mediator -> Outcome} pathway
#'
#' @param group Grouping variable name for independent variables for modeling
#'   terms together
#'
#' @param operation Modification of the term to be applied when combining with
#'   data
#'
#' @param label Display-quality label describing the variable
#'
#' @param description Option for further descriptions or definitions needed for
#'   the term, potentially part of a data dictionary
#'
#' @param distribution If its associated with a data vector, describes the
#'   distribution pattern of the original term
#'
#' @param class Class of the variable itself, either expected or measured, such
#'   as `character` or `numeric` or `factor`
#'
#' @param type The type of variable, either categorical (qualitative) or
#'   continuous (quantitative)
#'
#' @param subtype How the variable itself is more specifically subcategorized,
#'   e.g. ordinal, continuous, dichotomous, etc
#'
#' @section Pluralized Arguments:
#'
#' For the arguments that would be dispatched for objects that are plural, e.g.
#' containing multiple terms such as a `formula` object, the input should be
#' wrapped within a `list()`.
#'
#' For example, for the __role__ argument, it would be written:
#'
#' `role = list(X ~ "exposure", Y ~ "outcome", M ~ "mediator", C ~ "covariate")`
#'
#' This applies for all others plural objects and arguments.
#'
#' @name tx
#' @export
term_rex <- function(x,
					 side = character(),
					 role = character(),
					 group = character(),
					 operation = character(),
					 label = character(),
					 description = character(),
					 distribution = character(),
					 type = character(),
					 subtype = character(),
					 ...) {

	# Early break
	if (length(x) == 0) {
		message(
			paste0(
				"No `",
				class(x)[1],
				"` object was provided, resulting in a [0] length `term_rex` object."
			)
		)
		return(new_term())
	}

	# Missing values
	if (length(side) == 0)
		side <- NA
	if (length(role) == 0)
		role <- "unknown"
	if (length(group) == 0)
		group <- NA
	if (length(operation) == 0)
		operation <- NA
	if (length(label) == 0)
		label <- NA
	if (length(description) == 0)
		description <- NA
	if (length(distribution) == 0)
		distribution <- NA
	if (length(type) == 0)
		type <- NA
	if (length(subtype) == 0)
		subtype <- NA

	# Casting
	x <- vec_cast(x, character())
	side <- vec_cast(side, character())
	role <- vec_cast(role, character())
	group <- vec_cast(group, character())
	operation <- vec_cast(operation, character())
	label <- vec_cast(label, character())
	description <- vec_cast(description, character())
	distribution <- vec_cast(distribution, character())
	type <- vec_cast(type, character())
	subtype <- vec_cast(subtype, character())

	new_term(
		term = x,
		side = side,
		role = role,
		group = group,
		operation = operation,
		label = label,
		description = description,
		distribution = distribution,
		type = type,
		subtype = subtype
	)
}


#' @rdname tx
#' @export
tx = term_rex

# Record Definition ------------------------------------------------------------

#' record of formula terms
#' @keywords internal
#' @noRd
new_term <- function(term = character(),
                     side = character(),
                     role = character(),
                     group = character(),
                     operation = character(),
                     label = character(),
                     description = character(),
                     distribution = character(),
                     type = character(),
                     subtype = character()) {

  vec_assert(term, ptype = character())
  vec_assert(side, ptype = character())
  vec_assert(role, ptype = character())
  vec_assert(group, ptype = character())
  vec_assert(operation, ptype = character())
  vec_assert(label, ptype = character())
  vec_assert(description, ptype = character())
  vec_assert(distribution, ptype = character())
  vec_assert(type, ptype = character())
  vec_assert(subtype, ptype = character())

  new_rcrd(list(
    "term" = term,
    "side" = side,
    "role" = role,
    "group" = group,
    "operation" = operation,
    "label" = label,
    "description" = description,
    "distribution" = distribution,
    "type" = type,
    "subtype" = subtype
  ),
  class = "term_rex"
  )
}

#' @keywords internal
#' @noRd
methods::setOldClass(c("term_rex", "rcrds_rcrd"))


# Casting and coercion ---------------------------------------------------------

### term() ###

#' @export
vec_ptype2.term_rex.term_rex <- function(x, y, ...) {
  x
}

#' @export
vec_cast.term_rex.term_rex <- function(x, to, ...) {
  x
}

### character() ###

#' @export
vec_ptype2.term_rex.character <- function(x, y, ...) {
  # `x` is term
  # `y` is character
  y
}

#' @export
vec_ptype2.character.term_rex <- function(x, y, ...) {
  # `x` is character
  # `y` is term
  x
}

#' @export
vec_cast.term_rex.character <- function(x, to, ...) {
  # Order is flipped, such that `x` is character
  attributes(x) <- NULL
  x[[1]]
}

#' @export
vec_cast.character.term_rex <- function(x, to, ...) {
  # Order is flipped, such that `x` is term
  attributes(x) <- NULL
  x[[1]]
}

### list_of() ###

#' @export
vec_ptype2.rcrds_list_of.term_rex <- function(x, y, ...) {
  x
}

#' @export
vec_ptype2.term_rex.rcrds_list_of <- function(x, y, ...) {
  y
}

#' @export
vec_cast.rcrds_list_of.term_rex <- function(x, to, ...) {
  tl <- as.list(x) # Convert to list
  lot <- new_list_of(tl, ptype = term_rex()) # make new list of
  lot # return list of terms
}

#' @export
vec_cast.term_rex.rcrds_list_of <- function(x, to, ...) {
  t <- vec_list_cast(x, term_rex()) # Convert to a flattened record
  t # Return record of terms
}

# Arithmetic -------------------------------------------------------------------

#' @export
vec_arith.term_rex <- function(op, x, y, ...) {
  UseMethod("vec_arith.term_rex", y)
}

#' @export
vec_arith.term_rex.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}

#' @export
vec_arith.term_rex.term_rex <- function(op, x, y, ...) {
  switch(op,
    "+" = {
      c(x, y)
    },
    stop_incompatible_op(op, x, y)
  )
}

# Output -----------------------------------------------------------------------

#' @export
format.term_rex <- function(x, ...) {

	tm <- vec_data(x)
	fmt_tx <- character()

	if (vec_size(x) == 0) {
		fmt_tx <- new_term()
	} else if (has_cli() & vec_size(x) > 0) {
		for (i in 1:nrow(tm)) {

			if (tm$role[i] == "outcome") {
				t <- tm$term[i]
				fmt_tx <- append(fmt_tx, cli::col_yellow(t))
			}

			if (tm$role[i] == "exposure") {
				t <- tm$term[i]
				fmt_tx <- append(fmt_tx, cli::col_magenta(t))
			}

			if (tm$role[i] == "mediator") {
				t <- tm$term[i]
				fmt_tx <- append(fmt_tx, cli::col_cyan(t))
			}

			if (tm$role[i] == "covariate") {
				t <- tm$term[i]
				fmt_tx <- append(fmt_tx, cli::col_blue(t))
			}

			if (tm$role[i] == "strata") {
				t <- tm$term[i]
				fmt_tx <- append(fmt_tx, cli::col_br_yellow(t))
			}

			if (tm$role[i] == "unknown") {
				t <- tm$term[i]
				fmt_tx <- append(fmt_tx, cli::col_white(t))
			}

		}
	} else {
		for (i in 1:nrow(tm)) {
			fmt_tx <- append(fmt_tx, tm$term[i])
		}
	}

	# Return
	fmt_tx

}

#' @export
obj_print_data.term_rex <- function(x, ...) {

	if (vec_size(x) == 0) {
		new_term()
	} else if (vec_size(x) > 1) {
		cat(format(x), sep = "\n")
	} else {
		cat(format(x))
	}
}

#' @export
vec_ptype_full.term_rex <- function(x, ...) {
	"term_rex"
}

#' @export
vec_ptype_abbr.term_rex <- function(x, ...) {
	"tx"
}
