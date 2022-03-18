# Terms ------------------------------------------------------------------------

#' Term records
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' @param x An object of the following types that can be coerced to a `term_rx`
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
term_rx <- function(x = unspecified(), ...) {
  UseMethod("term_rx", object = x)
}

#' @rdname tx
#' @export
term_rx.character <- function(x,
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
				"` object was provided, resulting in a [0] length `term_rx` object."
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
term_rx.formula <- function(x,
							role = list(),
							group = list(),
							label = list(),
							description = list(),
							distribution = list(),
							type = list(),
							subtype = list(),
							...) {
	# Early break
	if (length(x) == 0) {
		message(
			paste0(
				"No `",
				class(x)[1],
				"` object was provided, resulting in a [0] length `term_rx` object."
			)
		)
		return(new_term())
	}

	# Validate
	validate_class(role, "list")
	validate_class(group, "list")
	validate_class(label, "list")
	validate_class(description, "list")
	validate_class(distribution, "list")
	validate_class(type, "list")
	validate_class(subtype, "list")
	roles <- formula_args_to_list(role)
	groups <- formula_args_to_list(group)
	labels <- formula_args_to_list(label)
	descriptions <- formula_args_to_list(description)
	distributions <- formula_args_to_list(distribution)
	types <- formula_args_to_list(type)
	subtypes <- formula_args_to_list(subtype)

	# All terms are needed to build term record
	left <- lhs(x)
	right <- rhs(x, tidy = TRUE)
	all <- c(left, right)
	n <- length(all)

	# The roles and operations need to be identified (upon which term they apply)
	right_ops <-
		rhs(x, tidy = FALSE) |>
		paste(collapse = " + ") |>
		{
			\(.x) paste("~", .x)
		}() |>
		stats::as.formula() |>
		all.vars(functions = TRUE, unique = FALSE) |>
		{
			\(.x) grep("~", .x, value = TRUE, invert = TRUE)
		}() |>
		{
			\(.x) grep("\\+", .x, value = TRUE, invert = TRUE)
		}() |>
		{
			\(.x) {
				.y <- as.list(.x[!(.x %in% right)])
				names(.y) <- .x[which(!.x %in% right) + 1]
				.y
			}
		}()

	# Check to see if it is a "role" or a data transformation
	which_ops <- vapply(right_ops,
						FUN.VALUE = TRUE,
						function(.x) {
							.y <-
								try(getFromNamespace(.x, c("base", "stats", "utils", "methods")), silent = TRUE)
							if (class(.y) == "try-error") {
								.y <- FALSE
							} else if (class(.y) == "function") {
								.y <- TRUE
							}
						})
	data_ops <- right_ops[which_ops]

	# Roles, with default of LHS as `outcome` and RHS as `covariate`
	role_ops <- right_ops[!which_ops]

	other <- right[!(right %in% names(role_ops))]
	other_ops <- rep("covariate", length(other))
	names(other_ops) <- other
	other_ops <- as.list(other_ops)

	left_ops <- rep("outcome", length(left))
	names(left_ops) <- left
	left_ops <- as.list(left_ops)

	role_ops <- c(roles, role_ops, left_ops, other_ops)

	for (i in seq_along(role_ops)) {
		if (role_ops[[i]] == "X") {
			role_ops[[i]] <- "exposure"
		}

		if (role_ops[[i]] == "M") {
			role_ops[[i]] <- "mediator"
		}
	}

	# Create terms
	term_list <- list()

	for (i in 1:n) {
		# Make parameters
		t <- all[i]
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

		# Labels
		lab <- if (t %in% names(labels)) {
			labels[[t]]
		} else {
			NA
		}

		# Place into term list after casting appropriate classes
		term_list[[i]] <- term_rx.character(
			x = vec_cast(t, character()),
			side = vec_cast(side, character()),
			role = vec_cast(role, character()),
			group = vec_cast(grp, character()),
			operation = vec_cast(op, character()),
			label = vec_cast(lab, character())
		)
	}

	# Return as a record of terms
	term_list |>
		vec_list_cast(to = term_rx())
}

#' @rdname tx
#' @export
term_rx.data.frame <- function(x, ...) {
	# Early break
	if (length(x) == 0) {
		message(
			paste0(
				"No `",
				class(x)[1],
				"` object was provided, resulting in a [0] length `term_rx` object."
			)
		)
		return(new_term())
	}

	# TODO
	message("Not currently implemented")
}

#' @rdname tx
#' @export
term_rx.lm <- function(x,
					   role = list(),
					   group = list(),
					   label = list(),
					   description = list(),
					   distribution = list(),
					   type = list(),
					   subtype = list(),
					   ...) {

	# Obtain original formula
	f <- formula(x)

	# Generate terms
	term_rx.formula(
		f,
		role = role,
		group = group,
		label = label,
		description = description,
		distribution = distribution,
		type = type,
		subtype = subtype
	)

}

#' @rdname tx
#' @export
term_rx.glm <- function(x,
					   role = list(),
					   group = list(),
					   label = list(),
					   description = list(),
					   distribution = list(),
					   type = list(),
					   subtype = list(),
					   ...) {

	# Obtain original formula
	f <- formula(x)

	# Generate terms
	term_rx.formula(
		f,
		role = role,
		group = group,
		label = label,
		description = description,
		distribution = distribution,
		type = type,
		subtype = subtype
	)

}

#' @rdname tx
#' @export
term_rx.model_fit <- function(x,
							  role = list(),
							  group = list(),
							  label = list(),
							  description = list(),
							  distribution = list(),
							  type = list(),
							  subtype = list(),
							  ...) {

	# Acceptable model types
	model_types <- c("lm", "glm")

	# Get model fit and pass to appropriate term_rx dispatcher
	m <- x$fit
	if (class(m) %in% model_types) {
		term_rx(m)
	}

}


#' @rdname tx
#' @export
term_rx.formula_rx <- function(x, ...) {
	# Early break
	if (length(x) == 0) {
		message(
			paste0(
				"No `",
				class(x)[1],
				"` object was provided, resulting in a [0] length `term_rx` object."
			)
		)
		return(new_term())
	}

	attr(x, "terms")
}

#' @rdname tx
#' @export
term_rx.default <- function(x = unspecified(), ...) {
	# Early break
	if (length(x) == 0) {
		return(new_term())
	}

	stop("`term()` is not defined for a `",
		 class(x)[1],
		 "` object.",
		 call. = FALSE)

}

#' @rdname tx
#' @export
tx = term_rx

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
  class = "term_rx"
  )
}

#' @keywords internal
#' @noRd
methods::setOldClass(c("term_rx", "rcrds_rcrd"))


# Casting and coercion ---------------------------------------------------------

### term() ###

#' @export
vec_ptype2.term_rx.term_rx <- function(x, y, ...) {
  x
}

#' @export
vec_cast.term_rx.term_rx <- function(x, to, ...) {
  x
}

### character() ###

#' @export
vec_ptype2.term_rx.character <- function(x, y, ...) {
  # `x` is term
  # `y` is character
  y
}

#' @export
vec_ptype2.character.term_rx <- function(x, y, ...) {
  # `x` is character
  # `y` is term
  x
}

#' @export
vec_cast.term_rx.character <- function(x, to, ...) {
  # Order is flipped, such that `x` is character
  attributes(x) <- NULL
  x[[1]]
}

#' @export
vec_cast.character.term_rx <- function(x, to, ...) {
  # Order is flipped, such that `x` is term
  attributes(x) <- NULL
  x[[1]]
}

### list_of() ###

#' @export
vec_ptype2.rcrds_list_of.term_rx <- function(x, y, ...) {
  x
}

#' @export
vec_ptype2.term_rx.rcrds_list_of <- function(x, y, ...) {
  y
}

#' @export
vec_cast.rcrds_list_of.term_rx <- function(x, to, ...) {
  tl <- as.list(x) # Convert to list
  lot <- new_list_of(tl, ptype = term_rx()) # make new list of
  lot # return list of terms
}

#' @export
vec_cast.term_rx.rcrds_list_of <- function(x, to, ...) {
  t <- vec_list_cast(x, term_rx()) # Convert to a flattened record
  t # Return record of terms
}

# Arithmetic -------------------------------------------------------------------

#' @export
vec_arith.term_rx <- function(op, x, y, ...) {
  UseMethod("vec_arith.term_rx", y)
}

#' @export
vec_arith.term_rx.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}

#' @export
vec_arith.term_rx.term_rx <- function(op, x, y, ...) {
  switch(op,
    "+" = {
      c(x, y)
    },
    stop_incompatible_op(op, x, y)
  )
}

# Output -----------------------------------------------------------------------

#' @export
format.term_rx <- function(x, ...) {

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
obj_print_data.term_rx <- function(x, ...) {

	if (vec_size(x) == 0) {
		new_term()
	} else if (vec_size(x) > 1) {
		cat(format(x), sep = "\n")
	} else {
		cat(format(x))
	}
}

#' @export
vec_ptype_abbr.term_rx <- function(x, ...) {
	"tx"
}
