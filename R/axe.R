
#' @export
axe.formula <- function(x,
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
				"` object was provided, resulting in a [0] length `axe` object."
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
		term_list[[i]] <- axe.character(
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
		vec_list_cast(to = axe())
}

#' @rdname tx
#' @export
axe.data.frame <- function(x, ...) {
	# Early break
	if (length(x) == 0) {
		message(
			paste0(
				"No `",
				class(x)[1],
				"` object was provided, resulting in a [0] length `axe` object."
			)
		)
		return(new_term())
	}

	# TODO
	message("Not currently implemented")
}

#' @rdname tx
#' @export
axe.lm <- function(x,
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
	axe.formula(
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
axe.glm <- function(x,
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
	axe.formula(
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
axe.model_fit <- function(x,
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

	# Get model fit and pass to appropriate axe dispatcher
	m <- x$fit
	if (class(m) %in% model_types) {
		axe(m)
	}

}


#' @rdname tx
#' @export
axe.formula_rx <- function(x, ...) {
	# Early break
	if (length(x) == 0) {
		message(
			paste0(
				"No `",
				class(x)[1],
				"` object was provided, resulting in a [0] length `axe` object."
			)
		)
		return(new_term())
	}

	attr(x, "terms")
}

#' @rdname tx
#' @export
axe.default <- function(x = unspecified(), ...) {
	# Early break
	if (length(x) == 0) {
		return(new_term())
	}

	stop("`term()` is not defined for a `",
		 class(x)[1],
		 "` object.",
		 call. = FALSE)

}