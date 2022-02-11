# list of formulas ----

#' Prescribed Formula Lists
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' This function introduces a super  that combines both the `list` class (and
#' its derivative `list_of`) and the `formula` class.
#'
#' @name list_of_formulas
#' @export
list_of_formulas <- function(x = list_of(), ...) {
	UseMethod("list_of_formulas", object = x)
}

#' @rdname list_of_formulas
#' @export
list_of_formulas.formula_rx <- function(x,
										name = deparse1(substitute(x)),
										pattern = character(),
										...) {

	# Check pattern
	if (length(pattern) == 0) {
		pattern <- "direct"
	}
	if (!pattern %in% c("direct", "sequential", "parallel")) {
		stop(
			"The pattern ", deparse(pattern), " is not yet supported.",
			call. = FALSE
		)
	}

	# Get components from formula
	cl <- as.character(x)
	ops <- attr(x, "operations")
	t <- attr(x, "terms")

	# Get labels
	labs <- labels.term_rx(t)

	# Get roles
	rls <- roles.term_rx(t)

	# Get groups
	grps <- groups.term_rx(t)

	# Expansion of formulas
	lof <- perform_ops(ops)
	names(lof) <-
		sapply(names(lof),
					 function(x) {
					 	paste(name, sep = "_", x)
					 },
					 USE.NAMES = FALSE)

	new_list_of_formulas(
		formula_list = lof,
		labels = labs,
		roles = rls,
		groups = grps
	)
}

#' @rdname list_of_formulas
#' @export
list_of_formulas.default <- function(x = list_of(), ...) {

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

# vctrs ----

#' Formula list
#' @keywords internal
#' @noRd
new_list_of_formulas <- function(formula_list = list(),
								 labels = list(),
								 roles = list(),
								 groups = list()) {
	new_list_of(x = formula_list,
		ptype = list(),
		class = "list_of_formulas",
		labels = labels,
		roles = roles,
		groups = groups
	)

}

#' @keywords internal
#' @noRd
methods::setOldClass(c("list_of_formulas", "vctrs_vctr"))

# casting and coercion ----

#' @export
vec_ptype2.vctrs_list_of.character <- function(x, y, ...) {
	x
}

#' @export
vec_ptype2.character.vctrs_list_of <- function(x, y, ...) {
	y
}

#' @export
vec_cast.vctrs_list_of.character <- function(x, to, ...) {
	cl <- as.list(x) # Make list of characters
	loc <- new_list_of(cl, ptype = character()) # Turn into list_of class
	loc # Return list of characters
}

#' @export
vec_cast.character.vctrs_list_of <- function(x, to, ...) {
	cv <- unlist(x) # Flatten list of characters
	cv # Return character vector (named)
}


# fitting ----

#' Fitting a list of formulas
#'
#' @return A list of model fits
#'
#' @param object A `list_of_formulas` that can be fit by a modeling function,
#'   such as [stats::lm()]
#'
#' @rdname fit
#' @export
fit_fmls <- function(object, .f, ..., data) {

	cl <- match.call()
	args <- list(...)
	validate_class(data, c("tbl_df", "data.frame"))
	args$data <- quote(data)

	.fn <- eval(cl[[3]])
	if (!is.function(.fn)) {
		stop("The argument `.f = ",
				 paste(cl[[3]]),
				 "` is not a acceptable function.")
	}


	y <- lapply(object, function(.x) {
		f <- .x
		do.call(.fn, args = c(formula = f, args))
	})

	y
}

# tables ----

#' Explode out a `list_of_formulas` object
#' @return A `data.frame` object that is an explosion of the underlying
#'   formulas, giving out the underlying terms, patterns, etc corresponding
#'   formula
#' @export
explode <- function(x, ...) {

	validate_class(x, "list_of_formulas")

	nm <- names(x)
	rls <- roles(x)
	labs <- labels(x)

	# Group and covariate management
	grps <- groups(x)

	if (length(grps) > 0) {
		grp_nms <- unlist(unique(grps))
		g <- list()
		for (i in grp_nms) {
			g <-
				names(grps)[grps == i] |>
				{
					\(.x) paste(.x, collapse = ", ")
				}() |>
				{
					\(.x) append(g, .x)
				}()
		}

		cov <-
			names(rls)[rls == "covariate"] |>
			{
				\(.x) .x[!(.x %in% names(grps))]
			}() |>
			{
				\(.x) append(g, .x)
			}()

	} else {
		cov <- as.list(names(rls)[rls == "covariate"])
	}

	# Name/term splits
	nms <-
		strsplit(nm, "_") |>
		{\(.x) do.call(rbind, .x)}() |>
		data.frame()
	colnames(nms) <- c("name", ".id", "pattern")

	# Always broken into groups by term
		# y = outcome
		# x = exposure
		# m = mediator
		# p = predictor (confounder)

	nms$outcome <- substr(nms$.id, start = 1, stop = 2)
	nms$exposure <- substr(nms$.id, start = 3, stop = 4)
	nms$covariate <- substr(nms$.id, start = 5, stop = 6)
	nms$mediator <- substr(nms$.id, start = 7, stop = 8)

	# Rename the specific terms (if available)
	for (i in 1:nrow(nms)) {
		for (j in c("outcome", "exposure", "covariate", "mediator")) {
			y <- as.integer(substr(nms[[j]][i], start = 2, stop = 2))
			if (y == 0) {

				z <- NA
				# z <- names(rls)[rls == j]
				# if (length(z) == 0 | j != "covariate") {
				# 	z <- NA
				# } else {
				# 	z <- paste(z, collapse = ", ")
				# }
			} else if (y >= 1) {
				if (j == "covariate") {
					z <- cov[[y]]
				} else {
					z <- names(rls)[rls == j][y]
				}
			}

			nms[[j]][i] <- z
		}
	}

	nms[names(nms) == ".id"] <- nm


	# Cleans up final table after merging in formulas
	tbl <-
		list_to_table(x, id = ".id", val = "formula") |>
		merge(nms, by = ".id", sort = FALSE)

	# Return
	tbl |>
		subset(select = c(name,
						  pattern,
						  outcome,
						  exposure,
						  covariate,
						  mediator,
						  formula))

}
