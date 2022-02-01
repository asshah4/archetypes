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
list_of_formulas <- function(x, ...) {
	UseMethod("list_of_formulas", object = x)
}

#' @rdname list_of_formulas
#' @export
list_of_formulas.formula_rx <- function(x,
																				name = deparse1(substitute(x)),
																				...) {

	if (length(x) == 0) {
		return(new_list_of_formulas())
	}

	# Get components from formula
	cl <- as.character(x)
	ops <- attr(x, "operations")
	t <- attr(x, "terms")
	tm <- vec_data(t)

	# Get labels
	labs <- getComponent(t, "label")

	# Get roles
	rls <- getComponent(t, "role")

	# Get groups
	grps <- getComponent(t, "group")

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
list_of_formulas.default <- function(x, ...) {

	# Early break if not viable method dispatch
	if (length(x) == 0) {
		return(new_list_of_formulas())
	} else {
		stop(
			"`list_of_formulas()` is not defined for a `", class(x)[1], "` object.",
			call. = FALSE
		)
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

	new_list_of(
		x = formula_list,
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
fit.list_of_formulas <- function(object, .f, ..., data) {

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
	nms$mediator <- substr(nms$.id, start = 5, stop = 6)
	nms$covariate <- substr(nms$.id, start = 7, stop = 8)

	# Rename the specific terms (if available)
	for (i in 1:nrow(nms)) {
		for (j in c("outcome", "exposure", "mediator", "covariate")) {
			y <- as.integer(substr(nms[[j]][i], start = 2, stop = 2))
			if (y == 0) {

				z <- names(rls)[rls == j]
				if (length(z) == 0 | j != "covariate") {
					z <- NA
				} else {
					z <- paste(z, collapse = ", ")
				}
			} else if (y >= 1) {
				z <- names(rls)[rls == j][y]
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
		subset(select = c(name, pattern, outcome, exposure, covariate, mediator, formula))

}
