#' Script to List
#' @keywords internal
#' @noRd
construct_script <- function(rx) {

	validate_class(rx, "script")
	pattern <- field(rx, "pattern")
	t <- field(rx, "terms")[[1]]
	rls <- roles(t)
	tbl <- list_to_table(rls, id = "terms", val = "roles")
	tm <- vec_data(t)

	# Variables of each type
	outcomes <- names(rls[rls == "outcome"])
	exposures <- names(rls[rls == "exposure"])
	predictors <- names(rls[rls == "predictor"])
	confounders <- names(rls[rls == "confounder"])
	mediators <- names(rls[rls == "mediator"])
	unknowns <- names(rls[rls == "unknown"])

	# Counts of each variable type
	out <- length(outcomes)
	exp <- length(exposures)
	prd <- length(predictors)
	con <- length(confounders)
	med <- length(mediators)
	unk <- length(unknowns)

	# Covariates and grouped variables that are not part of the main outcome and
	# exposure relationships must be separated out
	tier_list <- tiers(t)
	tier_lvls <- as.character(unique(tier_list))
	tier_vars <- character()
	for (i in seq_along(tier_lvls)) {
		tier_vars[i] <-
			tier_list[tier_list == tier_lvls[i]] |>
			names() |>
			paste(collapse = " + ")
	}

	covariates <-
		c(predictors, confounders) |>
		{\(.x) .x[!(.x %in% names(tier_list))]}() |>
		c(tier_vars)

	# Empty list for combinations
	fl <- list()

	# Normal
	if (med == 0) {
		for (o in seq_along(outcomes)) {
			if (exp == 0) {
				fl <-
					pattern_switch(
						formula_list = fl,
						pattern = pattern,
						left = outcomes[o],
						right = NULL,
						others = covariates
					)
			} else {
				for (x in seq_along(exposures)) {
					fl <-
						pattern_switch(
							formula_list = fl,
							pattern = pattern,
							left = outcomes[o],
							right = exposures[x],
							others = covariates
						)
				}
			}
		}

	}


	# Mediation
	if (med > 0) {
		# Outcome ~ Exposure
		for (o in seq_along(outcomes)) {

			if (exp == 0) {
				fl <- pattern_switch(
					fl,
					pattern = pattern,
					left = outcomes[o],
					right = NULL,
					others = covariates
				)
			} else {
				for (x in seq_along(exposures)) {
					fl <- pattern_switch(
						fl,
						pattern = pattern,
						left = outcomes[o],
						right = exposures[x],
						others = covariates
					)
				}
			}
		}

		# Outcome ~ Mediator
		for (o in seq_along(outcomes)) {

			for (m in seq_along(mediators)) {
				fl <- pattern_switch(
					fl,
					pattern = pattern,
					left = outcomes[o],
					right = mediators[m],
					others = NULL
				)
			}
		}

		# Mediator ~ Exposure
		for (m in seq_along(mediators)) {

			if (exp == 0) {
				fl <- pattern_switch(
					fl,
					pattern = pattern,
					left = mediators[m],
					right = NULL,
					others = covariates
				)
			} else {
				for (x in seq_along(exposures)) {
					fl <- pattern_switch(
						fl,
						pattern = pattern,
						left = mediators[m],
						right = exposures[x],
						others = covariates
					)
				}
			}
		}
	}

	# Back to archetypes
	fa <- formula_archetype()
	for (i in fl) {
		v <-
			i |>
			stats::formula() |>
			all.vars()
		vt <- tbl[tbl$terms %in% v, ]

		f <- formula_archetype(
			x = i,
			outcomes = vt$terms[vt$roles == "outcome"],
			predictors = vt$terms[vt$roles == "predictor"],
			exposures = vt$terms[vt$roles == "exposure"],
			confounders = vt$terms[vt$roles == "confounder"],
			mediators = vt$terms[vt$roles == "mediator"],
			unknowns = vt$terms[vt$roles == "unknown"],
			parent = as.character(rx),
			origin = "script",
			pattern = pattern,
			stage = "simple"
		)

		fa <- append(fa, f)
	}

	# Return list
	fa
}

#' Pattern switcher
#' @return List of formulas
#' @keywords internal
#' @noRd
pattern_switch <- function(formula_list,
						   pattern,
						   left,
						   right,
						   others) {

	switch(
		pattern,
		direct = {
			f <-
				c(right, others) |>
				paste(collapse = " + ") |>
				{
					\(.x) paste(left, .x, sep = " ~ ")
				}()

			formula_list <- append(formula_list, f)
		},
		sequential = {
			for (n in 0:length(others)) {
				f <-
					c(right, others[0:n]) |>
					paste0(collapse = " + ") |>
					{
						\(.x) paste(left, .x, sep = " ~ ")
					}()

				formula_list <- append(formula_list, f)
			}
		},
		parallel = {
			# Modifier for covariates in mediation
			if (is.null(others)) {
				seq_others <- 1
			} else {
				seq_others <- seq_along(others)
			}

			for (n in seq_others) {
				f <-
					c(right, others[n]) |>
					paste0(collapse = " + ") |>
					{
						\(.x) paste(left, .x, sep = " ~ ")
					}()

				formula_list <- append(formula_list, f)
			}
		}
	)

	# Return
	formula_list

}
