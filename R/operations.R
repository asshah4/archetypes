#' Identifying formula-level operations
#' @return A list object of operations
#' @keywords internal
#' @noRd
identify_ops <- function(x = term_rx(), pattern, ...) {

	# Retrieve all basic term information
	tm <- vec_data(x)
	dep_vars <- lhs(x)
	ind_vars <- co_vars <- rhs(x)

	# Special roles
	rls <- getComponent(x, "role")
	exp_vars <- character()
	med_vars <- character()
	for (i in seq_along(rls)) {
		if (rls[[i]] == "exposure") {
			exp_vars <- append(exp_vars, names(rls[i]))
		}
		if (rls[[i]] == "mediator") {
			med_vars <- append(med_vars, names(rls[i]))
		}
	}

	# Groups (grouped variables are not part of traditional covariates)
	grp <- getComponent(x, "group")
	grp_vars <- names(grp)
	grp_vars <- grp_vars[!(grp_vars %in% dep_vars)]
	grp <- grp[grp_vars]
	if (length(grp) > 0) {
		grps <- grp |>
			list_to_table() |>
			unstack() |>
			as.list()
	} else {
		grps <- NA
	}

	# Covariates
	spec_vars <- c(exp_vars, med_vars, grp_vars)
	co_vars <- co_vars[!(co_vars %in% spec_vars)]

	# Instruction list
	list(
		expand_by_pattern = pattern,
		outcomes = dep_vars,
		number_of_outcomes = length(dep_vars),
		predictors = ind_vars,
		number_of_predictors = length(ind_vars),
		exposures = exp_vars,
		number_of_exposures = length(exp_vars),
		mediators = med_vars,
		number_of_mediators = length(med_vars),
		covariates = co_vars,
		number_of_covariates = length(co_vars),
		groups = grps,
		number_of_groups = length(unique(grp))
	)

}

#' Perform the formula level operations
#' @return List of formulas to be converted to the appropriate class
#' @keywords internal
#' @noRd
perform_ops <- function(ops, ...) {

	# For formula expansion:
		# outcomes
		# expansion pattern
		# grouping (if available)

	pattern <- ops$expand_by_pattern

	if (ops$number_of_outcomes >= 1) {
		out <- ops$outcomes
	} else {
		out <- NULL
	}

	if (ops$number_of_predictors >= 1) {
		prd <- ops$predictors
	} else {
		prd <- NULL
	}

	if (ops$number_of_exposures >= 1) {
		exp <- ops$exposures
	} else {
		exp <- character()
	}

	if (ops$number_of_mediators >= 1) {
		med <- ops$mediators
	} else {
		med <- character()
	}

	if (ops$number_of_covariates >= 1) {
		cov <- ops$covariates
	} else {
		cov <- NULL
	}

	# Group members are removed from "covariates" in the operation identification
	if (ops$number_of_groups >= 1) {
		grp_lst <- ops$groups
		grp <- character()
		for (i in seq_along(grp_lst)) {
			g <- paste(grp_lst[[i]], collapse = " + ")
			grp <- append(grp, g)
		}
	} else {
		grp <- NULL
	}

	# Make the right-sided vector of covariates (excluding exposures)
	# These variables are affected by the expansion pattern
	others <- c(grp, cov)

	# Mediation
	if (length(med) > 0) {

		lof <- list()

		# Outcome ~ Exposure
		for (o in seq_along(out)) {
			outcomes <- out[o]
			for (x in seq_along(exp)) {
				exposures <- exp[x]
				switch(
					pattern,
					direct = {
						f <-
							c(exposures, others) |>
							paste(collapse = " + ") |>
							{\(.x) paste(outcomes, .x, sep = " ~ ")}() |>
							as.formula()

						lof <- append(lof, f)
						names(lof)[length(lof)] <-
							paste0("y",
										 o,
										 "x",
										 x,
										 "c",
										 0,
										 "_",
										 substr(pattern, 1, 3))
					},
					sequential = {
						for (n in 0:length(others)) {
							f <-
								c(exposures, others[0:n]) |>
								paste0(collapse = " + ") |>
								{\(.x) paste(outcomes, .x, sep = " ~ ")}() |>
								as.formula()

							lof <- append(lof, f)
							names(lof)[length(lof)] <-
								paste0("y",
											 o,
											 "x",
											 x,
											 "c",
											 n,
											 "_",
											 substr(pattern, 1, 3))
						}
					},
					parallel = {
						for (n in 0:length(others)) {
							f <-
								c(exposures, others[n]) |>
								paste0(collapse = " + ") |>
								{\(.x) paste(outcomes, .x, sep = " ~ ")}() |>
								as.formula()

							lof <- append(lof, f)
							names(lof)[length(lof)] <-
								paste0("y",
											 o,
											 "x",
											 x,
											 "c",
											 n,
											 "_",
											 substr(pattern, 1, 3))
						}
					},
				)
			}
		}

		# Outcome ~ Mediator
		for (o in seq_along(out)) {
			outcomes <- out[o]
			for (m in seq_along(med)) {
				mediators <- med[m]
				f <-
					mediators |>
					{\(.x) paste(outcomes, .x, sep = " ~ ")}() |>
					as.formula()
				lof <- append(lof, f)
				names(lof)[length(lof)] <-
					paste0("y",
								 o,
								 "m",
								 m,
								 "c",
								 0,
								 "_",
								 substr(pattern, 1, 3))
			}
		}

		# Mediator ~ Exposure
		for (m in seq_along(med)) {
			mediators <- med[m]
			for (x in seq_along(exp)) {
				exposures <- exp[x]
				switch(
					pattern,
					direct = {
						f <-
							c(exposures, others) |>
							paste(collapse = " + ") |>
							{\(.x) paste(mediators, .x, sep = " ~ ")}() |>
							as.formula()

						lof <- append(lof, f)
						names(lof)[length(lof)] <-
							paste0("m",
										 m,
										 "x",
										 x,
										 "c",
										 0,
										 "_",
										 substr(pattern, 1, 3))
					},
					sequential = {
						for (n in 0:length(others)) {
							f <-
								c(exposures, others[0:n]) |>
								paste0(collapse = " + ") |>
								{\(.x) paste(mediators, .x, sep = " ~ ")}() |>
								as.formula()

							lof <- append(lof, f)
							names(lof)[length(lof)] <-
								paste0("m",
											 m,
											 "x",
											 x,
											 "c",
											 n,
											 "_",
											 substr(pattern, 1, 3))
						}
					},
					parallel = {
						for (n in 0:length(others)) {
							f <-
								c(exposures, others[n]) |>
								paste0(collapse = " + ") |>
								{\(.x) paste(mediators, .x, sep = " ~ ")}() |>
								as.formula()

							lof <- append(lof, f)
							names(lof)[length(lof)] <-
								paste0("m",
											 m,
											 "x",
											 x,
											 "c",
											 n,
											 "_",
											 substr(pattern, 1, 3))
						}
					},
				)

			}
		}

		# Return
		return(lof)

	} else if (length(med) == 0) {

		lof <- list()

		# Outcome ~ Exposure
		for (o in seq_along(out)) {
			outcomes <- out[o]
			for (x in seq_along(exp)) {
				exposures <- exp[x]
				switch(
					pattern,
					direct = {
						f <-
							c(exposures, others) |>
							paste(collapse = " + ") |>
							{\(.x) paste(outcomes, .x, sep = " ~ ")}() |>
							as.formula()

						lof <- append(lof, f)
						names(lof)[length(lof)] <-
							paste0("y",
										 o,
										 "x",
										 x,
										 "c",
										 0,
										 "_",
										 substr(pattern, 1, 3))
					},
					sequential = {
						for (n in 0:length(others)) {
							f <-
								c(exposures, others[0:n]) |>
								paste(collapse = " + ") |>
								{\(.x) paste(outcomes, .x, sep = " ~ ")}() |>
								as.formula()

							lof <- append(lof, f)
							names(lof)[length(lof)] <-
								paste0("y",
											 o,
											 "x",
											 x,
											 "c",
											 n,
											 "_",
											 substr(pattern, 1, 3))
						}
					},
					parallel = {
						for (n in 0:length(others)) {
							exposures <- exp[x]
							f <-
								c(exposures, others[n]) |>
								paste(collapse = " + ") |>
								{\(.x) paste(outcomes, .x, sep = " ~ ")}() |>
								as.formula()

							lof <- append(lof, f)
							names(lof)[length(lof)] <-
								paste0("y",
											 o,
											 "x",
											 x,
											 "c",
											 n,
											 "_",
											 substr(pattern, 1, 3))
						}
					},
				)
			}
		}

	# Return list of formulas
	return(lof)

	}

}

