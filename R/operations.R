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
	for (i in seq_along(rls)) {
		if (rls[[i]] == "exposure") {
			exp_vars <- append(exp_vars, names(rls[i]))
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
	co_vars <- co_vars[!(co_vars %in% exp_vars) & !(co_vars %in% grp_vars)]

	# Instruction list
	list(
		expand_by_pattern = pattern,
		outcomes = dep_vars,
		number_of_outcomes = length(dep_vars),
		predictors = ind_vars,
		number_of_predictors = length(ind_vars),
		exposures = exp_vars,
		number_of_exposures = length(exp_vars),
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
perform_ops <- function(x = term_rx(), ops, ...) {

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
	others <- c(grp, cov)

	switch(
		pattern,
		sequential = {
			f <- list()

			for (i in seq_along(out)) {
				x <- out[i]
				for (j in seq_along(exp)) {
					y <- exp[j]
					for (k in 0:length(others)) {
						z <-
							c(y, others[0:k]) |>
							paste0(collapse = " + ") |>
							{\(.x) paste(x, .x, sep = " ~ ")}() |>
							as.formula()

						f <- append(f, z)
					}
				}
			}

		},
		parallel = {
			f <- list()

			for (i in seq_along(out)) {
				x <- out[i]
				for (j in seq_along(exp)) {
					y <- exp[j]
					for (k in 0:length(others)) {
						z <-
							c(y, others[k]) |>
							paste0(collapse = " + ") |>
							{\(.x) paste(x, .x, sep = " ~ ")}() |>
							as.formula()

						f <- append(f, z)
					}
				}
			}
		},
		direct = {
			f <- list()

			for (i in seq_along(out)) {
				x <- out[i]
				for (j in seq_along(exp)) {
					y <- exp[j]

					z <-
						c(y, others) |>
						paste0(collapse = " + ") |>
						{\(.x) paste(x, .x, sep = " ~ ")}() |>
						as.formula()

					f <- append(f, z)
				}
			}
		}
	)

	# Return list of formulas
	f
}

expand_formula <- function(formula,
													 labels = NULL,
													 combination,
													 table = FALSE,
													 ...) {
	# Initial deparsing of variables
	outcomes <-
		gsub(" ", "", unlist(strsplit(deparse(formula[[2]]), "\ \\+\ ")))
	predictors <- labels(stats::terms(formula))
	exposures <- labels[["exposures"]]
	fixed <- labels[["fixed"]]
	confounders <- labels[["confounders"]]

	# Covariates should not contain any additional labels
	covariates <- setdiff(labels(stats::terms(formula)), c(fixed, exposures))
	covariates <- gsub("\\)", "", gsub("C\\(", "", covariates))

	if (length(exposures) == 0) {
		exposures <- NA
	}

	if (length(fixed) == 0) {
		fixed <- NULL
	}

	# Reset the covariates and ensure fixed variables are primary/upfront
	covariates <- setdiff(covariates, c(fixed, exposures))
	predictors <- c(fixed, covariates)

	# Based on approach
	switch(
		combination,
		direct = {
			num <- sum(any(!is.null(covariates), !is.null(fixed)))

			tbl <-
				tibble::tibble(number = 1:num) %>%
				mutate(., vars = list(predictors)) %>%
				tidyr::expand_grid(exposures = exposures, .) %>%
				mutate(vars = purrr::map2(vars, exposures, ~ c(.y, .x))) %>%
				mutate(vars = purrr::map(vars, ~ na.omit(.x))) %>%
				tidyr::expand_grid(outcomes = outcomes, .)
		},
		sequential = {
			# Number of covariates to sequence through
			mod <- unique(!is.na(exposures))
			num <- sum(mod + length(covariates))

			tbl <-
				tibble::tibble(number = 1:num) %>%
				mutate(vars = purrr::map(
					number,
					~ c(fixed, covariates[(1 - mod):(.x - mod)])
				)) %>%
				tidyr::expand_grid(exposures = exposures, .) %>%
				mutate(vars = purrr::map2(vars, exposures, ~ c(.y, .x))) %>%
				mutate(vars = purrr::map(vars, ~ na.omit(.x))) %>%
				tidyr::expand_grid(outcomes = outcomes, .)
		},
		parallel = {
			# If no exp or fixed, than based on num. of covariates
			# If no covariates, than based on num. of fixed and exposures
			if (length(covariates) == 0) {
				num <- sum(any(c(!is.null(fixed), !is.na(exposures))))
			}

			if (length(covariates) > 0) {
				num <- length(covariates)
			}

			tbl <-
				tibble::tibble(number = 1:num) %>%
				mutate(
					vars = purrr::map(number, ~ c(fixed, covariates[.x]))
				) %>%
				tidyr::expand_grid(exposures = exposures, .) %>%
				mutate(vars = purrr::map2(vars, exposures, ~ c(.y, .x))) %>%
				mutate(vars = purrr::map(vars, ~ na.omit(.x))) %>%
				tidyr::expand_grid(outcomes = outcomes, .)
		}
	)

	# Return expanded formulae
	tbl <- tbl %>%
		mutate(exposures = sapply(exposures, paste, collapse = ", ")) %>%
		mutate(formulae = {
			purrr::map_chr(vars, ~ paste(unlist(.x), collapse = " + ")) %>%
				paste(outcomes, ., sep = " ~ ") %>%
				lapply(., formula)
		})

	# Return either list or table
	if (table) {
		tbl
	} else {
		tbl$formulae
	}
}
