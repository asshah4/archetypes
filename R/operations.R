#' Identifying formula-level operations
#' @return A list object of operations
#' @keywords internal
#' @noRd
identify_ops <- function(x = term_rcrd(), pattern, ...) {

	# Potential operations
		# Split by dependent variables
		# Split by special independent variables
		# Add covariates in sequence
		# Add covariates in parallel

	tm <- vec_data(x)
	dv <- lhs(x)
	iv <- rhs(x)
	rls <- getComponent(x, "role")

	# Groups
	grp <- getComponent(x, "group")
	grp_tbl <- list_to_table(grp, id = "term", val = "group")
	grp_ct <- table(grp_tbl) |> colSums()
	grp_lst <- unstack(grp_tbl)

	# Instruction list
	list(
		expand_by_pattern = pattern,
		split_by_outcomes = dv,
		number_of_groups = length(grp_ct),
		group_list = grp_lst
	)

}

#' Perform the formula level operations
#' @return List of formulas to be converted to the appropriate class
#' @keywords internal
#' @noRd
perform_ops <- function(x = term_rcrd(), ops, ...) {

	# For formula expansion:
		# outcomes
		# expansion pattern
		# grouping (if available)

	out <- ops$split_by_outcomes
	ptrn <- ops$expand_by_pattern

	# Right hand side first
	right_lst <- list()

	#


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
