# nocov start

#' Deprecated deparser
old_deparser <- function(...) {
	# Initial deparsing of variables
	outcomes <-
		gsub(" ", "", unlist(strsplit(deparse(f[[2]]), "\ \\+\ ")))
	predictors <- labels(stats::terms(f))
	exposures <- grep("X\\(", predictors, value = TRUE)
	fixed <- grep("F\\(", predictors, value = TRUE)

	# Confounders need to be identified
	confounders <- grep("C\\(", predictors, value = TRUE)
	confounders <- gsub("\\)", "", gsub("C\\(", "", confounders))

	# Covariates should not contain any additional labels
	covariates <- setdiff(labels(stats::terms(f)), c(fixed, exposures))
	covariates <- gsub("\\)", "", gsub("C\\(", "", covariates))

	# exposures should be cleaned from original modifiers if present, or nulled
	if (length(exposures) > 0) {
		exposures <-
			gsub("\\)$", "", gsub("X\\(", "", exposures)) %>%
			paste(outcomes, ., sep = " ~ ") %>%
			lapply(., stats::formula) %>%
			lapply(., stats::terms) %>%
			lapply(., labels) %>%
			unique() %>%
			unlist()
	} else if (length(exposures) == 0) {
		exposures <- NULL
	}

	# Fixed variables may included mixed effect objects or objects with
	# parenthesis. They should be modified to maintain the original structure.
	if (length(fixed) > 0) {
		fixed <- gsub("\\)$", "", gsub("F\\(", "", fixed))
		fixed[grepl("\\|", fixed)] <-
			gsub("\\(", "", gsub("\\)", "", grep("\\|", fixed, value = TRUE)))
		fixed[grepl("\\|", fixed)] <-
			paste0("(", fixed[grepl("\\|", fixed)], ")")
	} else if (length(fixed) == 0) {
		fixed <- NULL
	}

	# Reset the covariates and ensure fixed variables are primary/upfront
	covariates <- setdiff(covariates, c(fixed, exposures))
	predictors <- c(exposures, covariates, fixed)
	if (all(covariates == predictors)) {
		covariates <- NULL
	}

	# Return labeled list
	list(
		outcomes = outcomes,
		predictors = predictors,
		exposures = exposures,
		covariates = covariates,
		fixed = fixed,
		confounders = confounders
	)

}

# nocov end
