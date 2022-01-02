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

#' Divide a Formula into Parts
#'
#' An `rx` formula can be split into simpler parts as needed. If there are
#' multiple dependent variables, then they can be split into individual
#' formulas. If there are multiple independent variables, they can also be split
#' into individual formulas.
#'
#' @return A list of `rx` objects
#'
#' @param x An object of `rx` or `formula` class (or can be coerced into one)
#'
#' @param side Which side of the formula should be divided into parts. This
#'   parameter can is a character vector from `c("lhs", "rhs")`, and defines how
#'   this formula division should occur.
#'
#' @family tools
#' @rdname math
#' @export
divide <- function(x, side, ...) {

	validate_class(x, "rx")

	# Terms
	lhs <- get_lhs(x)
	rhs <- get_rhs(x)

	# Reconstitute left
	if (side == "lhs") {
		flist <-
			paste(rhs, collapse = " + ") %>%
			paste(lhs, ., sep = " ~ ") %>%
			lapply(., formula) %>%
			lapply(., give_env) %>%
			lapply(., rx)
	}

	# Reconstitute right
	if (side == "rhs") {
		flist <-
			paste(lhs, collapse = " + ") %>%
			paste(., rhs, sep = " ~ ") %>%
			lapply(., formula) %>%
			lapply(., give_env) %>%
			lapply(., rx)
	}

	# Return
	flist

}


has_color <- function() {
	isTRUE(requireNamespace("crayon", quietly = TRUE) && crayon::has_color())
}

forks_style <- function(x) {

	# Number of roles
	n <- length(x)

	# Solarized Colors
	yellow <- crayon::make_style("#b58900")
	orange <- crayon::make_style("#cb4b16")
	red <- crayon::make_style("#dc322f")
	magenta <- crayon::make_style("#d33682")
	violet <- crayon::make_style("#6c71c4")
	blue <- crayon::make_style("#268bd2")
	cyan <- crayon::make_style("#2aa198")
	green <- crayon::make_style("#859900")

	# Colors for forks
	role_colors <- list()

	# Based on number of roles
	if (n == 2) {
		role_colors[role_names[1]] <- list(yellow)
		role_colors[role_names[2]] <- list(red)
	} else if (n == 3) {
		role_colors[role_names[1]] <- list(yellow)
		role_colors[role_names[2]] <- list(red)
		role_colors[role_names[3]] <- list(blue)
	} else if (n == 4) {
		role_colors[role_names[1]] <- list(yellow)
		role_colors[role_names[2]] <- list(red)
		role_colors[role_names[3]] <- list(blue)
		role_colors[role_names[4]] <- list(orange)
	}

	# Return
	role_colors
}

# nocov end
