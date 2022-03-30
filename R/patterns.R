#' Expand script into a list of formulas
#' @keywords internal
#' @noRd
deconstruct_patterns <- function(rx, pattern) {

  # Validate
  validate_class(rx, "script")

  # Retrieve all basic term information
  t <- field(rx, "terms")[[1]]
  tm <- vec_data(t)
  dep_vars <- lhs(t)
  ind_vars <- co_vars <- rhs(t)

  # Special roles
  rls <- roles(t)
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
  grp <- groups(t, "group")
  grp_vars <- names(grp)
  grp_vars <- grp_vars[!(grp_vars %in% dep_vars)]
  grp <- grp[grp_vars]
  if (length(grp) > 0) {
    grps <- grp |>
      list_to_table() |>
      utils::unstack() |>
      as.list()
  } else {
    grps <- NA
  }

  # Covariates
  spec_vars <- c(exp_vars, med_vars, grp_vars)
  co_vars <- co_vars[!(co_vars %in% spec_vars)]

  # Instructions
  outcomes <- dep_vars
  number_of_outcomes <- length(dep_vars)
  predictors <- ind_vars
  number_of_predictors <- length(ind_vars)
  exposures <- exp_vars
  number_of_exposures <- length(exp_vars)
  mediators <- med_vars
  number_of_mediators <- length(med_vars)
  covariates <- co_vars
  number_of_covariates <- length(co_vars)
  groups <- grps
  number_of_groups <- length(unique(grp))

  if (number_of_outcomes >= 1) {
    out <- outcomes
  } else {
    out <- NULL
  }

  if (number_of_predictors >= 1) {
    prd <- predictors
  } else {
    prd <- NULL
  }

  if (number_of_exposures >= 1) {
    exp <- exposures
  } else {
    exp <- character()
  }

  if (number_of_mediators >= 1) {
    med <- mediators
  } else {
    med <- character()
  }

  if (number_of_covariates >= 1) {
    cov <- covariates
  } else {
    cov <- NULL
  }

  # Group members are removed from "covariates" in the operation identification
  if (number_of_groups >= 1) {
    grp_lst <- groups
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

  # Complete mediation differently than general expansion
  if (length(med) > 0) {
    out_exp <- list()

    # Outcome ~ Exposure
    for (o in seq_along(out)) {
      outcomes <- out[o]

      if (length(exp) == 0) {
        out_exp <-
          pattern_switcher(
            formula_list = out_exp,
            pattern = pattern,
            left = outcomes,
            right = NULL,
            others = others
          )
      } else {
        for (x in seq_along(exp)) {
          out_exp <-
            pattern_switcher(
              formula_list = out_exp,
              pattern = pattern,
              left = outcomes,
              right = exp[x],
              others = others
            )
        }
      }
    }

    switch(pattern,
    	   direct = {
    	   	y <- paste0("y", seq_along(out))
    	   	x <- paste0("x", seq_along(exp))
    	   	c <- paste0("c", ifelse(length(others) > 0, 1, 0))
    	   	m <- paste0("m", 0)
    	   },
    	   sequential = {
    	   	y <- paste0("y", seq_along(out))
    	   	x <- paste0("x", seq_along(exp))
    	   	c <- paste0("c", 0:length(others))
    	   	m <- paste0("m", 0)
    	   },
    	   parallel = {
    	   	y <- paste0("y", seq_along(out))
    	   	x <- paste0("x", seq_along(exp))
    	   	c <- paste0("c", 1:length(others))
    	   	m <- paste0("m", 0)
    	   })

    nm <-
      expand.grid(y = y, x = x, c = c, m = m) |>
      {
        \(.x) paste0(.x$y, .x$x, .x$c, .x$m)
      }() |>
      {
        \(.x) paste0(.x, "_", substr(pattern, start = 1, stop = 3))
      }() |>
      sort()

    names(out_exp) <- nm

    # Outcome ~ Mediator
    out_med <- list()
    for (o in seq_along(out)) {
      outcomes <- out[o]
      for (m in seq_along(med)) {
        mediators <- med[m]

        out_med <-
          pattern_switcher(
            formula_list = out_med,
            pattern = pattern,
            left = outcomes,
            right = mediators,
            others = NULL
          )
      }
    }

    switch(pattern,
      direct = {
        y <- paste0("y", seq_along(out))
        x <- paste0("x", 0)
        c <- paste0("c", 0)
        m <- paste0("m", seq_along(med))
      },
      sequential = {
        y <- paste0("y", seq_along(out))
        x <- paste0("x", 0)
        c <- paste0("c", 0)
        m <- paste0("m", seq_along(med))
      },
      parallel = {
        y <- paste0("y", seq_along(out))
        x <- paste0("x", 0)
        c <- paste0("c", 0)
        m <- paste0("m", seq_along(med))
      }
    )

    nm <-
      expand.grid(y = y, x = x, c = c, m = m) |>
      {
        \(.x) paste0(.x$y, .x$x, .x$c, .x$m)
      }() |>
      {
        \(.x) paste0(.x, "_", substr(pattern, start = 1, stop = 3))
      }() |>
      sort()

    names(out_med) <- nm

    # Mediator ~ Exposure
    med_exp <- list()
    for (m in seq_along(med)) {
      mediators <- med[m]

      if (length(exp) == 0) {
        med_exp <-
          pattern_switcher(
            formula_list = med_exp,
            pattern = pattern,
            left = mediators,
            right = NULL,
            others = others
          )
      } else {
        for (x in seq_along(exp)) {
          med_exp <-
            pattern_switcher(
              formula_list = med_exp,
              pattern = pattern,
              left = mediators,
              right = exp[x],
              others = others
            )
        }
      }
    }

    switch(pattern,
      direct = {
        y <- paste0("y", 0)
        x <- paste0("x", seq_along(exp))
        c <- paste0("c", ifelse(length(others) > 0, 1, 0))
        m <- paste0("m", seq_along(med))
      },
      sequential = {
        y <- paste0("y", 0)
        x <- paste0("x", seq_along(exp))
        c <- paste0("c", 0:length(others))
        m <- paste0("m", seq_along(med))
      },
      parallel = {
        y <- paste0("y", 0)
        x <- paste0("x", seq_along(exp))
        c <- paste0("c", 1:length(others))
        m <- paste0("m", seq_along(med))
      }
    )

    nm <-
      expand.grid(y = y, x = x, c = c, m = m) |>
      {
        \(.x) paste0(.x$y, .x$x, .x$c, .x$m)
      }() |>
      {
        \(.x) paste0(.x, "_", substr(pattern, start = 1, stop = 3))
      }() |>
      sort()

    names(med_exp) <- nm

    lof <- c(out_exp, out_med, med_exp)

    # Return
    return(lof)
  } else if (length(med) == 0) {
    lof <- list()

    # Outcome ~ Exposure
    for (o in seq_along(out)) {
      outcomes <- out[o]

      if (length(exp) == 0) {
        lof <-
          pattern_switcher(
            lof,
            pattern = pattern,
            left = outcomes,
            right = NULL,
            others = others
          )
      } else {
        for (x in seq_along(exp)) {
          lof <-
            pattern_switcher(
              formula_list = lof,
              pattern = pattern,
              left = outcomes,
              right = exp[x],
              others = others
            )
        }
      }
    }


    switch(
      pattern,
      direct = {
        y <- paste0("y", seq_along(out))
        x <- paste0("x", seq_along(exp))
        if (length(exp) == 0) {x <- paste0("x", 0)}
        c <- paste0("c", ifelse(length(others) > 0, 1, 0))
        m <- paste0("m", length(med))
      },
      sequential = {
        y <- paste0("y", seq_along(out))
        x <- paste0("x", seq_along(exp))
        if (length(exp) == 0) {x <- paste0("x", 0)}
        c <- paste0("c", 0:length(others))
        m <- paste0("m", length(med))
      },
      parallel = {
        y <- paste0("y", seq_along(out))
        x <- paste0("x", seq_along(exp))
        c <- paste0("c", 1:length(others))
        m <- paste0("m", length(med))
      }
    )

    nm <-
      expand.grid(y = y, x = x, c = c, m = m) |>
      {
        \(.x) paste0(.x$y, .x$x, .x$c, .x$m)
      }() |>
      {
        \(.x) paste0(.x, "_", substr(pattern, start = 1, stop = 3))
      }() |>
      sort()

    names(lof) <- nm

    # Return list of formulas
    return(lof)
  }

}

#' Pattern switcher
#' @return List of formulas
#' @keywords internal
#' @noRd
pattern_switcher <- function(formula_list,
							 pattern,
							 left,
							 right,
							 others) {


	switch(pattern,
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
		   })

	# Return
	formula_list

}


#' Rebuild into formula list
#' @keywords internal
#' @noRd
reconstruct_patterns <- function(formula_list, terms) {

	validate_class(formula_list, "list")
	nm <- names(formula_list)
	rls <- roles(terms)
	rls[rls == "predictor"] <- "covariate"
	labs <- labels(terms)

	# Group and covariate management
	grps <- groups(terms)

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
	colnames(nms) <- c("tag", "pattern")

	# Always broken into groups by term
		# y = outcome
		# x = exposure
		# m = mediator
		# p = predictor (confounder)

	nms$outcome <- substr(nms$tag, start = 1, stop = 2)
	nms$exposure <- substr(nms$tag, start = 3, stop = 4)
	nms$covariate <- substr(nms$tag, start = 5, stop = 6)
	nms$mediator <- substr(nms$tag, start = 7, stop = 8)

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

	nms$tag <- nm


	# Cleans up final table after merging in formulas
	list_to_table(formula_list, id = "tag", val = "formula") |>
		merge(nms, by = "tag", sort = FALSE) |>
		subset(select = c(tag,
						  pattern,
						  outcome,
						  exposure,
						  covariate,
						  mediator,
						  formula))

}