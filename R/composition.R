#' Carrying out the instructions of a `script`
#'
#' This function uses the components of a `script` object to expand a list of
#' formulas that can subsequently be used.
#'
#' @param s A `script` object from the `prescribe()` or `rx()` functions
#' @export
compose_script <- function(s) {
  validate_class(s, "script")
  pattern <- field(s, "pattern")
  t <- field(s, "terms")[[1]]
  rls <- roles(t)
  tbl <- list_to_table(rls, id = "terms", val = "roles")
  tm <- vec_data(t)

  # Variables of each type
  outcomes <- names(rls[rls == "outcome"])
  exposures <- names(rls[rls == "exposure"])
  confounders <- names(rls[rls == "confounder"])
  mediators <- names(rls[rls == "mediator"])
  unknowns <- names(rls[rls == "unknown"])
  independent <- names(rls[rls == "independent"])
  dependent <- names(rls[rls == "dependent"])
  strata <- names(rls[rls == "strata"])

  # Outcomes are a mix of both specified outcomes and dependent variables
  outcomes <- c(outcomes, dependent)

  # Counts of each variable type to be potentially included
  out <- length(outcomes) # Mix of response/outcome variables
  exp <- length(exposures)
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
    c(independent, confounders) |>
    {
      \(.x) .x[!(.x %in% names(tier_list))]
    }() |>
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

    rl <- table_to_list(vt)

    f <- formula_archetype(
      x = i,
      role = list_to_formula_args(rl),
      strata = strata,
      family = as.character(rx),
      source = "script",
      pattern = pattern
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
    }
  )

  # Return
  formula_list
}

#' Identify order or complexity of a set of terms or formula
#' @export
decipher <- function(t) {
  validate_class(t, "term_archetype")

  #############
  ### ORDER ###
  #############
  order <- integer()

    # ZEROETH
      # Only single term object
    # FIRST
      # Does not follow rules of roles
      # LHS = 1
      # RHS = 1
    # SECOND
      # Follows rules of roles
      # LHS = 1
      # RHS = exposure + confounder
      # RHS = mediator (no confounders allowed)
      # RHS =/= outcome
    # THIRD
      # Does not follow rules of roles
      # LHS = 1
      # RHS > 1 exposure
      # RHS > 1 mediator
      # RHS = exposure + mediator
    # FOURTH
      # LHS > 1

  vt <- vec_data(t)
  rls <- roles(t)
  outcome <- names(rls[rls == "outcome"])
  predictor <- names(rls[rls == "predictor"])
  exposure <- names(rls[rls == "exposure"])
  confounder <- names(rls[rls == "confounder"])
  mediator <- names(rls[rls == "mediator"])
  unknown <- names(rls[rls == "unknown"])
  strata <- names(rls[rls == "strata"])

  # Number of variables
  out <- length(outcome)
  exp <- length(exposure)
  prd <- length(c(confounder, predictor))
  med <- length(mediator)
  unk <- length(unknown)

  # Number of left and right terms
  left <- sum(out)
  right <- sum(exp, prd, med, unk)

  # Zeroeth order
  if (length(t) == 1) {
    order <- 0L
  }

  # First order
  if (left == 1 & right == 1) {
    order <- 1L
  }

  # Second order
  if (left == 1 & right >= 1) {

    if (med > 0 & right = 1) {
      order <- 2L
    }

    if (exp > 0 & med == 0) {
      order <- 2L
    }

    if (exp == 0 & med == 0 & prd > 1) {
      order <- 2L
    }

  }

  # Third order

  # Fourth order
  if (left > 1) {
    order <- 4L
  }

}
