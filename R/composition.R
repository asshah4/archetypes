#' Decompose scripts into a level/order below, down to level 2 for formula
#' @noRd
decompose_roles <- function(s) {

  # Validation, also can take more than one script at a time
  validate_class(s, "script")
  sl <- s

  for (i in seq_along(sl)) {
    t <- field(s[i], "terms")[[1]]
    vt <- vec_data(t)
    order <- field(s[i], "order")

    # Roles
    rls <- roles(t)
    outcome <- names(rls[rls == "outcome"])
    predictor <- names(rls[rls == "predictor"])
    exposure <- names(rls[rls == "exposure"])
    confounder <- names(rls[rls == "confounder"])
    mediator <- names(rls[rls == "mediator"])
    covariates <- c(confounder, predictor)

    # Creating formulas one level down
    if (order == 3) {

      # Exposure on the right
      for (j in seq_along(exposure)) {
        f <- paste0(
          outcome,
          " ~ ",
          paste(c(exposure[j], covariates), collapse = " + ")
        )
        mt <- match_terms(t, stats::formula(f))
        p <- field(s[i], "pattern")
        sl <- append(
          sl,
          new_script(
            formula = f,
            terms = mt,
            pattern = p,
            order = decipher(mt)
          )
        )
      }

      # Mediation if present
      if (length(mediator) > 0) {
        for (j in 1:seq_along(mediator)) {
          # Mediator on the right
          f <- paste0(
            outcome,
            " ~ ",
            mediator[j]
          )
          mt <- match_terms(t, stats::formula(f))
          p <- field(s[i], "pattern")
          sl <- append(
            sl,
            new_script(
              formula = f,
              terms = mt,
              pattern = p,
              order = decipher(mt)
            )
          )

          # Mediator on the left
          f <- paste0(
            mediator[j],
            " ~ ",
            paste(c(exposure, covariates), collapse = " + ")
          )
          mt <- match_terms(t, stats::formula(f))
          p <- field(s[i], "pattern")
          sl <- append(
            sl,
            new_script(
              formula = f,
              terms = mt,
              pattern = p,
              order = decipher(mt)
            )
          )
        }
      }
    }

    if (order == 4) {
      for (j in seq_along(outcome)) {
        f <- paste0(
          outcome[j],
          " ~ ",
          paste(c(exposure, covariates), collapse = " + ")
        )
        mt <- match_terms(t, stats::formula(f))
        p <- field(s[i], "pattern")
        sl <- append(
          sl,
          new_script(
            formula = f,
            terms = mt,
            pattern = p,
            order = decipher(mt)
          )
        )
      }
    }
  }

  # Return scripts, expected to have one level order less
  unique(sl)
}

#' Expand the patterns that affect the covariates of a term set
#' @noRd
decompose_patterns <- function(s) {

  # Validation, also can take more than one script at a time
  validate_class(s, "script")

  t <- field(s, "terms")[[1]]
  vt <- vec_data(t)
  pattern <- field(s, "pattern")

  # Roles
  rls <- roles(t)
  outcome <- names(rls[rls == "outcome"])
  predictor <- names(rls[rls == "predictor"])
  exposure <- names(rls[rls == "exposure"])
  confounder <- names(rls[rls == "confounder"])
  mediator <- names(rls[rls == "mediator"])

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
    c(confounder, predictor) |>
    {
      \(.x) .x[!(.x %in% names(tier_list))]
    }() |>
    c(tier_vars)

  # Empty list for combinations for all combinations
  fl <- list()

  switch(pattern,
    direct = {
      f <-
        c(exposure, mediator, covariates) |>
        paste(collapse = " + ") |>
        {
          \(.x) paste(outcome, .x, sep = " ~ ")
        }()

      fl <- append(fl, f)
    },
    sequential = {
      for (n in 0:length(covariates)) {
        f <-
          c(exposure, mediator, covariates[0:n]) |>
          paste0(collapse = " + ") |>
          {
            \(.x) paste(outcome, .x, sep = " ~ ")
          }()

        fl <- append(fl, f)
      }
    },
    parallel = {
      # Modifier for covariates in mediation
      if (is.null(covariates)) {
        seq_covariates <- 1
      } else {
        seq_covariates <- seq_along(covariates)
      }

      for (n in seq_covariates) {
        f <-
          c(exposure, mediator, covariates[n]) |>
          paste0(collapse = " + ") |>
          {
            \(.x) paste(outcome, .x, sep = " ~ ")
          }()

        fl <- append(fl, f)
      }
    }
  )

  # Return
  fl
}


#' Identify order or complexity of a set of terms or formula
#' @export
decipher <- function(t) {
  validate_class(t, "term_archetype")

  ### ORDER ###

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
  if (length(t) == 2) {
    order <- 1L
  }

  # Second order
  if (length(t) >= 2) {
    if (out == 1 & any(exp) & med == 0) {
      order <- 2L
    }
    if (out == 0 & med == 1 & exp == 1) {
      order <- 2L
    }
    if (out == 1 & med == 1 & exp == 0 & prd == 0) {
      order <- 2L
    }
    if (out == 1 & prd > 1 & exp == 0) {
      order <- 2L
    }
    if (out == 1 & prd > 1 & exp == 0) {
      order <- 2L
    }
    if (out == 1 & prd >= 1 & exp == 1) {
      order <- 2L
    }
  }

  # Third order
  if (length(t) > 2) {
    if (all(exp, med)) {
      order <- 3L
    }
    if (exp > 1) {
      order <- 3L
    }
    if (med > 1) {
      order <- 3L
    }
  }

  # Fourth order
  if (left > 1) {
    order <- 4L
  }

  # Return
  order
}
