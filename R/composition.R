#' Recompose scripts into a level/order below, down to level 2 for formula
#' @noRd
recompose_roles <- function(s) {

  # Validation, also can take more than one script at a time
  validate_class(s, "script")
  sl <- s

  for (i in seq_along(sl)) {
    t <- field(sl[i], "terms")[[1]]
    order <- decipher(t)
    p <- field(sl[i], "pattern")

    # Roles
    rls <- roles(t)
    outcome <- names(rls[rls == "outcome"])
    predictor <- names(rls[rls == "predictor"])
    exposure <- names(rls[rls == "exposure"])
    confounder <- names(rls[rls == "confounder"])
    mediator <- names(rls[rls == "mediator"])
    strata <- names(rls[rls == "strata"])
    covariates <- c(confounder, predictor)

    # Creating formulas one level down
    if (order == 2) {
      if (length(mediator) > 0 & length(outcome) == 0) {
        left <- mediator
        right <- setdiff(rhs(t), mediator)
      } else {
        left <- lhs(t)
        right <- rhs(t)
      }

      if (p == "direct") {
        right <- paste0(right, collapse = " + ")
      }

      for (j in seq_along(left)) {
        for (k in seq_along(right)) {

            f <- paste0(left[j], " ~ ", right[k])
            if (length(strata) > 0) { for (l in seq_along(strata)) {
              mt <-
                match_terms(t, stats::formula(f)) |>
                add(tm(strata[l], role = "strata", side = "meta"))
              sl <- append(
                sl,
                new_script(
                  formula = f,
                  terms = mt,
                  pattern = p,
                  order = decipher(mt)
                )
              )
            }} else {
              mt <- match_terms(t, stats::formula(f))
              p <- field(sl[i], "pattern")
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
    }

    if (order == 3) {

      # Exposure on the right if outcome is present
      if (length(outcome) > 0) {
        for (j in seq_along(exposure)) {
          f <- paste0(
            outcome,
            " ~ ",
            paste(c(exposure[j], covariates), collapse = " + ")
          )
          if (length(strata) > 0) for (k in seq_along(strata)) {
            mt <-
              match_terms(t, stats::formula(f)) |>
              add(tm(strata[k], role = "strata", side = "meta"))
            p <- field(sl[i], "pattern")
            sl <- append(
              sl,
              new_script(
                formula = f,
                terms = mt,
                pattern = p,
                order = decipher(mt)
              )
            )
          } else {
            mt <- match_terms(t, stats::formula(f))
            p <- field(sl[i], "pattern")
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

      # Mediation if present
      if (length(mediator) > 0) {
        for (j in 1:seq_along(mediator)) {
          # Mediator on the right if outcome is available
          if (length(outcome) > 0) {
            f <- paste0(
              outcome,
              " ~ ",
              mediator[j]
            )
            if (length(strata) > 0) for (k in seq_along(strata)) {
              mt <-
                match_terms(t, stats::formula(f)) |>
                add(tm(strata[k], role = "strata", side = "meta"))
              p <- field(sl[i], "pattern")
              sl <- append(
                sl,
                new_script(
                  formula = f,
                  terms = mt,
                  pattern = p,
                  order = decipher(mt)
                )
              )
            } else {
              mt <- match_terms(t, stats::formula(f))
              p <- field(sl[i], "pattern")
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

          # Mediator on the left
          f <- paste0(
            mediator[j],
            " ~ ",
            paste(c(exposure, covariates), collapse = " + ")
          )
          # Adding strata to the decomposition if needed
          if (length(strata) > 0) for (k in seq_along(strata)) {
            mt <-
              match_terms(t, stats::formula(f)) |>
              add(tm(strata[k], role = "strata", side = "meta"))
            p <- field(sl[i], "pattern")
            sl <- append(
              sl,
              new_script(
                formula = f,
                terms = mt,
                pattern = p,
                order = decipher(mt)
              )
            )
          } else {
            mt <- match_terms(t, stats::formula(f))
            p <- field(sl[i], "pattern")
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
    }

    if (order == 4) {
      for (j in seq_along(outcome)) {
        f <- paste0(
          outcome[j],
          " ~ ",
          paste(c(exposure, mediator, covariates), collapse = " + ")
        )
        mt <- match_terms(t, stats::formula(f))
        p <- field(sl[i], "pattern")
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

#' Decompose and expand the patterns that affect the covariates of a script
#' @noRd
decompose_patterns <- function(s) {

  # Validation, also can take more than one script at a time
  validate_class(s, "script")

  # Empty list for combinations for all combinations
  fl <- list()

  # Handle the special case of the order being 4
  for (i in seq_along(s)) {

    if (field(s[i], "order") == 4) {
      fl <- append(fl, as.character(s[i]))
    }

  }

  for (i in seq_along(s)) {

    t <- field(s[i], "terms")[[1]]
    vt <- vec_data(t)
    pattern <- field(s[i], "pattern")

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

    # Define left and right
    if (length(mediator) > 0) {
      left <- mediator
      right <- c(outcome, exposure)
    }
    if (length(mediator) == 0) {
      left <- outcome
      right <- exposure
    }

    switch(pattern,
      direct = {
        f <-
          c(right, covariates) |>
          paste(collapse = " + ") |>
          {
            \(.x) paste(left, .x, sep = " ~ ")
          }()

        fl <- append(fl, f)
      },
      sequential = {
        p <- ifelse(length(right) == 0, 1, 0)
        for (n in p:length(covariates)) {
          f <-
            c(right, covariates[0:n]) |>
            paste0(collapse = " + ") |>
            {
              \(.x) paste(left, .x, sep = " ~ ")
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
            c(right, covariates[n]) |>
            paste0(collapse = " + ") |>
            {
              \(.x) paste(left, .x, sep = " ~ ")
            }()

          fl <- append(fl, f)
        }
      },
      fundamental = {
        all_right <- c(right, covariates)
        for (j in seq_along(outcome)) {
          for (k in seq_along(all_right)) {
            f <- paste(outcome[j], all_right[k], sep = " ~ ")
            fl <- append(fl, f)
          }
        }
      },
    )
  }

  # Return
  unique(fl)
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
  # Strata = 0

  # SECOND
  # Follows rules of roles
  # LHS = 1
  # RHS = exposure + confounder
  # RHS = mediator (no confounders allowed)
  # RHS =/= outcome
  # Strata = 1

  # THIRD
  # Does not follow rules of roles
  # LHS = 1
  # RHS > 1 exposure
  # RHS > 1 mediator
  # RHS = exposure + mediator
  # Strata > 1

  # FOURTH
  # LHS > 1

  rls <- roles(t)
  outcome <- names(rls[rls == "outcome"])
  predictor <- names(rls[rls == "predictor"])
  exposure <- names(rls[rls == "exposure"])
  confounder <- names(rls[rls == "confounder"])
  mediator <- names(rls[rls == "mediator"])
  unknown <- names(rls[rls == "unknown"])
  strata <- names(rls[rls == "strata"])

  # Number of variables
  out <- length(unique(outcome))
  exp <- length(exposure)
  prd <- length(c(confounder, predictor))
  med <- length(mediator)
  unk <- length(unknown)
  sta <- length(strata)

  # Number of left and right terms
  left <- sum(out)
  right <- sum(exp, prd, med, unk)
  n <- sum(left, right)

  # Zeroeth order
  if (length(t) == 1) {
    order <- 0L
  }

  # First order
  if (n == 2) {
    order <- 1L
  }

  # Second order
  if (length(t) >= 2 & sta <= 1) {
    if (out == 1 & any(exp) & med == 0) {
      order <- 2L
    }
    if (out == 0 & med == 1 & exp == 1) {
      order <- 2L
    }
    if (out == 1 & med == 1 & exp == 0 & prd == 0) {
      order <- 2L
    }
    if (out == 1 & prd > 1 & exp == 0 & med == 0) {
      order <- 2L
    }
    if (out == 1 & prd > 1 & exp == 1 & med == 0) {
      order <- 2L
    }
    if (sta == 1 & exp == 1) {
      order <- 2L
    }
  }

  # Third order
  if (length(t) > 2) {
    if (all(out, exp, med)) {
      order <- 3L
    }
    if (exp > 1) {
      order <- 3L
    }
    if (med > 1) {
      order <- 3L
    }
    if (sta > 1) {
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
