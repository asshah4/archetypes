#' Identifying formula-level operations
#' @return A list object of operations
#' @keywords internal
#' @noRd
identify_ops <- function(x = term(), pattern) {

  # Retrieve all basic term information
  tm <- vec_data(x)
  dep_vars <- lhs(x)
  ind_vars <- co_vars <- rhs(x)

  # Special roles
  rls <- roles(x)
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
  grp <- groups(x, "group")
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

  # Complete mediation differently than general expansion
  if (length(med) > 0) {
    out_exp <- list()

    # Outcome ~ Exposure
    for (o in seq_along(out)) {
      outcomes <- out[o]

      if (length(exp) == 0) {
        out_exp <-
          pattern_expander(
            formula_list = out_exp,
            pattern = pattern,
            left = outcomes,
            right = NULL,
            others = others
          )
      } else {
        for (x in seq_along(exp)) {
          out_exp <-
            pattern_expander(
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
        c <- paste0("c", 0:length(others))
        m <- paste0("m", 0)
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

    names(out_exp) <- nm

    # Outcome ~ Mediator
    out_med <- list()
    for (o in seq_along(out)) {
      outcomes <- out[o]
      for (m in seq_along(med)) {
        mediators <- med[m]

        out_med <-
          pattern_expander(
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
          pattern_expander(
            formula_list = med_exp,
            pattern = pattern,
            left = mediators,
            right = NULL,
            others = others
          )
      } else {
        for (x in seq_along(exp)) {
          med_exp <-
            pattern_expander(
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
        c <- paste0("c", 0:length(others))
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
          pattern_expander(
            lof,
            pattern = pattern,
            left = outcomes,
            right = NULL,
            others = others
          )
      } else {
        for (x in seq_along(exp)) {
          lof <-
            pattern_expander(
              formula_list = lof,
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
        m <- paste0("m", length(med))
      },
      sequential = {
        y <- paste0("y", seq_along(out))
        x <- paste0("x", seq_along(exp))
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
pattern_expander <- function(formula_list,
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
        }() |>
        stats::as.formula()

      formula_list <- append(formula_list, f)
    },
    sequential = {
      for (n in 0:length(others)) {
        f <-
          c(right, others[0:n]) |>
          paste0(collapse = " + ") |>
          {
            \(.x) paste(left, .x, sep = " ~ ")
          }() |>
          stats::as.formula()

        formula_list <- append(formula_list, f)
      }
    },
    parallel = {

      for (n in 1:length(others)) {
        f <-
          c(right, others[n]) |>
          paste0(collapse = " + ") |>
          {
            \(.x) paste(left, .x, sep = " ~ ")
          }() |>
          stats::as.formula()

        formula_list <- append(formula_list, f)
      }
    },
  )

  # Return
  formula_list
}
