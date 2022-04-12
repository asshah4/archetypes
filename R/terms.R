# term_archetype ---------------------------------------------------------------

#' term_archetype records
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' @param x An object of the following types that can be coerced to a
#'   `term_archetype` object. If it is an object that contains multiple terms,
#'   such as `formula`, the parameters are pluralized and should be contained
#'   via a list of formulas. See details for further explanation.
#'
#'   * `character`
#'
#'   * `formula`
#'
#'   * `lm`
#'
#'   * `data.frame`
#'
#' @param side states the side of the formula the variable belongs on:
#'
#'   * __left__: for variables that are intended to be dependent
#'
#'   * __right__: for variables that are intended to be independent
#'
#'   * __meta__: for variables that are intended to explain relationships
#'
#'   * __unknown__: for variables that have unknown or undetermined sides, such
#'   as unknown position between other variables (e.g. potential mediators,
#'   conditioning variables, etc)
#'
#' @param role Specific roles the variable plays within the formula. These are
#'   of particular importance, as they serve as special terms that can effect
#'   how a formula is interpreted. Please see the _Roles_ section below for
#'   further details. The options for roles are as below:
#'
#'   * __outcome__: outcome/dependent variable that serves as an individual
#'   variable in the \eqn{exposure -> outcome} relationship (DEFAULT for LHS variables)
#'
#'   * __predictor__: predictors of the outcomes (DEFAULT for RHS variables)

#'   * __exposure__: predictor variable that serves as a primary or key
#'   variable in the \eqn{exposure -> outcome} relationship
#'
#'   * __confounder__: predictor variable that is thought to be a confounder of
#'   the causal relationship in the \eqn{exposure <- confounder -> outcome}
#'   pathway, normally thought of as an adjustment or controlling variable
#'
#'   * __mediator__: predictor variable that is thought to be a causal
#'   interm_archetypeediary in the \eqn{exposure -> mediator -> outcome} pathway

#'   * __unknown__: default role of a variable that has not yet been assigned a
#'   place, such as a potential intermediary object
#'
#' @param tier Grouping variable names for covariates or __confounders__ for
#'   modeling terms together
#'
#' @param label Display-quality label describing the variable
#'
#' @param description Option for further descriptions or definitions needed for
#'   the term_archetype, potentially part of a data dictionary
#'
#' @param distribution If its associated with a data vector, describes the
#'   distribution pattern of the original term_archetype
#'
#' @param class Class of the variable itself, either expected or measured, such
#'   as `character` or `numeric` or `factor`
#'
#' @param type Type of variable, either categorical (qualitative) or
#'   continuous (quantitative)
#'
#' @param subtype How the variable itself is more specifically subcategorized,
#'   e.g. ordinal, continuous, dichotomous, etc
#'
#' @param operation Modification of the term to be applied when
#'   combining with data
#'
#' @section Pluralized Arguments:
#'
#' For the arguments that would be dispatched for objects that are plural, e.g.
#' containing multiple terms such as a `formula` object, the input should be
#' wrapped within a `list()`.
#'
#' For example, for the __role__ argument, it would be written:
#'
#' `role = list(X ~ "exposure", Y ~ "outcome", M ~ "mediator", C ~ "confounder")`
#'
#' This applies for all others plural objects and arguments.
#'
#' @inheritSection script Roles
#'
#' @name terms
#' @export
term_archetype <- function(x = unspecified(), ...) {
  UseMethod("term_archetype", object = x)
}

#' @rdname terms
#' @export
term_archetype.character <- function(x,
                                     side = character(),
                                     role = character(),
                                     tier = character(),
                                     label = character(),
                                     description = character(),
                                     distribution = character(),
                                     operation = character(),
                                     type = character(),
                                     subtype = character(),
                                     ...) {

  # Early Break if needed
  if (validate_empty(x)) {
    return(new_term())
  }

  # missing values
  if (length(side) == 0) {
    side <- "unknown"
  }
  if (length(role) == 0) {
    role <- "predictor"
  }
  if (length(tier) == 0) {
    tier <- NA
  }
  if (length(operation) == 0) {
    operation <- NA
  }
  if (length(label) == 0) {
    label <- NA
  }
  if (length(description) == 0) {
    description <- NA
  }
  if (length(distribution) == 0) {
    distribution <- NA
  }
  if (length(type) == 0) {
    type <- NA
  }
  if (length(subtype) == 0) {
    subtype <- NA
  }

  # Casting
  x <- vec_cast(x, character())
  side <- vec_cast(side, character())
  role <- vec_cast(role, character())
  tier <- vec_cast(tier, character())
  label <- vec_cast(label, character())
  description <- vec_cast(description, character())
  distribution <- vec_cast(distribution, character())
  type <- vec_cast(type, character())
  subtype <- vec_cast(subtype, character())
  operation <- vec_cast(operation, character())

  new_term(
    terms = x,
    side = side,
    role = role,
    tier = tier,
    operation = operation,
    label = label,
    description = description,
    distribution = distribution,
    type = type,
    subtype = subtype
  )
}

#' @rdname terms
#' @export
term_archetype.formula <- function(x,
                                   role = list(),
                                   tier = list(),
                                   label = list(),
                                   description = list(),
                                   distribution = list(),
                                   type = list(),
                                   subtype = list(),
                                   ...) {

  # Early Break if needed
  if (validate_empty(x)) {
    return(new_term())
  }

  # validate
  validate_class(role, "list")
  validate_class(tier, "list")
  validate_class(label, "list")
  validate_class(description, "list")
  validate_class(distribution, "list")
  validate_class(type, "list")
  validate_class(subtype, "list")
  roles <- formula_to_named_list(role)
  tiers <- formula_to_named_list(tier)
  labels <- formula_to_named_list(label)
  descriptions <- formula_to_named_list(description)
  distributions <- formula_to_named_list(distribution)
  types <- formula_to_named_list(type)
  subtypes <- formula_to_named_list(subtype)

  # All terms are needed to build term_archetype record
  left <- lhs(x)
  right <- rhs(x, tidy = TRUE)
  all <- c(left, right)
  n <- length(all)

  # Roles and operations need to be identified (on which terms they apply)
  right_ops <-
    rhs(x, tidy = FALSE) |>
    paste(collapse = " + ") |>
    {
      \(.x) paste("~", .x)
    }() |>
    stats::as.formula() |>
    all.vars(functions = TRUE, unique = FALSE) |>
    {
      \(.x) grep("~", .x, value = TRUE, invert = TRUE)
    }() |>
    {
      \(.x) grep("\\+", .x, value = TRUE, invert = TRUE)
    }() |>
    {
      \(.x) {
        .y <- as.list(.x[!(.x %in% right)])
        names(.y) <- .x[which(!.x %in% right) + 1]
        .y
      }
    }()

  # check to see if it is a "role" or a data transformation
  which_ops <- right_ops %in% c("O", "X", "M", "C", "S")
  role_ops <- right_ops[which_ops]
  data_ops <- right_ops[!which_ops]

  other <- right[!(right %in% names(role_ops))]
  other_ops <- rep("predictor", length(other))
  names(other_ops) <- other
  other_ops <- as.list(other_ops)

  left_ops <- rep("outcome", length(left))
  names(left_ops) <- left
  left_ops <- as.list(left_ops)

  role_ops <- c(role_ops, left_ops, other_ops)

  for (i in seq_along(role_ops)) {
    if (role_ops[[i]] == "O") {
      role_ops[[i]] <- "outcome"
    }
    if (role_ops[[i]] == "X") {
      role_ops[[i]] <- "exposure"
    }

    if (role_ops[[i]] == "M") {
      role_ops[[i]] <- "mediator"
    }

    if (role_ops[[i]] == "C") {
      role_ops[[i]] <- "confounder"
    }

    if (role_ops[[i]] == "S") {
      role_ops[[i]] <- "strata"
    }
  }

  # create terms
  term_list <- list()

  for (i in 1:n) {
    # make parameters
    t <- all[i]

    # Sides and meta terms
    side <- if (t %in% names(role_ops[role_ops == "strata"])) {
      "meta"
    } else if (t %in% left) {
      "left"
    } else if (t %in% right) {
      "right"
    }

    # Data transforms
    op <- if (t %in% names(data_ops)) {
      data_ops[[t]]
    } else {
      NA
    }

    # Roles
    role <- if (t %in% names(role_ops)) {
      role_ops[[t]]
    } else {
      NA
    }


    # Tiers
    tier <-
      if (t %in% names(tiers) & t %in% names(role_ops[role_ops %in% c("exposure", "mediator", "strata", "outcome")])) {
        message(
          "The term `",
          t,
          "` cannot be given a tier as it is not an ordinary predictor."
        )
      } else if (t %in% names(tiers)) {
        tiers[[t]]
      } else {
        NA
      }

    # Labels
    lab <- if (t %in% names(labels)) {
      labels[[t]]
    } else {
      NA
    }

    # place into term_archetype list after casting appropriate classes
    term_list[[i]] <- term_archetype.character(
      x = vec_cast(t, character()),
      side = vec_cast(side, character()),
      role = vec_cast(role, character()),
      tier = vec_cast(tier, character()),
      operation = vec_cast(op, character()),
      label = vec_cast(lab, character())
    )
  }

  # return as a record of terms
  term_list |>
    vec_list_cast(to = term_archetype())
}

#' @rdname terms
#' @export
term_archetype.data.frame <- function(x, ...) {

  # Early Break if needed
  if (validate_empty(x)) {
    return(new_term())
  }

  # TODO
  message("not currently implemented")
}

#' @rdname terms
#' @export
term_archetype.lm <- function(x,
                              role = list(),
                              tier = list(),
                              label = list(),
                              description = list(),
                              distribution = list(),
                              type = list(),
                              subtype = list(),
                              ...) {

  # Early Break if needed
  if (validate_empty(x)) {
    return(new_term())
  }

  # obtain original formula
  f <- stats::formula(x)

  # generate terms
  term_archetype.formula(
    f,
    role = role,
    tier = tier,
    label = label,
    description = description,
    distribution = distribution,
    type = type,
    subtype = subtype
  )
}

#' @rdname terms
#' @export
term_archetype.glm <- function(x,
                               role = list(),
                               tier = list(),
                               label = list(),
                               description = list(),
                               distribution = list(),
                               type = list(),
                               subtype = list(),
                               ...) {

  # Early Break if needed
  if (validate_empty(x)) {
    return(new_term())
  }

  # obtain original formula
  f <- stats::formula(x)

  # generate terms
  term_archetype.formula(
    f,
    role = role,
    tier = tier,
    label = label,
    description = description,
    distribution = distribution,
    type = type,
    subtype = subtype
  )
}

#' @rdname terms
#' @export
term_archetype.model_fit <- function(x,
                                     role = list(),
                                     tier = list(),
                                     label = list(),
                                     description = list(),
                                     distribution = list(),
                                     type = list(),
                                     subtype = list(),
                                     ...) {

  # Early Break if needed
  if (validate_empty(x)) {
    return(new_term())
  }

  # acceptable model types
  model_types <- c("lm", "glm")

  # get model fit and pass to appropriate term_archetype dispatcher
  m <- x$fit
  if (class(m) %in% model_types) {
    term_archetype(m)
  }
}

#' @rdname terms
#' @export
term_archetype.term_archetype <- function(x, ...) {
  # Early Break if needed
  if (validate_empty(x)) {
    return(new_term())
  }

  # Return the same
  x
}

#' @rdname terms
#' @export
term_archetype.formula_archetype <- function(x, ...) {
  # Early Break if needed
  if (validate_empty(x)) {
    return(new_term())
  }

  # Convert to basic terms
  tl <- term_archetype()
  d <- vec_data(x)[c("outcome", "exposure", "confounder", "mediator", "unknown", "strata")]

  for (i in seq_along(x)) {

    f <- field(x[i], "formula")
    t <-
      stats::formula(f) |>
      term_archetype.formula()

    rl <-
      d[i, ] |>
      unlist() |>
      stats::na.omit()

    rls <- as.list(names(rl))
    names(rls) <- unname(rl)

    tl <- append(tl, set_roles(t, roles = rls))

  }

  # Clean up terms and return
  unique(tl) |>
    vec_data() |>
    {\(.x) {
      .x$side[.x$role %in% c("exposure", "mediator", "predictor")] <- "right"
      .x$side[.x$role == "outcome"] <- "left"
      .x
    }}() |>
    vec_restore(tm()) |>
    unique()

}

#' @rdname terms
#' @export
term_archetype.script <- function(x, ...) {
  # Early Break if needed
  if (validate_empty(x)) {
    return(new_term())
  }

  # Return to terms
  field(x, "terms")[[1]]
}

#' @rdname terms
#' @export
term_archetype.default <- function(x = unspecified(), ...) {
  # Early break
  if (length(x) == 0) {
    return(new_term())
  }

  stop("`term_archetype()` is not defined for a `",
    class(x)[1],
    "` object.",
    call. = FALSE
  )
}


#' @rdname terms
#' @export
tm <- term_archetype

# Record definition ------------------------------------------------------------

#' record of formula term_archetype
#' @keywords internal
#' @noRd
new_term <- function(terms = character(),
                     side = character(),
                     role = character(),
                     tier = character(),
                     label = character(),
                     description = character(),
                     distribution = character(),
                     operation = character(),
                     type = character(),
                     subtype = character(),
                     order = integer()) {
  # Validation
  vec_assert(terms, ptype = character())
  vec_assert(side, ptype = character())
  vec_assert(role, ptype = character())
  vec_assert(tier, ptype = character())
  vec_assert(label, ptype = character())
  vec_assert(description, ptype = character())
  vec_assert(distribution, ptype = character())
  vec_assert(operation, ptype = character())
  vec_assert(type, ptype = character())
  vec_assert(subtype, ptype = character())
  vec_assert(order, ptype = integer())

  # Forced order
  if (length(terms) > 0) {
    order <- 0L
  }

  new_rcrd(
    list(
      "terms" = terms,
      "side" = side,
      "role" = role,
      "tier" = tier,
      "label" = label,
      "description" = description,
      "distribution" = distribution,
      "operation" = operation,
      "type" = type,
      "subtype" = subtype,
      "order" = order
    ),
    class = "term_archetype"
  )
}

#' @keywords internal
#' @noRd
methods::setOldClass(c("term_archetype", "rcrds_rcrd"))

# Output -----------------------------------------------------------------------

#' @export
format.term_archetype <- function(x, ...) {
  tm <- vec_data(x)
  fmt <- character()

  if (vec_size(x) == 0) {
    fmt <- new_term()
  } else if (has_cli() & vec_size(x) > 0) {
    for (i in 1:nrow(tm)) {
      if (tm$role[i] == "outcome") {
        t <- tm$terms[i]
        fmt <- append(fmt, cli::col_yellow(t))
      }

      if (tm$role[i] == "exposure") {
        t <- tm$terms[i]
        fmt <- append(fmt, cli::col_magenta(t))
      }

      if (tm$role[i] == "mediator") {
        t <- tm$terms[i]
        fmt <- append(fmt, cli::col_cyan(t))
      }

      if (tm$role[i] == "confounder") {
        t <- tm$terms[i]
        fmt <- append(fmt, cli::col_blue(t))
      }

      if (tm$role[i] == "strata") {
        t <- tm$terms[i]
        fmt <- append(fmt, cli::col_br_white(t))
      }

      if (tm$role[i] == "unknown") {
        t <- tm$terms[i]
        fmt <- append(fmt, t)
      }

      if (tm$role[i] == "predictor") {
        t <- tm$terms[i]
        fmt <- append(fmt, t)
      }
    }
  } else {
    for (i in 1:nrow(tm)) {
      fmt <- append(fmt, tm$terms[i])
    }
  }

  # return
  fmt
}

#' @export
obj_print_data.term_archetype <- function(x, ...) {
  if (vec_size(x) == 0) {
    new_term()
  } else if (vec_size(x) > 1) {
    cat(format(x), sep = "\n")
  } else {
    cat(format(x))
  }
}

#' @export
vec_ptype_full.term_archetype <- function(x, ...) {
  "term_archetype"
}

#' @export
vec_ptype_abbr.term_archetype <- function(x, ...) {
  "tm"
}

# Casting and coercion ---------------------------------------------------------

### term_archetype() ###

#' @export
vec_ptype2.term_archetype.term_archetype <- function(x, y, ...) {
  x
}

#' @export
vec_cast.term_archetype.term_archetype <- function(x, to, ...) {
  x
}

### character() ###

#' @export
vec_ptype2.term_archetype.character <- function(x, y, ...) {
  # `x` is term_archetype
  # `y` is character
  y
}

#' @export
vec_ptype2.character.term_archetype <- function(x, y, ...) {
  # `x` is character
  # `y` is term_archetype
  x
}

#' @export
vec_cast.term_archetype.character <- function(x, to, ...) {
  # order is flipped, such that `x` is character
  attributes(x) <- NULL
  x[[1]]
}

#' @export
vec_cast.character.term_archetype <- function(x, to, ...) {
  # order is flipped, such that `x` is term_archetype
  attributes(x) <- NULL
  x[[1]]
}

### list_of() ###

#' @export
vec_ptype2.rcrds_list_of.term_archetype <- function(x, y, ...) {
  x
}

#' @export
vec_ptype2.term_archetype.rcrds_list_of <- function(x, y, ...) {
  y
}

#' @export
vec_cast.rcrds_list_of.term_archetype <- function(x, to, ...) {
  tl <- as.list(x) # convert to list
  lot <- new_list_of(tl, ptype = term_archetype()) # make new list of
  lot # return list of term_archetype
}

#' @export
vec_cast.term_archetype.rcrds_list_of <- function(x, to, ...) {
  t <- vec_list_cast(x, term_archetype()) # convert to a flattened record
  t # return record of term_archetype
}

# Formulas

#' @export
formula.term_archetype <- function(x, ...) {
  paste(lhs(x), collapse = " + ") |>
    paste(paste(rhs(x), collapse = " + "), sep = " ~ ") |>
    stats::as.formula()
}

