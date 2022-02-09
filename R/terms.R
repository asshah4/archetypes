# terms ----

#' Term records
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' @param x An object of the following types that can be coerced to a `term_rx` object
#'
#'   * `character`
#'
#'   * `formula`
#'
#' @param side Left or right hand side of the equation
#'
#' @param role Specific roles the variable plays within the formula. These are
#'   of particular importance, as they serve as special terms that can effect
#'   how a formula is interpreted. The options for roles are as below:
#'
#'   * __exposure__: a predictor variable that serves as a primary or key
#'   variable in the \eqn{Exposure ~ Outcome} relationship
#'
#' @param group Grouping variable name for independent variables for modeling
#'   terms together
#'
#' @param operation Modification of the term to be applied when combining with
#'   data
#'
#' @param label Display-quality label describing the variable
#'
#' @return An object of class `formula_rx`
#' @name trx
#' @export
term_rx <- function(x = character(), ...) {
  UseMethod("term_rx", object = x)
}

#' @rdname trx
#' @export
term_rx.character <- function(x = character(),
                              side = character(),
                              role = character(),
                              group = character(),
                              type = character(),
                              operation = character(),
                              label = character(),
                              ...) {


  # Break early if need be
  if (length(x) == 0) {
    return(new_term())
  }

  # Missing values
  if (length(x) == 0) x <- NA
  if (length(side) == 0) side <- NA
  if (length(role) == 0) role <- "unknown"
  if (length(group) == 0) group <- NA
  if (length(type) == 0) type <- NA
  if (length(operation) == 0) operation <- NA
  if (length(label) == 0) label <- NA

  # Casting
  x <- vec_cast(x, character())
  side <- vec_cast(side, character())
  role <- vec_cast(role, character())
  group <- vec_cast(group, character())
  type <- vec_cast(type, character())
  operation <- vec_cast(operation, character())
  label <- vec_cast(label, character())

  new_term(
    term = x,
    side = side,
    role = role,
    group = group,
    type = type,
    operation = operation,
    label = label
  )
}

#' @rdname trx
#' @export
term_rx.formula <- function(x = formula(),
                            roles = list(),
                            groups = list(),
                            types = list(),
                            labels = list(),
                            ...) {

  # Break early if need be
  if (length(x) == 0) {
    return(new_term())
  }

  # Validate
  groups <- formula_args_to_list(groups)
  labels <- formula_args_to_list(labels)
  roles <- formula_args_to_list(roles)
  validate_class(roles, "list")
  validate_class(groups, "list")
  validate_class(types, "list")
  validate_class(labels, "list")

  # All terms are needed to build term record
  left <- lhs(x)
  right <- rhs(x, tidy = TRUE)
  all <- c(left, right)
  n <- length(all)

  # The roles and operations need to be identified (upon which term they apply)
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

  # Check to see if it is a "role" or a data transformation
  which_ops <- vapply(
    right_ops,
    FUN.VALUE = TRUE,
    function(.x) {
      .y <-
        try(getFromNamespace(.x, c("base", "stats", "utils", "methods")), silent = TRUE)
      if (class(.y) == "try-error") {
        .y <- FALSE
      } else if (class(.y) == "function") {
        .y <- TRUE
      }
    }
  )
  data_ops <- right_ops[which_ops]

  # Roles, with default of LHS as `outcome` and RHS as `covariate`
  role_ops <- right_ops[!which_ops]

  other <- right[!(right %in% names(role_ops))]
  other_ops <- rep("covariate", length(other))
  names(other_ops) <- other
  other_ops <- as.list(other_ops)

  left_ops <- rep("outcome", length(left))
  names(left_ops) <- left
  left_ops <- as.list(left_ops)

  role_ops <- c(roles, role_ops, left_ops, other_ops)

  for (i in seq_along(role_ops)) {
    if (role_ops[[i]] == "X") {
      role_ops[[i]] <- "exposure"
    }

    if (role_ops[[i]] == "M") {
      role_ops[[i]] <- "mediator"
    }
  }

  # Create terms
  term_list <- list()

  for (i in 1:n) {
    # Make parameters
    t <- all[i]
    side <- if (t %in% left) {
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

    # Groups
    grp <- if (t %in% names(groups)) {
      groups[[t]]
    } else {
      NA
    }

    # Groups
    typ <- if (t %in% names(types)) {
      groups[[t]]
    } else {
      NA
    }

    # Labels
    lab <- if (t %in% names(labels)) {
      labels[[t]]
    } else {
      NA
    }

    # Casting
    x <- vec_cast(t, character())
    side <- vec_cast(side, character())
    role <- vec_cast(role, character())
    grp <- vec_cast(grp, character())
    typ <- vec_cast(typ, character())
    op <- vec_cast(op, character())
    lab <- vec_cast(lab, character())


    # Place into term list
    term_list[[i]] <- term_rx.character(
      x = x,
      side = side,
      role = role,
      group = grp,
      type = typ,
      operation = op,
      label = lab
    )
  }

  # Return as a record of terms
  term_list |>
    vec_list_cast(to = term_rx())
}

#' @rdname trx
#' @export
term_rx.formula_rx <- function(x, ...) {
  attr(x, "terms")
}

#' @rdname trx
#' @export
term_rx.default <- function(x, ...) {
  stop("`term()` is not defined for a `", class(x)[1], "` object.",
    call. = FALSE
  )
}

#' @rdname trx
#' @export
trx <- term_rx


# rcrd ----

#' record of formula terms
#' @keywords internal
#' @noRd
new_term <- function(term = character(),
                     side = character(),
                     role = character(),
                     group = character(),
                     type = character(),
                     operation = character(),
                     label = character()) {
  vec_assert(term, ptype = character())
  vec_assert(side, ptype = character())
  vec_assert(role, ptype = character())
  vec_assert(group, ptype = character())
  vec_assert(type, ptype = character())
  vec_assert(operation, ptype = character())
  vec_assert(label, ptype = character())

  new_rcrd(list(
    "term" = term,
    "side" = side,
    "role" = role,
    "group" = group,
    "type" = type,
    "operation" = operation,
    "label" = label
  ),
  class = "term_rx"
  )
}

#' @keywords internal
#' @noRd
methods::setOldClass(c("term_rx", "rcrds_rcrd"))


# casting and coercion ----

### term() ###

#' @export
vec_ptype2.term_rx.term_rx <- function(x, y, ...) {
  x
}

#' @export
vec_cast.term_rx.term_rx <- function(x, to, ...) {
  x
}

### character() ###

#' @export
vec_ptype2.term_rx.character <- function(x, y, ...) {
  # `x` is term
  # `y` is character
  y
}

#' @export
vec_ptype2.character.term_rx <- function(x, y, ...) {
  # `x` is character
  # `y` is term
  x
}

#' @export
vec_cast.term_rx.character <- function(x, to, ...) {
  # Order is flipped, such that `x` is character
  attributes(x) <- NULL
  x[[1]]
}

#' @export
vec_cast.character.term_rx <- function(x, to, ...) {
  # Order is flipped, such that `x` is term
  attributes(x) <- NULL
  x[[1]]
}

### list_of() ###

#' @export
vec_ptype2.rcrds_list_of.term_rx <- function(x, y, ...) {
  x
}

#' @export
vec_ptype2.term_rx.rcrds_list_of <- function(x, y, ...) {
  y
}

#' @export
vec_cast.rcrds_list_of.term_rx <- function(x, to, ...) {
  tl <- as.list(x) # Convert to list
  lot <- new_list_of(tl, ptype = term_rx()) # make new list of
  lot # return list of terms
}

#' @export
vec_cast.term_rx.rcrds_list_of <- function(x, to, ...) {
  t <- vec_list_cast(x, term_rx()) # Convert to a flattened record
  t # Return record of terms
}



# operations ----

#' @export
vec_arith.term_rx <- function(op, x, y, ...) {
  UseMethod("vec_arith.term_rx", y)
}

#' @export
vec_arith.term_rx.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}

#' @export
vec_arith.term_rx.term_rx <- function(op, x, y, ...) {
  switch(op,
    "+" = {
      c(x, y)
    },
    stop_incompatible_op(op, x, y)
  )
}
