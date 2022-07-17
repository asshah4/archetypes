# Conversion -------------------------------------------------------------------

#' Convert between lists, list-formulas, and tables
#'
#' @param x
#'
#' * For `list_to_table()`: A named `list` object
#'
#' * For `table_to_list()`: A `data.frame` object
#'
#' @param id Name of column that contains term_archetype
#'
#' @param val Name of column that contains specific values
#'
#' @param ... Further arguments passed to or from other methods
#'
#' @details
#'
#' `table_to_list()`:
#'
#' Takes a `data.frame` and uses the columns to generate a named list. This
#' removes the original column names, as it assumes that the data is contained
#' within the frame itself. It defaults to using the first column as the names
#' of the list.
#'
#' `formula_to_named_list()`:
#'
#' Handling of list-formula arguments. Stylistic choice to make arguments
#' entered in the form of a list, with each entry being a formula. The LHS will
#' always be the terms, and the RHS will always be the non-terms item (e.g.
#' group, label, role, etc).
#'
#' `named_list_to_formula()`:
#'
#' Converts a named list set to a formula pattern. The LHS is the term, and the
#' RHS is the value, such as a label, role, or tier.
#'
#' @name list-helpers
#' @export
list_to_table <- function(x, id = "terms", val = "ops", ...) {
  tbl <- as.data.frame(cbind(names(x), unlist(unname(x))))
  colnames(tbl) <- c(id, val)
  tbl
}

#' @rdname list-helpers
#' @export
table_to_list <- function(x, id = "terms", ...) {
  validate_class(x, "data.frame")

  if (ncol(x) == 2) {
    tbl <- x
    nms <- tbl[[id]]
    val <- tbl[[which(!colnames(tbl) %in% id)]]
    names(val) <- nms
    return(as.list(val))
  } else if (ncol(x) == 1) {
    tbl <- x
    return(as.list(tbl))
  } else {
    stop("table_to_list() requires there to be a data.frame of either 1 or 2 columns.",
      call. = FALSE
    )
  }
}

#' @rdname list-helpers
#' @export
formula_to_named_list <- function(x, ...) {
  validate_class(x, "list")

  pl <- list()

  for (i in seq_along(x)) {

    # term_archetype (left)
    f <- x[[i]]

    if (class(f[[2]]) == "character" | class(f[[2]]) == "name") {
      t <- as.character(f[[2]])
    } else if (class(f[[2]]) == "call") {
      t <- as.character(f[[2]])[-1]
    }

    # Descriptor (right)
    if (class(f[[3]]) == "character" | class(f[[3]]) == "name") {
      d <- as.character(f[[3]])
    } else if (class(f[[3]]) == "call") {
      d <- as.character(f[[3]])[-1]
    }

    y <- rep(d, length(t))
    names(y) <- t
    pl <- append(pl, y)
  }

  # Return paired/named list
  pl
}

#' @rdname list-helpers
#' @export
named_list_to_formula <- function(x, ...) {
  validate_class(x, "list")

  fl <- list()
  for (i in seq_along(x)) {
    f <-
      paste0(names(x)[i], " ~ ", '"', unname(x)[i], '"') |>
      stats::formula()
    fl <- append(fl, f)
  }

  # Returned list of formula arguments
  fl
}

# Formula Tools ----------------------------------------------------------------

#' Tools for working with formula-like objects
#' @name sides
#' @export
lhs <- function(x, ...) {
  UseMethod("lhs", object = x)
}

#' @rdname sides
#' @export
rhs <- function(x, ...) {
  UseMethod("rhs", object = x)
}

#' @rdname sides
#' @export
rhs.term_archetype <- function(x, ...) {
  tms <- vec_data(x)
  tms$terms[tms$side == "right"]
}

#' @rdname sides
#' @export
lhs.term_archetype <- function(x, ...) {
  tms <- vec_data(x)
  tms$terms[tms$side == "left"]
}

#' @rdname sides
#' @export
rhs.formula_archetype <- function(x, ...) {
  field(x, "right")[[1]]
}

#' @rdname sides
#' @export
lhs.formula_archetype <- function(x, ...) {
  field(x, "left")[[1]]
}

#' @rdname sides
#' @param tidy Logical value to decide if operations should be removed from the
#'   terms. If `FALSE`, then the operations will remain included.
#' @export
rhs.formula <- function(x, tidy = FALSE, ...) {
  if (length(x) == 2) {
    pos <- 2
  }
  if (length(x) == 3) {
    pos <- 3
  }

  if (tidy) {
    x[[pos]] |>
      deparse1() |>
      {
        \(.x) paste("~", .x)
      }() |>
      stats::as.formula() |>
      all.vars(functions = FALSE, unique = FALSE)
  } else {
    labels(stats::terms(x))
  }
}

#' @rdname sides
#' @export
lhs.formula <- function(x, tidy = FALSE, ...) {
  if (length(x) == 2) {
    return(character())
  }

  # Shift over to simplify evaluation
  y <-
    x[[2]] |>
    deparse1() |>
    {
      \(.x) paste("~", .x)
    }() |>
    stats::as.formula()

  if (tidy) {
    left <- all.vars(y, functions = FALSE, unique = FALSE)
  } else {
    left <- labels(stats::terms(y))
  }

  # Return
  left
}

#' @rdname sides
#' @export
rhs.script <- function(x, ...) {
  x |>
    tm() |>
    rhs()
}

#' @rdname sides
#' @export
lhs.script <- function(x, ...) {
  x |>
    tm() |>
    lhs()
}

# Getters ----

#' Retrieval functions for `forks` classes
#' @name getters
#' @export
roles <- function(x, ...) {
  UseMethod("roles", object = x)
}

#' @rdname getters
#' @export
roles.term_archetype <- function(x, ...) {
  vec_data(x) |>
    {
      \(.x) .x[, c("terms", "role")]
    }() |>
    table_to_list()
}

#' @rdname getters
#' @export
roles.script <- function(x, ...) {
  attr(x, "term_archetype") |>
    vec_data() |>
    {
      \(.x) .x[, c("terms", "role")]
    }() |>
    table_to_list()
}

#' @rdname getters
#' @export
get_terms <- function(x, field = NA, value = NA) {
  if (class(x)[1] == "term_archetype") {
    if (!is.na(field) & !is.na(field)) {
      t <- x[field(x, field) == value]
    } else {
      t <- x
    }
  }

  if (class(x)[1] == "formula_archetype") {
    # Convert to basic terms
    tl <- term_archetype()

    for (i in seq_along(x)) {
      t <- c(
        field(x[i], "outcome")[[1]],
        field(x[i], "predictor")[[1]],
        field(x[i], "exposure")[[1]],
        field(x[i], "confounder")[[1]],
        field(x[i], "mediator")[[1]],
        field(x[i], "unknown")[[1]],
        field(x[i], "strata")[[1]]
      )

      tl <- append(tl, t)
    }

    t <- unique(tl)
  }


  t
}


#' @rdname getters
#' @export
labels.term_archetype <- function(object, ...) {
  vec_data(object) |>
    {
      \(.x) .x[, c("terms", "label")]
    }() |>
    table_to_list() |>
    {
      \(.x) .x[!is.na(.x)]
    }()
}

#' @rdname getters
#' @export
labels.script <- function(object, ...) {
  object |>
    tm() |>
    labels.term_archetype()
}

#' @rdname getters
#' @export
labels.list_of_formulas <- function(object, ...) {
  attr(object, "terms") |>
    labels.term_archetype()
}


#' @rdname getters
#' @export
tiers <- function(x, ...) {
  UseMethod("tiers", object = x)
}

#' @rdname getters
#' @export
tiers.term_archetype <- function(x, ...) {
  vec_data(x) |>
    {
      \(.x) .x[, c("terms", "tier")]
    }() |>
    table_to_list() |>
    {
      \(.x) .x[!is.na(.x)]
    }()
}

#' @rdname getters
#' @export
tiers.script <- function(x, ...) {
  attr(x, "terms") |>
    vec_data() |>
    {
      \(.x) .x[, c("terms", "tier")]
    }() |>
    table_to_list() |>
    {
      \(.x) .x[!is.na(.x)]
    }()
}

#' @rdname getters
#' @export
tiers.list_of_formulas <- function(x, ...) {
  attr(x, "terms") |>
    tiers.term_archetype()
}


# Term Tools -------------------------------------------------------------------

#' Set components of term_archetype
#' @return A modified
#' @name setters
#' @export
set_roles <- function(x, roles, ...) {
  validate_class(roles, "list")

  # Update and append roles
  rls <- append(roles(x), roles)

  # If roles are not appropriate, should stop or error now
  accepted_roles <-
    c(
      "outcome",
      "predictor",
      "exposure",
      "confounder",
      "mediator",
      "strata",
      "unknown"
    )

  if (!all(rls %in% accepted_roles)) {
    stop(
      "An invalid role was entered. It should be one of: `c(",
      paste(accepted_roles, collapse = ", "),
      ")`"
    )
  }

  # Save the most "recent" updated label and erase prior if duplicate
  t <- vec_data(x)
  for (i in seq_along(rls)) {
    t$role[t$terms == names(rls[i])] <- rls[[i]]
  }

  vec_restore(t, to = term_archetype())
}

#' @rdname setters
#' @export
set_tiers <- function(x, tiers, ...) {
  validate_class(x, "term_archetype")
  validate_class(tiers, "list")

  # Append tiers
  grps <-
    tiers.term_archetype(x) |>
    append(tiers)

  t <- vec_data(x)

  for (i in seq_along(grps)) {
    t$tier[t$terms == names(grps[i])] <- grps[[i]]
  }

  vec_restore(t, to = term_archetype())
}

#' @rdname setters
#' @export
set_labels <- function(x, labels, ...) {
  validate_class(x, "term_archetype")
  validate_class(labels, "list")

  # Update and append labels
  labs <-
    labels.term_archetype(x) |>
    append(labels)

  # Save the most "recent" updated label and erase prior if duplicate
  t <- vec_data(x)
  for (i in seq_along(labs)) {
    t$label[t$terms == names(labs[i])] <- labs[[i]]
  }

  vec_restore(t, to = term_archetype())
}

#' @rdname setters
#' @export
add_strata <- function(x, strata, ...) {
  validate_class(x, "term_archetype")
  validate_class(strata, "character")

  strata_term <- term_archetype.character(
    x = strata,
    side = "meta",
    role = "strata",
    ...
  )

  # Return in combination
  c(x, strata_term)
}

#' Match the terms with the a formula, returning a subset of terms
#' @noRd
match_terms <- function(t, f) {
  validate_class(t, "term_archetype")

  if ("formula" %in% class(f)) {
    vars <- c(lhs(f), rhs(f))
  } else if ("character" %in% class(f)) {
    vars <- f
  }

  # Terms
  vt <- vec_data(t)

  # New term creation and matching
  mt <-
    vt[vt$terms %in% vars, ] |>
    vec_restore(to = term_archetype())

  # Return
  mt
}

# Updating Functions -----------------------------------------------------------

#' Update Prescriptions
#'
#' These are a variety of functions to help update and modify objects from the
#' `{forks}` package.
#' @return An object of the original class
#' @name updates
#' @export
update.term_archetype <- function(object, parameters, ...) {
  object
}

#' @rdname updates
#' @export
update.script <- function(object, parameters, ...) {
  t <- term_archetype(object)

  if (class(parameters) == "formula") {

    ### LHS
    all_left <- lhs(parameters, tidy = TRUE)
    plus_left <- lhs(parameters, tidy = FALSE)

    # Add
    if (length(plus_left) > 0) {
      for (i in seq_along(plus_left)) {
        .t <- term_archetype(x = plus_left[i], role = "outcome", side = "left")
        t <- c(t, .t)
      }
    }

    # Subtract
    minus_left <- setdiff(all_left, plus_left)

    tm <- vec_data(t)
    left <-
      tm[tm$side == "left" & !(tm$terms %in% minus_left), ] |>
      vec_restore(term_archetype())

    ### RHS
    all_right <- rhs(parameters, tidy = TRUE)
    plus_right <- rhs(parameters, tidy = FALSE)

    # Add
    if (length(plus_right) > 0) {
      .t <-
        paste(plus_right, collapse = " + ") |>
        {
          \(.x) paste("~", .x)
        }() |>
        stats::as.formula() |>
        term_archetype()

      t <- c(t, .t)
    }

    # Subtract
    minus_right <- setdiff(all_right, plus_right)

    tm <- vec_data(t)
    right <-
      tm[tm$side == "right" & !(tm$terms %in% minus_right), ] |>
      vec_restore(term_archetype())

    # Combine both sides
    t <- c(left, right)
  }

  # Return
  prescribe(t)
}

#' @rdname updates
#' @export
add <- function(object, ...) {
  UseMethod("add", object = object)
}

#' @rdname updates
#' @export
add.script <- function(object, parameters, ...) {
  obj <- term_archetype(object)

  switch(class(parameters)[1],
    term_archetype = {
      f <-
        obj |>
        {
          \(.x) c(.x, parameters)
        }() |>
        prescribe()
    },
    formula = {
      f <-
        term_archetype(parameters) |>
        {
          \(.x) c(obj, .x)
        }() |>
        prescribe()
    }
  )

  # Return
  f
}

#' @rdname updates
#' @export
add.term_archetype <- function(object, parameters, ...) {
  validate_class(parameters, "term_archetype")

  # Find the "older" term_archetype that is a duplicate
  c(object, parameters) |>
    vec_data() |>
    {
      \(.x) {
        .x[!duplicated(.x$terms, fromLast = TRUE), ]
      }
    }() |>
    vec_restore(to = term_archetype())
}
