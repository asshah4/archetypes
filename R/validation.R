#' Validate class of objects
#' @keywords internal
#' @noRd
validate_class <- function(x, what) {
  if (!inherits(x, what)) {
    stop(
      deparse(substitute(x)),
      " needs to inherit from `",
      paste("c(", paste(what, collapse = ", "),
        ")",
        sep = ""
      ),
      "`, but is of class `",
      class(x),
      "`.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' Validate if an empty object is given to a function
#' @keywords internal
#' @noRd
validate_empty <- function(x) {
  # x is the primary argument of the parent function
  n <- length(x)

  # Print message if needed
  if (n == 0) {
    message(
      "An empty `",
      class(x)[1],
      "` was provided, resulting in a [0] length object."
    )
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Validate if the models are part of an acceptable/supported type
#' @keywords internal
#' @noRd
validate_models <- function(x) {

  # Essentially, which items are supported by the term_archetype deconstructor
  supported_classes <-
    c(
      "lm",
      "glm",
      "model_fit"
    )

  if (!any(class(x) %in% supported_classes)) {
    stop("`model_archetype()` is not defined for a `",
      class(x)[1],
      "` object.",
      call. = FALSE
    )
  }
}

#' Check the complexity of a formula
#' @keywords internal
#' @noRd
check_complexity <- function(x) {

  validate_class(x, "term_archetype")
  y <- vec_data(x)
  out <- c(y$terms[y$role == "outcome"], y$terms[y$role == "dependent"])
  exp <- y$terms[y$role == "exposure"]
  ind <- y$terms[y$role == "independent"]
  med <- y$terms[y$role == "mediator"]

  # Complexity levels:
    # "unit" = a 1:1 relationship, one LHS and one RHS term
    # "simple" = may have covariates or additional terms, but only 1 outcome
    # "complex" = may still have multiple outcomes or predictors
  if (length(y$side[y$side %in% c("left", "right")]) == 2) {
    z <- "unit"
  } else if (length(out) == 1 & length(exp) < 2) {
    z <- "simple"
  } else {
    z <- "complex"
  }

  # Return
  z
}

#' Identification of formula and formula-adjacent objects
#'
#' @param x Confirmation of an object being of the following classes:
#'
#'   * `term`
#'   * `rx`
#'
#' @name check
#' @export
is_term <- function(x) {
  inherits(x, "term_archetype")
}

#' @rdname check
#' @export
is_script <- function(x) {
  inherits(x, "script")
}

#' @rdname check
#' @export
is_formula <- function(x) {
  inherits(x, "formula_archetype")
}
