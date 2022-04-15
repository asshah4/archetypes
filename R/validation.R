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

  fn <- deparse(sys.calls()[[sys.nframe() - 1]][[1]])

  # Print message if needed
  if (n == 0) {
    message(
      "`",
      fn,
      "()` recieved an empty `",
      class(x)[1],
      "` argument, returning a [0] length object."
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

  fn <- deparse(sys.calls()[[sys.nframe() - 1]][[1]])

  if (!any(class(x) %in% supported_classes)) {
    stop(
      "`",
      fn,
      "()` is not defined for a `",
      class(x)[1],
      "` object.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' Identification of formula and formula-adjacent objects
#'
#' @param x Confirmation of an object being of the following classes:
#'
#'   * `term_archetype`
#'   * `formula_archetype`
#'   * `script`
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
