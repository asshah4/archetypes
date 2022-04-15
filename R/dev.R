# nocov start

# Terms ------------------------------------------------------------------------

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

# Arithmetic -------------------------------------------------------------------

#' Divide a Formula into Parts
#'
#' An `rx` formula can be split into simpler parts as needed. If there are
#' multiple dependent variables, then they can be split into individual
#' formulas. If there are multiple independent variables, they can also be split
#' into individual formulas.
#'
#' @return A list of `rx` objects
#'
#' @param x An object of `rx` or `formula` class (or can be coerced into one)
#'
#' @param side Which side of the formula should be divided into parts. This
#'   parameter can is a character vector from `c("lhs", "rhs")`, and defines how
#'   this formula division should occur.
#'
#' @family tools
#' @rdname math
#' @export
divide <- function(x, side, ...) {
  validate_class(x, "rx")

  # Terms
  lhs <- get_lhs(x)
  rhs <- get_rhs(x)

  # Reconstitute left
  if (side == "lhs") {
    flist <-
      paste(rhs, collapse = " + ") %>%
      paste(lhs, ., sep = " ~ ") %>%
      lapply(., formula) %>%
      lapply(., give_env) %>%
      lapply(., rx)
  }

  # Reconstitute right
  if (side == "rhs") {
    flist <-
      paste(lhs, collapse = " + ") %>%
      paste(., rhs, sep = " ~ ") %>%
      lapply(., formula) %>%
      lapply(., give_env) %>%
      lapply(., rx)
  }

  # Return
  flist
}



# nocov end
