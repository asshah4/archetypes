# Models -----------------------------------------------------------------------

#' Model archetype
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' @param strata_info User-defined `formula`, with the LHS being the column
#'   representing the strata, and the RHS being the level used in the model. For
#'   example, `am ~ 0` in the `mtcars` data, the model was subset for only
#'   cars that did not have automatic transmission named in the `am` variable.
#'
#' @name models
#' @export
model_archetype <- function(x = unspecified(), ...) {
  UseMethod("model_archetype", object = x)
}

#' @rdname models
#' @export
model_archetype.lm <- function(x,
                               name = deparse(substitute(x)),
                               label = list(),
                               role = list(),
                               fmls = formula_archetype(),
                               description = character(),
                               strata_info = formula(),
                               ...) {

  # Wrap model
  m <- list(x)

  # Need the term and formulas
  # Terms should be extracted and updated with the roles and labels as needed
  t <- tm(x, label = label, role = role)

  if (length(fmls) == 0) {
    # Top level or first formula made is what we want
    f <- formula_archetype(t, order = 1:4)[1]
  } else {
    validate_class(fmls, "formula_archetype")
    f <- fmls
    if (is.na(f)) {
      f <- formula_archetype(t, order = 1:4)[1]
    }
  }

  # Type and subtypes
  type <- class(x)[1]
  subtype <- class(x)[2]

  # Labels and descriptions for model
  if (length(description) == 0) {
    description <- NA_character_
  }
  # Strata description from formula to character for moving it in vectors
  # Formula environment doesn't matter here
  if (length(strata_info) == 0) {
    strata_info <- NA_character_
  } else if (inherits(strata_info, "formula")) {
    strata_info <- deparse1(strata_info)
  }

  # Creation
  new_model(
    model = m,
    type = type,
    subtype = subtype,
    name = name,
    fmls = f,
    description = description,
    strata_info = strata_info
  )
}

#' @rdname models
#' @export
model_archetype.glm <- model_archetype.lm

#' @rdname models
#' @export
model_archetype.model_fit <- model_archetype.lm

#' @rdname models
#' @export
model_archetype.list <- function(x,
                                 name = deparse1(substitute(x)),
                                 label = list(),
                                 role = list(),
                                 fmls = formula_archetype(),
                                 description = character(),
                                 strata_info = formula(),
                                 ...) {

  # Validated early break
  if (validate_empty(x)) {
    return(new_model())
  }

  # Create a vector of the model archetypes to prepare to iterate through list
  n <- length(x)
  ma <- model_archetype()

  # Flush out the rest of the formulas if they are not available
  if (length(fmls) < n) {
    n_fmls <- length(fmls)
    n_fill <- n - n_fmls
    f <- c(fmls, vec_init(fmls(), n_fill))
  } else if (length(fmls) == n) {
    f <- fmls
  }

  # Get names if needed
  if (length(name) == n) {
    names(x) <- name
  } else {
    nms <- paste0(name, sep = "_", 1:n)
  }

  # Go through the list
  for (i in 1:n) {
    validate_models(x[[i]]) # Can only be used WITHIN the function

    if (is.null(names(x)[i])) {
      nm <- paste0(name, "_", i)
    } else if (names(x)[i] == "") {
      nm <- paste0(name, "_", i)
    } else {
      nm <- names(x)[i]
    }

    m <- model_archetype(
      x[[i]],
      name = nm,
      label = label,
      role = role,
      fmls = f[i],
      description = description,
      strata_info = strata_info,
    )

    ma <- append(ma, m)
  }

  # Return
  ma
}

#' @rdname models
#' @export
model_archetype.default <- function(x = unspecified(), ...) {
  # Early break
  if (length(x) == 0) {
    return(new_model())
  }

  stop("`model_archetype()` is not defined for a `",
    class(x)[1],
    "` object.",
    call. = FALSE
  )
}

#' @rdname models
#' @export
md <- model_archetype

# Vector Definition ------------------------------------------------------------

#' Model vector definition
#' @keywords internal
#' @noRd
new_model <- function(model = list(),
                      type = character(),
                      subtype = character(),
                      name = character(),
                      fmls = formula_archetype(),
                      description = character(),
                      strata_info = character()) {

  # Validation
  vec_assert(model, ptype = list())
  vec_assert(type, ptype = character())
  vec_assert(subtype, ptype = character())
  vec_assert(name, ptype = character())
  vec_assert(fmls, ptype = formula_archetype())
  vec_assert(description, ptype = character())
  vec_assert(strata_info, ptype = character())

  # Model archetype description is essentially deconstructed here
  # class = defined by the model_archetype, its base class, and a list
  # user defined descriptors = description
  # model defined descriptors = type, subtype
  # model level findings = statistics, formula
  # internals = terms, term descriptors... contained within the script
  new_rcrd(
    fields = list(
      "model" = model,
      "type" = type,
      "subtype" = subtype,
      "name" = name,
      "description" = description,
      "strata_info" = strata_info,
      "fmls" = fmls
    ),
    class = "model_archetype"
  )
}

#' @keywords internal
#' @noRd
methods::setOldClass(c("model_archetype", "vctrs_vctr"))

# Output -----------------------------------------------------------------------

#' @export
format.model_archetype <- function(x, ...) {

  # Character representation of formula
  if (vec_size(x) == 0) {
    return()
  } else {
    fmt <-
      sapply(x, FUN = function(.x) {
        f <- field(field(.x, "fmls"), "formula")
        t <- field(.x, "type")
        st <- field(.x, "subtype")

        if (!is.na(st)) {
          if (st == "model_fit") {
            cl <- paste0(st, t)
          } else {
            cl <- t
          }
        } else {
          cl <- t
        }

        paste0(cl, "(", f, ")")
      })
  }
  # Return
  fmt

}

#' @export
obj_print_data.model_archetype <- function(x, ...) {
  if (vec_size(x) == 0) {
    new_model()
  }

  if (vec_size(x) >= 1) {
    cat(format(x), sep = "\n")
  } else {
    cat(format(x))
  }
}

#' @importFrom pillar pillar_shaft
#' @export
pillar_shaft.model_archetype <- function(x, ...) {
  out <- format(x)
  pillar::new_pillar_shaft_simple(out, align = "left")
}

#' @export
vec_ptype_full.model_archetype <- function(x, ...) {
  "model_archetype"
}

#' @export
vec_ptype_abbr.model_archetype <- function(x, ...) {
  "md"
}

# Casting and coercion ---------------------------------------------------------

#' @export
vec_ptype2.model_archetype.model_archetype <- function(x, y, ...) {
  x
}

#' @export
vec_cast.model_archetype.model_archetype <- function(x, to, ...) {
  x
}
