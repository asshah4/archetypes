# Models -----------------------------------------------------------------------

#' Model archetype
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' @name models
#' @export
model_archetype <- function(x = unspecified(), ...) {
	UseMethod("model_archetype", object = x)
}

#' @rdname models
#' @export
model_archetype.lm <- function(x = unspecified(),
							name = deparse(substitute(x)),
							label = character(),
							description = character(),
							term_labels = list(),
							term_roles = list(),
							...) {

	# Wrap model
	m <- list(x)

	# Need the term and formulas
	# Terms should be extracted and updated with the roles and labels as needed
	t <- tm(x, label = term_labels, role = term_roles)
	f <- rx(t)

	# Type and subtypes
	type <- class(x)[1]
	subtype <- class(x)[2]

	# Create a shorter call object as a character
	if ("model_fit" %in% class(x)) {
		call_name <-
			stats::na.omit(c(subtype, type)) |>
			paste0(collapse = "")
	} else {
		call_name <- type
	}
	cl <- paste0(call_name, "(", f, ")")

	# Labels and descriptions for model
	if (length(label) == 0) {label <- NA_character_}
	if (length(description) == 0) {description <- NA_character_}

	# Create tag/hash
	tag <-
		paste0(
			ifelse(name == "", "unknown", name),
			"_",
			type,
			"_",
			ifelse(is.na(subtype), NA, subtype),
			"_",
			"T",
			length(t),
			"L",
			ifelse(is.na(label), 0, 1),
			"D",
			ifelse(is.na(description), 0, 1)
		)

	new_model(
		model = m,
		type = type,
		subtype = subtype,
		name = name,
		label = label,
		description = description,
		call = cl,
		script = f,
		tag = tag
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
model_archetype.list <- function(x = unspecified(),
								 label = character(),
								 description = character(),
								 term_labels = list(),
								 term_roles = list(),
								 ...) {

	# Create a vector of the model archetypes
	ma <- model_archetype()

	# Go through the list
	for (i in seq_along(x)) {
		validate_models(x[[i]])

		m <- model_archetype(x[[i]],
							 name = names(x)[i],
							 term_labels = term_labels,
							 term_roles = term_roles)

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
		 call. = FALSE)

}

#' @rdname models
#' @export
md = model_archetype

# Vector Definition ------------------------------------------------------------

#' Model vector definition
#' @keywords internal
#' @noRd
new_model <- function(model = list(),
					  type = character(),
					  subtype = character(),
					  name = character(),
					  label = character(),
					  description = character(),
					  call = character(),
					  script = prescribe(),
					  tag = character()) {

	# Validation
	vec_assert(model, ptype = list())
	vec_assert(type, ptype = character())
	vec_assert(subtype, ptype = character())
	vec_assert(name, ptype = character())
	vec_assert(label, ptype = character())
	vec_assert(description, ptype = character())
	vec_assert(call, ptype = character())
	vec_assert(tag, ptype = character())

	# TODO
	# Scripts are not yet validated

	# Model archetype description is essentially deconstructed here
	# class = defined by the model_archetype, its base class, and a list
	# user defined descriptors = tag, label, description
	# model defined descriptors = type, subtype, call
	# model level findings = statistics, formula
	# internals = terms, term descriptors... contained within the script
	new_rcrd(
		fields = list(
			"model" = model,
			"type" = type,
			"subtype" = subtype,
			"name" = name,
			"label" = label,
			"description" = description,
			"call" = call,
			"script" = script,
			"tag" = tag
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
	field(x, "call")
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
