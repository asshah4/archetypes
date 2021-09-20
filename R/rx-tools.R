#' Decompose a Labeled Formula
#'
#' This function takes a formula that has been labeled and parses it through to
#' identify the individual roles of the terms. The labels are set via the
#' [rx::set_rx_labels()] function, which modifies global options. The default
#' presumes the that left-hand side of the equation will be labeled __lhs__ and
#' the right-hand side will be labeled __rhs__.
#'
#' @inheritParams rx
#'
#' @keywords internal
#' @export
deparse_formula <- function(f, ...) {

	# Obtain global options
	lhs <- getOption("rx.lhs")
	rhs <- getOption("rx.rhs")
	labs <- getOption("rx.labels")

	# Obtain left and right hand side of equation
	all_terms <- all.vars(f)
	raw_terms <- labels(stats::terms(f))
	lhs_terms <- gsub(" ", "", unlist(strsplit(deparse(f[[2]]), "\ \\+\ ")))
	rhs_terms <- setdiff(all_terms, lhs_terms)

	# The list of labels to be returned
	label_list <- list()
	label_list[[lhs]] <- lhs_terms
	label_list[[rhs]] <- rhs_terms

	# If available, each prescribed label should be parsed through for wrappers
	if (length(labs) > 0) {

		labs <- labs[!names(labs) %in% c("lhs", "rhs")]

		for (i in 1:length(labs)) {

			# TODO wrapper validation
			wrpr <- names(labs[i])
			lbl <- labs[[i]]

			regex <- paste0(wrpr, "\\(")
			wrpr_terms <-
				grep(regex, raw_terms, value = TRUE) %>%
				gsub(regex, "", .) %>%
				gsub("\\)$", "", .)

			label_list[[lbl]] <- wrpr_terms

		}

	}

	label_list

}

#' Find Labels from Object
#'
#' Retrieve the term labels from the `rx` object if available. At minimum, will have an _outcomes_ and _predictors_.
#'
#' @return A character vector or list of such vectors from an object of the `rx` class.
#'
#' @param object An object of `rx` class
#'
#' @inheritParams base::labels
#'
#' @export
labels.rx <- function(object, ...) {

	validate_class(object, "rx")

	attributes(object)$roles %>%
		table_to_list()

}

# Operations ----

#' Merge Two Formulas
#'
#' @description
#'
#' Merge two formulas together based on common terms.
#'
#' @return An object of `rx` class
#'
#' @param x,y Objects of class `rx`, or can be coerced to `rx` class
#'
#' @family tools
#' @export
merge.rx <- function(x, y, ...) {

	# Second argument validation
	validate_class(y, c("rx", "formula"))
	if (!("rx" %in% class(y))) {
		y <- rx(y)
	}

	lhs_x <- gsub(" ", "", unlist(strsplit(deparse(x[[2]]), "\ \\+\ ")))
	lhs_y <- gsub(" ", "", unlist(strsplit(deparse(y[[2]]), "\ \\+\ ")))
	lhs <-
		c(lhs_x, lhs_y) %>%
		unique() %>%
		paste0(., collapse = " + ")

	rhs_x <- gsub(" ", "", unlist(strsplit(deparse(x[[3]]), "\ \\+\ ")))
	rhs_y <- gsub(" ", "", unlist(strsplit(deparse(y[[3]]), "\ \\+\ ")))
	rhs <- unique(c(rhs_x, rhs_y))

	# Attributes need to be extracted as well
	attr_x <- attributes(x)$roles
	attr_y <- attributes(y)$roles
	attr_x[setdiff(names(attr_y), names(attr_x))] <- FALSE
	attr_y[setdiff(names(attr_x), names(attr_y))] <- FALSE
	named_list <-
		rbind(attr_x, attr_y) %>%
		table_to_list() %>%
		lapply(., unique)

	f <-
		reformulate(rhs, lhs) %>%
		rx(., roles = named_list)

	environment(f) <- parent.frame()

	# Return
	f

}

#' Formula Operations
#'
#' This is a method that extends the [methods::Ops()] group generic function to
#' include generic operators for `rx` formulas. This is limited to `rx` formulas
#' as to not impact the functionality of generic [stats::formula()] objects.
#'
#' @return An object of `rx` class
#'
#' @inheritParams methods::Ops
#' @family tools
#' @export
Ops.rx <- function(e1, e2) {
	FUN <- .Generic
	if (FUN == "+") {
		x <- merge(e1, e2)
		environment(x) <- parent.frame()
		return(x)
	} else {
		stop("Other unary and binary operators are not available for this class.")
	}
}

# Label Themes ----

#' Set Custom Labels
#'
#' @description
#'
#' This function allows for setting global options for how `rx` should label
#' individual terms in a formula. The default labels are `lhs` for the response
#' or dependent variables, and the `rhs` are the terms or independent variables,
#' which require no shorthand or wrappers to work. This will exist for the
#' reminder of the working session.
#'
#' @details
#'
#' To modify the defaults, simply provide a named list pair in the argument
#' __labels__ that renames `lhs` and `rhs`. For example, the default labels can
#' be revised using `list(lhs = "outcomes", rhs = "predictors")`.
#'
#' To add new labels, add additional named/paired arguments. The name serves as
#' the wrapper for the formula, creating a shorthand for adding labels. For
#' example, using `list(X = "exposure")` will label any component of a formula
#' as an "exposure" variable. To use the wrapping feature, simply use the
#' __name__ of the list argument in parentheses around the individual term, as
#' below.
#'
#' \deqn{mpg ~ X(wt) + hp + cyl}
#'
#' @return Changes to global options within the working environment.
#'
#' @param labels A named list pair of character vectors that should be used to
#'   "wrap" formula terms, and change their labels. The default options labels
#'   the independent variables as `rhs` and the dependent variables as `lhs`.
#'
#' @name options
#' @export
set_rx_labels <- function(labels) {

	# Current labels should be appended with new labels, overwriting old names
	old_labs <- getOption("rx.labels")
	dupes <- names(old_labs) %in% names(labels)
	old_labs[dupes] <- NULL
	new_labs <- append(old_labs, labels)

	# Primary labels should be preserved and placed into a special category
	new_lhs <- getOption("rx.lhs")
	new_rhs <- getOption("rx.rhs")

	for (i in names(new_labs)) {

		if (i == new_lhs) {
			new_lhs <- new_labs[[i]]
			new_labs[i] <- NULL
		}

		if (i == new_rhs) {
			new_rhs <- new_labs[[i]]
			new_labs[i] <- NULL
		}

	}

	# Options should be updated
	op.rx <- list(
		rx.lhs = new_lhs,
		rx.rhs = new_rhs,
		rx.labels = new_labs
	)

	options(op.rx)

}

#' @rdname options
#' @export
reset_rx_labels <- function() {

	op.rx <- list(
		rx.default = list(
			rx.lhs = "lhs",
			rx.rhs = "rhs"
		),
		rx.lhs = "lhs",
		rx.rhs = "rhs",
		rx.labels = list()
	)

	options(op.rx)

}
