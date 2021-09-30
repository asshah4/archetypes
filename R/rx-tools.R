#' Decompose a Labeled Formula
#'
#' This function takes a formula that has been labeled and parses it through to
#' identify the individual roles of the terms. The labels are set via the
#' [rx::set_rx_labels()] function, which modifies global options. The default
#' presumes the that left-hand side of the equation will be labeled __lhs__ and
#' the right-hand side will be labeled __rhs__.
#'
#' @return A named list of variables under their specific labels
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
	rhs_terms <- raw_terms <- get_rhs(f, tidy = FALSE)
	lhs_terms <- get_lhs(f)

	# Search for mixed/random effect specifiers
	special_index <-
		rhs_terms %>%
		grepl("\\(", .)
	rhs_terms[special_index] <-
		rhs_terms[special_index] %>%
		sub("[[:alnum:]]\\(", "", .) %>%
		sub("\\)$", "", .)

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

#' Formula Operations
#'
#' This is a method that extends the [methods::Ops()] group generic function to
#' include generic operators for `rx` formulas. This is limited to `rx` formulas
#' as to not impact the functionality of generic [stats::formula()] objects.
#'
#' @return An object or list of objects of `rx` class
#'
#' @inheritParams methods::Ops
#'
#' @family tools
#' @name math
#' @export
Ops.rx <- function(e1, e2) {
	FUN <- .Generic
	if (FUN == "+") {
		x <- add(e1, e2)
		environment(x) <- parent.frame()
		return(x)
	} else if (FUN == "-") {
		x <- subtract(e1, e2)
		environment(x) <- parent.frame()
		return(x)
	} else {
		stop("Other unary and binary operators are not available for this class.")
	}
}

#' Add Two Formulas Together
#'
#' @description
#'
#' Merge two formulas together based on common terms.
#'
#' @return An object of `rx` class
#'
#' @param x,y Objects of class `rx`, or can be coerced to `rx` class
#'
#' @param ... Further arguments passed to or from other methods
#'
#' @family tools
#' @rdname math
#' @export
add <- function(x, y, ...) {

	# Second argument validation
	validate_class(y, c("rx", "formula"))
	if (!("rx" %in% class(y))) {
		y <- rx(y)
	}

	lhs_x <- get_lhs(x)
	lhs_y <- get_lhs(y)
	lhs <-
		c(lhs_x, lhs_y) %>%
		unique() %>%
		paste0(., collapse = " + ")

	rhs_x <- get_rhs(x)
	rhs_y <- get_rhs(y)
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
		stats::reformulate(rhs, lhs) %>%
		rx(., roles = named_list)

	environment(f) <- parent.frame()

	# Return
	f

}

#' Subtract Formula From Another Formula
#'
#' Subtract the elements of one formula from that of another. This only applies
#' to formulas where the primary object is a superset of the secondary object.
#' This is limited to formulas that are `rx` formula objects as to inot impact
#' the functionality of generic [stats::formula()] objects.
#'
#' @return An `rx` object
#'
#' @inheritParams add
#'
#' @family tools
#' @rdname math
#' @export
subtract <- function(x, y, ...) {

	# Check that x is a superset of y
	xvars <- all.vars(x)
	yvars <- all.vars(y)

	if (!all(yvars %in% xvars)) {
		stop(
			"The second object is not a subset of the first and cannot be subtracted.",
			call. = FALSE
		)
	}

	# Second argument validation
	validate_class(x, "rx")
	validate_class(y, c("rx", "formula"))
	if (!("rx" %in% class(y))) {
		y <- rx(y)
	}

	# Terms
	lhs_x <- get_lhs(x)
	lhs_y <- get_lhs(y)
	rhs_x <- get_rhs(x, tidy = FALSE)
	rhs_y <- get_rhs(y, tidy = FALSE)

	# Options
	lhs_label <- getOption("rx.lhs")
	rhs_label <- getOption("rx.rhs")

	# Attributes for both outcomes of X and Y
	out_x <-
		attributes(x)$roles %>%
		.[.[lhs_label] == TRUE, ]
	out_y <-
		attributes(y)$roles %>%
		.[.[lhs_label] == TRUE, ]
	out_x[which(!(out_x$terms %in% out_y$terms)),]
	pred_x <-
		attributes(x)$roles %>%
		.[.[rhs_label] == TRUE, ]
	pred_y <-
		attributes(y)$roles %>%
		.[.[rhs_label] == TRUE, ]

	named_list <-
		rbind(out_x[which(!(out_x$terms %in% out_y$terms)), ],
					pred_x[which(!(pred_x$terms %in% pred_y$terms)), ]) %>%
		table_to_list() %>%
		lapply(., unique)

	# New formula
	lhs <-
		setdiff(lhs_x, lhs_y) %>%
		unique() %>%
		paste0(., collapse = " + ")

	rhs <- setdiff(rhs_x, rhs_y)

	f <-
		stats::reformulate(rhs, lhs) %>%
		rx(., roles = named_list)

	give_env(f)

	# Return
	f

}

