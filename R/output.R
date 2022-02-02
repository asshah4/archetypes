#' @export
has_cli <- function() {
	isTRUE(requireNamespace("cli", quietly = TRUE))
}

#' @export
forks_style <- function(x) {
	UseMethod("forks_style", object = x)
}

# Terms ----

#' @export
format.term_rx <- function(x, ...) {

	tm <- vec_data(x)
	fmt_tx <- character()

	if (vec_size(x) == 0) {
		fmt_tx <- new_term()
	} else if (has_cli() & vec_size(x) > 0) {
		for (i in 1:nrow(tm)) {

			if (tm$role[i] == "outcome") {
				t <- tm$term[i]
				fmt_tx <- append(fmt_tx, cli::col_yellow(t))
			}

			if (tm$role[i] == "exposure") {
				t <- tm$term[i]
				fmt_tx <- append(fmt_tx, cli::col_magenta(t))
			}

			if (tm$role[i] == "mediator") {
				t <- tm$term[i]
				fmt_tx <- append(fmt_tx, cli::col_cyan(t))
			}

			if (tm$role[i] == "covariate") {
				t <- tm$term[i]
				fmt_tx <- append(fmt_tx, cli::col_blue(t))
			}

			if (tm$role[i] == "unknown") {
				t <- tm$term[i]
				fmt_tx <- append(fmt_tx, cli::col_white(t))
			}

		}
	} else {
		for (i in 1:nrow(tm)) {
			fmt_tx <- append(fmt_tx, tm$term[i])
		}
	}

	# Return
	fmt_tx

}

#' @export
obj_print_data.term_rx <- function(x, ...) {

	if (vec_size(x) == 0) {
		new_term()
	} else if (vec_size(x) > 1) {
		cat(format(x), sep = "\n")
	} else {
		cat(format(x))
	}
}

#' @export
vec_ptype_abbr.term_rx <- function(x, ...) {
	"tx"
}

# Formulas ----

#' @export
format.formula_rx <- function(x, ...) {

	f <- vec_data(x)
	t <- attr(x, "terms")
	tm <- vec_data(t)
	fmts <- format(t)

	out <- fmts[which(tm$role == "outcome")]
	exp <- fmts[which(tm$role == "exposure")]
	med <- fmts[which(tm$role == "mediator")]
	cov <- fmts[which(tm$role == "covariate")]

	left <- paste(out, collapse = " + ")
	right <- paste(c(exp, med, cov), collapse = " + ")
	both <- paste(left, right, sep = " ~ ")

	# Return
	both

}

#' @export
obj_print_data.formula_rx <- function(x, ...) {
	if (vec_size(x) == 0) {
		new_term()
	} else if (vec_size(x) > 1) {
		cat(format(x), sep = "\n")
	} else {
		cat(format(x))
	}
}

#' @export
vec_ptype_abbr.formula_rx <- function(x, ...) {
	"fx"
}

# Formula Lists ----

#' @export
format.list_of_formulas <- function(x, ...) {

	f <- lapply(vec_data(x), function(.x) {
		attributes(.x) <- NULL
		.x
	})
	f <- unname(f)
	f <- as.character(f)
	f

}

#' @export
obj_print_data.list_of_formulas <- function(x, ...) {
	if (length(x) == 0) {
		return()
	}

	if (length(x) >= 1) {
		cat(format(x), sep = "\n")
	} else {
		cat(format(x))
	}
}

#' @export
vec_ptype_full.list_of_formulas <- function(x, ...) {
	"list_of_formulas"
}

#' @export
vec_ptype_abbr.list_of_formulas <- function(x, ...) {
	"fmls"
}

# Model Lists ----

#' @export
format.list_of_models <- function(x, ...) {

	lapply(x, FUN = function(.x) {
		print(.x)
	})

}

#' @export
vec_ptype_full.list_of_models <- function(x, ...) {
	"list_of_models"
}

#' @export
vec_ptype_abbr.list_of_models <- function(x, ...) {
	"mdls"
}
