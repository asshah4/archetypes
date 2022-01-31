#' @export
has_color <- function() {
	isTRUE(requireNamespace("crayon", quietly = TRUE) && crayon::has_color())
}

#' @export
has_cli <- function() {
	isTRUE(requireNamespace("cli", quietly = TRUE))
}

#' @export
forks_style <- function(x) {
	UseMethod("forks_style", object = x)
}

#' @export
forks_style.term_rx <- function(x) {

	# Number of roles
	role_list <- getComponent(x, "roles")
	role_names <- unique(unlist(unname(role_list)))
	n <- length(role_names)

	# Solarized Colors
	yellow <- crayon::make_style("#b58900")
	orange <- crayon::make_style("#cb4b16")
	red <- crayon::make_style("#dc322f")
	magenta <- crayon::make_style("#d33682")
	violet <- crayon::make_style("#6c71c4")
	blue <- crayon::make_style("#268bd2")
	cyan <- crayon::make_style("#2aa198")
	green <- crayon::make_style("#859900")

	# Colors for forks
	role_colors <- list()

	# Based on number of roles
	if (n == 2) {
		role_colors[role_names[1]] <- list(yellow)
		role_colors[role_names[2]] <- list(red)
	} else if (n == 3) {
		role_colors[role_names[1]] <- list(yellow)
		role_colors[role_names[2]] <- list(red)
		role_colors[role_names[3]] <- list(blue)
	} else if (n == 4) {
		role_colors[role_names[1]] <- list(yellow)
		role_colors[role_names[2]] <- list(red)
		role_colors[role_names[3]] <- list(blue)
		role_colors[role_names[4]] <- list(orange)
	}

	# Return
	role_colors
}

print.rx <- function(x, ...) {

	if (has_color()) {

		role_list <- roles(x)
		max_colors <- 4
		if (length(role_list) > max_colors) {
			attributes(x) <- NULL
			print(x)
			return()
		}
		role_colors <- rx_style(x)

		# Colored RHS roles
		rhs_names <- names(role_colors[-1])
		all_rhs <- get_rhs(x)
		colored_rhs <- all_rhs
		for (i in 1:length(rhs_names)) {
			spec_rhs <- unname(unlist(role_list[rhs_names[[i]]]))
			pos <- which(all_rhs %in% spec_rhs)
			colored_rhs[pos] <- role_colors[[rhs_names[[i]]]](colored_rhs[pos])
		}

		# LHS colors
		colored_lhs <- role_colors[[1]](role_list[[names(role_colors)[1]]])

		# Combine
		lhs <- paste(colored_lhs, collapse = " + ")
		rhs <- paste(colored_rhs, collapse = " + ")
		cat(lhs, "~", rhs)

	} else {
		attributes(x) <- NULL
		print(x)
		return()
	}

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
