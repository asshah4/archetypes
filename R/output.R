#' Print an `rx` formula
#'
#' @export
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

has_color <- function() {
	isTRUE(requireNamespace("crayon", quietly = TRUE) && crayon::has_color())
}

rx_style <- function(x) {

	# Number of roles
	role_list <- roles(x)
	lhs <- getOption("rx.lhs")
	rhs <- getOption("rx.rhs")
	custom <- unname(unlist(getOption("rx.roles")))
	if (length(custom) > 0) {
		role_names <- c(lhs, custom)
	} else {
		role_names <- c(lhs, rhs)
	}
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

	# Colors for rx
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
