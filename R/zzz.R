# nocov start

.onLoad <- function(libname, pkgname) {

	# Global options
	op <- options()
	op.rx <- list(
		rx.default = list(
			rx.lhs = "lhs",
			rx.rhs = "rhs"
		),
		rx.lhs = "lhs",
		rx.rhs = "rhs",
		rx.labels = list()
	)

	# Don't overwrite existing options (unlikely but possible)
	toset <- !(names(op.rx) %in% names(op))
	if (any(toset)) { options(op.rx[toset]) }

}

# nocov end
