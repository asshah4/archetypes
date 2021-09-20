# Sandbox

x <- mpg ~ cyl
y <- wt ~ hp

z <- c(x, y)

a <- as.vector(c(x, y), mode = "any")

f <- mpg + wt ~ X(hp) + X(cyl) + gear + drat + F(qsec)
labs <- deparse_formula(f)

labs %>%
	stack() %>%
	tidyr::pivot_wider(names_from = "values", values_from = "ind", values_fill = NA)

labs %>%
	tibble::as_tibble()

y <- stack(labs)
names(y) <- c("terms", "roles")
y$id <- TRUE
z <- reshape(y, direction = "wide", idvar = "terms", timevar = "roles")
names(z) <- gsub("id\\.", "", names(z))
z[is.na(z)] <- FALSE


y %>%
	dplyr::group_by(values)
