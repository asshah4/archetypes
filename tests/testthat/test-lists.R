test_that("simple list of formulas can be generated", {

	f <- mpg + wt ~ X(hp) + X(cyl) + gear + drat + qsec
	labels <- list(mpg ~ "Mileage", hp ~  "Horsepower")
	groups <- list(c(drat, qsec) ~ "speed", wt ~ "hardware")

	t <- term_rcrd(f, labels = labels, groups = groups)
	x <- formula_vctr(t)




})
