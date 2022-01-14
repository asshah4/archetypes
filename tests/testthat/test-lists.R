test_that("simple list of formulas can be generated", {

	f <- mpg + wt ~ X(hp) + X(cyl) + gear + drat + qsec
	labels <- list(mpg ~ "Mileage", hp ~  "Horsepower")
	groups <- list(c(drat, qsec) ~ "speed", wt ~ "hardware")
	t <- term_rx(f, labels = labels, groups = groups)

	# Sequential check
	x <- formula_rx(t, pattern = "sequential")
	fl <- list_of_formulas(x)
	expect_s3_class(fl, "list_of_formulas")
	expect_s3_class(fl, "vctrs_list_of")
	expect_type(fl, "list")
	expect_length(fl, 12)

	# Sequential check
	x <- formula_rx(t, pattern = "parallel")
	fl <- list_of_formulas(x)
	expect_s3_class(fl, "list_of_formulas")
	expect_length(fl, 12)



})
