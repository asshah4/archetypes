test_that("simple list of formulas can be generated", {

	# Method dispatch
	expect_length(list_of_formulas(formula_rx()), 0)
	expect_error(list_of_formulas("error"))

	# General formula check
	f <- mpg + wt ~ X(hp) + X(cyl) + gear + drat + qsec
	labels <- list(mpg ~ "Mileage", hp ~  "Horsepower")
	groups <- list(c(drat, qsec) ~ "speed", wt ~ "hardware")
	t <- term_rx(f, labels = labels, groups = groups)
	xd <- formula_rx(t, pattern = "direct")
	xs <- formula_rx(t, pattern = "sequential")
	xp <- formula_rx(t, pattern = "parallel")
	ld <- list_of_formulas(xd)
	ls <- list_of_formulas(xs)
	lp <- list_of_formulas(xp)
	expect_length(ld, 4)
	expect_length(ls, 12)
	expect_length(lp, 12)
	expect_output(print(names(ld[1])), "xd")
	expect_equal(attributes(ld)$roles$hp, "exposure")
	expect_equal(attributes(ld)$labels$hp, "Horsepower")

	# Mediation check
	f <- mpg + wt ~ X(hp) + M(cyl) + gear + drat + qsec
	labels <- list(mpg ~ "Mileage", hp ~  "Horsepower")
	groups <- list(c(drat, qsec) ~ "speed", wt ~ "hardware")
	t <- term_rx(f, labels = labels, groups = groups)
	xd <- formula_rx(t, pattern = "direct")
	xs <- formula_rx(t, pattern = "sequential")
	xp <- formula_rx(t, pattern = "parallel")
	ld <- list_of_formulas(xd)
	ls <- list_of_formulas(xs)
	lp <- list_of_formulas(xp)
	expect_length(ld, 5)
	expect_equal(length(ls), length(lp))

	# Printing
	expect_output(print(ld), "mpg")
	expect_output(print(new_list_of_formulas()), "[0]")
	if (isTRUE(requireNamespace("tibble", quietly = TRUE))) {
		tibble::tibble(ld) |>
			print() |>
			expect_output("<fmls>")
	}


})

test_that("lists can be fit", {

	f <- mpg + wt ~ X(hp) + X(cyl) + gear + drat + qsec
	labels <- list(mpg ~ "Mileage", hp ~  "Horsepower")
	groups <- list(c(drat, qsec) ~ "speed", wt ~ "hardware")
	t <- term_rx(f, labels = labels, groups = groups)
	x <- formula_rx(t, pattern = "sequential")
	lof <- list_of_formulas(x)
	lom <- fit.list_of_formulas(lof, lm, data = mtcars)
	expect_type(lom, "list")
	expect_length(lom, 12)


})
