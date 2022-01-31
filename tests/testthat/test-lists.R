test_that("simple list of formulas can be generated", {

	# Method dispatch
	expect_length(list_of_formulas(formula_rx()), 0)
	expect_error(list_of_formulas("error"))

	# Super simple check
	f <- mpg + wt ~ hp + cyl + gear
	x <- frx(f)
	fl <- fmls(x)

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
	lp <- fmls(xp) # Abbreviated/shortcut
	expect_length(ld, 4)
	expect_length(ls, 12)
	expect_length(lp, 12)
	expect_output(print(names(ld[1])), "xd")
	expect_equal(attributes(ld)$roles$hp, "exposure")
	expect_equal(attributes(ld)$labels$hp, "Horsepower")
	expect_match(names(ls)[1], "xs_y1x1c0m0_seq")

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

test_that("inputs are correct", {

	f <- frx(y ~ x + M(m))
	expect_s3_class(f, "formula_rx")

	# Long formulas may break names in pipe
	expect_s3_class({
		frx(Surv(stop_cv, status_cv) ~ X(lf_rest_zn) + X(bpm_rest_zn) + X(hf_rest_zn) + X(lf_stress_zn) + X(bpm_stress_zn) + X(hf_stress_zn) + M(rdr_msi_bl)) |>
		fmls()
	}, "list_of_formulas")

})

test_that("mediation creates appropriate lists", {

	# Simple mediation
	x <- Surv(stop, status) ~ X(primary) + X(secondary) + M(mediator)
	t <- trx(x)
	f <- frx(t)
	lof <- list_of_formulas(f)
	expect_length(lof, 5)

	# Mediation with covariates
	x <- Surv(stop, status) + Surv(stop, censor) ~ X(exposure) + M(mediator) + covariate
	t <- trx(x)
	f <- frx(t)
	lof <- list_of_formulas(f)
	expect_length(lof, 5)


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


test_that("lists can be decomposed into tables", {

	f <- mpg + wt ~ X(hp) + X(cyl) + M(gear) + drat + qsec
	labels <- list(mpg ~ "Mileage", hp ~  "Horsepower")
	groups <- list(c(drat, qsec) ~ "speed")
	t <- term_rx(f, labels = labels, groups = groups)
	x <- formula_rx(t, pattern = "sequential")
	lof <- list_of_formulas(x)
	expect_match(names(lof)[14], "x_y0x2c1m1_seq")
	expect_length(lof, 14)
	tbl <- explode(lof)
	expect_length(tbl, 7)
	expect_equal(nrow(tbl), 14)


})

