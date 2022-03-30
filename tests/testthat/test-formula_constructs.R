test_that("simple list of prescribed formulas can be generated", {

	# Method dispatch
	expect_length(formula_construct(prescribe()), 0)
	expect_message(formula_construct(prescribe()))
	expect_error(formula_construct("error"))

	# Super simple check
	f <- mpg + wt ~ hp + cyl + gear
	x <- rx(f)
	fl <- fmls(x)
	expect_s3_class(fl, "formula_construct")

	# General formula check
	f <- mpg + wt ~ X(hp) + X(cyl) + gear + drat + qsec
	labels <- list(mpg ~ "Mileage", hp ~  "Horsepower")
	groups <- list(c(drat, qsec) ~ "speed", wt ~ "hardware")
	t <- term_archetype(f, label = labels, group = groups)
	x <- prescribe(t)
	ld <- formula_construct(x, pattern = "direct")
	ls <- formula_construct(x, pattern = "sequential")
	lp <- fmls(x, pattern = "parallel") # Abbreviated/shortcut
	expect_length(ld, 4)
	expect_length(ls, 12)
	expect_length(lp, 8)

	# Mediation check
	# TODO
	f <- mpg + wt ~ X(hp) + M(cyl) + gear + drat + qsec
	labels <- list(mpg ~ "Mileage", hp ~  "Horsepower")
	groups <- list(c(drat, qsec) ~ "speed", wt ~ "hardware")
	t <- term_archetype(f, label = labels, group = groups)
	x <- prescribe(t)
	ld <- formula_construct(x, pattern = "direct")
	ls <- formula_construct(x, pattern = "sequential")
	lp <- formula_construct(x, pattern = "parallel")
	expect_length(ld, 5)
	expect_length(ls, 11)
	expect_length(lp, 8)

	# Printing
	expect_output(print(ld), "mpg")
	expect_output(print(new_formula_construct()), "[0]")
	if (isTRUE(requireNamespace("tibble", quietly = TRUE))) {
		tibble::tibble(ld) |>
			print() |>
			expect_output("<fmls>")
	}

})

test_that("inputs are correct", {

	f <- rx(y ~ x + M(m))
	expect_s3_class(f, "script")

	# Long formulas may break names in pipe
	expect_s3_class({
		rx(Surv(stop_cv, status_cv) ~ X(lf_rest_zn) + X(bpm_rest_zn) + X(hf_rest_zn) + X(lf_stress_zn) + X(bpm_stress_zn) + X(hf_stress_zn) + M(rdr_msi_bl)) |>
		fmls()
	}, "formula_construct")

})

test_that("mediation creates appropriate lists", {

	# Simple mediation
	x <- Surv(stop, status) ~ X(primary) + X(secondary) + M(mediator)
	t <- tx(x)
	f <- rx(t)
	lof <- formula_construct(f)
	expect_length(lof, 5)

	# Mediation with covariates
	x <- Surv(stop, status) + Surv(stop, censor) ~ X(exposure) + M(mediator) + covariate
	t <- tx(x)
	f <- rx(t)
	lof <- formula_construct(f)
	expect_length(lof, 5)

})

test_that("generic formulas can be put together to create a list of formulas", {

	f1 <- mpg ~ cyl + hp
	f2 <- mpg ~ am + wt
	f3 <- hp ~ gear + qsec
	fl <- list(f1, f2, f3)

	lof <- formula_construct(fl)
	expect_length(lof, 3)
	expect_length(term_archetype(lof), 8)
	expect_length(attr(lof, "script"), 1)
})



