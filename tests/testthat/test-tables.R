test_that("lists can be decomposed into tables", {

	f <- mpg + wt ~ X(hp) + X(cyl) + M(gear) + drat + qsec + disp
	labels <- list(mpg ~ "Mileage", hp ~  "Horsepower")
	groups <- list(c(drat, qsec) ~ "speed")
	t <- term_rx(f, labels = labels, groups = groups)
	x <- formula_rx(t, pattern = "sequential")
	lof <- list_of_formulas(x)
	expect_match(names(lof)[20], "x_y0x2c2m1_seq")
	expect_length(lof, 20)
	tbl <- cast(lof)
	expect_length(tbl, 7)
	expect_equal(nrow(tbl), 20)
	expect_equal(tbl$covariate[1], NA_character_)
	expect_equal(tbl$covariate[20], "disp")


})

