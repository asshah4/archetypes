test_that("validation methods", {
	f1 <- mpg ~ wt
	r1 <- c(exposures = "wt")
	expect_error(validate_roles(f1, r1))

	f2 <- mpg ~ hp
	set_rx_labels(list(rhs = "predictors"))
	r2 <- list(predictors = "wt")
	expect_error(validate_roles(f2, r2))
	reset_rx_labels()
})

