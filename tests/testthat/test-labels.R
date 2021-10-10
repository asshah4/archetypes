test_that("labels and themes are set correctly", {
	set_rx_theme("murmur")
	rhs <- getOption("rx.rhs")
	expect_true(rhs == "predictors")

	set_rx_theme("ggdag")
	labs <- getOption("rx.roles")
	expect_true("L" %in% names(labs))

	reset_rx_roles()
})

test_that("labels and roles can be extracted", {

	f <- mpg ~ hp + wt + gear + cyl
	object <- rx(f)
	labs <- labels(object)
	expect_type(labs, "list")

	rls <- roles(object)
	expect_named(rls, c("lhs", "rhs"))
	expect_length(rls, 2)

	# TODO If no labels are present, should not error

})

test_that("global labels can be adjusted", {

	f <- mpg + cyl ~ X(wt) + hp + X(vs)
	rls <- list(lhs = "outcomes", rhs = "predictors", X = "exposure")
	set_rx_roles(rls)
	expect_equal(getOption("rx.lhs"), "outcomes")
	x <- rx(f)
	expect_equal(ncol(attributes(x)$roles), 4)
	reset_rx_roles()

})
