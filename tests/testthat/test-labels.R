test_that("labels and themes are set correctly", {
	set_rx_theme("murmur")
	rhs <- getOption("rx.rhs")
	expect_true(rhs == "predictors")

	set_rx_theme("ggdag")
	labs <- getOption("rx.labels")
	expect_true("L" %in% names(labs))

	reset_rx_labels()
})

test_that("labels can be extracted", {

	f <- mpg ~ hp + wt + gear + cyl
	object <- rx(f)
	labs <- labels.rx(object)
	expect_named(labs, c("lhs", "rhs"))
	expect_length(labs, 2)

	# TODO If no labels are present, should not error

})

test_that("global labels can be adjusted", {

	f <- mpg + cyl ~ X(wt) + hp + X(vs)
	labels <- list(lhs = "outcomes", rhs = "predictors", X = "exposure")
	set_rx_labels(labels)
	expect_equal(getOption("rx.lhs"), "outcomes")
	x <- rx(f)
	expect_equal(ncol(attributes(x)$roles), 4)
	reset_rx_labels()

})
