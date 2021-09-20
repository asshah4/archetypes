test_that("addition works", {

	f1 <- mpg + wt ~ hp
	f2 <- wt ~ hp + cyl

	x <- rx(f1)
	y <- rx(f2)

	# Merge
	z <- merge(x, y)
	expect_s3_class(z, "rx")

	# Add
	z <- x + y
	expect_s3_class(z, "rx")

	# Subtract (should only work if the LHS > 1 and RHS > 1)

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
