test_that("arithmetic works", {

	f1 <- mpg + wt ~ hp + cyl + gear
	f2 <- wt ~ hp + cyl

	x <- rx(f1)
	y <- f2

	# Merge
	merge_z <- merge(x, y)
	add_z <- x + y
	expect_s3_class(merge_z, "rx")
	expect_equal(merge_z, add_z, ignore_attr = TRUE)

	# Split
	split_x <- split(x, y)
	split_z <- add_z - y
	expect_s3_class(split_x, "rx")
	expect_equal(split_x, split_z, ignore_attr = TRUE)
	expect_error({
		rx(mpg ~ hp) - rx(wt ~ hp)
	})
	expect_error(x/y)

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
