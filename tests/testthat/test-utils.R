test_that("general utilities work", {

	x <- list("a", "b", "c")
	y <- list(A = "a", B = "b", "c")
	expect_false(is.named(x))
	expect_false(is.named(y))

})
