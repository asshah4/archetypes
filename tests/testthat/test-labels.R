test_that("labels and themes are set correctly", {
	set_rx_theme("murmur")
	rhs <- getOption("rx.rhs")
	expect_true(rhs == "predictors")

	set_rx_theme("ggdag")
	labs <- getOption("rx.labels")
	expect_true("L" %in% names(labs))

	reset_rx_labels()
})
