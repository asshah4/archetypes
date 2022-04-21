test_that("term lists can be made", {

	t1 <- tm(mpg ~ wt + hp + gear)
	t2 <- tm(am ~ wt + hp + gear)
	t3 <- tm(cyl ~ wt + hp + gear)
	x <- list(t1, t2, t3)
	tl <- term_list(x)
	expect_s3_class(tl, "term_list")
	expect_output(print(term_list()), "<term_list\\[0\\]>")


  if (isTRUE(requireNamespace("tibble", quietly = TRUE))) {
    tibble::tibble(tl) |>
      print() |>
      expect_output("<tmls>")
  }

})
