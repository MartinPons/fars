

context("check correct filename")

test_that("correct filename",
          expect_match(make_filename(2013), "accident_2013.csv.bz2")
)
