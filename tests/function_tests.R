# function_tests.R

library(testthat)

# fars_read"accident_%d.csv.bz2"

throws_error(fars_read("inst/extdata/accident_2008.csv.bz2"))
