# function_tests.R

# fars_read
expect_that(fars_read(accident_2013.csv.bz2), is_a("data.frame"))
throws_error(fars_read(accident_2010.csv.bz2))

#make_filename
expect_that(make_filename(2013), matches("accident_2013.csv.bz2"))

# fars_read_years
expect_that(fars_read_years(c(2013, 2014)), is_a("list"))
