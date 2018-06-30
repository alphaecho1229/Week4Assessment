library(testthat)
library(Week4Assessment)

#test_check("Week4Assessment")

expect_that(fars_read("badfilename"), throws_error())

#expect_match(make_filename("2007"), "accident_2007.csv.bz2")
