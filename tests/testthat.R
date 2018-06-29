library(testthat)
library(Week4Assessment)

#test_check("Week4Assessment")

expect_that(fars_read("badfilename"), shows_message())

expect_that(make_filename("2007"), matches("accident_2007.csv.bz2"))
