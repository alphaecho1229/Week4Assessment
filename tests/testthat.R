library(testthat)

test_file_name <- make_filename(2018)

expect_match(test_file_name, "accident_2018.csv.bz2")
