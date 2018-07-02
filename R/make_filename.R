#' Create the proper file name
#'
#' This function takes a year and uses it to create a string representing a file name.
#' Will result in an error if the string cannot be coerced to an integer
#'
#' @param  year A string or numeric in "yyyy" format
#'
#' @return This function returns a string generated using the year provided
#'
#' @examples
#' \donttest{
#' make_filename("2015")
#'
#' year <- 2015
#' fileName <- make_filename(year)
#' }
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  file <- sprintf("accident_%d.csv.bz2", year)
  system.file("extdata", file, package = "Week4Assessment")
}
