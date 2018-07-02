#' Read in files for multiple years
#'
#' This function takes a vector of strings or numerics,
#' uses lapply to call make_filename() for each year in the vector,
#' and attempts to read in data for each file name with fars_read.
#'
#' The function then uses mutate() to add a column specifying the year
#' and returns a dataframe consisting of the month and year of each row.
#'
#' If fars_read() fails to read in data for a particular year,
#' it prints a warning to the user and returns NULL.
#'
#' @param years a vector of strings or numerics, each in "yyyy" format.
#'
#' @return A list of dataframes, one for each year in years
#'
#' @examples
#' ## fars_read_years(2014)
#'
#' ## years <- 2013:2015
#' ## data <- fars_read_years(years)
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr %>%
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}
