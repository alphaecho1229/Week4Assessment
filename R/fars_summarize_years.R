#' Summarize the number of accidents in each month
#'
#' This function takes a vector of strings or numerics representing
#' which years will be summarized. The vector is passed to
#' fars_read_years to create a list of dataframes.
#'
#' The function then calls bind_rows to combine the dataframes into one.
#' The single dataframe is then passed into group_by and summarize to
#' obtain the total number of accidents each month.
#'
#' Finally, the summarized dataframe is passed to spread to create a dataframe
#' where the columns are each year, the rows are the months, and the values are the
#' number of accidents each month for each year.
#'
#' If years does not consist of strings or numerics representing the years to summarize,
#' an error may result.
#'
#' @param years a vector of strings or numerics, each in "yyyy" format.
#'
#' @return a dataframe consisting of the number of accidents of each month,
#' with the summarized years as the column names.
#'
#' @examples
#' \donttest{
#' fars_summarize_years(2015)
#'
#' years <- 2013:2015
#' df <- fars_summarize_years(years)
#' }
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}
