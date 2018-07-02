#' Read a FARS file
#'
#' This functions checks to see if a given file exists,
#' and if it does, it reads it in and returns the data
#' as a data frame. If the file does not exist, it returns
#' a message that the file does not exist
#'
#' @param filename a character string representing the name of the file to be read
#'
#' @return This function returns a data frame consisting of
#'  the data read in from the given file name, or a message that
#'  the file does not exist
#'
#' @examples
#' \donttest{
#' fars_read("accident_2013.csv.bz2")
#'
#' name <- "accident_2013.csv.bz2"
#' fars_read(name)
#' }
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}
