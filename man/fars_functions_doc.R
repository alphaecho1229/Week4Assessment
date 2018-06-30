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
#' fars_read("data.csv")
#'
#' name <- "data.csv"
#' fars_read(name)
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
#' make_filename("2018")
#'
#' year <- 2018
#' fileName <- make_filename(year)
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

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
#' fars_read_years(c(2016, 2017, 2018))
#'
#' years <- 2005:2014
#' data <- fars_read_years(years)
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr >%>
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
#' fars_summarize_years(c(2005, 2010, 2015))
#'
#' years <- 2000:2009
#' df <- fars_summarize_years(years)
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

#' Generate a map of a state showing locations of accidents throughout a given year.
#'
#' This function takes in a numbered code for a state and a year as arguments,
#' generates a file name, and reads in data from the file into a data frame.
#'
#' The function checks to make sure the state number is valid (returns a message "invalid state number" otherwise),
#' and creates a new dataframe consisting only of accidents from that state.
#'
#' This function will also result in an error if an invalid value for year is given.
#'
#' If there are no accidents in the state for that year, it returns a message to the user.
#'
#' The function then checks for longitude values greater than 900 and
#' latitude values greater than 90 and turns them into NAs.
#'
#' The function then generates the map using longitude and latitude values for the points.
#'
#' @param state.num a string or numeric representing the coded value for
#' a state.
#'
#' @param year a string or numeric representing the year for which we want to load accident data.
#'
#' @return a map of a state showing locations of the accidents for a given year
#'
#' @examples
#' fars_map_state(05, 2005)
#'
#' state <- 05
#' year <- 2005
#' fars_map_state(state, year)
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
