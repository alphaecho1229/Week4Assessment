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
#' \donttest{
#' fars_map_state(05, 2013)
#'
#' state <- 05
#' year <- 2013
#' fars_map_state(state, year)
#' }
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
