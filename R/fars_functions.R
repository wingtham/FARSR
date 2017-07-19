#' Read FARS data from a file into R
#'
#' The function acessess and reads in data from the given filename. It
#' throws an error if the file does not exist. The data is then represented as a
#' tibble. Note that any warning and progress messages are suppressed during the
#' read process.
#'
#' @param filename A string specifying the file name of the FARS data
#'
#' @return a tibble representation of the data
#'
#' @examples
#' \dontrun{fars_read('accident_2015.csv.bz2')}
#'
#' @export
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}


#' Create filename according to Year
#'
#' The function creates the file name according to the input argument. The
#' filename should correspond to the year of the FARS data and is in the format
#' of "accident_YYYY_.csv.bz2", with YYYY being the year of interest.
#'
#' @param year Indicating the year of interest. It should be entered as 4 digit
#'   integer.
#'
#' @return a string specifying the filename
#'
#' @examples
#' \dontrun{make_filename(2013)}
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}


#' Read data from multiple files into R
#'
#' The function reads in data from filenames created from the input
#' argument. \code{\link{make_filename}} is called to create the filenames. For
#' each of the filenames, an attempt is made to read the file, and only column
#' /code{MONTH} and /code{year} are selected and stored as element in a list; if
#' the operations fail, the list element corresponding to the file will have no
#' records. Once all files are read, the list is returned.
#'
#' @param years A list or vector specifying the years of interest
#'
#' @return a list containing data read from the files. Each element corresponds
#'   to one particular year specified by the file name.
#'
#' @examples
#' \dontrun{fars_read_years(c(2013:2015))}
#'
#' @export
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @seealso \code{\link{fars_read}} for reading files, and
#'   \code{\link{make_filename}} for creating the filename
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


#' Summary of the FARS data by Year and Month
#'
#' The function shows number of data points per month and year in a tabular
#' format. The summarising operation is done in the following steps: \enumerate{
#' \item data is specified and read from file, refer
#' \code{\link{fars_read_years}}, \item data of each year is combined, \item
#' data is grouped by month and year, \item data is then counted and tabulated }
#'
#' @param years A list or vector specifying the years of interest
#'
#' @return a data frame showing number of data points per MONTH and year. Rows
#'   correspond to MONTH while columns correspond to year
#'
#' @examples
#' \dontrun{fars_summarise_years(c(2013:2015))}
#'
#' @export
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#' @seealso \code{\link{fars_read_year}} for reading files, and
#'   \code{\link{make_filename}} for creating the filename
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}


#' Visualising Incidents of selected state and year
#'
#' The function plots incidents of selected state and year from the FARS
#' dataset. It has exception handling to ensure\itemize{\item the file name is
#' valid (refer \code{\link{fars_read}}), \item the state number does exist in
#' the data set, and \item there is indeed data for the selected state number.}
#' Incidents that has longitude over 900 and Latitude over 90 are discarded.
#'
#' @param state.num an integer representing the State Number
#'
#' @param year Indicating the year of interest. It should be entered as 4 digit
#'   integer.
#'
#' @return None.
#'
#' @examples
#' \dontrun{fars_map_state(1, 2015)}
#'
#' @export
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#' @seealso \code{\link{fars_read}} for reading file
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
