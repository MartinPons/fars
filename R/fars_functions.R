#' Read data downloaded from FARS
#' 
#' Reads the data downloaded from 
#' the \href{http://www.nhtsa.gov/Data/Fatality-Analysis-Reporting-System-(FARS)}{Fatality Analysis Reporting System}
#' into an R data.frame. A csv.bz2 file must exist in the working directory.
#' Otherwise the function throws an error.
#' 
#' @param filename A character string giving the name of the file to read
#' 
#' @return The function returns a data.frame
#' 
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' 
#' @examples
#' fars_read(accident_2013.csv.bz2)
#' fars_read(accident_2014.csv.bz2)

fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Create a file name for a FARS year
#' 
#' .
#' @param year four digit year either as a number or character string
#' 
#' @return a character string matching a FARS filename
#' 
#' @examples 
#' make_filename(2013)
#' make_filename(2014)

make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' List months and years of every accident from FARS
#' 
#' Creates a list of data.frames with month and year of accidents
#' loaded from a collection of files from FARS. 
#' 
#' @param years vector of four digit years either as numbers or as character strings.
#' The years must correspond to one file previously downloaded in the current 
#' working directory, otherwise the function returns NULL
#' 
#' @return a list of data.frames. Each element of the list references an specific year
#' and contains a log from accidents including only year and month information
#' 
#' @importFrom dplyr mutate select
#' 
#' @examples 
#' fars_read_years(c(2013, 2014)
#' fars_read_years(c("2014", "2015")))
#' fars_read_years(2012)
#' 
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


#' Count accidents for month and year
#' 
#' Comptutes the total number of accidens from FARS data for given years
#' 
#' @inheritParams fars_read_years
#' 
#' @return data.frame with the total number of accidents for every month and
#'  year given in \code{years} 
#'  
#' @importFrom dplyr bind_rows group_by summarise
#' @importFrom tidyr spread
#' 
#' @examples 
#' fars_summarize_years(c("2013", "2014"))
#' fars_summarise_years(2015)
#' fars_summarise_years(2012)
#' 
#' @export

fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>% 
                dplyr::group_by(year, MONTH) %>% 
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Plot state accidents
#' 
#' Draws a map with all the accidents in an specific USA state and year. If there
#' are no accidents for the data selected the function doesn't plot anything
#' and a message is shown in the console.
#' 
#' @param state.num a number of a US state either as a number or character string.
#' If the estate number provided is not a valid state number the function throws an error.
#' @inheritParams make_filename
#' 
#' @return this function doesn't return anything. A map is drawn.
#' 
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#' 
#' @examples 
#' fars_map_state(20, 2014)
#' fars_map_state(31, 2015)
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
