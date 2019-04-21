#' Interactive Plotting
#'
#' Interactive trend plotting using \code{xts} and \code{dygraphs} packages in R for quick data exploration.
#'
#' A GUI is shown for the user to select the variables to be plotted against a timeseries.
#'
#' @import dplyr
#' @import xts
#' @import dygraphs
#'
#' @param data_df Dataframe for plotting. Must include a datetime column (POSIXct).
#' @param regex_string Regex-based searching for variables in \code{data_df}. Defaults to \emph{NULL}.
#' @param datetime_colname Column name for the datetime column in \code{data_df}. Defaults to "DateTime".
#'
#' @return None
#'
#' @examples
#' interactivePlot(data_df)
#'
#' @export

interactivePlot <- function(data_df, regex_string=NULL, datetime_colname="DateTime"){
  if (!is.null(regex_string)){
    variable_list <- grep(regex_string, grep(datetime_colname,colnames(data_df), inver=TRUE, value=TRUE), value=TRUE)
  } else {
    variable_list <- grep(datetime_colname,colnames(data_df), inver=TRUE, value=TRUE)
  }
  sel_variable <- select.list(variable_list, multiple = TRUE, graphics = TRUE)
  trend <- xts(data_df %>% dplyr::select(one_of(sel_variable)), data_df[[datetime_colname]])
  dyObj <- dygraph(trend) %>% dyRangeSelector() %>% dyRoller() %>%
    dyOptions(connectSeparatedPoints = TRUE, useDataTimezone = TRUE) %>%
    dyLegend(width=350)
  print(dyObj)
}
