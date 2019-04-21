#' Scale data (Z-Transformation)
#'
#' Scales a dataframe based on Z-transformation (based on \code{base::scale()} function in R). Also returns a supplementary dataframe
#' which contains information on the Mean and Standard Deviation of the variables being scaled.
#'
#' only numeric columns will be scaled, non-numeric columns are maintained as-is.
#'
#' @import dplyr
#'
#' @param data_df Training dataset including both predictor/input variables (x) and prediction/output variable (y).
#' @seealso \code{\link{rescale_data()}} \code{\link{train_test_split()}} \code{\link{prepare_TrainTestSplit()}}
#'
#' @return List of two objects:
#' \item{scaled_data_df}{Scaled dataframe. Only detected numeric columns are scaled.}
#' \item{scale_attributes}{Dataframe of scale attributes (Mean, Std_Dev).}
#'
#' @examples
#' scaled_data_list <- scale_data(data_df)
#' scaled_data_df <- scaled_data_list[[1]]
#' scale_attributes <- scaled_data_list[[2]]
#'
#' @export

scale_data <- function(data_df){
  # Identifying numeric columns and scaling them
  numeric_colnames <- colnames(data_df)[sapply(data_df, function(x) is.numeric(x))]

  # If there is a column with Inf, warning will be triggered and exit.
  inf_colnames <- colnames(data_df[numeric_colnames])[sapply(data_df[numeric_colnames], function(x) max(x) == Inf)]
  if (length(inf_colnames) > 0){
    message("Error in scale_data(data_df): Columns with Inf found.")
    break
  }

  # Extracting the non-numeric columns to join them separately
  non_numeric_colnames <- colnames(data_df)[!sapply(data_df, function(x) is.numeric(x))]
  # Scaling the data
  scaled_data_df <- scale(data_df[numeric_colnames], center = TRUE, scale = TRUE)
  scale_attributes <- data.frame(Variables = colnames(scaled_data_df),
                                 Mean = attr(scaled_data_df, "scaled:center"),
                                 StdDev = attr(scaled_data_df, "scaled:scale"))
  scaled_data_df <- cbind(scaled_data_df, data_df[non_numeric_colnames])
  scaled_data_df <- scaled_data_df %>% dplyr::select(one_of(colnames(data_df)))

  return(list(scaled_data_df, scale_attributes))
}
