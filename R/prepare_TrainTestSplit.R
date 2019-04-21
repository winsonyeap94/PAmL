#' Prepares Train-Test Data With Scaling
#'
#' Prepares train, validation, and test dataset with option to scale (Z-transform) parameters.
#'
#' only numeric columns will be scaled, non-numeric columns are maintained as-is.
#'
#' @import dplyr
#'
#' @param data_df Complete dataset including both predictor/input variables (x) and prediction/output variable (y)
#' @param type String,
#' \itemize{
#'  \item "rescale" to rescale a new dataframe object based on the given \code{scale_attributes}.
#'  \item "unscale" to unscale a dataframe back to its original values.
#'  \item \emph{NULL} no scaling is done (Default).
#' }
#' @param complete.cases Logical, whether incomplete rows should be removed. Default is \emph{TRUE}.
#' @param train_test_ratio Ratio of (Train + Validation) Data to Test Data.
#' @param train_validation_ratio Ratio of Train Data to Validation Data.
#'
#' @seealso \code{\link{rescale_data}} \code{\link{train_test_split}} \code{\link{scale_data}}
#'
#' @return List of three objects:
#' \item{train_df}{Training dataframe}
#' \item{validation_df}{Validation dataframe}
#' \item{test_df}{Test dataframe}
#'
#' @examples
#' traintestsplit_list <- prepare_TrainTestSplit(data_df, type="temporal")
#' train_df <- traintestsplit_list[[1]]
#' validation_df <- traintestsplit_list[[2]]
#' test_df <- traintestsplit_list[[3]]
#'
#' @export

prepare_TrainTestSplit <- function(data_df, type=NULL, complete.cases = TRUE, train_test_ratio = 0.7, train_validation_ratio = 0.8){
  if (complete.cases == TRUE){
    data_df <- data_df %>% filter(complete.cases(.))
  }
  train_test_list <- train_test_split(data_df, train_ratio=train_test_ratio, type=type)
  train_df <- train_test_list[[1]]
  test_df <- train_test_list[[2]]
  train_validation_list <- train_test_split(train_df, train_ratio=train_validation_ratio, type=type)
  train_df <- train_validation_list[[1]]
  validation_df <- train_validation_list[[2]]

  if (!is.null(type)){
    scaled_data_list <- scale_data(train_df)
    scaled_train_df <- scaled_data_list[[1]]
    scale_attributes <- scaled_data_list[[2]]

    scaled_validation_df <- rescale_data(validation_df, scale_attributes = scale_attributes, scale_method = "rescale")
    scaled_test_df <- rescale_data(test_df, scale_attributes = scale_attributes, scale_method = "rescale")
    return(list(scaled_train_df, scaled_validation_df, scaled_test_df, scale_attributes))
  } else {
    return(list(train_df, validation_df, test_df))
  }
}
