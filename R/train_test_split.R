#' Train-Test Split
#'
#' Temporal-based or random-based train-test split.
#'
#' This function can be used in sequence after another for Train-Validation-Test splitting.
#'
#' @param input_df Dataframe that is to be split.
#' @param train_ratio Ratio of training data to test data.
#' @param type String,
#' \itemize{
#'  \item "temporal" for time-dependant splitting of data.
#'  \item "random" for time-independent splitting of data (sampling at random).
#' }
#'
#' @seealso \code{\link{prepare_TrainTestSplit}}
#'
#' @return List consisting of:
#' \item{train_df}[Training dataframe]
#' \item{test_df}{Test dataframe}
#'
#' @examples
#' train_test_list <- train_test_split(data_df, 0.7, type="temporal")
#'
#' @export

train_test_split <- function(input_df, train_ratio, type){

  if (type == "temporal"){
    cutoff_idx <- floor(nrow(input_df) * train_ratio)
    train_df <- input_df[seq(1, cutoff_idx),]
    test_df <- input_df[seq(cutoff_idx+1, nrow(input_df)),]
  } else if (type == "random"){
    train_idx <- sample(seq(1, nrow(input_df)), floor(nrow(input_df) * train_ratio))
    train_df <- input_df[train_idx,]
    test_df <- input_df[-train_idx,]
  } else {
    message("Invalid 'type' argument provided. 'type' needs to be of either 'temporal' or 'random'.")
    break
  }
  return(list(train_df, test_df))
}
