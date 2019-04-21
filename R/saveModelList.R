#' Create, append, and save Model List
#'
#' Create and save the Model List for export & record.
#'
#' @import dplyr
#' @import janitor
#'
#' @param model_filename String, file name to save the Model List.
#' @param algorithm String,
#' \itemize{
#'  \item "lm" OLS Linear Regression.
#'  \item "randomForest" Random Forest.
#'  \item "gbm" Gradient Boosting Machine.
#' }
#' @param input_features String, vector of input features used in model training.
#' @param predicted_feature String, name of the feature being predicted.
#' @param variableSelection_method String,
#' \itemize{
#'  \item "none" No variable selection, all input features are used.
#'  \item "lasso' LASSO variable selection.
#'  \item "randomForest" Random Forest variable selection based on variable importance.
#' }
#' @param scale Logical, indicate whether data was scaled for model training.
#' @param pca Logical, indicate whether data was transformed via PCA.
#' @param hyperparameter_list List, hyperparameters fed into the model.
#' @param error_metrics DataFrame, error metrics that are generated from \code{evaluate_RegressionModel}.
#' @param train_validation_ratio Ratio of Train Data to Validation Data.
#' @param train_test_ratio Ratio of Train Data and Validation Data to Test Data.
#'
#' @seealso \code{\link{evaluate_RegressionModel}}
#'
#' @return DataFrame of ModelList after updating.
#'
#' @export

saveModelList <- function(model_filename, algorithm, input_features, predicted_feature,
                          variableSelection_method, scale, pca, hyperparameter_list,
                          error_metrics, train_validation_ratio, train_test_ratio){

  # Coercing hyperparameter_list into a string
  if (class(hyperparameter_list) == "list"){
    hyperparameter_string <- paste(names(hyperparameter_list), hyperparameter_list,sep=" = ",collapse=" , " )
  } else {
    stop(paste0("ERROR: hyperparameter_list should be of class list. Got class ", class(hyperparameter_list), " instead."))
  }

  # Creating ModelList_Row to be appended to ModelList
  ModelList_Row <- data.frame(model_id,
                              model_filename,
                              algorithm = algorithm,
                              input_features = paste(input_features, collapse = ', '),
                              predicted_feature = predicted_feature,
                              variableSelection_method = variableSelection_method,
                              scale = scale,
                              pca = pca,
                              hyperparameters = hyperparameter_string,
                              train_test_ratio = train_test_ratio,
                              train_validation_ratio = train_validation_ratio)

  ModelList_Row <- ModelList_Row[rep(1, nrow(error_metrics)),]
  ModelList_Row <- cbind(ModelList_Row, error_metrics %>% clean_names() %>% as.data.frame())

  # Check if ModelList exists in global environment. If not, try reading from ModelList.csv.
  # If ModelList.csv exists, read and append. Else, create new and append.
  if (exists("ModelList")){
    ModelList <- rbind(ModelList, ModelList_Row)
  } else if (file.exists("./Model/ModelList.csv")){
    ModelList <- fread("./Model/ModelList.csv") %>% as.data.frame()
    ModelList <- rbind(ModelList, ModelList_Row)
  } else {
    ModelList <- ModelList_Row
  }

  # Saving ModelList.csv as backup in case there's any crash mid-run.
  if (!dir.exists("./Model/")){
    dir.create("./Model/")
  }
  write.csv(ModelList, "./Model/ModelList.csv", row.names=FALSE)

  return(ModelList)
}
