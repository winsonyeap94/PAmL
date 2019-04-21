#' Rescaling/Unscaling Data
#'
#' Rescaling or unscaling of data.
#'
#' Rescaling is generally used for rescaling of test data based on training data \code{scale_attributes}.
#'
#' Unscaling is generally used for unscaling of predicted data for evaluation.
#'
#' @import dplyr
#'
#' @param data_df Dataframe including both predictor/input variables (x) and prediction/output variable (y)
#' @param scale_attributes DataFrame of scaling attributes (Mean, Std Dev) which are obtained from \code{scale_data()} function
#' @param scale_method String,
#' \itemize{
#'  \item "rescale" to rescale a new dataframe object based on the given \code{scale_attributes}.
#'  \item "unscale" to unscale a dataframe back to its original values.
#' }
#' @seealso \code{\link{scale_data}}
#'
#' @return Rescaled/unscaled dataframe.
#'
#' @examples
#' data_df <- rescale_data(data_df, scale_attributes, "rescale")
#'
#' @export

rescale_data <- function(data_df, scale_attributes, scale_method){
  # data_df : Training dataset including both predictor/input variables (x) and prediction/output variable (y)
  # scale_attributes : DataFrame of scaling attributes (Mean, Std Dev) which are obtained from scale_data() function
  # scale_method : String of either "rescale" or "unscale" to indicate whether to perform rescaling of a new DataFrame based on the given scale_attributes
  #                or to unscale them back to their original values

  if (scale_method == "rescale"){
    for (numeric_colname in scale_attributes$Variables){
      mean <- scale_attributes %>% filter(Variables == numeric_colname) %>% dplyr::select(Mean) %>% unlist(., use.names=FALSE)
      stddev <- scale_attributes %>% filter(Variables == numeric_colname) %>% dplyr::select(StdDev) %>% unlist(., use.names=FALSE)
      data_df[[numeric_colname]] <- (data_df[[numeric_colname]] - mean) / stddev
    }
  } else if (scale_method == "unscale"){
    for (numeric_colname in scale_attributes$Variables){
      mean <- scale_attributes %>% filter(Variables == numeric_colname) %>% dplyr::select(Mean) %>% unlist(., use.names=FALSE)
      stddev <- scale_attributes %>% filter(Variables == numeric_colname) %>% dplyr::select(StdDev) %>% unlist(., use.names=FALSE)
      data_df[[numeric_colname]] <- (data_df[[numeric_colname]] * stddev) + mean
    }
  } else {
    message("Invalid 'scale_method' argument provided. 'scale_method' needs to be of either 'rescale' or 'unscale'.")
    break
  }

  return(data_df)
}
