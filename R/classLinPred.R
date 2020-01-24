#' This class will store the data being tested.
#' @param ExprSet ExpressionSet
#' @param betas named list of beta values.
#' @param time_col character object giving the column name for the follow up time
#' @param ind_col character object giving the column name for the event indicator
#' @param Plots list of plots
#' @keywords Linear Predictors classification
#' @export
#' @examples
#' x <- LinearPredictor()
#'

LinearPredictor <- setClass("LinearPredictor",
                         slots = c(ExprSet = "ExpressionSet",
                                   betas = "vector",
                                   time_col = "character",
                                   ind_col = "character",
                                   Plots = "list"))
