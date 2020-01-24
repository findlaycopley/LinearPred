#' @title genLinearPred function
#' @param Data object of class LinearPredictor
#' @keywords linear predictor
#' @export
#' @examples
#' ## Pass the function a matrix and a named list of beta values
#' ## The beta values must have the same names as the rows in the matrix
#' genLinearPred(Data)
#'
#' ## Returns the class object with the linear predictor added to the ExpressionSet slot
#' ## Also filters out all genes that are not in either the beta values of the ExpressionSet

genLinearPred <- function(Data) {
        ## filter the betas down to only those in matrix
        Data@betas <- Data@betas[names(Data@betas) %in% featureNames(Data@ExprSet)]
        ## Filter the matrix down to the remaining beta genes
        Data@ExprSet <- Data@ExprSet[names(Data@betas),]
        fData(Data@ExprSet)$betas <- Data@betas
        ## It's important that these are now ordered exactly the same.
        ## This is done here by using the names of the beta values to subset the matrix
        ## Multiply the expression of the genes by the beta values.
        Data@ExprSet$LinearPred <- colSums(
                sweep(exprs(Data@ExprSet),
                      MARGIN=1,
                      Data@betas,
                      `*`)
                )
        Data
}
