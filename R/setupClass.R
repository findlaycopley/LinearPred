#' @title setupClass function
#' @param data matrix or ExpressionSet object
#' @param betas named vector of beta values
#' @param time vector of follow up time values (if not in ExprSet), or FALSE to ignore
#' @param ind vector of event indicators (0 = no event/1 = event; if not in ExprSet), or FALSE to ignore
#' @param time_col character object giving the column name for the follow up time
#' @param ind_col character object giving the column name for the event indicator
#' @keywords
#' @export
#' @examples
#' ## If you have your ExprSet with columns called time and ind
#'
#' setupClass(ExprSet, betas)
#'
#' ## If you have your ExprSet with columns named differently
#'
#' setupClass(ExprSet,
#'         betas,
#'         time_col = "PROG_TIME",
#'         ind_col = "PROG_IND")
#'
#' ## If you have an expression matrix, beta values and seperate time and ind vectors
#' ## Make sure the vectors and the matrix are all in the correct orders.
#'
#' setupClass(data = matrix,
#'         betas,
#'         time = time_vector,
#'         ind = ind_vector,
#'         time_col = "PROG_TIME",
#'         ind_col = "PROG_IND")
#'

setupClass <- function(data,
                       betas,
                       time=FALSE,
                       ind=FALSE,
                       time_col = "time",
                       ind_col = "ind") {
        if (class(data) == "ExpressionSet") {
                rv <- LinearPredictor(ExprSet = data, betas = betas)
        }
        if (class(data) == "matrix") {
                rv <- LinearPredictor(ExprSet = ExpressionSet(assayData = data),
                                      betas = betas)
        }
        if ( ! time[1] %>% as.character() == "FALSE" ) {
                rv@ExprSet[[ time_col ]] <- time
        }
        if ( ! ind[1] %>% as.character() == "FALSE" ) {
                rv@ExprSet[[ ind_col ]] <- ind
        }
        rv@time_col = time_col
        rv@ind_col = ind_col
        rv
}


