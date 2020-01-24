#' @title genLinearPred function
#' @param Data matrix
#' @param betas named list of beta values
#' @keywords linear predictor
#' @export
#' @examples
#' ## Pass the function a matrix and a named list of beta values
#' ## The beta values must have the same names as the rows in the matrix
#' genLinearPred(betas, matrix)

findBestCuttoff <- function(Data, time="time", ind="ind", PRINT=TRUE) {
        Data@ExprSet$time <- pData(Data@ExprSet)[,time]
        Data@ExprSet$ind <- pData(Data@ExprSet)[,ind]
        Lower_q <- 0.2
        Upper_q <- 0.8
        ## Order Linear Predictors
        Data@ExprSet = Data@ExprSet[,order(Data@ExprSet$LinearPred)]
        ## find the position of Q1 and Q3 in the dataset.
        q1 <- round(length(Data@ExprSet$LinearPred)*Lower_q);
        q3 <- round(length(Data@ExprSet$LinearPred)*Upper_q);
        ## Set up survival formula
        surv <- Surv(Data@ExprSet$time, Data@ExprSet$ind)
        ## Generate Cutoffs
        ## run from Lower_q to Upper_q in as many jumps as there are entries between q1 and q2
        cutoffs <- seq(Lower_q,Upper_q,
                       by=(Upper_q-Lower_q)/(q3-q1))
        ## Loop through the cutoffs and get the pvalue from a cox test
        pvals <- lapply(q1:q3, function(x) {
                group <- ifelse(1:length(sampleNames(Data@ExprSet)) >= x, 1, 0)
                pval <- summary(coxph(surv ~ group))$sctest[3]
        }) %>% unlist()
        ## get the lowest Pvalue and the cutoff it corresponds to.
        best_p <- min(pvals)
        best_cutoff <- cutoffs[pvals == best_p]
        best_int <- (q1:q3)[pvals == best_p]
        ## plot the cutoffs and the pvalues.
        Data@Plots[["CutOff"]] <- cbind(cutoffs, -log2(pvals)) %>%
                data.frame() %>%
                setNames(c("Percentiles","p_values")) %>%
                ggplot(aes(x=cutoffs,y=p_values)) +
                geom_point(pch="*", size=5) +
                geom_vline(xintercept = best_cutoff, linetype="dashed", colour="red") +
                geom_label(x=cutoffs[1], y=-log(best_p,2)/2, hjust=0,
                           label=paste("Best cut-off = ",
                                       round(best_cutoff,digits=2),
                                       "\nLog-rank P = ",
                                       round(best_p,digits=4),
                                       "\nInteger = ",
                                       best_int, sep=""))+
                theme_bw()

        if ( PRINT == TRUE ) {
                print(Data@Plots[["CutOff"]]) }
        Data@ExprSet$BestGroup <- ifelse(1:length(sampleNames(Data@ExprSet)) >= best_int, "High Risk", "Low Risk")
        Data
        }
