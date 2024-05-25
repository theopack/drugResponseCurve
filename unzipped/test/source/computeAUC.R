library(Bolstad2)


computeAUC <- function(dfCurDR, pred.df, pred, diffMax){


    res <- list()
    try2auc <- function(pred.df, pred){tryCatch({
        auc <- sintegral(pred.df[,1], pred)$int
        return(auc)
    },
    error = function(e){
        auc<-NA
        return(auc)}#,
    #finally = return(fit)
    )}


    auc <- try2auc(pred.df, pred[,1])


    # AUC

    res[["auc"]] <- auc

    # AUC relative
    res[["aucR"]] <- unname(auc / (diffMax * (max(dfCurDR$dosage[is.finite(dfCurDR$dosage)]) -
                                                  min(dfCurDR$dosage[is.finite(dfCurDR$dosage)]))))

    return(res)
}
