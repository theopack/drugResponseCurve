

plotAndAUCIC <- function(dfCurDR, model_fixed, diffMaxY){
    res <- list()
    if(! is.null(model_fixed)){

        pred.df <- expand.grid(dose=seq(max(dfCurDR$dosage[is.finite(dfCurDR$dosage)]), min(dfCurDR$dosage[is.finite(dfCurDR$dosage)]),length=10000))

        pred <- suppressWarnings(predict(model_fixed,newdata=10^pred.df,interval="confidence"))

        pl <- plotCurveIC(dfCurDR, pred.df, pred)

        # print(pl1)

        auc <- computeAUC(dfCurDR, pred.df, pred[,1, drop=FALSE], diffMaxY)
        aucL <- computeAUC(dfCurDR, pred.df, pred[,2, drop=FALSE], diffMaxY)
        aucH <- computeAUC(dfCurDR, pred.df, pred[,3, drop=FALSE], diffMaxY)

        #res[["pred"]] <- pred
        res[["pl"]] <- pl
        res[["auc"]] <- auc
        res[["aucL"]] <- aucL
        res[["aucH"]] <- aucH
    }
    return(res)

}
