
cauchyPlotAndAUC <- function(dfCurDR, model_fixed, diffMaxY){
    res <- list()
    if(! is.null(model_fixed)){

        pred.df <- expand.grid(dose=seq(max(dfCurDR$dosage[is.finite(dfCurDR$dosage)]), min(dfCurDR$dosage[is.finite(dfCurDR$dosage)]),length=10000))
        # (x, hs, yMin, EC50, yMax)
        pred <- drCurve(10^(pred.df), model_fixed[1], model_fixed[2], 10^(model_fixed[3]), 100*model_fixed[4])/100

        pl <- plotCurve(dfCurDR, pred.df, pred)

        # print(pl1)

        auc <- computeAUC(dfCurDR, pred.df, pred[,1, drop=FALSE], diffMaxY)


        #res[["pred"]] <- pred
        res[["pl"]] <- pl
        res[["auc"]] <- auc

    }
    return(res)

}
