

processDRCurveAvr <- function(drug, dfRes){
    dfResDR <- dfRes[toupper(dfRes$drug_a) == toupper(drug),]
    dfResDR <- dfResDR[dfResDR$dosage_type == "Averaged",]
    dfResDR$dosage <- dfResDR$dosage_concentration_a
    dfResDR$dosC <- 10^dfResDR$dosage
    dfResDR$norm <- dfResDR$percent_viability/100
    dfResDR$normCur <- dfResDR$percent_viability/100

    model_fixed <- fitCurveNormal2P(dfResDR)
    diffMaxY <- NA
    aucPl <- NULL
    if(!is.null(model_fixed)){
        diffMaxY <- 1
        aucPl <- plotAndAUCIC(dfResDR, model_fixed, diffMaxY)
    }

    res[["val2pNMax"]] <- list()
    res[["val2pNMax"]][["model"]] <- model_fixed
    res[["val2pNMax"]][["CHAR"]] <- aucPl
    res[["val2pNMax"]][["PROCESSED"]] <- list(auc = dfResDR[dfResDR$dosage_type == "Averaged", "auc"][1]/100,
                                              aucR = dfResDR[dfResDR$dosage_type == "Averaged", "relative_auc"][1]/100)
    return(res[["val2pNMax"]])

}
