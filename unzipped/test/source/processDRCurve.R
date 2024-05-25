

processDRCurve <- function(drug, dfCur){

    ctrlName <- c("Stauro" , "Positive Control")
    dfCurDR <- dfCur[dfCur$Layer2 == drug &
                         !is.na(dfCur$Conc) &
                         (is.na(dfCur$Transfer.Status) | dfCur$Transfer.Status != "Not processed"),]
    dfCtrlP <- dfCur[dfCur$Layer2 == ctrlName[2] &
                         (is.na(dfCur$Transfer.Status) | dfCur$Transfer.Status != "Not processed"),]
    dfCtrlP$Conc <- 0
    dfCtrlN <- dfCur[dfCur$Layer2 == ctrlName[1] &
                         (is.na(dfCur$Transfer.Status) | dfCur$Transfer.Status != "Not processed"),]
    dfCtrlN$Conc <- Inf
    dfCurDR <- rbind(dfCurDR, dfCtrlP, dfCtrlN)

    dfCurDR$Conc <- dfCurDR$Conc
    dfCurDR <- dfCurDR[order(dfCurDR$Conc),]
    dfCurDR$dosage <- log10(dfCurDR$Conc*1e6)
    dfCurDR$dosC <- dfCurDR$Conc*1e6

    res <- list()
    res[["data"]] <- dfCurDR


    dfCurDR$normCur <- dfCurDR$norm
    model_fixed <- fitCurveNormal4P(dfCurDR)
    diffMaxY <- NA
    aucPl <- NULL
    if(!is.null(model_fixed)){
        diffMaxY <- model_fixed$parmMat[model_fixed$parNames[[2]]=='d',1] - model_fixed$parmMat[model_fixed$parNames[[2]]=='c',1]
        aucPl <- plotAndAUCIC(dfCurDR, model_fixed, diffMaxY)
    }
    res[["ctrl4p"]] <- list()
    res[["ctrl4p"]][["model"]] <- model_fixed
    res[["ctrl4p"]][["CHAR"]] <- aucPl

    dfCurDR$normCur <- dfCurDR$normL
    model_fixed <- fitCurveNormal2P(dfCurDR)
    diffMaxY <- NA
    aucPl <- NULL
    if(!is.null(model_fixed)){
        diffMaxY <- 1
        aucPl <- plotAndAUCIC(dfCurDR, model_fixed, diffMaxY)
    }

    res[["val2pL0_1"]] <- list()
    res[["val2pL0_1"]][["model"]] <- model_fixed
    res[["val2pL0_1"]][["CHAR"]] <- aucPl

    dfCurDR$normCur <- dfCurDR$norm
    model_fixed <- fitCurveCauchy4P(dfCurDR)
    diffMaxY <- NA
    aucPL <- NULL
    if(!is.null(model_fixed)){
        # (x, hs, yMin, EC50, yMax)
        diffMaxY <-model_fixed$par[4] - model_fixed$par[2]
        aucPL <- cauchyPlotAndAUC(dfCurDR, model_fixed$par, diffMaxY)
    }
    res[["cauchy4p"]] <- list()
    res[["cauchy4p"]][["model"]] <- model_fixed
    res[["cauchy4p"]][["CHAR"]] <- aucPL

    dfCurDR$normCur <- dfCurDR$normL
    model_fixed <- fitCurveCauchy3P(dfCurDR)
    diffMaxY <- NA
    aucPL <- NULL
    if(!is.null(model_fixed)){
        # (x, hs, yMin, EC50, yMax)
        diffMaxY <-1 - model_fixed$par[2]
        aucPL <- cauchyPlotAndAUC(dfCurDR,
                                  c(model_fixed$par[1], model_fixed$par[2],
                                    model_fixed$par[3], 1), diffMaxY)
    }
    res[["cauchy3p"]] <- list()
    res[["cauchy3p"]][["model"]] <- model_fixed
    res[["cauchy3p"]][["CHAR"]] <- aucPL

    return(res)
}
