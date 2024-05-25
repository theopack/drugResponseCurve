library(drc)

fitCurveNormal2P <- function(dfCurDR){


    try2fit <- function(dfCurDR){tryCatch({
        fit <- drm(normCur~dosC, data=dfCurDR,
                   fct=LL.4(fixed=c(NA, 0, 1, NA)))

        return(fit)
    },
    error = function(e){
        fit<-NULL
        return(fit)}#,
    #finally = return(fit)
    )}
    model_fixed<- try2fit(dfCurDR)


    return(model_fixed)
}
