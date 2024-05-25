library(drc)

fitCurveNormal4P <- function(dfCurDR){
    res <- list()

    try2fit <- function(dfCurDR){tryCatch({
        fit <- drm(normCur~dosC, data=dfCurDR,
                   fct=LL.4())
        return(fit)
    },
    error = function(e){
        fit<-NULL
        return(fit)}#,
    #finally = return(fit)
    )}

    model_fixed <- try2fit(dfCurDR)

    return(model_fixed)
}
