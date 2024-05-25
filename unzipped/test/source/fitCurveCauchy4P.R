library(CoreGx)

fitCurveCauchy4P <- function(dfCurDR){
    fCur <- function (x, pars){
        return(pars[2] + (pars[4] - pars[2])/(1 + (10^x/10^pars[3])^pars[1]))
    }

    try2fit <- function(dfCurDR){tryCatch({
        median_n <- 1
        scale <- 0.07
        family <-"Cauchy"
        trunc <- FALSE
        lower_bounds <- c(0, 0, -6, 0.5)
        upper_bounds <- c(4, 1, 6, 2)
        concCur <- c(rep(1e-10, length(which(abs(dfCurDR[,"dosC"]) < 1e-10) )),
                     dfCurDR[is.finite(log(dfCurDR[,"dosC"])),"dosC"],
                     rep(1e10, length(which(!is.finite(dfCurDR[,"dosC"]) ))))
        viaCur <- c(dfCurDR[which(abs(dfCurDR[,"dosC"]) < 1e-10), "normCur"] ,
                    dfCurDR[is.finite(log(dfCurDR[,"dosC"])), "normCur"],
                    dfCurDR[which(!is.finite(dfCurDR[,"dosC"]) ), "normCur"])
        gritty_guess <- c(pmin(pmax(1, lower_bounds[1]),
                               upper_bounds[1]),
                          pmin(pmax(min(viaCur), lower_bounds[2]),
                               upper_bounds[2]),
                          pmin(pmax(log10(concCur[which.min(abs(viaCur - 1/2))]), lower_bounds[3]),
                               upper_bounds[3]),
                          pmin(pmin(max(viaCur), upper_bounds[4]),
                               lower_bounds[4]))

        fit <- optim(par = gritty_guess,
                     fn = function(t) {
                         CoreGx:::.residual(
                             x = log10(concCur), y = viaCur, n = median_n, pars = t, f = fCur,
                             scale = scale, family = family, trunc = trunc
                         )
                     },
                     lower = lower_bounds,
                     upper = upper_bounds,
                     control = list(
                         factr = 1e-08,
                         ndeps = rep(1e-4, times = length(gritty_guess)),
                         trace = 0
                     ),
                     method = "L-BFGS-B")
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
