library(CoreGx)

fitCurveCauchy3P <- function(dfCurDR){
    fCur <- function (x, pars){
        return(pars[2] + (1 - pars[2])/(1 + (10^x/10^pars[3])^pars[1]))
    }

    try2fit <- function(dfCurDR){tryCatch({
        median_n <- 1
        scale <- 0.07
        family <-"Cauchy"
        trunc <- FALSE
        lower_bounds <- c(0, 0, -6)
        upper_bounds <- c(4, 1, 6)
        concCur <- c(dfCurDR[is.finite(log(dfCurDR[,"dosC"])),"dosC"])
        viaCur <- c( dfCurDR[is.finite(log(dfCurDR[,"dosC"])), "normCur"])
        gritty_guess <- c(pmin(pmax(1, lower_bounds[1]),
                               upper_bounds[1]),
                          pmin(pmax(min(viaCur), lower_bounds[2]),
                               upper_bounds[2]),
                          pmin(pmax(log10(concCur[which.min(abs(viaCur - 1/2))]), lower_bounds[3]),
                               upper_bounds[3]))


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
    fit<- try2fit(dfCurDR)

    return(fit)
}
