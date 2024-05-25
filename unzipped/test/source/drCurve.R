

drCurve <- function(x, hs, yMin, EC50, yMax){
    return(yMin + (yMax-yMin) / (1+10^(hs*(log10(x) - log10(EC50)))) )
}
