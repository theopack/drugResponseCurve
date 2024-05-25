#' @title TODO
#'
#' @description TODO
#'
#' @param dfCurDR TODO
#'
#' @param pred.df TODO
#'
#' @param pred TODO
#'
#' @return TODO
#'
#' @examples
#'
#' ## TODO
#'
#' @author Pascal Belleau, Astrid Deschênes and Alexander Krasnitz
#' @importFrom ggplot2
#' @encoding UTF-8
#' @export


plotCurveIC <- function(dfCurDR, pred.df, pred){

    df1 <- data.frame(ldose = pred.df[,1],
                      viability = pred[,1],
                      lower = pred[,2],
                      upper = pred[,3])

    maxVia <- min(2, max(df1$viability, dfCurDR$norm))
    minVia <- max(-1, min(df1$viability, dfCurDR$norm))

    pl <- ggplot(df1,aes(x=ldose, y=viability)) +
        geom_line(color="black") +
        ylim(minVia,maxVia) +
        geom_line(data=df1,aes(x=ldose,y=viability), color="black") +
        geom_line(data=df1, aes(x=ldose,y=upper), color="red") +
        geom_line(data=df1, aes(x=ldose,y=lower), color="blue") +
        geom_point(data=dfCurDR, aes(x=dosage,y=norm)) +
        geom_hline(yintercept = 1, linetype="dashed",
                   color = "black", linewidth=0.2) +
        theme_bw()

    return(pl)

}
