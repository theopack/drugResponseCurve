library(ggpubr)

generateGraphCauchy <- function(res, drug, pathOut){
    dfCurDR <- res$data

    if(! dir.exists(file.path(pathOut, drug))){
        dir.create(file.path(pathOut, drug))
    }
    if("CHAR" %in% names(res[["cauchy4p"]]) &
       "pl" %in% names(res[["cauchy4p"]][["CHAR"]]) &
       "CHAR" %in% names(res[["cauchy3p"]]) &
       "pl" %in% names(res[["cauchy3p"]][["CHAR"]])){

        figure <-  suppressWarnings(ggarrange(res[["cauchy4p"]][["CHAR"]]$pl,res[["cauchy3p"]][["CHAR"]]$pl,
                                              ncol = 2,nrow=1))

        png(file.path(pathOut, drug, paste0(dfCurDR$ID[1], "_", dfCurDR$org[1], ".", drug, ".Cauchy.png")))
        suppressWarnings(print(figure))
        dev.off()
    }
}
