library(ggpubr)

generateGraphNormal <- function(res, drug, pathOut){
    dfCurDR <- res$data
    if(! dir.exists(file.path(pathOut, drug))){
        dir.create(file.path(pathOut, drug))
    }

    pl <- ggplot(data=dfCurDR, aes(x=dosage,y=norm))+geom_point()+
        geom_hline(yintercept = 1, linetype="dashed",
                   color = "black", linewidth=0.2)+
        theme_bw()

    if("CHAR" %in% names(res[["ctrl4p"]]) &
       "pl" %in% names(res[["ctrl4p"]][["CHAR"]]) &
       "CHAR" %in% names(res[["val2pL0_1"]]) &
       "pl" %in% names(res[["val2pL0_1"]][["CHAR"]]) &
       "CHAR" %in% names(res[["val2pNMax"]]) &
       "pl" %in% names(res[["val2pNMax"]][["CHAR"]])){

        figure <-  suppressWarnings(ggarrange(res[["ctrl4p"]][["CHAR"]]$pl,res[["val2pL0_1"]][["CHAR"]]$pl,
                                              res[["val2pNMax"]][["CHAR"]]$pl, pl, ncol = 2,nrow=2))

        png(file.path(pathOut, drug, paste0(dfCurDR$ID[1], "_", dfCurDR$org[1], ".", drug, ".png")))
        suppressWarnings(print(figure))
        dev.off()
    }else{
        png(file.path(pathOut, drug, paste0(dfCurDR$ID[1], "_", dfCurDR$org[1], ".", drug, ".pl", ".png")))
        print(pl)
        dev.off()
    }
}
