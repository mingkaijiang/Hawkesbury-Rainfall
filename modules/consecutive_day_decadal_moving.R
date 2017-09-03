##############################################################################################################
## Compute consecutive dry days based on decadal information - moving time series
consecutive_day_decadal_moving<-function(Datfile,
                                  sourceDir = DAILY.DATA.DIRECTORY, 
                                  destDir = DAILY.OUTPUT.DIRECTORY) {
    ## This function calcualtes consecutive day days indices 
    ## based on decadal information;
    ## Consecutive dry days: # consecutive no rain days / # days in a season
    ## Southern Hemisphere year starts on 1/Jul
    
    # prepare file list
    dir.create(destDir, showWarnings = FALSE)

    inName <- paste0(sourceDir, "/", Datfile)
    outName <- paste0(destDir, "/", Datfile)
    
    # read in file and prepare the df
    dd <- read.csv(inName)
    colnames(dd)<-c("date","year","month","day","prcp")
    dd$doy <- yday(dd$date)
    
    year.list <- unique(dd$year)
    e.year <- length(year.list)
    year.list <- year.list[10:e.year]
    
    # prepare output df
    outDF <- data.frame(year.list, NA, NA, NA, 
                        NA, NA, NA, NA)
    colnames(outDF) <- c("year", "decadal_max", "Q1", "Q1_occurrence",
                         "Q5", "Q10", "Q5_occurrence", "Q10_occurrence")

    # count # days 
    for (j in outDF$year) {
        # extract the three periods
        
        decDF <- subset(dd, year >= (j-9) & year <= j)

        # consecutive dry days in each season and year
        dec_cons <- rle(decDF$prcp)
        
        # max consecutive dry days
        outDF[outDF$year == j, "decadal_max"] <- max(dec_cons$lengths[dec_cons$values==0]) 

        # rank decadal consecutive drought days
        cons_list <- dec_cons$lengths[dec_cons$values==0]
        Q1 <- round(quantile(cons_list, probs=0.99),0)
        Q5 <- round(quantile(cons_list, probs=0.95),0)
        Q10 <- round(quantile(cons_list, probs=0.9),0)
        
        Q1_TF <- cons_list>=Q1
        Q5_TF <- cons_list>=Q5
        Q10_TF <- cons_list>=Q10
        
        Q1_occurrence <- table(Q1_TF)["TRUE"]
        Q5_occurrence <- table(Q5_TF)["TRUE"]
        Q10_occurrence <- table(Q10_TF)["TRUE"]
        
        # percentile based consecutive day indices
        outDF[outDF$year == j, "Q1"] <- Q1
        outDF[outDF$year == j, "Q5"] <- Q5
        outDF[outDF$year == j, "Q10"] <- Q10
        outDF[outDF$year == j, "Q1_occurrence"] <- Q1_occurrence
        outDF[outDF$year == j, "Q5_occurrence"] <- Q5_occurrence
        outDF[outDF$year == j, "Q10_occurrence"] <- Q10_occurrence
    }

    
    # write output
    write.csv(outDF, outName, row.names=F)
    
}
