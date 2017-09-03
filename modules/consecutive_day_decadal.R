##############################################################################################################
## Compute consecutive dry days based on decadal information
consecutive_day_annual<-function(Datfile,
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
    
    # prepare output df
    outDF <- data.frame(unique(dd$year), NA, NA, NA, 
                        NA, NA, NA, NA, NA, NA, NA)
    colnames(outDF) <- c("year", "spr", "sum", "aut", "win", "ann",
                         "spr_pct", "sum_pct", "aut_pct", "win_pct", "ann_pct")

    # count # days 
    for (j in outDF$year) {
        # extract the three periods
        spring <- subset(dd[dd$year == j,], month >= 10 & month <= 12)
        summer <- subset(dd[dd$year == j,], month >= 1 & month <= 3)
        autumn <- subset(dd[dd$year == j,],  month >= 4 & month <= 6)
        winter <- subset(dd[dd$year == j,],  month >= 7 & month <= 9)
        
        # count number of days in each season and year
        spr_days <- length(spring$prcp)
        sum_days <- length(summer$prcp)
        aut_days <- length(autumn$prcp)
        win_days <- length(winter$prcp)
        ann_days <- length(dd[dd$year == j, "prcp"])
        
        # consecutive dry days in each season and year
        spr_cons <- rle(spring$prcp)
        sum_cons <- rle(summer$prcp)
        aut_cons <- rle(autumn$prcp)
        win_cons <- rle(winter$prcp)
        ann_cons <- rle(dd[dd$year == j, "prcp"])
        
        # max consecutive dry days
        outDF[outDF$year == j, "spr"] <- max(spr_cons$lengths[spr_cons$values==0]) 
        outDF[outDF$year == j, "sum"] <- max(sum_cons$lengths[sum_cons$values==0]) 
        outDF[outDF$year == j, "aut"] <- max(aut_cons$lengths[aut_cons$values==0]) 
        outDF[outDF$year == j, "win"] <- max(win_cons$lengths[win_cons$values==0]) 
        outDF[outDF$year == j, "ann"] <- max(ann_cons$lengths[ann_cons$values==0]) 
        
        # percent
        outDF[outDF$year == j, "spr_pct"] <- max(spr_cons$lengths[spr_cons$values==0]) / spr_days
        outDF[outDF$year == j, "sum_pct"] <- max(sum_cons$lengths[sum_cons$values==0]) / sum_days
        outDF[outDF$year == j, "aut_pct"] <- max(aut_cons$lengths[aut_cons$values==0]) / aut_days
        outDF[outDF$year == j, "win_pct"] <- max(win_cons$lengths[win_cons$values==0]) / win_days
        outDF[outDF$year == j, "ann_pct"] <- max(ann_cons$lengths[ann_cons$values==0]) / ann_days
    }

    
    # write output
    write.csv(outDF, outName, row.names=F)
    
}
