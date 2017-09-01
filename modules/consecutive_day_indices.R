##############################################################################################################
## Compute consecutive dry and wet days based on growing season information
consecutive_day_indices<-function(Datfile,
                                  sourceDir = DAILY.DATA.DIRECTORY, 
                                  destDir = DAILY.OUTPUT.DIRECTORY) {
    ## This function calcualtes consecutive day days indices 
    ## based on seasonal information;
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
    
    # check if the location is in southern hemisphere
    if (s.date < e.date) {
        # growing season starts before the ending date within a year
        
        # prepare output df
        outDF <- data.frame(unique(dd$year), NA, NA, NA, 
                            NA, NA, NA)
        colnames(outDF) <- c("year", "dry_before", "dry_growing", "dry_after",
                             "wet_before", "wet_growing", "wet_after")
        
        # growing period
        g.period <- e.date - s.date + 1
        b.period <- s.date - 0
        a.period <- 366 - e.date + 1
        
        # count # days 
        for (j in outDF$year) {
            # extract the three periods
            before_g <- subset(dd[dd$year == j,], doy < s.date)
            during_g <- subset(dd[dd$year == j,], doy >= s.date & doy <= e.date)
            after_g <- subset(dd[dd$year == j,],  doy > e.date)
            
            # consecutive dry days in the three periods
            dry_before <- rle(before_g$prcp)
            dry_during <- rle(during_g$prcp)
            dry_after <- rle(after_g$prcp)
            
            outDF[outDF$year == j, "dry_before"] <- max(dry_before$lengths[dry_before$values==0]) / b.period
            outDF[outDF$year == j, "dry_growing"] <- max(dry_during$lengths[dry_during$values==0]) / g.period
            outDF[outDF$year == j, "dry_after"] <- max(dry_after$lengths[dry_after$values==0]) / a.period
            
            # consecutive wet days in the three periods
            wet_before <- rle(before_g$prcp)
            wet_during <- rle(during_g$prcp)
            wet_after <- rle(after_g$prcp)
            
            outDF[outDF$year == j, "wet_before"] <- max(wet_before$lengths[wet_before$values>0]) / b.period
            outDF[outDF$year == j, "wet_growing"] <- max(wet_during$lengths[wet_during$values>0]) / g.period
            outDF[outDF$year == j, "wet_after"] <- max(wet_after$lengths[wet_after$values>0]) / a.period
        }
    } else {
        # southern hemisphere, need to take one year out (1st year)
        
        # prepare output df
        outDF <- data.frame(unique(dd$year), NA, NA, NA, 
                            NA, NA, NA)
        colnames(outDF) <- c("year", "dry_before", "dry_growing", "dry_after",
                             "wet_before", "wet_growing", "wet_after")
        
        outDF <- outDF[-1,]
        
        # growing period
        g.period <- 366 - s.date + e.date
        b.period <- s.date - 181 
        a.period <- 181 - e.date
        
        # count # days 
        for (j in outDF$year) {
            # extract the three periods
            before_g <- subset(dd[dd$year == (j - 1), ], doy < s.date & doy >= 181)
            d1 <- subset(dd[dd$year == (j - 1), ], doy >= s.date)
            d2 <- subset(dd[dd$year == j, ], doy <= e.date)
            during_g <- rbind(d1, d2)
            after_g <- subset(dd[dd$year == j,],  doy > e.date & doy < 181)
            
            # consecutive dry days in the three periods
            dry_before <- rle(before_g$prcp)
            dry_during <- rle(during_g$prcp)
            dry_after <- rle(after_g$prcp)
            
            outDF[outDF$year == j, "dry_before"] <- max(dry_before$lengths[dry_before$values==0]) / b.period
            outDF[outDF$year == j, "dry_growing"] <- max(dry_during$lengths[dry_during$values==0]) / g.period
            outDF[outDF$year == j, "dry_after"] <- max(dry_after$lengths[dry_after$values==0]) / a.period
            
            # consecutive wet days in the three periods
            wet_before <- rle(before_g$prcp)
            wet_during <- rle(during_g$prcp)
            wet_after <- rle(after_g$prcp)
            
            outDF[outDF$year == j, "wet_before"] <- max(wet_before$lengths[wet_before$values>0]) / b.period
            outDF[outDF$year == j, "wet_growing"] <- max(wet_during$lengths[wet_during$values>0]) / g.period
            outDF[outDF$year == j, "wet_after"] <- max(wet_after$lengths[wet_after$values>0]) / a.period
        }
    }
    
    # write output
    write.csv(outDF, outName)
    
    print(paste0("finish k loop ", k))
}
