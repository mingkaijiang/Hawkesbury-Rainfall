##############################################################################################################
## Compute consecutive dry days based on whole time series information
consecutive_day_whole<-function(Datfile,
                                  sourceDir = DAILY.DATA.DIRECTORY, 
                                  destDir = DAILY.OUTPUT.DIRECTORY) {
    ## This function calcualtes consecutive day days indices 
    ## based on whole time series information;
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
    
    # consecutive dry days 
    ent_cons <- rle(dd$prcp)
        
    # total consecutive dry days
    out <- ent_cons$lengths[ent_cons$values==0]

    # write output
    write.csv(out, outName, row.names=F)
    
}
