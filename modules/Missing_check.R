##############################################################################################################
## Filter data for Year range > 10 for long-term trend analysis 
## Also check for missing data issue, missing data should not be > 80%
Missing_check<-function(Datfile, 
                        sourceDir = DAILY.DATA.DIRECTORY, 
                        destDir = DAILY.OUTPUT.DIRECTORY)
{
    # sorting out file names
    dir.create(destDir, showWarnings = FALSE)
    inName <- paste0(sourceDir, "/", Datfile)
    outName <- paste0(destDir, "/", Datfile)
    
    # read in file
    dd <- read.csv(inName)
    dd$date <- as.Date(paste(dd$Year, dd$Month, dd$Day, sep="-"),
                       format = "%Y-%m-%d")
    colnames(dd) <- c("ID", "BM_number", "Year", "Month", "Day", "Rainfall",
                      "period", "Quality", "Date")
    
    # create continuous time series
    t.series <- seq.Date(from = min(dd$date), to = max(dd$date),
                         by = "day")
    
    # check for missing data %
    target <- length(t.series)
    
    d2 <- dd[complete.cases(dd),]
    reality <- nrow(d2)
    miss_percent <- (target - reality) / target
    
    # create outout df
    out <- data.frame(t.series, NA, NA, NA, NA)
    
    colnames(out) <- c("date", "Year", "Month", "Day", "Rainfall", "Period", "Quality")
    
    
    out$value <- dd$value[match(out$date, dd$date)]
    
    out$Year <- as.numeric(format(out$date, "%Y"))
    out$Month <- as.numeric(format(out$date, "%m"))
    out$Day <- as.numeric(format(out$date, "%d"))
    out[is.na(out$value), "value"] <- -99.9
    

    


}
