year_2018_rainfall_generation <- function(DatFile, 
                                          sourceDir = "Analyses/Gap_Filled", 
                                          destDir = "Analyses/Year_2018_rainfall") {
    #### Criteria:
    #### 1. Pick years when annual prec is 650 - 750 mm/yr
    #### 2. Calculate the consecutive dry and wet day indices for summer only, for those years
    #### 3. Get an all-time averages for one consecutive dry and wet day estimates for summer
    #### 4. Summer is defined as Dec, Jan and Feb
    #### 5. Create randomly a total of 60 rainfall days in summer (out of 90 days)
    #### 6. The total amount of rainfall = 300 mm
    #### 7. Out of the 60 rainfall events, 30 are small (1 - 5 mm)
    ####                                   20 are medium (5 - 30 mm)
    ####                                   10 are large (30 - 80 mm)
    #### 8. Let the consecutive dry and wet days in this new dataset following the distribution of historic records
    
    # prepare file list
    dir.create(destDir, showWarnings = FALSE)
    
    # prepare output list
    inName <- paste0(sourceDir, "/", Datfile)
    outName <- paste0(destDir, "/", Datfile)
    outName1 <- gsub(".csv", "_dry.csv", outName)
    outName2 <- gsub(".csv", "_wet.csv", outName)
    
    # read in file and prepare the df
    dd <- read.csv(inName)
    colnames(dd)<-c("date","year","month","day","prcp")
    dd$doy <- yday(dd$date)
    
    # set wet and dry conditions
    dd$wet_or_dry <- ifelse(dd$prcp==0, 0, 1)
    
    # prepare the looping year list
    yr.list <- unique(dd$year)
    l <- length(yr.list)
    yr.list <- yr.list[-1]
    
    # create an annual total rainfall df
    ann_tot_DF <- data.frame(yr.list, NA)
    colnames(ann_tot_DF) <- c("year", "ann_tot")
    
    # calcualte annual rainfall and extract only useful years
    # i.e. ann tot <= 750 and >= 650 mm
    for (i in yr.list) {
        # extract annual df
        jannov <- subset(dd[dd$year == i,], month >= 1 & month <= 11)
        annDF <- rbind(dec, jannov)
        
        # calculate summer and annual total rainfall
        ann_tot_DF[ann_tot_DF$year == i, "ann_tot"] <- sum(annDF$prcp)
    }
    
    ann_tot_DF_upd <- subset(ann_tot_DF, ann_tot >= 650 & ann_tot <= 750)
    yr.list.upd <- unique(ann_tot_DF_upd$year)
    
    # create output DF
    dryDF <- matrix(ncol = length(yr.list.upd)+1, nrow = 90)
    colnames(dryDF) <- c("cons_days", paste0("yr_", yr.list.upd))
    dryDF <- as.data.frame(dryDF)
    dryDF$cons_days <- c(1:90)
    wetDF <- dryDF
        
    # count # days 
    for (j in yr.list.upd) {
        
        col.name <- paste0("yr_", j)
        
        # extract summer period
        dec <- subset(dd[dd$year == j-1,], month == 12)
        janfeb <- subset(dd[dd$year == j,], month >= 1 & month <= 2)
        sumDF <- rbind(dec, janfeb)
        
        # consecutive dry days in each season and year
        sum_cons <- rle(sumDF$wet_or_dry)

        # sorting
        sum_cons_dry <- sort(sum_cons$lengths[sum_cons$values == 0], decreasing = TRUE)
        sum_cons_wet <- sort(sum_cons$lengths[sum_cons$values > 0], decreasing = TRUE)
        
        # checking frequencies
        dry.tab <- as.data.frame(table(sum_cons_dry))
        wet.tab <- as.data.frame(table(sum_cons_wet))
        
        # assigning onto output df
        for (k in dry.tab$sum_cons_dry) {
            dryDF[dryDF$cons_days == k, col.name] <- dry.tab[dry.tab$sum_cons_dry == k, "Freq"]
        }
        
        for (k in wet.tab$sum_cons_wet) {
            wetDF[wetDF$cons_days == k, col.name] <- wet.tab[wet.tab$sum_cons_wet == k, "Freq"]
        }
    }
    
    # ignore all NA rows
    wet_na_row <- apply(wetDF, 1, function(x) sum(is.na(x)))
    dry_na_row <- apply(dryDF, 1, function(x) sum(is.na(x)))
    
    # write output
    write.csv(dryDF[1:17,], outName1, row.names=F)
    write.csv(wetDF[1:12,], outName2, row.names=F)
    
    # prepare dataframe 
    wetDF <- wetDF[1:12,]
    dryDF <- dryDF[1:17,]
    
    ## to randomly sample from these data frame to generate one wet and dry time series
    # NA to 0
    wetDF[is.na(wetDF)] <- 0
    dryDF[is.na(dryDF)] <- 0
    
    # prepare output df that stores predicted wet and dry days
    wet_pred <- data.frame(wetDF$cons_days, NA)
    colnames(wet_pred) <- c("cons_days", "pred")
    
    dry_pred <- data.frame(dryDF$cons_days, NA)
    colnames(dry_pred) <- c("cons_days", "pred")

   for (m in 1:12) {
       # number of days that have m consecutive days
       num_cons <- as.vector(as.matrix(wetDF[wetDF$cons_days == m, 2:16]))
       myfreq <- as.data.frame(table(num_cons))
       myprob <- data.frame(myfreq, NA)
       colnames(myprob) <- c("num_cons", "freq", "prob")
       csum <- sum(myprob$freq)
       myprob$prob <- myprob$freq/csum

       d <- sample(myprob$num_cons,size = 1, prob = myprob$prob)
       
       wet_pred[wet_pred$cons_days == m, "pred"] <- as.character(d)
    }
    
    for (m in 1:17) {
        # number of days that have m consecutive days
        num_cons <- as.vector(as.matrix(dryDF[dryDF$cons_days == m, 2:16]))
        myfreq <- as.data.frame(table(num_cons))
        myprob <- data.frame(myfreq, NA)
        colnames(myprob) <- c("num_cons", "freq", "prob")
        csum <- sum(myprob$freq)
        myprob$prob <- myprob$freq/csum
        
        d <- sample(myprob$num_cons, size = 1, prob = myprob$prob)
        
        dry_pred[dry_pred$cons_days == m, "pred"] <- as.character(d)
    }
    
    wet_pred$pred <- as.numeric(wet_pred$pred)
    dry_pred$pred <- as.numeric(dry_pred$pred)
    
    # down sizing the prediction number of days
    tot_estimate <- sum(wet_pred$cons_days * wet_pred$pred) + sum(dry_pred$cons_days * dry_pred$pred)

    down_ratio <- 90.0/tot_estimate
    
    wet_pred$pred_down <- round(wet_pred$pred * down_ratio,0)
    dry_pred$pred_down <- round(dry_pred$pred * down_ratio,0)
    
    # total number of predicted wet days
    tot_wet <- sum(wet_pred$cons_days * wet_pred$pred_down)
    
    # generate random precipitation events
    # according to 3:2:1 ratio of small, medium, and large precipitation categories
    # i.e. assign 20 days with small prec
    #             12 days with medium prec
    #             6 days with large prec
    # total must equal 300 mm
    sm.value <- seq(1,5, 0.1)
    md.value <- c(6:30)
    lg.value <- c(31:40)
    
    rain.events <- random.sample(sm.value, md.value, lg.value)
    
    # mix and match
    rain.days <- sample(rain.events)
    
    # now create time series prec and no prec days
    
    
}


# sample randomly from prec events and checking conditions
random.sample <- function(sm.value, md.value, lg.value) {
    repeat {
        sm.series <- sample(sm.value, 20)
        md.series <- sample(md.value, 12)
        lg.series <- sample(lg.value, 6)
        out <- c(sm.series, md.series, lg.series)
        tot_rain <- sum(out)
        
        
        # exit if the condition is met
        # 20% variation
        if (tot_rain >= 230 & tot_rain <= 360) break
    }
    return(out)
}
